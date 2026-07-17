#!/usr/bin/env bash

set -euo pipefail

ROOT="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
APP="${1:-$ROOT/release/Haskellite.app}"
CONTENTS="$APP/Contents"
MACOS="$CONTENTS/MacOS"
FRAMEWORKS="$CONTENTS/Frameworks"
RESOURCES="$CONTENTS/Resources"
EXECUTABLE="$MACOS/haskellite"
SIGN_IDENTITY="${MACOS_SIGN_IDENTITY:--}"
export MACOSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-12.0}"

fail() {
  echo "build-macos: $*" >&2
  exit 1
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || fail "missing command '$1'"
}

if [[ "$(uname -s)" != "Darwin" ]]; then
  fail "this script must run on macOS"
fi

[[ "$APP" == *.app ]] || fail "output path must end in .app"
[[ "$APP" != "/" ]] || fail "refusing to use the filesystem root as output"

for command_name in cabal codesign install_name_tool otool plutil; do
  require_command "$command_name"
done

if ! command -v pkg-config >/dev/null 2>&1 || ! pkg-config --exists sdl2; then
  fail "SDL2 development files are missing; run: brew install sdl2 bzip2 pkg-config"
fi

rm -rf -- "$APP"
mkdir -p "$MACOS" "$FRAMEWORKS" "$RESOURCES/assets" "$RESOURCES/licenses"

echo "Building Haskellite"
BUILD_ARGS=(
  exe:haskellite
  -O2
  --ghc-options=-optl-Wl,-headerpad_max_install_names
)
(
  cd "$ROOT"
  cabal build "${BUILD_ARGS[@]}"
)
BINARY="$(cd "$ROOT" && cabal list-bin "${BUILD_ARGS[@]}")"
[[ -x "$BINARY" ]] || fail "Cabal did not produce an executable at $BINARY"

cp "$BINARY" "$EXECUTABLE"
cp "$ROOT/packaging/Info.plist" "$CONTENTS/Info.plist"
cp "$ROOT/assets/NotoSans-Regular.ttf" "$RESOURCES/assets/NotoSans-Regular.ttf"
cp "$ROOT/LICENSE" "$RESOURCES/licenses/Haskellite-LICENSE.txt"
cp "$ROOT/assets/NotoSans-LICENSE.txt" "$RESOURCES/licenses/NotoSans-LICENSE.txt"
cp "$ROOT/README.md" "$RESOURCES/README.md"

plutil -lint "$CONTENTS/Info.plist" >/dev/null

declare -a MACHO_SOURCE_QUEUE=("$BINARY")
declare -a MACHO_BUNDLE_QUEUE=("$EXECUTABLE")
declare -a PROCESSED=()
declare -a COPIED_SOURCES=()
declare -a COPIED_DESTINATIONS=()
SDL3_ENQUEUED=false
ENQUEUED_DESTINATION=""

already_processed() {
  local candidate="$1"
  local processed
  for processed in "${PROCESSED[@]:-}"; do
    [[ "$processed" == "$candidate" ]] && return 0
  done
  return 1
}

is_system_library() {
  case "$1" in
    /System/Library/*|/usr/lib/*) return 0 ;;
    *) return 1 ;;
  esac
}

expand_loader_path() {
  local value="$1"
  local source_owner="$2"
  value="${value//@loader_path/$(dirname -- "$source_owner")}"
  value="${value//@executable_path/$(dirname -- "$BINARY")}"
  printf '%s\n' "$value"
}

list_rpaths() {
  otool -l "$1" \
    | awk '$1 == "cmd" && $2 == "LC_RPATH" { getline; getline; sub(/^[[:space:]]*path[[:space:]]+/, ""); sub(/[[:space:]]+\(offset.*$/, ""); print }'
}

resolve_dependency() {
  local dependency="$1"
  local source_owner="$2"
  local suffix rpath candidate

  case "$dependency" in
    @loader_path/*|@executable_path/*)
      candidate="$(expand_loader_path "$dependency" "$source_owner")"
      [[ -f "$candidate" ]] && printf '%s\n' "$candidate" && return 0
      ;;
    @rpath/*)
      suffix="${dependency#@rpath/}"
      while IFS= read -r rpath; do
        [[ -n "$rpath" ]] || continue
        rpath="$(expand_loader_path "$rpath" "$source_owner")"
        candidate="$rpath/$suffix"
        [[ -f "$candidate" ]] && printf '%s\n' "$candidate" && return 0
      done < <(
        list_rpaths "$source_owner"
        if [[ "$source_owner" != "$BINARY" ]]; then
          list_rpaths "$BINARY"
        fi
      )
      ;;
    /*)
      [[ -f "$dependency" ]] && printf '%s\n' "$dependency" && return 0
      ;;
  esac

  return 1
}

enqueue_library() {
  local source="$1"
  local name="${2:-$(basename -- "$source")}"
  local destination index known_source

  destination="$FRAMEWORKS/$name"
  known_source=""
  for ((index = 0; index < ${#COPIED_DESTINATIONS[@]}; index++)); do
    if [[ "${COPIED_DESTINATIONS[$index]}" == "$destination" ]]; then
      known_source="${COPIED_SOURCES[$index]}"
      break
    fi
  done

  if [[ -n "$known_source" ]]; then
    [[ "$source" -ef "$known_source" ]] || fail "two different dependencies have the name $name"
  else
    cp -L "$source" "$destination"
    chmod u+w "$destination"
    install_name_tool -id "@rpath/$name" "$destination"
    COPIED_SOURCES+=("$source")
    COPIED_DESTINATIONS+=("$destination")
    MACHO_SOURCE_QUEUE+=("$source")
    MACHO_BUNDLE_QUEUE+=("$destination")
  fi

  ENQUEUED_DESTINATION="$destination"
}

enqueue_sdl3_runtime() {
  local sdl3_libdir sdl3_source

  [[ "$SDL3_ENQUEUED" == true ]] && return 0
  pkg-config --exists sdl3 \
    || fail "the installed SDL2 compatibility library requires SDL3; run: brew install sdl3"
  sdl3_libdir="$(pkg-config --variable=libdir sdl3)"
  sdl3_source="$sdl3_libdir/libSDL3.dylib"
  [[ -f "$sdl3_source" ]] \
    || fail "SDL3 was found, but $sdl3_source is missing"

  echo "Detected sdl2-compat; bundling SDL3"
  enqueue_library "$sdl3_source" "libSDL3.dylib"
  SDL3_ENQUEUED=true
}

copy_dependency() {
  local owner="$1"
  local source_owner="$2"
  local dependency="$3"
  local source name destination bundled_path

  is_system_library "$dependency" && return 0

  source="$(resolve_dependency "$dependency" "$source_owner")" \
    || fail "cannot resolve $dependency required by $source_owner"
  is_system_library "$source" && return 0

  name="$(basename -- "$source")"
  enqueue_library "$source"
  destination="$ENQUEUED_DESTINATION"
  bundled_path="@executable_path/../Frameworks/$name"

  if [[ "$name" == libSDL2*.dylib ]] \
    && grep -a -q "Failed loading SDL3 library" "$source"; then
    enqueue_sdl3_runtime
  fi

  install_name_tool -change "$dependency" "$bundled_path" "$owner"
}

echo "Bundling dynamic libraries"
queue_index=0
while (( queue_index < ${#MACHO_BUNDLE_QUEUE[@]} )); do
  source_macho="${MACHO_SOURCE_QUEUE[$queue_index]}"
  bundled_macho="${MACHO_BUNDLE_QUEUE[$queue_index]}"
  queue_index=$((queue_index + 1))
  already_processed "$bundled_macho" && continue
  PROCESSED+=("$bundled_macho")
  install_id="$(otool -D "$source_macho" 2>/dev/null | tail -n +2 | head -n 1 || true)"

  while IFS= read -r dependency; do
    [[ -z "$dependency" || "$dependency" == "$install_id" ]] && continue
    copy_dependency "$bundled_macho" "$source_macho" "$dependency"
  done < <(otool -L "$source_macho" | tail -n +2 | sed -E 's/^[[:space:]]*([^[:space:]]+).*/\1/')
done

for macho in "${MACHO_BUNDLE_QUEUE[@]}"; do
  while true; do
    rpath=""
    while IFS= read -r candidate_rpath; do
      rpath="$candidate_rpath"
      break
    done < <(list_rpaths "$macho")
    [[ -n "$rpath" ]] || break
    install_name_tool -delete_rpath "$rpath" "$macho"
  done
done

for macho in "${MACHO_BUNDLE_QUEUE[@]}"; do
  while IFS= read -r dependency; do
    [[ -z "$dependency" ]] && continue
    if is_system_library "$dependency"; then
      continue
    fi
    case "$dependency" in
      @executable_path/../Frameworks/*)
        dependency_name="${dependency##*/}"
        [[ -f "$FRAMEWORKS/$dependency_name" ]] || fail "bundle is missing $dependency_name required by $macho"
        ;;
      @rpath/*)
        [[ "$macho" == "$FRAMEWORKS/${dependency##*/}" ]] || fail "unresolved load path $dependency in $macho"
        ;;
      *) fail "unbundled load path $dependency remains in $macho" ;;
    esac
  done < <(otool -L "$macho" | tail -n +2 | sed -E 's/^[[:space:]]*([^[:space:]]+).*/\1/')
done

echo "Signing Haskellite.app"
SIGN_ARGS=(--force --options runtime --sign "$SIGN_IDENTITY")
if [[ "$SIGN_IDENTITY" != "-" ]]; then
  SIGN_ARGS+=(--timestamp)
fi

for dylib in "$FRAMEWORKS"/*; do
  [[ -f "$dylib" ]] || continue
  codesign "${SIGN_ARGS[@]}" "$dylib"
done
codesign "${SIGN_ARGS[@]}" \
  --entitlements "$ROOT/packaging/macos.entitlements" \
  "$APP"

codesign --verify --deep --strict --verbose=2 "$APP"

echo
echo "Created $APP"
if [[ "$SIGN_IDENTITY" == "-" ]]; then
  echo "The app is ad-hoc signed for local use. Copy it to /Applications to install it."
else
  echo "The app is Developer ID signed and ready to be archived for notarization."
fi
