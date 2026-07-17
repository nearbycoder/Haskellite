# Packaging Haskellite

The executable is self-contained except for SDL2 and the platform C++ runtime.
The Parakeet model and sherpa-onnx runtime are deliberately first-run downloads,
so release bundles stay small and the same checksum-pinned installer code is
used on every platform.

## Linux

Build with `cabal build -O2`, copy the executable reported by
`cabal list-bin haskellite`, install `haskellite.desktop`, and declare SDL2 as a
package dependency. X11 builds also require X11 and Xtst; Wayland shortcut
registration uses the session D-Bus and XDG Desktop Portal. AppImage/Flatpak
builds should bundle SDL2. The app stores data below
`XDG_DATA_HOME/haskellite` and installs a per-user desktop entry when needed so
the host portal can identify it.

## macOS

Build a movable, ad-hoc-signed application bundle on a Mac with:

```bash
brew install sdl2 bzip2 pkg-config
./packaging/build-macos.sh
cp -R release/Haskellite.app /Applications/
```

The script places the executable in `Contents/MacOS`, copies application data
into `Contents/Resources`, recursively bundles non-system dylibs in
`Contents/Frameworks`, fixes their load paths, and signs the finished bundle.
Pass a different output path as its first argument if needed. The executable
targets the architecture of the Mac performing the build.

To create a release for other Macs, provide a Developer ID Application signing
identity and then submit the resulting bundle to Apple's notary service:

```bash
MACOS_SIGN_IDENTITY='Developer ID Application: Example (TEAMID)' \
  ./packaging/build-macos.sh
ditto -c -k --keepParent release/Haskellite.app release/Haskellite.zip
xcrun notarytool submit release/Haskellite.zip --keychain-profile notarytool --wait
xcrun stapler staple release/Haskellite.app
```

The signing entitlements allow microphone input and loading the checksum-pinned
sherpa-onnx runtime downloaded during first-run setup. Input Monitoring and
Accessibility approval may also be requested for the global shortcut and
focused-field paste.

## Windows

Place `haskellite.exe`, `SDL2.dll`, and the MinGW C++ runtime DLLs in the same
directory. Embed `windows.manifest` for per-monitor DPI awareness. The downloaded
sherpa-onnx and ONNX Runtime DLLs live under the user's application data folder
and are loaded by absolute path.

Always include the project license, Noto font license, and model attribution in
distributed packages.
