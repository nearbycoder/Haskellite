# Packaging Haskellite

The executable is self-contained except for SDL2 and the platform C++ runtime.
The Parakeet model and sherpa-onnx runtime are deliberately first-run downloads,
so release bundles stay small and the same checksum-pinned installer code is
used on every platform.

## Linux

Build with `cabal build -O2`, copy the executable reported by
`cabal list-bin haskellite`, install `haskellite.desktop`, and declare SDL2 as a
package dependency. AppImage/Flatpak builds should bundle SDL2. The app stores
data below `XDG_DATA_HOME/haskellite`.

## macOS

Place the executable in `Haskellite.app/Contents/MacOS`, copy the Noto font into
the Cabal data directory, and bundle SDL2 as a framework or dylib. Merge the
provided `Info.plist` so macOS displays the microphone permission explanation.
Code-sign after all dylib paths have been fixed with `install_name_tool`.

## Windows

Place `haskellite.exe`, `SDL2.dll`, and the MinGW C++ runtime DLLs in the same
directory. Embed `windows.manifest` for per-monitor DPI awareness. The downloaded
sherpa-onnx and ONNX Runtime DLLs live under the user's application data folder
and are loaded by absolute path.

Always include the project license, Noto font license, and model attribution in
distributed packages.
