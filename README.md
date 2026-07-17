# Haskellite

Haskellite is a private, cross-platform desktop voice-to-text app written in
Haskell. It records from a microphone, separates speech into natural phrases,
and transcribes locally with
[NVIDIA Parakeet TDT 0.6B v3](https://huggingface.co/nvidia/parakeet-tdt-0.6b-v3).
No account, cloud API,
Python process, or network connection is needed after the one-time model setup.

## What is implemented

- Native desktop UI on Linux, macOS, and Windows through SDL2 and Dear ImGui.
- Float PCM microphone capture with device selection and a live level meter.
- Haskell voice-activity segmentation with pre-roll, configurable sensitivity,
  trailing-silence detection, and a maximum phrase length.
- Local NVIDIA Parakeet TDT 0.6B v3 INT8 inference in 25 European languages.
- Automatic punctuation, capitalization, and language detection from Parakeet.
- Editable transcript, clipboard copy, and timestamped UTF-8 text export.
- First-run model/runtime installer with streaming progress and pinned SHA-256
  checksums.
- Headless install, diagnostics, microphone check, and WAV transcription tools.
- Automated tests for segmentation, WAV handling, checksums, and asset discovery.

The handwritten application and binding code is Haskell (`.hs`/`.hsc`). There
is no C shim and no Python sidecar. Like any Haskell desktop app, Haskellite uses
native system libraries: SDL2 for windows/audio and sherpa-onnx/ONNX Runtime to
execute the NVIDIA model. Those libraries are loaded through Haskell FFI.

## Quick start

Install GHC 9.10.3 and Cabal 3.16 (the easiest route is
[GHCup](https://www.haskell.org/ghcup/)), plus the SDL2 and bzip2 development
packages for your OS.

```bash
cabal update
cabal build all
cabal run haskellite
```

On first launch, choose **Install Parakeet**. The download is about 473 MB and
uses roughly 670 MB after extraction. It is stored in the platform data folder,
not in the source checkout.

Common dependency commands:

```bash
# Ubuntu / Debian
sudo apt install libsdl2-dev libbz2-dev pkg-config g++

# Arch / CachyOS
sudo pacman -S sdl2 bzip2 pkgconf gcc

# macOS
brew install sdl2 bzip2 pkg-config

# Windows, from an MSYS2 CLANG64 shell
pacman -S mingw-w64-clang-x86_64-SDL2 mingw-w64-clang-x86_64-bzip2 \
  mingw-w64-clang-x86_64-pkgconf
```

On Windows, make sure `C:\msys64\clang64\bin` is on `PATH` and its
`lib\pkgconfig` directory is on `PKG_CONFIG_PATH` before running Cabal.

## Using Haskellite

1. Start Haskellite and wait for the status to say **Ready**.
2. Select a microphone or leave **System default** selected.
3. Press **Start listening** and speak naturally.
4. Pause for the configured interval (700 ms by default). Haskellite submits
   the phrase to Parakeet and keeps listening for the next one.
5. Edit, copy, or save the resulting transcript.

The sensitivity slider is in dBFS. Move it toward `-60` for quieter voices and
toward `-20` if background noise triggers recording.

## Command-line tools

```bash
# Install and checksum-verify the runtime and model
cabal run haskellite -- install

# Verify the installed files
cabal run haskellite -- diagnostics

# Verify that the default microphone produces audio
cabal run haskellite -- check-microphone

# Transcribe a WAV file without opening the desktop UI
cabal run haskellite -- transcribe recording.wav
```

WAV input supports mono or multi-channel PCM16, PCM32, and float32. Multi-channel
audio is downmixed in Haskell, and sherpa-onnx resamples to the model rate.

## Architecture

```text
SDL2 microphone
      │ float PCM
      ▼
Haskell capture queue ──► Haskell VAD / phrase segmentation
                                  │ complete utterance
                                  ▼
                         Parakeet worker thread
                                  │ sherpa-onnx C ABI
                                  ▼
                         ONNX Runtime + Parakeet
                                  │ UTF-8 JSON
                                  ▼
                       Haskell transcript state ──► UI / clipboard / text file
```

The UI, audio producer, segmenter, and recognizer are separate threads joined by
bounded STM queues. Slow inference cannot block the SDL event loop, while the
bounded queues prevent unbounded memory growth. See
[docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for ownership and failure details.

## Models, licensing, and privacy

- The model is NVIDIA `parakeet-tdt-0.6b-v3`, converted to ONNX and quantized by
  the sherpa-onnx project. NVIDIA publishes the model under CC BY 4.0.
- sherpa-onnx and ONNX Runtime are native runtime dependencies downloaded from
  the pinned [sherpa-onnx 1.13.2
  release](https://github.com/k2-fsa/sherpa-onnx/releases/tag/v1.13.2).
- Noto Sans is included under the SIL Open Font License 1.1.
- Haskellite sends no telemetry and uploads no audio. Downloads only occur when
  installing missing model/runtime files.

## Development

```bash
cabal build all
cabal test --test-show-details=direct
cabal run haskellite -- diagnostics
```

The CI matrix builds and tests Linux, macOS, and Windows. Release packaging notes
and OS metadata live in [`packaging/`](packaging/README.md).
