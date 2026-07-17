# Haskellite architecture

## Design constraints

Haskellite is an offline desktop application with a Haskell application layer.
It must keep rendering responsive while audio and a 600-million-parameter model
run concurrently, recover from partial first-run downloads, and never retain an
unbounded audio stream in memory.

## Components

### `Haskellite.Audio`

This `.hsc` module is a direct Haskell FFI binding to the small SDL2 audio
surface Haskellite needs. Capture uses SDL's pull-mode queue rather than a
foreign callback. A Haskell worker dequeues mono float PCM into a bounded STM
queue. SDL performs device-format conversion to 16 kHz mono float when needed.

### `Haskellite.VAD`

The VAD is a pure Haskell state machine. Each input block produces a normalized
level plus optional `VoiceStarted` and `UtteranceReady` events. It retains a
short pre-roll, applies a minimum speech duration, completes phrases after
trailing silence, and imposes a hard maximum phrase length. Its pure interface
makes timing edge cases deterministic in tests.

### `Haskellite.Controller`

The controller owns two workers per listening session:

- The audio worker consumes PCM blocks, advances the VAD, and writes completed
  utterances into a bounded recognition queue.
- The recognition worker owns ordering, calls Parakeet sequentially, and emits
  transcript segments back to the UI.

Stopping a session flushes the active phrase, closes capture, then drains the
recognition queue before reporting completion.

### `Haskellite.Internal.Sherpa` and `Haskellite.Parakeet`

The internal `.hsc` binding calculates C struct sizes and offsets from the
vendored sherpa-onnx 1.13.2 header at build time. It dynamically loads the
platform runtime at application startup and resolves only eight functions from
the stable offline-recognition ABI. Paths and JSON are passed as UTF-8 bytes.

The public `Parakeet` module owns the runtime handle and recognizer together, so
the dynamic libraries cannot be unloaded before the native recognizer is freed.

### `Haskellite.Runtime`

The runtime manager maps the current OS/architecture to a pinned release asset,
streams downloads to `.part` files, verifies SHA-256, and extracts tar+bzip2 in
Haskell. The installer recursively discovers archive contents instead of
depending on the release archive's outer directory name.

Supported runtime targets are Linux x86_64/aarch64, macOS universal2, and
Windows x86_64/arm64. CPU inference is the portable default.

### Desktop UI and CLI

The SDL2/Dear ImGui desktop loop never performs a download or inference call.
Background workers post typed events through an STM queue, which the UI drains
at the beginning of each frame. A shared UTF-8 text reference backs the editable
transcript.

The same core library powers headless install, diagnostics, microphone check,
and WAV transcription commands. This gives CI and support workflows a path that
does not require a display server.

## Resource ownership

```text
Application
├── SDL window + renderer + font atlas
├── Engine
│   ├── ONNX Runtime dynamic library
│   ├── sherpa-onnx dynamic library
│   └── offline Parakeet recognizer
└── Optional listening session
    ├── SDL capture device
    ├── audio/VAD worker
    └── recognition worker
```

Destruction happens from the leaves upward. Session stop waits for both workers;
engine close destroys the recognizer before unloading libraries; application
close tears down ImGui before SDL.

## Failure behavior

- Interrupted downloads leave only a `.part` file and are retried.
- A checksum mismatch deletes the archive and never extracts it.
- Audio queue saturation drops the newest block instead of growing memory.
- Recognition errors are surfaced beside the transcript; the next phrase can
  still be processed.
- A missing or mismatched native runtime becomes an actionable setup error in
  both the UI and `diagnostics` command.
