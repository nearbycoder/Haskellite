# Changelog

## 0.1.0.0 — 2026-07-16

- Initial Haskell desktop application for Linux and macOS.
- Selectable multilingual 600M, English Quality 600M, and English Fast 110M
  NVIDIA Parakeet models with pinned downloads.
- SDL2 microphone capture, Haskell voice segmentation, transcript editing,
  clipboard copy, and text export.
- Global shortcut dictation, compact listening overlay, audio cues,
  focused-field delivery, and background system-tray operation.
- Hotkey listening keeps the previously focused application and input active.
- macOS requests Accessibility permission and posts a complete Command+V chord
  for reliable automatic delivery.
- Append-only per-activation history with copyable recent dictations.
- First-run runtime/model installer with progress and SHA-256 verification.
- Headless diagnostics and WAV transcription commands.
- Windows backend retained as deferred code and excluded from active CI and
  packaging support.
