# spotify-player

**A terminal Spotify player (TUI) built on the [hespot](https://github.com/alyxcial/hespot)
library.** A brick/vty interface with a now-playing panel, **cover art rendered right in
the terminal**, a progress bar and a queue — mpv plays the audio over its IPC socket,
hespot fetches and decrypts the tracks over the CDN.

> Educational / interoperability project. Use your own account; audio needs Premium.

```text
┌──────────┐  Now playing
│ ▀▀▀▀▀▀▀▀ │  Blinding Lights
│ ▀▀cover▀ │  The Weeknd
│ ▀▀▀▀▀▀▀▀ │
└──────────┘  ████████████░░░░░░░░░░  1:23 / 3:20   playing
──────────────────────────────────────────────────
 > 1. The Weeknd - Blinding Lights
   2. The Weeknd - Starboy
   3. The Weeknd - Save Your Tears
──────────────────────────────────────────────────
 ready   |  space pause · n/p next/prev · ↑↓ select · ⏎ play · a add · q quit
```

## Requirements

- **mpv** — the playback backend (driven over its JSON IPC socket).
- Cached hespot credentials — run `hespot oauth-login` once.
- The sibling **hespot** checkout next door (wired up in `cabal.project`).
- A truecolor / 256-color terminal for the cover art (kitty, wezterm, foot, alacritty…).

## Running

```sh
cabal run spotplay
```

It takes a few seconds to start (launching mpv, connecting, minting tokens), then the
TUI appears.

## Keys

| Key | Action |
| --- | --- |
| `o` or `/` | enter a URL to **play** now |
| `a` | enter a URL to **add** to the queue |
| `space` | pause / resume |
| `n` | next track |
| `p` / `b` | previous track |
| `↑` `↓` / `k` `j` | move the queue selection |
| `⏎` | play the selected queue item |
| `s` | stop |
| `q` / `Esc` | quit |

A URL is `spotify:track:…` / `spotify:album:…`, an `open.spotify.com` link, or the bare
base-62 id. Adding an album expands it into the queue.

## Speed

Tracks are fetched over hespot's **CDN path** (login5 + client-token → `storage-resolve`
→ HTTPS), so buffering is a few seconds, not the ~50 s of the legacy access-point channel.
The first track also pays a one-time token setup at startup.

## Audio output

By default mpv picks your system's audio device. Set `SPOTPLAY_AO` to force an mpv
`--ao` — e.g. `SPOTPLAY_AO=pulse`, or `SPOTPLAY_AO=null` for a silent / headless run.

## Building

```sh
cabal build        # builds against ../hespot (see cabal.project)
cabal run spotplay
```

Requires GHC 9.6 + cabal 3, **mpv**, and the sibling `hespot` checkout.
