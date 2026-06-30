# spotify-player

**A terminal Spotify player built on the [hespot](https://github.com/alyxcial/hespot)
library.** Queue tracks and albums and control playback from a tiny REPL — mpv does
the audio over its JSON IPC socket, hespot does the protocol, crypto and download.

> Educational / interoperability project. Use your own account; audio needs Premium.

## Requirements

- **mpv** — the playback backend (controlled over its IPC socket).
- Cached hespot credentials — run `hespot oauth-login` once.
- The sibling **hespot** checkout next door (wired up in `cabal.project`).

## Usage

```sh
cabal run spotplay
```

Then, at the `spotplay>` prompt:

```text
play <url>     play a track or album now (replaces the queue)
queue <url>    add a track or album to the queue        (alias: add)
pause          toggle pause / resume                    (alias: p)
next | n       next track
prev | b       previous track
stop           stop playback
now            current track + a progress bar
ls             list the queue
help | ?       this help
quit | q       quit
```

A `<url>` is `spotify:track:…` / `spotify:album:…`, an `open.spotify.com` URL, or the
bare base-62 id.

```text
spotplay> play spotify:album:4yP0hdKOZPNshxUOjY0cZj
spotplay> now
  1/14  The Weeknd - Alone Again
  ████░░░░░░░░░░░░░░░░░░░░░░  0:42 / 4:10  playing
spotplay> next
```

## Note on buffering

Audio is streamed over hespot's legacy access-point channel, which is slow — fetching
an 8 MB track takes ~30–60 s (the same speed `spotdl` sees). So each track **buffers
for a while before it starts** (you'll see `buffering NN%`); the queue still plays
through, it's just not instant. The real fix is the CDN download path — resolve the
file's CDN URL via the modern token stack and fetch it over HTTPS — a planned hespot
improvement that would speed this up everywhere.

## Audio output

By default mpv picks your system's audio device. Set `SPOTPLAY_AO` to force an mpv
`--ao` — e.g. `SPOTPLAY_AO=pulse`, or `SPOTPLAY_AO=null` for a silent / headless run.

## Building

```sh
cabal build        # builds against ../hespot (see cabal.project)
cabal run spotplay
```

Requires GHC 9.6 + cabal 3, **mpv**, and the sibling `hespot` checkout.
