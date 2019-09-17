# m3u8 [![Build Status](https://travis-ci.org/cloudcrypt/m3u8.svg?branch=master)](https://travis-ci.org/cloudcrypt/m3u8) ![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/cloudcrypt/m3u8?include_prereleases)
Haskell library for m3u8 stream parsing, downloading, and decrypting.

Provides conversion of segmented WEBVTT subtitle files into consolidated SRT subtitle files (through `srtc`)

### Building
Run `stack build` to build `m3u8` library and executables `sdl` and `srtc`

### Binaries
Available at https://github.com/cloudcrypt/m3u8/releases
- `sdl`: m3u8 video/audio/subtitle stream downloading, supports interactive and cli modes
- `srtc`: Converts segmented (merged) WEBVTT subtitles to SRT format 
