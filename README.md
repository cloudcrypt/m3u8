# m3u8 [![Build Status](https://travis-ci.org/cloudcrypt/m3u8.svg?branch=master)](https://travis-ci.org/cloudcrypt/m3u8)
Haskell library for m3u8 stream parsing, downloading, and decrypting.

Provides conversion of segmented WEBVTT subtitle files into consolidated SRT subtitle files (through `M3U8.Subtitles.convert`)

### Building
`stack build`

### Binaries
Available at https://github.com/cloudcrypt/m3u8/releases
- sdl: m3u8 video/audio/subtitle stream downloading, supports interactive and cli modes
- srtc: Converts segmented (merged) WEBVTT subtitles to SRT format 
