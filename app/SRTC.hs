{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SRTC where

import M3U8.Subtitles

import System.Console.CmdArgs

data SRTCArgs = SRTCArgs {file :: String}
     deriving (Show, Eq, Data, Typeable)

srtc_args :: SRTCArgs
srtc_args = SRTCArgs
    {file = def &= argPos 0 &= typFile }
    &= program "srtc"
    &= summary "srtc"
    &= help "Converts segmented (merged) WEBVTT subtitles to SRT format"

main :: IO ()
main = do
    SRTCArgs f <- cmdArgs srtc_args
    _ <- convert f
    return ()
