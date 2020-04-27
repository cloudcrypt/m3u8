{-# LANGUAGE OverloadedStrings #-}

module SDL where

import M3U8
import M3U8.UI
import M3U8.Util

run :: CLIMode -> IO ()
run cm@Auto{merge_subtitles=merge_subtitles, url=url, out=out} = do
    ss <- streams url
    showChoices ss "Stream"
    let stream = last (filter (\s -> (streamType s) == Video) ss)
    putStrLn $ "Selected Stream: "++(show stream)
    processStream out stream ss cm
run cm@Interactive = do
    url <- getUserLine "Enter m3u8 link or path"
    fileName <- getUserLine "Enter title"

    ss <- streams url
    (stream, currStreamUrl) <- getUserChoice (map (\s -> (s, streamUrl s)) ss) "Stream"
    processStream fileName stream ss cm

    _ <- getUserLine "Press Enter to exit..."
    return ()

main :: IO ()
main = do
    current_args <- getArgs "sdl" "Downloads, decrypts, and merges m3u8 video/audio/subtitle streams"
    run current_args
