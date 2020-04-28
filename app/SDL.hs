{-# LANGUAGE OverloadedStrings #-}

module SDL where

import M3U8
import M3U8.UI
import M3U8.Util

run :: CLIMode -> IO ()
run cm@Auto{merge_subtitles=merge_subtitles, url=url, out=out, slow=slow, concurrency=concurrency} = do
    ss <- streams url
    showChoices ss "Stream"
    let stream = last (filter (\s -> (streamType s) == Video) ss)
    putStrLn $ "Selected Stream: "++(show stream)
    processStream out concurrency slow stream ss cm
run cm@Interactive = do
    url <- getUserLine "Enter m3u8 link or path"
    fileName <- getUserLine "Enter title"
    concurrency <- getUserInt "Max Concurrency (int, default = 15)" 15
    slowStr <- getUserLine "Slow/Rate-Limited download mode (on/off, default = off)"
    let slow = case slowStr of
                "on" -> True
                _ -> False

    ss <- streams url
    (stream, currStreamUrl) <- getUserChoice (map (\s -> (s, streamUrl s)) ss) "Stream"
    processStream fileName concurrency slow stream ss cm

    _ <- getUserLine "Press Enter to exit..."
    return ()

main :: IO ()
main = do
    current_args <- getArgs "sdl" "0.2.0" "Downloads, decrypts, and merges m3u8 video/audio/subtitle streams"
    run current_args
