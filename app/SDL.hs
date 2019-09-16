{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SDL where

import Control.Monad
import Data.Char
import System.Directory
import System.Process (rawSystem)
import System.Exit (ExitCode(..))

import M3U8
import M3U8.Util
import M3U8.Downloader
import M3U8.Subtitles

import System.Console.CmdArgs

data SDLMode = Auto {merge_subtitles :: Bool
                    ,url :: String
                    ,out :: String}
             | Interactive
     deriving (Show, Eq, Data, Typeable)

sdl_args :: SDLMode
sdl_args = Auto
    {merge_subtitles = True &= help "Download and merge subtitles if present"
    ,url = def &= argPos 0 &= typ "URL"
    ,out = def &= argPos 1 &= typFile}

sdl_modes :: SDLMode
sdl_modes = (modes [Interactive &= auto, sdl_args]) 
            &= program "sdl"
            &= summary "sdl"
            &= help "Downloads, decrypts, and merges m3u8 video/audio/subtitle streams"

getSubtitles :: Stream -> String -> Bool -> IO ()
getSubtitles (Stream s url Subtitle) videoFileName merge_subtitles = do
    subtitleFile <- saveStream ((initFileName videoFileName)++"_subtitle_"++(valOrAlt s "lang" "NAME")) url
    putStrLn $ "Subtitle stream saved to \""++subtitleFile++"\"\n"
    putStrLn "Converting subtitle stream to SRT format..."
    subtitleFile' <- convert subtitleFile
    putStrLn $ "Converted subtitle stream saved to \""++subtitleFile'++"\"\n"
    fileExists <- doesFileExist subtitleFile
    when fileExists (removeFile subtitleFile)
    case merge_subtitles of
        False -> return ()
        True -> do
            exitCode <- rawSystem "mkvmerge" ["-o", ((initFileName videoFileName)++".mkv"), videoFileName, subtitleFile']
            case exitCode of
                ExitSuccess -> do
                    fileExists' <- doesFileExist videoFileName
                    when fileExists' (removeFile videoFileName)
                    putStrLn $ "Merged file saved to \""++(initFileName videoFileName)++".mkv\"\n"
                ExitFailure _ -> return ()

mergeSubtitles :: [Stream] -> Stream -> String -> SDLMode -> IO ()
mergeSubtitles ((Stream s url Subtitle):_) (Stream _ _ Video) videoFileName Auto{merge_subtitles=merge_subtitles} = getSubtitles (Stream s url Subtitle) videoFileName merge_subtitles
mergeSubtitles ((Stream s url Subtitle):_) (Stream _ _ Video) videoFileName Interactive = do
    userMergeSubtitles <- liftM (map toLower) $ getUserLine "Merge subtitles into video file? (yes/no)"
    let merge_subtitles = case userMergeSubtitles of
                            "yes" -> True 
                            "no" -> False
                            _ -> error $ "Error: Unrecognized response: "++userMergeSubtitles
    getSubtitles (Stream s url Subtitle) videoFileName merge_subtitles
mergeSubtitles _ _ _ _ = return ()

saveStream :: String -> String -> IO String
saveStream fileName stream_url = do
    segInfos <- segmentUrls stream_url
    putStrLn $ (show (length segInfos))++" segments found.\n"
    segmentFiles <- saveSegments $ map fst segInfos
    let savedFile = (fileName++(extension $ head segmentFiles))
    merge (zip segmentFiles $ map snd segInfos) savedFile
    putStrLn $ "Stream saved to \""++savedFile++"\"\n"
    return savedFile

processStream :: String -> Stream -> [Stream] -> SDLMode -> IO ()
processStream fileName s ss sm = do
    savedFile <- saveStream fileName (streamUrl s)
    mergeSubtitles (filter (\s -> (streamType s) == Subtitle) ss) s savedFile sm

run :: SDLMode -> IO ()
run sm@Auto{merge_subtitles=merge_subtitles, url=url, out=out} = do
    ss <- streams url
    showChoices ss "Stream"
    let stream = last (filter (\s -> (streamType s) == Video) ss)
    putStrLn $ "Selected Stream: "++(show stream)
    processStream out stream ss sm
run sm@Interactive = do
    url <- getUserLine "Enter m3u8 link"
    fileName <- getUserLine "Enter title"

    ss <- streams url
    (stream, currStreamUrl) <- getUserChoice (map (\s -> (s, streamUrl s)) ss) "Stream"
    processStream fileName stream ss sm

    _ <- getUserLine "Press Enter to exit..."
    return ()

main :: IO ()
main = do
    current_args <- cmdArgs sdl_modes
    run current_args
