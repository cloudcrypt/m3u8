{-# LANGUAGE DeriveDataTypeable #-}

module M3U8.UI
    (
        getArgs,
        CLIMode(..)
    ) where

import System.Console.CmdArgs

data CLIMode = Auto {merge_subtitles :: Bool
                    ,slow :: Bool
                    ,concurrency :: Int
                    ,url :: String
                    ,out :: String}
             | Interactive
     deriving (Show, Eq, Data, Typeable)

cli_args :: CLIMode
cli_args = Auto
    {merge_subtitles = True &= help "Download and merge subtitles if present"
    ,slow = False &= help "Slow/Rate-Limited download mode"
    ,concurrency = 15 &= help "Maximum concurrency level (default=15)"
    ,url = def &= argPos 0 &= typ "<URL or PATH>"
    ,out = def &= argPos 1 &= typFile}

cli_modes :: String -> String -> String -> CLIMode
cli_modes name ver desc = (modes [Interactive &= auto, cli_args]) 
                    &= program name
                    &= summary (name++" v"++ver)
                    &= help desc
            
getArgs :: String -> String -> String -> IO CLIMode
getArgs name ver desc = cmdArgs (cli_modes name ver desc)
