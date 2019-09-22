{-# LANGUAGE DeriveDataTypeable #-}

module M3U8.UI
    (
        getArgs,
        CLIMode(..)
    ) where

import System.Console.CmdArgs

data CLIMode = Auto {merge_subtitles :: Bool
                    ,url :: String
                    ,out :: String}
             | Interactive
     deriving (Show, Eq, Data, Typeable)

cli_args :: CLIMode
cli_args = Auto
    {merge_subtitles = True &= help "Download and merge subtitles if present"
    ,url = def &= argPos 0 &= typ "URL"
    ,out = def &= argPos 1 &= typFile}

cli_modes :: String -> String -> CLIMode
cli_modes name desc = (modes [Interactive &= auto, cli_args]) 
                    &= program name
                    &= summary name
                    &= help desc
            
getArgs :: String -> String -> IO CLIMode
getArgs name desc = cmdArgs (cli_modes name desc)
