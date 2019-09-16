
module M3U8.Downloader 
    (
        saveSegments,
        merge,
        DecryptMode (Off, ZeroIV, SequentialIV)
    ) where

import System.Directory
import Network.HTTP.Conduit
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Control.Monad (liftM, when)
import System.Console.AsciiProgress (Options(..), displayConsoleRegions,
                                        isComplete, def, newProgressBar, tick, ProgressBar, complete)

import M3U8.Util
import M3U8.Crypto

fileName :: String -> String
fileName url = T.unpack $ last $ T.splitOn (T.pack "/") (T.pack url)

downloadHttpTimeout :: Manager -> String -> IO B.ByteString
downloadHttpTimeout manager url = do
    req <- parseUrlThrow url
    let req' = req {responseTimeout = responseTimeoutNone}
    liftM responseBody $ httpLbs req' manager

getBytes :: Manager -> ProgressBar -> String -> IO B.ByteString
getBytes manager pg url = do
    bytes <- downloadHttpTimeout manager url
    tick pg
    return bytes

getAllBytesList :: [String] -> IO [B.ByteString]
getAllBytesList urls = displayConsoleRegions $ do
    pg <- newProgressBar def { pgTotal = (toInteger $ length urls)
                             , pgOnCompletion = Just "Download :percent complete in :elapsed seconds"
                             , pgWidth = 100     
                             , pgFormat = "Downloading... :percent [:bar] :current/:total " ++
                                          "(for :elapsed, :eta remaining)"                  
                             }
    manager <- newManager tlsManagerSettings
    bytesList <- mapPool 15 (getBytes manager pg) urls
    complete pg
    return bytesList

getAllBytes :: [String] -> IO B.ByteString
getAllBytes urls = do
    bytesList <- getAllBytesList urls
    return $ B.concat bytesList

saveFile :: String -> B.ByteString -> IO ()
saveFile name bytes = B.writeFile name bytes

download :: String -> [String] -> IO ()
download name urls = displayConsoleRegions $ do
    bytes <- getAllBytes urls
    saveFile name bytes

-- ########################################

saveSegment :: Manager -> ProgressBar -> String -> IO String
saveSegment m pg url = do
    bytes <- downloadHttpTimeout m url
    let name = ("m3u8-temp-dir/"++(fileName url))
    B.writeFile name bytes
    tick pg
    return name

saveSegments :: [String] -> IO [String]
saveSegments urls = displayConsoleRegions $ do
    createDirectoryIfMissing True "m3u8-temp-dir"
    pg <- newProgressBar def { pgTotal = (toInteger $ length urls)
                             , pgOnCompletion = Just "Download :percent complete in :elapsed seconds"
                             , pgWidth = 100     
                             , pgFormat = "Downloading segments... :percent [:bar] :current/:total " ++
                                          "(for :elapsed, :eta remaining)"                  
                             }
    manager <- newManager tlsManagerSettings 
    files <- mapPool 15 (saveSegment manager pg) urls
    complete pg
    return files

data DecryptMode = Off | ZeroIV | SequentialIV
    deriving (Show, Eq)

appendAndDecrypt :: String -> ProgressBar -> (String, Maybe (B.ByteString, B.ByteString)) -> IO ()
appendAndDecrypt output pg (file, cryptInfo) = do
    bytes <- B.readFile file
    let decrBytes = case cryptInfo of
                        Nothing -> bytes
                        Just (key, iv) -> decryptIV key iv bytes
    B.appendFile output decrBytes
    removeFile file
    tick pg

merge :: [(String, Maybe (B.ByteString, B.ByteString))] -> String -> IO ()
merge fileInfos name = displayConsoleRegions $ do
    let (str1, str2) = case snd $ head fileInfos of
                        Nothing -> ("Merging segments", "Merge")
                        _ -> ("Decrypting and Merging segments", "Decryption and Merge")
    pg <- newProgressBar def { pgTotal = (toInteger $ length fileInfos)
                             , pgOnCompletion = Just (str2++" :percent complete in :elapsed seconds")
                             , pgWidth = 100     
                             , pgFormat = (str1++"... :percent [:bar] :current/:total ") ++
                                          "(for :elapsed, :eta remaining)"                  
                             }
    -- let files' = enumerate 1 files
    fileExists <- doesFileExist name
    when fileExists (removeFile name)
    mapM_ (appendAndDecrypt name pg) fileInfos
    removeDirectoryRecursive "m3u8-temp-dir"

-- saveSegmentProgress :: ProgressBar -> String -> B.ByteString -> IO ()
-- saveSegmentProgress pg name bytes = do
--     B.appendFile name bytes
--     tick pg

-- saveSegments :: String -> [B.ByteString] -> IO ()
-- saveSegments name bytes = displayConsoleRegions $ do
--     pg <- newProgressBar def { pgTotal = (toInteger $ length bytes)
--                              , pgOnCompletion = Just "File save :percent complete in :elapsed seconds"
--                              , pgWidth = 100     
--                              , pgFormat = "Saving file... :percent [:bar] :current/:total " ++
--                                           "(for :elapsed, :eta remaining)"                  
--                              }
--     fileExists <- doesFileExist name
--     when fileExists (removeFile name)
--     mapM_ (saveSegmentProgress pg name) bytes
