{-# LANGUAGE OverloadedStrings #-}

module M3U8.Downloader 
    (
        saveSegments,
        saveSegmentsSlow,
        merge,
        simpleHttpTls,
        readHttpOrPath,
        DecryptMode (Off, ZeroIV, SequentialIV)
    ) where

import System.Directory
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Control.Monad (liftM, when)
import System.Console.AsciiProgress (Options(..), displayConsoleRegions,
                                        isComplete, def, newProgressBar, tick, ProgressBar, complete)

import M3U8.Util
import M3U8.Crypto

import Control.Concurrent (threadDelay)

fileName :: String -> String
fileName url = T.unpack $ head $ T.splitOn (T.pack "?") $ last $ T.splitOn (T.pack "/") (T.pack url)

readHttpOrPath :: String -> IO String
readHttpOrPath path = case (take 7 path) == "http://" || (take 8 path) == "https://" of
                        True -> do
                            httpResponse <- simpleHttpTls path
                            return $ toString httpResponse
                        False -> readFile path

simpleHttpTls :: String -> IO BL.ByteString
simpleHttpTls url = do
    manager <- newManager tlsManagerSettings
    req <- parseUrlThrow url
    let req' = req {requestHeaders = [("user-agent", "curl/7.68.0")]}
    liftM responseBody $ httpLbs req' manager

downloadHttpTimeout :: Manager -> String -> (String -> IO Request) -> IO (Response BL.ByteString)
downloadHttpTimeout manager url requestParser = do
    req <- requestParser url
    let req' = req {responseTimeout = responseTimeoutNone, requestHeaders = [("user-agent", "curl/7.68.0")]}
    httpLbs req' manager

-- getBytes :: Manager -> ProgressBar -> String -> IO BL.ByteString
-- getBytes manager pg url = do
--     bytes <- downloadHttpTimeout manager url
--     tick pg
--     return bytes

-- getAllBytesList :: [String] -> IO [BL.ByteString]
-- getAllBytesList urls = displayConsoleRegions $ do
--     pg <- newProgressBar def { pgTotal = (toInteger $ length urls)
--                              , pgOnCompletion = Just "Download :percent complete in :elapsed seconds"
--                              , pgWidth = 100     
--                              , pgFormat = "Downloading... :percent [:bar] :current/:total " ++
--                                           "(for :elapsed, :eta remaining)"                  
--                              }
--     manager <- newManager tlsManagerSettings
--     bytesList <- mapPool 1 (getBytes manager pg) urls
--     complete pg
--     return bytesList

-- getAllBytes :: [String] -> IO BL.ByteString
-- getAllBytes urls = do
--     bytesList <- getAllBytesList urls
--     return $ BL.concat bytesList

-- saveFile :: String -> BL.ByteString -> IO ()
-- saveFile name bytes = BL.writeFile name bytes

-- download :: String -> [String] -> IO ()
-- download name urls = displayConsoleRegions $ do
--     bytes <- getAllBytes urls
--     saveFile name bytes

-- ########################################

saveSegmentsSlow' :: [String] -> [String] -> Manager -> ProgressBar -> IO [String]
saveSegmentsSlow' [] files _ _ = do
    return files
saveSegmentsSlow' (url:urls) files m pg = do
    resp <- downloadHttpTimeout m url parseRequest
    case statusCode $ responseStatus resp of
        200 -> do
            let bytes = responseBody resp
            let name = ("m3u8-temp-dir/"++(fileName url))
            BL.writeFile name bytes
            tick pg
            saveSegmentsSlow' urls (files++[name]) m pg
        429 -> do
            let headerMap = Map.fromList $ responseHeaders resp
            let retryVal = (read (toString $ BL.fromStrict $ headerMap Map.! "retry-after") :: Int)
            threadDelay (retryVal * 1000000)
            saveSegmentsSlow' (url:urls) files m pg
        val -> error $ "HTTP Error "++(show val)

saveSegmentsSlow :: [String] -> IO [String]
saveSegmentsSlow urls = displayConsoleRegions $ do
    createDirectoryIfMissing True "m3u8-temp-dir"
    pg <- newProgressBar def { pgTotal = (toInteger $ length urls)
                             , pgOnCompletion = Just "Download :percent complete in :elapsed seconds"
                             , pgWidth = 100     
                             , pgFormat = "Downloading segments... :percent [:bar] :current/:total " ++
                                          "(for :elapsed, :eta remaining)"                  
                             }
    manager <- newManager tlsManagerSettings 
    files <- saveSegmentsSlow' urls [] manager pg
    complete pg
    return files

saveSegment :: Manager -> ProgressBar -> String -> IO String
saveSegment m pg url = do
    resp <- downloadHttpTimeout m url parseUrlThrow
    let bytes = responseBody resp
    let name = ("m3u8-temp-dir/"++(fileName url))
    BL.writeFile name bytes
    tick pg
    return name

saveSegments :: [String] -> Int -> IO [String]
saveSegments urls concurrency = displayConsoleRegions $ do
    createDirectoryIfMissing True "m3u8-temp-dir"
    pg <- newProgressBar def { pgTotal = (toInteger $ length urls)
                             , pgOnCompletion = Just "Download :percent complete in :elapsed seconds"
                             , pgWidth = 100     
                             , pgFormat = "Downloading segments... :percent [:bar] :current/:total " ++
                                          "(for :elapsed, :eta remaining)"                  
                             }
    manager <- newManager tlsManagerSettings 
    files <- mapPool concurrency (saveSegment manager pg) urls
    complete pg
    return files

data DecryptMode = Off | ZeroIV | SequentialIV
    deriving (Show, Eq)

appendAndDecrypt :: String -> ProgressBar -> (String, Maybe (BL.ByteString, BL.ByteString)) -> IO ()
appendAndDecrypt output pg (file, cryptInfo) = do
    bytes <- BL.readFile file
    let decrBytes = case cryptInfo of
                        Nothing -> bytes
                        Just (key, iv) -> decryptIV key iv bytes
    BL.appendFile output decrBytes
    removeFile file
    tick pg

merge :: [(String, Maybe (BL.ByteString, BL.ByteString))] -> String -> IO ()
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

-- saveSegmentProgress :: ProgressBar -> String -> BL.ByteString -> IO ()
-- saveSegmentProgress pg name bytes = do
--     BL.appendFile name bytes
--     tick pg

-- saveSegments :: String -> [BL.ByteString] -> IO ()
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
