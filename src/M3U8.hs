
module M3U8 
    ( 
        streams,
        segmentUrls,
        Stream(..),
        StreamType(..)
    ) where

import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import Data.List.Split
import Network.HTTP.Conduit (simpleHttp)

import M3U8.Util

data Stream = Stream { getStreamMeta :: Map.Map String String
                     , getStreamUrl :: String 
                     , getStreamType :: StreamType } deriving (Eq)

instance Show Stream where
    show (Stream s url Video) = "Video Stream: "++(intercalate ", " $ filter (not . null) $ map (valOrAlt s "") ["RESOLUTION","AUDIO"])
    show (Stream s url Audio) = "Audio Stream: "++(valOrAlt s url "AUDIO")

data StreamType = Video | Audio
        deriving (Eq, Show)

toStream :: (Map.Map String String, String) -> Stream
toStream (meta, url) = case Map.member "RESOLUTION" meta of
                            True -> Stream meta url Video
                            False -> Stream meta url Audio

baseUrl :: String -> String
baseUrl url = reverse $ dropWhile (\x -> x /= '/') $ reverse url

fixUrl :: String -> String -> String
fixUrl base url = if (take 4 url == "http") then url else (base++url)

isStreamLine :: String -> Bool
isStreamLine str = take 18 str == "#EXT-X-STREAM-INF:"

parseMeta :: String -> Map.Map String String
parseMeta str = Map.fromList $ map (tuplify2 . splitOn "=" . init) matches
    where
        matches = getAllTextMatches ((drop 18 (str++",")) =~ "[^,]+=(([^,\"]+)|(\"[^\"]+\"))," :: AllTextMatches [] String)

streamsFromStr :: String -> String -> [Stream]
streamsFromStr manifestStr url = map toStream $ zip metas urls 
    where
        metaPairs = filter (isStreamLine . snd) $ enumerate 0 manifestLines
        urls = map (fixUrl (baseUrl url)) $ map ((!!) manifestLines . (+) 1 . fst) metaPairs
        metas = map parseMeta (map snd metaPairs)
        manifestLines = lines manifestStr

streams :: String -> IO [Stream]
streams url = do
    manifestHtml <- simpleHttp url
    let manifestStr = toString manifestHtml
    return $ streamsFromStr manifestStr url

segmentUrlsFromStr :: String -> String -> ([String], Maybe String)
segmentUrlsFromStr segmentsStr segmentsUrl = (urls, keyUrl)
    where
        segmentsLines = lines segmentsStr
        urls = map (fixUrl (baseUrl segmentsUrl)) $ filter (\x -> head x /= '#') segmentsLines
        keyUrl = case filter (\x -> take 25 x == "#EXT-X-KEY:METHOD=AES-128") $ lines segmentsStr of
            [] -> Nothing
            (x:_) -> Just $ fixUrl (baseUrl segmentsUrl) $ drop 5 $ init $ (x =~ ("URI=\"[^[:space:]]+\"" :: String) :: String)

segmentUrls :: String -> IO ([String], Maybe String)
segmentUrls url = do
    segmentHtml <- simpleHttp url
    let segmentStr = toString segmentHtml
    return $ segmentUrlsFromStr segmentStr url
