
module M3U8 
    ( 
        streams,
        segmentUrls
    ) where

import Text.Regex.Posix
import Network.HTTP.Conduit (simpleHttp)

import M3U8.Util

baseUrl :: String -> String
baseUrl url = reverse $ dropWhile (\x -> x /= '/') $ reverse url

fixUrl :: String -> String -> String
fixUrl base url = if (take 4 url == "http") then url else (base++url)

isVideoStreamLine :: String -> Bool
isVideoStreamLine str = take 18 str == "#EXT-X-STREAM-INF:" && contains "RESOLUTION" str

streamsFromStr :: String -> String -> [(String, String)]
streamsFromStr manifestStr url = zip (map snd metas) urls 
    where
        metas = filter (isVideoStreamLine . snd) $ enumerate 0 manifestLines
        urls = map (fixUrl (baseUrl url)) $ map ((!!) manifestLines . (+) 1 . fst) metas
        manifestLines = lines manifestStr

streams :: String -> IO [(String, String)]
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
