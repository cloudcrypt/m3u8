
module M3U8 
    ( 
        streams,
        segmentUrls,
        Stream(..),
        StreamType(..)
    ) where

import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import Data.List (intercalate, isPrefixOf)
import Data.List.Split
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

import M3U8.Util
import M3U8.Crypto

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
parseMeta str = Map.fromList $ map (\(k, v) -> (k, stripLR '"' v)) $ map (tuplify2 . splitOn "=" . init) matches
    where
        matches = getAllTextMatches ((snd $ splitAtFirst ':' (str++",")) =~ "[^,]+=(([^,\"]+)|(\"[^\"]+\"))," :: AllTextMatches [] String)

getMetaLine :: String -> [String] -> Maybe String
getMetaLine metaType lines = case filter (isPrefixOf ("#"++metaType++":")) lines of
    [] -> Nothing
    (x:_) -> Just x

getMeta :: String -> [String] -> Maybe (Map.Map String String)
getMeta metaType lines = case getMetaLine metaType lines of
    Nothing -> Nothing
    Just metaLine -> Just $ parseMeta metaLine

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

segmentUrlsFromStr :: String -> String -> ([(String, Maybe B.ByteString)], Maybe String)
segmentUrlsFromStr segmentsStr segmentsUrl = ((zip urls ivs), keyUrl)
    where
        segmentsLines = lines segmentsStr
        urls = map (fixUrl (baseUrl segmentsUrl)) $ filter (\x -> head x /= '#') segmentsLines
        keyMeta = getMeta "EXT-X-KEY" segmentsLines
        keyUrl = case keyMeta of
            Nothing -> Nothing
            Just metaMap -> case Map.lookup "URI" metaMap of
                Just val -> Just $ fixUrl (baseUrl segmentsUrl) val
                Nothing -> error "Error: EXT-X-KEY URI not found"
        ivs = case keyMeta of
            Nothing -> replicate (length urls) Nothing
            Just metaMap -> case Map.lookup "IV" metaMap of
                Just val -> error $ "Error: Discreet IV support not implemented (IV="++val++")"
                Nothing -> case getMetaLine "EXT-X-MEDIA-SEQUENCE" segmentsLines of
                    Nothing -> replicate (length urls) (Just zeroIV)
                    Just metaLine -> map (Just . genIV) [(read $ snd $ splitAtFirst ':' metaLine :: Int)..(length urls)]

maybeTplMapper :: B.ByteString -> (String, Maybe B.ByteString) -> (String, Maybe (B.ByteString, B.ByteString))
maybeTplMapper key (s, iv) = case iv of
    Nothing -> (s, Nothing)
    Just val -> (s, Just (key, val))

segmentUrls :: String -> IO [(String, Maybe (B.ByteString, B.ByteString))]
segmentUrls url = do
    segmentHtml <- simpleHttp url
    let segmentStr = toString segmentHtml
    let (segmentIvPairs, keyUrl) = segmentUrlsFromStr segmentStr url
    case keyUrl of
        Nothing -> return $ zip (map fst segmentIvPairs) (replicate (length segmentIvPairs) Nothing)
        Just val -> do
            key <- simpleHttp val
            return $ map (maybeTplMapper key) segmentIvPairs
