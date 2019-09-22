
module M3U8 
    ( 
        streams,
        segmentUrls,
        processStream,
        Stream(..),
        StreamType(..)
    ) where

import Text.Regex.Posix
import qualified Text.Regex.PCRE as PCRE
import qualified Data.Map.Strict as Map
import Data.List (intercalate, isPrefixOf)
import Data.List.Split
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

import Control.Monad
import Data.Char
import System.Directory
import System.Process (rawSystem)
import System.Exit (ExitCode(..))

import M3U8.Util
import M3U8.Crypto
import M3U8.Downloader
import M3U8.Subtitles
import M3U8.UI

data Stream = Stream { streamMeta :: Map.Map String String
                     , streamUrl :: String 
                     , streamType :: StreamType } deriving (Eq)

instance Show Stream where
    show (Stream s url Video) = "Video Stream: "++(intercalate ", " $ filter (not . null) $ map (valOrAlt s "") ["RESOLUTION","AUDIO"])
    show (Stream s url Audio) = "Audio Stream: "++(valOrAlt s url "AUDIO")
    show (Stream s url Subtitle) = "Subtitle Stream: "++(intercalate ", " $ filter (not . null) $ map (valOrAlt s "") ["NAME","LANGUAGE"])

data StreamType = Video | Audio | Subtitle
        deriving (Eq, Show)

toStream :: (Map.Map String String, String) -> Stream
toStream (meta, url) = case Map.member "RESOLUTION" meta of
                            True -> Stream meta url Video
                            False -> case meta Map.!? "TYPE" of
                                        Nothing -> Stream meta url Audio
                                        Just "SUBTITLES" -> Stream meta url Subtitle
                                        _ -> error "Error converting meta info map to stream"

baseUrl :: String -> String
baseUrl url = reverse $ dropWhile (\x -> x /= '/') $ reverse url

fixUrl :: String -> String -> String
fixUrl base url = if (take 4 url == "http") then url else (base++url)

isStreamLine :: Map.Map String String -> Bool
isStreamLine m = m_type == "EXT-X-STREAM-INF" || (m_type == "EXT-X-MEDIA" && (m Map.! "TYPE") == "SUBTITLES")
    where
        m_type = m Map.! "MetaType"

isMetaLine :: String -> Bool
isMetaLine str = str PCRE.=~ "^#[A-Z0-9-]+:[A-Z0-9-]+=" :: Bool

parseMeta :: String -> Map.Map String String
parseMeta str = Map.fromList $ (map (\(k, v) -> (k, stripLR '"' v)) $ map (splitAtFirst '=' . init) matches)++[("MetaType", drop 1 $ fst $ splitAtFirst ':' str)]
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

getUrl :: [String] -> (Int, Map.Map String String) -> String
getUrl ls (i, m) = valOrAlt m (ls !! i) "URI"

streamsFromStr :: String -> String -> [Stream]
streamsFromStr manifestStr url = map toStream $ zip (map snd metaPairs) urls 
    where
        manifestLines = lines manifestStr
        metaStrPairs = filter (isMetaLine . snd) $ enumerate 1 manifestLines
        metaPairs = filter (\(i, m) -> isStreamLine m) $ map (\(i, s) -> (i, parseMeta s)) metaStrPairs
        urls = map (fixUrl (baseUrl url)) $ map (getUrl manifestLines) metaPairs

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

-- ##############################

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

mergeSubtitles :: [Stream] -> Stream -> String -> CLIMode -> IO ()
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

processStream :: String -> Stream -> [Stream] -> CLIMode -> IO ()
processStream fileName s ss cm = do
    savedFile <- saveStream fileName (streamUrl s)
    mergeSubtitles (filter (\s -> (streamType s) == Subtitle) ss) s savedFile cm
