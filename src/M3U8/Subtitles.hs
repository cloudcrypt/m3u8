
module M3U8.Subtitles
    (
        convert
    ) where

import Data.List (isPrefixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Text.Regex.PCRE
import Text.Printf

import M3U8.Util (enumerate)

data ST = ST [Block]
    deriving (Show)

data Block = Block (Int, [Cue])
    deriving (Show)

data Cue = Cue (String, [String])
    deriving (Show)

parse :: [String] -> ST
parse xs = parse' xs (ST [])

parse' :: [String] -> ST -> ST
parse' [] st = st
parse' (x:xs) (ST bs)
    | "WEBVTT" `isPrefixOf` x = parse' (drop 1 xs) (addBlock (ST bs) (head xs))
    | isDurationLine x = parse' xs (addCue (ST bs) x)
    | isNumLine x = parse' xs (ST bs)
    | all isSpace x = parse' xs (ST bs)
    | otherwise = parse' xs (addDialog (ST bs) x)

addBlock :: ST -> String -> ST
addBlock (ST bs) str = ST (bs++[Block (i, [])]) where
    i = read (str =~ "\\d+" :: String) :: Int

addCue :: ST -> String -> ST
addCue (ST bs) str = ST ((init bs)++[Block (i, cs++[newCue])]) where
    Block (i, cs) = last bs
    newCue = Cue (str, [])

addDialog :: ST -> String -> ST
addDialog (ST bs) str = ST ((init bs)++[Block (i, (init cs)++[cue'])]) where
    Block (i, cs) = last bs
    Cue (s, ds) = last cs
    cue' = Cue (s, ds++[str])

isDurationLine :: String -> Bool
isDurationLine l = l =~ "^\\d+:\\d+:\\d+\\.\\d+ --> \\d+:\\d+:\\d+\\.\\d+$" :: Bool

isNumLine :: String -> Bool
isNumLine l = l =~ "^\\d+$" :: Bool

data I_ST = I_ST [I_Block]
    deriving (Show)

data I_Block = I_Block (Float, [I_Cue])
    deriving (Show)

data I_Cue = I_Cue (Float, Float, [String])
    deriving (Show)

process :: ST -> I_ST
process (ST bs) = I_ST (map (processBlock offset) bs) where
    Block (offset, _) = head bs

processBlock :: Int -> Block -> I_Block
processBlock offset (Block (i, cs)) = I_Block (offset', (map processCue cs)) where
    offset' = (fromIntegral (i - offset)) / (fromIntegral 90000)

processCue :: Cue -> I_Cue
processCue (Cue (str, ds)) = I_Cue (start, end, ds) where
    [start, end] = map (\x -> parseTimeStamp $ map (\y -> read y :: Float) $ splitOn ":" x) $ filter (\x -> x /= "-->") $ words str

parseTimeStamp :: [Float] -> Float
parseTimeStamp [hh, mm, ss] = (hh * 3600) + (mm * 60) + ss
parseTimeStamp ts = error $ "Error: Illegal timestamp: "++(show ts)

data SRT = SRT [I_Cue]

instance Show SRT where
    show (SRT ics) = intercalate "\n\n" $ map iCueToSrtString $ enumerate 1 ics

iCueToSrtString :: (Int, I_Cue) -> String
iCueToSrtString (i, (I_Cue (start, end, ds))) = (show i)++"\n"++metaLine++"\n"++(intercalate "\n" ds) where
    metaLine = (toTimeStamp start)++" --> "++(toTimeStamp end)

toTimeStamp :: Float -> String
toTimeStamp secs = hhstr++mmstr++(intercalate "," $ splitOn "." ss) where
    hh = (floor $ secs / 3600) :: Int
    hhstr = printf "%02d:" hh
    mm = (floor $ (secs - ((fromIntegral hh) * 3600)) / 60) :: Int
    mmstr = printf "%02d:" mm
    ss = printf "%.3f" (secs - ((fromIntegral hh) * 3600) - ((fromIntegral mm) * 60))

genSRT :: I_ST -> SRT
genSRT (I_ST ibs) = SRT (genSRT' ibs [])

genSRT' :: [I_Block] -> [I_Cue] -> [I_Cue]
genSRT' [] ics = ics
genSRT' ((I_Block (_, [])):xs) ics = genSRT' xs ics
genSRT' ((I_Block (offset, cs)):xs) ics = genSRT' xs (ics++cs') where
    cs' = map (toSrtCue offset) cs

toSrtCue :: Float -> I_Cue -> I_Cue
toSrtCue offset (I_Cue (start, end, ds)) = I_Cue ((offset+start), (offset+end), ds)

convert :: String -> String -> IO ()
convert input output = do
    subtitles <- readFile input
    let converted = show $ genSRT $ process $ parse $ lines subtitles
    writeFile output converted
