
module M3U8.Util
    (
        getUserLine,
        getUserChoice,
        mapPool,
        enumerate,
        contains,
        toString,
        parseStreamInfo,
        removeColons,
        tuplify2,
        valOrAlt,
        splitAtFirst,
        stripLR,
        extension
    ) where

import System.IO (hFlush, stdout)
import Data.Text (Text)
import Data.List (intercalate)
import Data.Char (chr)
import Text.Regex.Posix
import qualified Data.Text as Txt
import qualified Data.ByteString.Lazy as B
import qualified Data.Traversable as T
import qualified Data.Map.Strict as Map
import Control.Concurrent.Async
import Control.Concurrent.MSem
import Data.String.Unicode

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs

printList :: [String] -> IO ()
printList strs = putStrLn $ intercalate "\n" strs

contains :: String -> String -> Bool
contains s1 s2 = Txt.isInfixOf (Txt.pack s1) (Txt.pack s2)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

valOrAlt :: Ord a => Map.Map a b -> b -> a -> b
valOrAlt m alt key = case Map.lookup key m of
                        Just val -> val
                        Nothing -> alt

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

extension :: String -> String
extension str = "."++(reverse $ fst $ splitAtFirst '.' (reverse str))

stripLR :: Char -> String -> String
stripLR c str = case head str == last str && head str == c of
    True -> tail $ init str
    False -> str

removeColons :: String -> String
removeColons str = filter (\c -> c /= ':') str

enumerate :: Int -> [a] -> [(Int, a)]
enumerate start lst = zip [start..(length lst)] lst

toString :: B.ByteString -> String
toString bs = map (chr . fromEnum) (B.unpack bs)

printEnumerated :: (Int, String) -> IO ()
printEnumerated (i, str) = do putStrLn $ unicodeRemoveNoneAscii $ (show i)++":\t"++str

parseStreamInfo :: String -> String
parseStreamInfo str = init $ res++audio
    where
        res = str =~ ("RESOLUTION=[^[:space:],]+," :: String)
        audio = str =~ (",AUDIO=[^[:space:],]+," :: String)

getUserLine :: String -> IO String
getUserLine prompt = do
    putStr $ prompt++": "
    hFlush stdout
    getLine

getUserChoice :: Show a => [(a, String)] -> String -> IO (a, String)
getUserChoice lst choiceTypeStr = do
    putStrLn $ "\nAvailable "++choiceTypeStr++"s:"
    mapM printEnumerated $ enumerate 1 $ map (show . fst) lst
    userStr <- getUserLine "Enter selection"
    let userInt = read userStr :: Int
    putStrLn $ unicodeRemoveNoneAscii $ "\nSelected "++choiceTypeStr++": "++(show $ fst $ lst !! (userInt - 1))
    return $ lst !! (userInt - 1)
