
module M3U8.Crypto 
    ( 
        decrypt,
        decryptIV
    ) where

import Data.Binary
import Data.Int
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI
import Control.Monad (liftM)
import Control.Concurrent.Async
import Control.Exception
import System.Console.AsciiProgress (Options(..), displayConsoleRegions,
                                        isComplete, def, newProgressBar, tick, ProgressBar, complete)

import Codec.Crypto.SimpleAES
import Debug.Trace

import M3U8.Util

-- 16-byte long IV of zeros
zeroIV :: B.ByteString
zeroIV = B.concat [(encode (0 :: Int64)), (encode (0 :: Int64))]

genIV :: Int -> B.ByteString
genIV i = B.concat [(encode (0 :: Int64)), (encode ((fromIntegral i) :: Int64))]

decrypt :: B.ByteString -> B.ByteString -> B.ByteString
decrypt key encrBytes = crypt CBC (B.toStrict key) (B.toStrict zeroIV) Decrypt encrBytes

decryptIV :: B.ByteString -> (Int, B.ByteString) -> B.ByteString
decryptIV key (iv, encrBytes) = crypt CBC (B.toStrict key) (B.toStrict $ genIV iv) Decrypt encrBytes

decryptProgress :: ProgressBar -> B.ByteString -> B.ByteString -> IO BI.ByteString
decryptProgress pg key encrBytes = do
    let decrBytes = B.toStrict $ decrypt key encrBytes
    tick pg
    seq decrBytes $ return decrBytes

decryptParallel :: B.ByteString -> [B.ByteString] -> IO [BI.ByteString]
decryptParallel key encrBytes = displayConsoleRegions $ do
    pg <- newProgressBar def { pgTotal = (toInteger $ length encrBytes)
                             , pgOnCompletion = Just "Decryption :percent complete in :elapsed seconds"
                             , pgWidth = 100     
                             , pgFormat = "Decrypting stream data... :percent [:bar] :current/:total " ++
                                          "(for :elapsed, :eta remaining)"                  
                             }
    mapM (decryptProgress pg key) encrBytes
