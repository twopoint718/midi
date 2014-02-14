module Midi.ByteStream where

import Data.Binary.Put (Put, putWord8)
import Control.Monad (forM_)
import Data.Word (Word8)

class Streaming a where
  wordsFor :: a -> [Word8]
  putsStream :: a -> Put
  putsStream s = forM_ (wordsFor s) putWord8

toWord8 :: (Enum a) => a -> Word8
toWord8 = fromIntegral . fromEnum
