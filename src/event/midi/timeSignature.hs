module Event.Midi.TimeSignature
( setTimeSignatureEvent
) where

import Data.Binary.Put (Put, putWord8)
import Data.Word (Word8)

{-
  Set a time signature event, which takes the following format:

  [00] [ff] [58] [04] [04]  [02]  [30]  [08]
  time meta tsig size numer denom metro 32nds
-}

setTimeSignatureEvent :: Int -> Int -> Put
setTimeSignatureEvent numer denom = do
  putWord8 0x00                 -- time (0)
  putWord8 0xff                 -- meta event
  putWord8 0x58                 -- time signature event
  putWord8 0x04                 -- size of arguments
  putWord8 $ fromIntegral numer -- numerator
  putWord8 $ fromDenom denom    -- denominator (as 2^x)
  putWord8 0x30                 -- metronome (?)
  putWord8 0x08                 -- num of 32nd notes to a beat

-- denominator is input a power of 2; this corrects that confusing notation
fromDenom :: Int -> Word8
fromDenom d = floor $ logBase (fromIntegral 2) (fromIntegral d)
