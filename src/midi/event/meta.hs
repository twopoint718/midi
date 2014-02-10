module Midi.Event.Meta
( Key(..)
, Scale(..)
, setKeySignatureEvent
, setTempoEvent
, setTimeSignatureEvent
, trackEndEvent
) where

{-
  Meta events are used for special non-MIDI events, and use the 0xFF status
  that in a MIDI data stream would be used for a System Reset message (a
  System Reset message would not be useful within a MIDI file).

  From: http://www.somascape.org/midi/tech/mfile.html
-}

import Data.Binary.Put (Put, putWord8)
import Data.Word (Word8)

{-
  Set a key signature event, which takes the following format:

  [00] [ff] [59] [02] [00] [00]
  time meta ksig size key  scal
-}

data Key = C
         | G
         | D
         | A
         | E
         | B
         | FSharp
         | CSharp
         | GSharp
         | DSharp
         | ASharp
         | F
         deriving (Enum, Eq, Read, Show)

data Scale = Major
           | Minor
           deriving (Bounded, Enum, Eq, Read, Show)

setKeySignatureEvent :: Key -> Scale -> Put
setKeySignatureEvent key scale = do
    putWord8 0x00  -- time (0)
    putWord8 0xff  -- meta event
    putWord8 0x59  -- key signature event
    putWord8 0x02  -- size of arguments
    putKey   key   -- key signature
    putScale scale -- scale (major/minor)
  where
    putKey = putWord8 . fromIntegral . fromEnum
    putScale = putWord8 . fromIntegral . fromEnum

{-
  Set a tempo event, which takes the following format:

  [00] [ff] [51]  [03] [07 a1 20]
  time meta tempo size 500k=120bpm
-}

setTempoEvent :: Put
setTempoEvent = do
  putWord8 0x00        -- time (0)
  putWord8 0xff        -- meta event
  putWord8 0x51        -- we're setting tempo
  putWord8 0x03        -- size of arguments (3)
  putWord8 0x07        -- tempo (in µs/quarter note)
  putWord8 0xa1
  putWord8 0x20        -- TODO: 3-byte nums (500,000)

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
  where
    -- denominator is input a power of 2; this corrects that confusing notation
    fromDenom d = floor $ logBase (fromIntegral 2) (fromIntegral d)

{-
  End-of-track event, which takes the following format:

 [1a] [ff] [2f] [00]
 time meta eot  size
 (26)
-}

trackEndEvent :: Put
trackEndEvent = do
  putWord8 0x1a        -- time (26)
  putWord8 0xff        -- meta event
  putWord8 0x2f        -- end of track
  putWord8 0x00        -- size
