module Event.Midi.KeySignature
( Key(..)
, Scale(..)
, setKeySignatureEvent
) where

import Data.Binary.Put (Put, putWord8)
import Data.Word (Word8)

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

{-
  Set a key signature event, which takes the following format:

  [00] [ff] [59] [02] [00] [00]
  time meta ksig size key  scal
-}

setKeySignatureEvent :: Key -> Scale -> Put
setKeySignatureEvent key scale = do
  putWord8 0x00  -- time (0)
  putWord8 0xff  -- meta event
  putWord8 0x59  -- key signature event
  putWord8 0x02  -- size of arguments
  putKey   key   -- key signature
  putScale scale -- scale (major/minor)

putKey :: Key -> Put
putKey = putWord8 . fromIntegral . fromEnum

putScale :: Scale -> Put
putScale = putWord8 . fromIntegral . fromEnum
