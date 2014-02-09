module Midi.KeySignature
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
         deriving (Enum, Eq, Show)

data Scale = Major
           | Minor
           deriving (Enum, Eq, Show)

{-
  Set a key signature event, which takes the following format:

  [00] [ff] [59] [02] [00] [00]
  time meta ksig size key  scal
-}

setKeySignatureEvent :: Key -> Scale -> Put
setKeySignatureEvent key scale = do
  putWord8 0x00               -- time (0)
  putWord8 0xff               -- meta event
  putWord8 0x59               -- key signature event
  putWord8 0x02               -- size of arguments
  putWord8 $ fromKey key      -- key signature
  putWord8 $ fromScale scale  -- scale (major/minor)

fromKey :: Key -> Word8
fromKey k =
  case k of
    C      -> 0
    G      -> 1
    D      -> 2
    A      -> 3
    E      -> 4
    B      -> 5
    FSharp -> 6
    CSharp -> 7
    GSharp -> 8
    DSharp -> 9
    ASharp -> 10
    F      -> 11

fromScale :: Scale -> Word8
fromScale s =
  case s of
    Major -> 0
    Minor -> 1
