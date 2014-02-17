module Midi.Event.Meta
( Event (..)
, Key (..)
, Scale (..)
) where

import Data.Binary.Put (Put, putWord8)
import Control.Monad (forM_)
import Data.Word (Word8)

putEvent :: Event -> Put
putEvent e = forM_ (wordsFor e) putWord8

data Event = EndTrack
             | NoteOff Note
             | NoteOn Note
             | SetKeySignature Key Scale
             | SetTempo Int
             | SetTimeSignature Int Int
             deriving (Eq, Show)

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

data Note = C_ | C_' | D_ | D_' | E_ | F_ | F_' | G_ | G_' | A_ | A_' | B_
          | C0 | C0' | D0 | D0' | E0 | F0 | F0' | G0 | G0' | A0 | A0' | B0
          | C1 | C1' | D1 | D1' | E1 | F1 | F1' | G1 | G1' | A1 | A1' | B1
          | C2 | C2' | D2 | D2' | E2 | F2 | F2' | G2 | G2' | A2 | A2' | B2
          | C3 | C3' | D3 | D3' | E3 | F3 | F3' | G3 | G3' | A3 | A3' | B3
          | C4 | C4' | D4 | D4' | E4 | F4 | F4' | G4 | G4' | A4 | A4' | B4
          | C5 | C5' | D5 | D5' | E5 | F5 | F5' | G5 | G5' | A5 | A5' | B5
          | C6 | C6' | D6 | D6' | E6 | F6 | F6' | G6 | G6' | A6 | A6' | B6
          | C7 | C7' | D7 | D7' | E7 | F7 | F7' | G7 | G7' | A7 | A7' | B7
          | C8 | C8' | D8 | D8' | E8 | F8 | F8' | G8 | G8' | A8 | A8' | B8
          | C9 | C9' | D9 | D9' | E9 | F9 | F9' | G9
          deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Scale = Major
           | Minor
           deriving (Bounded, Enum, Eq, Read, Show)

wordsFor :: Event -> [Word8]

wordsFor EndTrack =
  [ 0xff                -- meta event
  , 0x2f                -- end of track
  , 0x00 ]              -- size

wordsFor (NoteOn note) =
  [ 0x00                -- on channel 1
  , toWord8 note        -- note
  , 0 ]                 -- min velocity

wordsFor (NoteOff note) =
  [ 0x00                -- on channel 1
  , toWord8 note        -- note
  , 0 ]                 -- min velocity


wordsFor (SetKeySignature key scale) =
  [ 0xff                -- meta event
  , 0x59                -- key signature event
  , 0x02                -- size of arguments
  , toWord8 key         -- key signature
  , toWord8 scale ]     -- scale (major/minor)

-- TODO: this needs more research
wordsFor (SetTempo tempo) =
  [ 0xff                -- meta event
  , 0x51                -- we're setting tempo
  , 0x03                -- size of arguments (3)
  , 0x07                -- tempo (in µs/quarter note)
  , 0xa1
  , 0x20 ]              -- TODO: 3-byte nums (500,000)

wordsFor (SetTimeSignature numer denom) =
  [ 0xff                -- meta event
  , 0x58                -- time signature event
  , 0x04                -- size of arguments
  , fromIntegral numer  -- numerator
  , denom'              -- denominator
  , 0x30                -- metronome (?)
  , 0x08 ]              -- num of 32nd notes to a beat
  -- denominator is input as a power of 2
  where denom' = floor $ logBase (fromIntegral 2) (fromIntegral denom)

toWord8 :: (Enum a) => a -> Word8
toWord8 = fromIntegral . fromEnum
