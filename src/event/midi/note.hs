module Event.Midi.Note
( Note(..)
, noteOffEvent
, noteOnEvent
) where

import Data.Binary.Put (Put, putWord8, putWord16be)
import Data.Word (Word8)

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

noteOnEvent :: Note -> Put
noteOnEvent note = do
  putWord8 1    -- delta time (TODO: Variable Length Value)
  putWord8 0x90 -- 0x9_ = "Note on", 0x_0 = channel 0
  putNote  note
  putWord8 105  -- max velocity

noteOffEvent :: Note -> Put
noteOffEvent note = do
  putWord16be 0x816f -- TODO: var length (239)
  putWord8    0x80   -- 0x8_ = "Note off", 0x_0 = channel 0
  putNote     note
  putWord8    0      -- max velocity

putNote :: Note -> Put
putNote = putWord8 . fromIntegral . fromEnum
