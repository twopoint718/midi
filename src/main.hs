module Main where

import Midi.ByteStream
import Midi.Event.Midi
import Midi.Event.Meta
import Midi.File (midi)

-- ALMOST THERE! need to write own "putStr" function to inject delay
-- between every bytestring
main :: IO ()
main = midi $ do
  putsStream $ SetKeySignature C Major -- size 6
  putsStream $ SetTimeSignature 4 4    -- size 8
  putsStream $ NoteOn C4               -- size 4
  putsStream $ NoteOff C4              -- size 5
