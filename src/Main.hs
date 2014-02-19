module Main where

import Midi.Event
import Midi.File (midi)

-- ALMOST THERE! need to write own "putStr" function to inject delay
-- between every bytestring
main :: IO ()
main = midi $ do
  putEvent $ SetKeySignature C Major -- size 6
  putEvent $ SetTimeSignature 4 4    -- size 8
  putEvent $ NoteOn C4               -- size 4
  putEvent $ NoteOff C4              -- size 5
