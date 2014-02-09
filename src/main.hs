module Main where

import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import Midi.Event
import Midi.KeySignature (Key(..), Scale(..), setKeySignatureEvent)
import Midi.TimeSignature (setTimeSignatureEvent)

midiFile = do
  writeMidiHeader
  writeTrackChunk 34           -- sizes of following events:
  setTempoEvent                -- size 7
  setKeySignatureEvent C Major -- size 6
  setTimeSignatureEvent 4 4    -- size 8
  noteOnEvent                  -- size 4
  noteOffEvent                 -- size 5
  trackEndEvent                -- size 4

main = BL.putStr $ runPut midiFile
