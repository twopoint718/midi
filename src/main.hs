module Main where

import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import Midi.Event

midiFile = do
  writeMidiHeader
  writeTrackChunk 34    -- sizes of following events:
  setTempoEvent         -- size 7
  setKeySignatureEvent  -- size 6
  setTimeSignatureEvent -- size 8
  noteOnEvent           -- size 4
  noteOffEvent          -- size 5
  trackEndEvent         -- size 4

main = BL.putStr $ runPut midiFile

