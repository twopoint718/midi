module Main where

import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import Midi.Event.Midi
import Midi.Event.Meta
import Midi.Chunk

midiFile = do
  writeHeaderChunk
  writeTrackChunk 34           -- sizes of following events:
  setTempoEvent                -- size 7
  setKeySignatureEvent C Major -- size 6
  setTimeSignatureEvent 4 4    -- size 8
  noteOnEvent C4               -- size 4
  noteOffEvent C4              -- size 5
  trackEndEvent                -- size 4

main = BL.putStr $ runPut midiFile
