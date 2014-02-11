module Midi.ChunkSpec (main, spec) where

import Midi.Chunk (writeHeaderChunk, writeTrackChunk)
import Test.Hspec

main :: IO ()
main = spec

spec :: Spec
spec = do
  describe 
