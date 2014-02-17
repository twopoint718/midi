{-# LANGUAGE OverloadedStrings #-}

module Midi.File
( midi
) where

import Data.Binary.Put (Put, putByteString, putWord16be, putWord32be, runPut)
import qualified Data.ByteString.Lazy as BL
import Midi.Event (Event(EndTrack), putEvent)

data Chunk = Header
           | Track Int

midi :: Put -> IO ()
midi events = BL.putStr . runPut $ do
  putChunk Header
  putChunk $ Track 0
  events
  putEvent EndTrack

putChunk :: Chunk -> Put

putChunk Header = do
  putByteString "MThd" -- chunk id, always this
  putWord32be   6      -- chunk size (3 Word16s)
  putWord16be   0      -- midi format (zero is easy)
  putWord16be   1      -- number of tracks
  putWord16be   480    -- time division (480 ticks/quarter note)

putChunk (Track len) = do
  putByteString "MTrk"
  putWord32be $ fromIntegral len    -- chunk size
