{-# LANGUAGE OverloadedStrings #-}

module Midi.Chunk
( writeHeaderChunk
, writeTrackChunk
) where

import Data.Binary.Put (Put, putByteString, putWord16be, putWord32be)

writeHeaderChunk :: Put
writeHeaderChunk = do
  putByteString "MThd" -- chunk id, always this
  putWord32be   6      -- chunk size (3 Word16s)
  putWord16be   0      -- midi format (zero is easy)
  putWord16be   1      -- number of tracks
  putWord16be   480    -- time division (480 ticks/quarter note)

writeTrackChunk :: Int -> Put
writeTrackChunk len = do
  putByteString "MTrk"
  putWord32be $ fromIntegral len    -- chunk size
