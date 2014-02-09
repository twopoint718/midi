{-# LANGUAGE OverloadedStrings #-}

module Event.Event where

import Data.Binary.Put (Put, putByteString, putWord8, putWord16be, putWord32be)
import Data.Word (Word32)

writeMidiHeader :: Put
writeMidiHeader = do
  putByteString "MThd" -- chunk id, always this
  putWord32be   6      -- chunk size (3 Word16s)
  putWord16be   0      -- midi format (zero is easy)
  putWord16be   1      -- number of tracks
  putWord16be   480    -- time division (480 ticks/quarter note)

writeTrackChunk :: Word32 -> Put
writeTrackChunk len = do
  putByteString "MTrk"
  putWord32be   len    -- chunk size

-- Set tempo
--
-- [00] [ff] [51]  [03] [07 a1 20]
-- time meta tempo size 500k=120bpm
setTempoEvent :: Put
setTempoEvent = do
  putWord8 0x00        -- time (0)
  putWord8 0xff        -- meta event
  putWord8 0x51        -- we're setting tempo
  putWord8 0x03        -- size of arguments (3)
  putWord8 0x07        -- tempo (in µs/quarter note)
  putWord8 0xa1
  putWord8 0x20        -- TODO: 3-byte nums (500,000)

-- End of track
--
-- [1a] [ff] [2f] [00]
-- time meta eot  size
-- (26)
trackEndEvent :: Put
trackEndEvent = do
  putWord8 0x1a        -- time (26)
  putWord8 0xff        -- meta event
  putWord8 0x2f        -- end of track
  putWord8 0x00        -- size
