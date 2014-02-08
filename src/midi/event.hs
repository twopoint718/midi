{-# LANGUAGE OverloadedStrings #-}

module Midi.Event where

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

-- Key signature
--
-- [00] [ff] [59] [02] [00] [00]
-- time meta ksig size key  scal
--                     (C)  (maj)
setKeySignatureEvent :: Put
setKeySignatureEvent = do
  putWord8 0x00        -- time (0)
  putWord8 0xff        -- meta event
  putWord8 0x59        -- key signature event
  putWord8 0x02        -- size of arguments
  putWord8 0x00        -- key in num of sharps+/flats- (C)
  putWord8 0x00        -- scale 0=major

-- Time signature

-- [00] [ff] [58] [04] [04]  [02]    [30]  [08]
-- time meta tsig size numer denom   metro 32nds
--                     (4)   (2^2=4) (48)  (8/32 in q.note)
setTimeSignatureEvent :: Put
setTimeSignatureEvent = do
  putWord8 0x00        -- time (0)
  putWord8 0xff        -- meta event
  putWord8 0x58        -- time signature event
  putWord8 0x04        -- size of arguments
  putWord8 0x04        -- numerator (4)
  putWord8 0x02        -- denominator (as 2^x)
  putWord8 0x30        -- metronome (?)
  putWord8 0x08        -- num of 32nd notes to a beat

noteOnEvent :: Put
noteOnEvent = do
  putWord8 1           -- delta time (TODO: Variable Length Value)
  putWord8 0x90        -- 0x9_ = "Note on", 0x_0 = channel 0
  putWord8 60          -- middle C
  putWord8 105         -- max velocity

noteOffEvent :: Put
noteOffEvent = do
  putWord16be 0x816f   -- TODO: var length (239)
  putWord8 0x80        -- 0x8_ = "Note off", 0x_0 = channel 0
  putWord8 60          -- middle C
  putWord8 0           -- max velocity

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
