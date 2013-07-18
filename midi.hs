{-# LANGUAGE OverloadedStrings #-}

module Midi where

import Data.Binary.Put
import Data.Bits ((.&.))
import Data.Word
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BL

-- These are somewhat like UTF-8 multibyte characters, if the high bit
-- of any byte is set, then it means another byte follows. Thus, the
-- highest single-byte value is: 127 == 0x7F == 0111 1111
--
-- Two-bytes go like this:
-- 128    == 1000 0001  0111 1111 (0x81 0x7F)
-- 129    == 1000 0010  0000 0000 (0x82 0x00)
-- ...
-- 16,382 == 1111 1111  0111 1110 (0xFF 0x7E)
-- 16,383 == 1111 1111  0111 1111 (0xFF 0x7F)
varLenQuantity :: Int -> Int
varLenQuantity n
  | n < 0     = undefined -- fix
  | n < 2^7   = n
  | n < 2^14  = 
  | n < 2^21  =
  | n < 2^28  =
  | otherwise = undefined -- fix

writeMidiHeader :: Put
writeMidiHeader = do
  putByteString "MThd" -- chunk id, always this
  putWord32be   6      -- chunk size (3 Word16s)
  putWord16be   0      -- midi format (zero is easy)
  putWord16be   1      -- number of tracks
  putWord16be   48     -- time division (48 ticks/quarter note)

writeTrackChunk :: Word32 -> Put
writeTrackChunk len = do
  putByteString "MTrk"
  putWord32be   len    -- chunk size

noteOnEvent :: Put
noteOnEvent = do
  putWord8 0           -- delta time (Variable Length Value)
  putWord8 0x90        -- 0x9_ = "Note on", 0x_0 = channel 0
  putWord8 60          -- middle C
  putWord8 127         -- max velocity

noteOffEvent :: Put
noteOffEvent = do
--  putWord8 0xFF        -- delta time (Variable Length Value)
  putWord16be 0xFFFF
  putWord8 0x80        -- 0x8_ = "Note off", 0x_0 = channel 0
  putWord8 60          -- middle C
  putWord8 127         -- max velocity

midiFile = do
  writeMidiHeader
  writeTrackChunk 4
  noteOnEvent
  writeTrackChunk 5
  noteOffEvent

main = BL.putStr $ runPut midiFile
