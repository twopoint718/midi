module Midi.DeltaTime
( toMultiByte
) where

import Data.Word (Word8)

{-
  These are somewhat like UTF-8 multibyte characters, if the high bit
  of any byte is set, then it means another byte follows. Thus, the
  highest single-byte value is: 127 == 0x7F == 0111 1111

  Two-bytes go like this:
  128    == 1000 0001  0111 1111 (0x81 0x7F)
  129    == 1000 0010  0000 0000 (0x82 0x00)
  ...
  16,382 == 1111 1111  0111 1110 (0xFF 0x7E)
  16,383 == 1111 1111  0111 1111 (0xFF 0x7F)
-}

-- TODO: Use a ShowS pattern instead of ++
toMultiByte :: Int -> [Word8]
toMultiByte n = bytes [] n 0
  where
    bytes words remain pow
      | remain >= 128^(pow+1) = bytes words remain (pow+1)
      | pow > 0 = bytes (words ++ [fromIntegral $ 128+t]) r (pow-1)
      | otherwise = words ++ [fromIntegral remain]
      where (t,r) = divMod remain $ 128^pow

