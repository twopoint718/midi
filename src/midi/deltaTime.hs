module Midi.DeltaTime
( DeltaTime(..)
, deltaTime
) where

import Data.Word (Word8)

newtype DeltaTime = DeltaTime { getDeltaTime :: [Word8] }
                    deriving (Eq, Show)

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

  This implementation treats each byte as a digit in a base-128 numerical
  system, and activates the high bit of each byte by adding 128. It first
  finds the highest power of 128 that the given integer can be divided by,
  then finds each digit by a successive decrease in power. A "show"
  pattern is used for extra efficiency.

  I'm not usually one for more comments than code, but the chances of me
  remembering how this sausage is made weeks from now is practically 0.
-}

deltaTime :: Int -> DeltaTime
deltaTime n = DeltaTime $ d ([] ++) n (highestPower n)
  where
     d :: ([Word8] -> [Word8]) -> Int -> Int -> [Word8]
     d bytes remain pow
      | pow > 0 = d (bytes . ([fromIntegral $ 128+t] ++)) r (pow-1)
      | otherwise = bytes [fromIntegral remain]
      where (t,r) = divMod remain $ 128^pow

highestPower :: Int -> Int
highestPower n = h n 0
  where
    h :: Int -> Int -> Int
    h n pow
      | n < 128^pow' = pow
      | otherwise = h n pow'
      where pow' = pow + 1
