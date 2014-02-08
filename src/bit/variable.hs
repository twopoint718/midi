module Bit.Variable (varLenQuantity, multiByte) where

import Data.Bits ((.&.), setBit, shiftR)
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

varLenQuantity :: Int -> [Word8]
varLenQuantity n
  | n < 0     = undefined -- fix
  | n < 2^7   = pack7Bits n
  | n < 2^14  = multiByte $ pack14Bits n
  | n < 2^21  = multiByte $ pack21Bits n
  | n < 2^28  = multiByte $ pack28Bits n
  | otherwise = undefined -- fix

-- sets the high bit of the first byte (signifies multibyte)
multiByte :: [Word8] -> [Word8]
multiByte (x:xs) = setBit x 7 : xs

mask :: Word8
mask = fromIntegral 0x7f

-- n & 0x7f (01111111b)
pack7Bits :: Int -> [Word8]
pack7Bits n = [fromIntegral n .&. mask]

-- (n >> 7) & 0x7f
pack14Bits :: Int -> [Word8]
pack14Bits n = fromIntegral (shiftR n 7) .&. mask
             : pack7Bits n

-- (n >> 14) & 0x7f
pack21Bits :: Int -> [Word8]
pack21Bits n = fromIntegral (shiftR n 14) .&. mask
             : pack14Bits n
            ++ pack7Bits n

-- (n >> 21) & 0x7f
pack28Bits :: Int -> [Word8]
pack28Bits n = fromIntegral (shiftR n 21) .&. mask
             : pack21Bits n
            ++ pack14Bits n
            ++ pack7Bits n
