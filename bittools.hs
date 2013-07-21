module BitTools (toHex, toBin, toHex8) where

import Data.Bits
import Data.Word

toHex :: Int -> String
toHex num = "0x" ++ map showNybble indices
  where
    pick i = 0xf .&. shiftR num i -- select the nybble at index i.
    showNybble i = "0123456789abcdef" !! (pick i)
    indices = reverse [0, 4..(bitSize num - 1)] -- [60,56,52..8,4,0]

toBin :: Int -> String
toBin n = reverse $ map toBit [0..bitSize n - 1]
  where
    toBit i = if n .&. bit i == 0
              then '0'
              else '1'

hexchars = "0123456789abcdef"
toHex8 :: Word8 -> String
toHex8 n = "0x" ++ [n1] ++ [n2]
  where n1 = hexchars !! fromIntegral (0xf .&. (shiftR n 4))
        n2 = hexchars !! fromIntegral (0xf .&. n)
