module Midi.DeltaTimeSpec (main, spec) where

import Control.Monad (forM_)
import Data.Word (Word8)
import Midi.DeltaTime (DeltaTime(..), deltaTime)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toMultiByte" $ do
    forM_ multiByteTranslations $ \(original, translated) ->
      it ("tranlates " ++ show original ++ " into " ++ show translated) $ do
        deltaTime original `shouldBe` DeltaTime translated

multiByteTranslations :: [(Int, [Word8])]
multiByteTranslations =
  [ (0x00000000, [0x00])
  , (0x00000040, [0x40])
  , (0x0000007F, [0x7F])
  , (0x00000080, [0x81, 0x00])
  , (0x00002000, [0xC0, 0x00])
  , (0x00003FFF, [0xFF, 0x7F])
  , (0x00004000, [0x81, 0x80, 0x00])
  , (0x00100000, [0xC0, 0x80, 0x00])
  , (0x001FFFFF, [0xFF, 0xFF, 0x7F])
  , (0x00200000, [0x81, 0x80, 0x80, 0x00])
  , (0x08000000, [0xC0, 0x80, 0x80, 0x00])
  , (0x0FFFFFFF, [0xFF, 0xFF, 0xFF, 0x7F]) ]
