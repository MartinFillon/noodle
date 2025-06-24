module Main (main) where

import qualified SerializerSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    SerializerSpec.spec