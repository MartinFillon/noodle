module Main (main) where

import qualified DeserializerSpec
import qualified SerializerSpec
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
    describe "Serializer" SerializerSpec.spec
    describe "Deserializer" DeserializerSpec.spec