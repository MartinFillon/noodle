module Main (main) where

import qualified DeserializerSpec
import qualified Json.ParserSpec
import qualified PrettyPrintSpec
import qualified SerializerSpec
import Test.Hspec (describe, hspec)
import qualified Yaml.ParserSpec

main :: IO ()
main = hspec $ do
    describe "Serializer" SerializerSpec.spec
    describe "Deserializer" DeserializerSpec.spec
    describe "Json Parser" Json.ParserSpec.spec
    describe "Yaml Parser" Yaml.ParserSpec.spec
    describe "PrettyPrint" PrettyPrintSpec.spec
