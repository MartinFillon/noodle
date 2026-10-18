module PrettyPrintSpec (spec) where

import Noodle.Json (Json (..), parseJson, prettyPrintJson)
import Noodle.Yaml (Yaml (..), parseYaml, prettyPrintYaml)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Pretty Printing" $ do
    describe "Json Pretty Printing" $ do
        it "formats false correctly (not as true)" $
            prettyPrintJson (JBool False) `shouldBe` "false"

        it "formats true correctly" $
            prettyPrintJson (JBool True) `shouldBe` "true"

        it "formats null correctly" $
            prettyPrintJson JNull `shouldBe` "null"

        it "formats empty string correctly" $
            prettyPrintJson (JString "") `shouldBe` "\"\""

        it "formats string with escapes correctly" $
            prettyPrintJson (JString "hello\n\"world\"") `shouldBe` "\"hello\\n\\\"world\\\"\""

        it "formats array of booleans" $
            prettyPrintJson (JArray [JBool True, JBool False]) `shouldBe` "[\n  true,\n  false\n]"

        it "roundtrips with parser" $ do
            let sample = JObject
                    [ ("name", JString "Alice")
                    , ("active", JBool False)
                    , ("score", JNumber 100.0)
                    , ("items", JArray [JNumber 1.0, JNumber 2.0])
                    ]
            parseJson (prettyPrintJson sample) `shouldBe` Right sample

    describe "Yaml Pretty Printing" $ do
        it "formats false correctly" $
            prettyPrintYaml (YBool False) `shouldBe` "false"

        it "formats true correctly" $
            prettyPrintYaml (YBool True) `shouldBe` "true"

        it "formats null correctly" $
            prettyPrintYaml YNull `shouldBe` "null"

        it "formats simple object" $
            prettyPrintYaml (YObject [("key", YString "value")]) `shouldBe` "key: \"value\""

        it "roundtrips with parser" $ do
            let sample = YObject
                    [ ("name", YString "Bob")
                    , ("active", YBool False)
                    , ("score", YNumber 42.0)
                    ]
            parseYaml (prettyPrintYaml sample) `shouldBe` Right sample
