module Json.ParserSpec (spec) where

import Data.Either (isLeft)
import Noodle.Json (Json (..), parseJson)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Json Parser" $ do
    describe "Literals" $ do
        it "parses true" $
            parseJson "true" `shouldBe` Right (JBool True)

        it "parses false" $
            parseJson "false" `shouldBe` Right (JBool False)

        it "parses null" $
            parseJson "null" `shouldBe` Right JNull

    describe "Numbers" $ do
        it "parses positive integer" $
            parseJson "42" `shouldBe` Right (JNumber 42.0)

        it "parses negative integer" $
            parseJson "-17" `shouldBe` Right (JNumber (-17.0))

        it "parses zero" $
            parseJson "0" `shouldBe` Right (JNumber 0.0)

        it "parses floating point" $
            parseJson "3.1415" `shouldBe` Right (JNumber 3.1415)

        it "parses scientific notation" $ do
            parseJson "1e3" `shouldBe` Right (JNumber 1000.0)
            parseJson "2.5E2" `shouldBe` Right (JNumber 250.0)
            parseJson "1e-2" `shouldBe` Right (JNumber 0.01)
            parseJson "1e+2" `shouldBe` Right (JNumber 100.0)

    describe "Strings" $ do
        it "parses empty string" $
            parseJson "\"\"" `shouldBe` Right (JString "")

        it "parses simple string" $
            parseJson "\"hello world\"" `shouldBe` Right (JString "hello world")

        it "parses string with escape sequences" $ do
            parseJson "\"\\\"\"" `shouldBe` Right (JString "\"")
            parseJson "\"\\\\\"" `shouldBe` Right (JString "\\")
            parseJson "\"\\/\"" `shouldBe` Right (JString "/")
            parseJson "\"\\b\"" `shouldBe` Right (JString "\b")
            parseJson "\"\\f\"" `shouldBe` Right (JString "\f")
            parseJson "\"\\n\"" `shouldBe` Right (JString "\n")
            parseJson "\"\\r\"" `shouldBe` Right (JString "\r")
            parseJson "\"\\t\"" `shouldBe` Right (JString "\t")

        it "parses combined escape sequence" $
            parseJson "\"line1\\nline2\\ttabbed\""
                `shouldBe` Right (JString "line1\nline2\ttabbed")

    describe "Arrays" $ do
        it "parses empty array" $
            parseJson "[]" `shouldBe` Right (JArray [])

        it "parses single element array" $
            parseJson "[42]" `shouldBe` Right (JArray [JNumber 42.0])

        it "parses multi-element array" $
            parseJson "[1, \"two\", true, null]"
                `shouldBe` Right (JArray [JNumber 1.0, JString "two", JBool True, JNull])

        it "parses nested arrays" $
            parseJson "[[1, 2], [3, 4]]"
                `shouldBe` Right
                    (JArray [JArray [JNumber 1.0, JNumber 2.0], JArray [JNumber 3.0, JNumber 4.0]])

        it "handles whitespace inside array" $
            parseJson "[  1 ,   2 ,  3  ]"
                `shouldBe` Right (JArray [JNumber 1.0, JNumber 2.0, JNumber 3.0])

    describe "Objects" $ do
        it "parses empty object" $
            parseJson "{}" `shouldBe` Right (JObject [])

        it "parses simple object" $
            parseJson "{\"key\": \"value\"}"
                `shouldBe` Right (JObject [("key", JString "value")])

        it "parses multi-key object" $
            parseJson "{\"a\": 1, \"b\": true, \"c\": null}"
                `shouldBe` Right (JObject [("a", JNumber 1.0), ("b", JBool True), ("c", JNull)])

        it "parses nested object" $
            parseJson "{\"outer\": {\"inner\": 42}}"
                `shouldBe` Right (JObject [("outer", JObject [("inner", JNumber 42.0)])])

        it "handles whitespace in object" $
            parseJson " { \n \"foo\" :  123 \n , \n \"bar\" : \"baz\" \n } "
                `shouldBe` Right (JObject [("foo", JNumber 123.0), ("bar", JString "baz")])

    describe "Error handling" $ do
        it "fails on unclosed string" $
            parseJson "\"unclosed" `shouldSatisfy` isLeft

        it "fails on unclosed array" $
            parseJson "[1, 2" `shouldSatisfy` isLeft

        it "fails on unclosed object" $
            parseJson "{\"key\": 1" `shouldSatisfy` isLeft

        it "fails on invalid token" $
            parseJson "invalid" `shouldSatisfy` isLeft
