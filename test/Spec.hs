module Main (main) where

import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Noodle.Json (Json (..))
import Noodle.Yaml (Yaml (..))
import Noodle.Toml (Toml (..))
import Noodle.Serialize (Serialize (serialize))
import Noodle.Serializer (Serializer (..))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "serialize as Json" jsonSpec
    describe "serialize as Yaml" yamlSpec
    describe "serialize as Toml" tomlSpec

data T1 = T1 Int deriving (Generic)

data T2 = T2
    { foo :: Int,
      bar :: String
    }
    deriving (Generic)

data T3 = T3
    { a :: T1,
      b :: T2,
      c :: Int
    }
    deriving (Generic)

data T4 = T41 Int | T42 Bool deriving (Generic)

instance Serializer f => Serialize T1 f
instance Serializer f => Serialize T2 f
instance Serializer f => Serialize T3 f
instance Serializer f => Serialize T4 f

jsonSpec :: Spec
jsonSpec = do
    it "should serialize a boolean" $ do
        serialize True `shouldBe` (JBool True)
    it "should serialize an int" $ do
        serialize (1 :: Int) `shouldBe` (JNumber 1)
    it "should serialize a double" $ do
        serialize (42.0 :: Double) `shouldBe` (JNumber 42.0)
    it "should serialize a word" $ do
        serialize (23 :: Word) `shouldBe` (JNumber 23)
    it "should serialize a string" $ do
        serialize "Hello World" `shouldBe` (JString "Hello World")
    it "should serialize an array of int" $ do
        serialize ([1, 3, 5, 6] :: [Int])
            `shouldBe` (JArray [JNumber 1, JNumber 3, JNumber 5, JNumber 6])
    it "should serialize a simple object" $ do
        serialize (T1 42) `shouldBe` (JNumber 42)
    it "should serialize an object with fields" $ do
        serialize (T2 42 "simply lovely")
            `shouldBe` (JObject [("foo", JNumber 42), ("bar", JString "simply lovely")])
    it "should serialize an object with subojects inside" $ do
        serialize (T3 (T1 42) (T2 24 "hello") 12)
            `shouldBe` ( JObject
                            [ ("a", JNumber 42),
                              ("b", JObject [("foo", JNumber 24), ("bar", JString "hello")]),
                              ("c", JNumber 12)
                            ]
                       )
    it "should serialize an object with multiple constructors" $ do
        serialize (T41 12) `shouldBe` (JNumber 12)
        serialize (T42 True) `shouldBe` (JBool True)

yamlSpec :: Spec
yamlSpec = do
    it "should serialize a boolean" $ do
        serialize True `shouldBe` (YBool True)
    it "should serialize an int" $ do
        serialize (1 :: Int) `shouldBe` (YNumber 1)
    it "should serialize a double" $ do
        serialize (42.0 :: Double) `shouldBe` (YNumber 42.0)
    it "should serialize a word" $ do
        serialize (23 :: Word) `shouldBe` (YNumber 23)
    it "should serialize a string" $ do
        serialize "Hello World" `shouldBe` (YString "Hello World")
    it "should serialize an array of int" $ do
        serialize ([1, 3, 5, 6] :: [Int])
            `shouldBe` (YArray [YNumber 1, YNumber 3, YNumber 5, YNumber 6])
    it "should serialize a simple object" $ do
        serialize (T1 42) `shouldBe` (YNumber 42)
    it "should serialize an object with fields" $ do
        serialize (T2 42 "simply lovely")
            `shouldBe` (YObject [("foo", YNumber 42), ("bar", YString "simply lovely")])
    it "should serialize an object with subojects inside" $ do
        serialize (T3 (T1 42) (T2 24 "hello") 12)
            `shouldBe` ( YObject
                            [ ("a", YNumber 42),
                              ("b", YObject [("foo", YNumber 24), ("bar", YString "hello")]),
                              ("c", YNumber 12)
                            ]
                       )
    it "should serialize an object with multiple constructors" $ do
        serialize (T41 12) `shouldBe` (YNumber 12)
        serialize (T42 True) `shouldBe` (YBool True)

tomlSpec :: Spec
tomlSpec = do
    it "should serialize a boolean" $ do
        serialize True `shouldBe` (TBool True)
    it "should serialize an int" $ do
        serialize (1 :: Int) `shouldBe` (TNumber 1)
    it "should serialize a double" $ do
        serialize (42.0 :: Double) `shouldBe` (TNumber 42.0)
    it "should serialize a word" $ do
        serialize (23 :: Word) `shouldBe` (TNumber 23)
    it "should serialize a string" $ do
        serialize "Hello World" `shouldBe` (TString "Hello World")
    it "should serialize an array of int" $ do
        serialize ([1, 3, 5, 6] :: [Int])
            `shouldBe` (TArray [TNumber 1, TNumber 3, TNumber 5, TNumber 6])
    it "should serialize a simple object" $ do
        serialize (T1 42) `shouldBe` (TNumber 42)
    it "should serialize an object with fields" $ do
        serialize (T2 42 "simply lovely")
            `shouldBe` (TObject [("foo", TNumber 42), ("bar", TString "simply lovely")])
    it "should serialize an object with subojects inside" $ do
        serialize (T3 (T1 42) (T2 24 "hello") 12)
            `shouldBe` ( TObject
                            [ ("a", TNumber 42),
                              ("b", TObject [("foo", TNumber 24), ("bar", TString "hello")]),
                              ("c", TNumber 12)
                            ]
                       )
    it "should serialize an object with multiple constructors" $ do
        serialize (T41 12) `shouldBe` (TNumber 12)
        serialize (T42 True) `shouldBe` (TBool True)
