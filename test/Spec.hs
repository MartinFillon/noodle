module Main (main) where

import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Noodle.Json (Json (..))
import Noodle.Serialize (Serialize (serialize))
import Noodle.Serializer (Serializer (..))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "serialize as Json" (jsonSpec)

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
                              ("b", JObject [("foo", 24), ("bar", "hello")]),
                              ("c", JNumber)
                            ]
                       )
    it "should serialize an object with multiple constructors" $ do
        serialize (T41 12) `shouldBe` (JNumber 12)
        serialize (T42 True) `shouldBe` (JBool True)
