{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SerializerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import GHC.Generics (Generic)
import Noodle.Json (Json (..))
import Noodle.Serialize (Serialize (serialize))
import Noodle.Serializer (Serializer (..))
import Noodle.Toml (Toml (..))
import Noodle.Yaml (Yaml (..))

spec :: Spec
spec = do
    describe "Json" (testSerializer @Json)
    describe "Yaml" (testSerializer @Yaml)
    describe "Toml" (testSerializer @Toml)

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

class (Serializer f, Eq f, Show f) => TestSerializer f

instance TestSerializer Json
instance TestSerializer Yaml
instance TestSerializer Toml

testSerializer :: forall f. TestSerializer f => Spec
testSerializer = do
    it "should serialize a boolean" $ serialize True `shouldBe` bool @f True
    it "should serialize an int" $ serialize (1 :: Int) `shouldBe` number @f 1
    it "should serialize a double" $
        serialize (42.0 :: Double) `shouldBe` number @f 42.0
    it "should serialize a word" $ serialize (23 :: Word) `shouldBe` number @f 23
    it "should serialize a string" $
        serialize "Hello World" `shouldBe` string @f "Hello World"
    it "should serialize an array of int" $
        serialize ([1, 3, 5, 6] :: [Int])
            `shouldBe` array @f [number @f 1, number @f 3, number @f 5, number @f 6]
    it "should serialize a simple object" $
        serialize (T1 42) `shouldBe` number @f 42
    it "should serialize an object with fields" $
        serialize (T2 42 "simply lovely")
            `shouldBe` object @f [("foo", number @f 42), ("bar", string @f "simply lovely")]
    it "should serialize an object with subojects inside" $
        serialize (T3 (T1 42) (T2 24 "hello") 12)
            `shouldBe` object @f
                [ ("a", number @f 42),
                  ("b", object @f [("foo", number @f 24), ("bar", string @f "hello")]),
                  ("c", number @f 12)
                ]
    it "should serialize an object with multiple constructors" $ do
        serialize (T41 12) `shouldBe` number @f 12
        serialize (T42 True) `shouldBe` bool @f True
