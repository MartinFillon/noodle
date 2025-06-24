{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DeserializerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import GHC.Generics (Generic)
import Noodle (Deserialize (..), Deserializer (..))
import Noodle.Json (Json (..))
import Noodle.Serializer (Serializer (..))
import Noodle.Toml (Toml (..))
import Noodle.Yaml (Yaml (..))

spec :: Spec
spec = do
    describe "Json" (testDeserializer @Json)
    describe "Yaml" (testDeserializer @Yaml)
    describe "Toml" (testDeserializer @Toml)

newtype T1 = T1 Int deriving (Generic, Eq, Show)

data T2 = T2
    { foo :: Int,
      bar :: String
    }
    deriving (Generic, Eq, Show)

data T3 = T3
    { a :: T1,
      b :: T2,
      c :: Int
    }
    deriving (Generic, Eq, Show)

data T4 = T41 Int | T42 Bool deriving (Generic, Eq, Show)

instance Deserializer f => Deserialize T1 f
instance Deserializer f => Deserialize T2 f
instance Deserializer f => Deserialize T3 f
instance Deserializer f => Deserialize T4 f

class (Serializer f, Deserializer f, Eq f, Show f) => TestDeserializer f

instance TestDeserializer Json
instance TestDeserializer Yaml
instance TestDeserializer Toml

testDeserializer :: forall f. TestDeserializer f => Spec
testDeserializer = do
    it "should deserialize a boolean" $
        deserialize (bool @f True) `shouldBe` Right True
    it "should deserialize a double" $
        deserialize (number @f 42.0) `shouldBe` Right (42.0 :: Double)
    it "should deserialize a int" $
        deserialize (number @f 42.0) `shouldBe` Right (42 :: Int)
    it "should deserialize a word" $
        deserialize (number @f 42.0) `shouldBe` Right (42 :: Word)
    it "should deserialize a string" $
        deserialize (string @f "Hello World") `shouldBe` Right "Hello World"
    it "should deserialize an array of int" $
        deserialize (array @f [number @f 1, number @f 3, number @f 5, number @f 6])
            `shouldBe` Right ([1, 3, 5, 6] :: [Int])
    it "should deserialize a simple object" $
        deserialize (number @f 42) `shouldBe` Right (T1 42)
    it "should deserialize an object with fields" $
        deserialize
            (object @f [("foo", number @f 42), ("bar", string @f "simply lovely")])
            `shouldBe` Right (T2 42 "simply lovely")
    it "should deserialize an object with subojects inside" $
        deserialize
            ( object @f
                [ ("a", number @f 42),
                  ("b", object @f [("foo", number @f 24), ("bar", string @f "hello")]),
                  ("c", number @f 12)
                ]
            )
            `shouldBe` Right (T3 (T1 42) (T2 24 "hello") 12)
    it "should deserialize an object with multiple constructors" $ do
        deserialize (number @f 12) `shouldBe` Right (T41 12)
        deserialize (bool @f True) `shouldBe` Right (T42 True)
