{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SerializerSpec (spec) where

import Prelude hiding (null)
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Scientific (Scientific)
import Numeric.Natural (Natural)
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

data OptionalRecord = OptionalRecord
    { optReq :: Int,
      optField :: Maybe String
    }
    deriving (Generic)

instance Serializer f => Serialize T1 f
instance Serializer f => Serialize T2 f
instance Serializer f => Serialize T3 f
instance Serializer f => Serialize T4 f
instance Serializer f => Serialize OptionalRecord f

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
    it "should serialize a float" $
        serialize (1.5 :: Float) `shouldBe` number @f 1.5
    it "should serialize a char" $
        serialize 'a' `shouldBe` string @f "a"
    it "should serialize ()" $
        serialize () `shouldBe` null @f
    it "should serialize Text" $
        serialize (T.pack "hello text") `shouldBe` string @f "hello text"
    it "should serialize Natural" $
        serialize (42 :: Natural) `shouldBe` number @f 42
    it "should serialize Scientific" $
        serialize (12.34 :: Scientific) `shouldBe` number @f 12.34
    it "should serialize NonEmpty" $
        serialize (1 :| [2, 3] :: NonEmpty Int)
            `shouldBe` array @f [number @f 1, number @f 2, number @f 3]
    it "should serialize Map String Int" $
        serialize (Map.fromList [("key", 100 :: Int)])
            `shouldBe` object @f [("key", number @f 100)]
    it "should serialize Map Text Int" $
        serialize (Map.fromList [(T.pack "key", 100 :: Int)])
            `shouldBe` object @f [("key", number @f 100)]
    it "should serialize Set Int" $
        serialize (Set.fromList [1 :: Int, 2])
            `shouldBe` array @f [number @f 1, number @f 2]
    it "should serialize Seq Int" $
        serialize (Seq.fromList [1 :: Int, 2])
            `shouldBe` array @f [number @f 1, number @f 2]
    it "should serialize a 2-tuple" $
        serialize (1 :: Int, "hello")
            `shouldBe` array @f [number @f 1, string @f "hello"]
    it "should serialize a 3-tuple" $
        serialize (1 :: Int, True, "hello")
            `shouldBe` array @f [number @f 1, bool @f True, string @f "hello"]
    it "should serialize Maybe (Just)" $
        serialize (Just (42 :: Int)) `shouldBe` number @f 42
    it "should serialize Maybe (Nothing)" $
        serialize (Nothing :: Maybe Int) `shouldBe` null @f
    it "should serialize Either (Left)" $
        serialize (Left (42 :: Int) :: Either Int String) `shouldBe` number @f 42
    it "should serialize Either (Right)" $
        serialize (Right "ok" :: Either Int String) `shouldBe` string @f "ok"
    it "should serialize record with Maybe field (Just)" $
        serialize (OptionalRecord 10 (Just "val"))
            `shouldBe` object @f [("optReq", number @f 10), ("optField", string @f "val")]
    it "should serialize record with Maybe field (Nothing)" $
        serialize (OptionalRecord 10 Nothing)
            `shouldBe` object @f [("optReq", number @f 10), ("optField", null @f)]
