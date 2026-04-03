{-# LANGUAGE InstanceSigs #-}

module Noodle.Yaml.Type (Yaml (..)) where

import Noodle.Deserializer (Deserializer (..))
import Noodle.Serializer (Serializer (..))

data Yaml
    = YObject [(String, Yaml)]
    | YString String
    | YNumber Double
    | YBool Bool
    | YNull
    | YArray [Yaml]
    deriving (Show, Eq)

instance Serializer Yaml where
    object :: [(String, Yaml)] -> Yaml
    object = YObject

    string :: String -> Yaml
    string = YString

    number :: Double -> Yaml
    number = YNumber

    bool :: Bool -> Yaml
    bool = YBool

    array :: [Yaml] -> Yaml
    array = YArray

    merge :: Yaml -> Yaml -> Yaml
    merge (YObject x) (YObject y) = YObject (x ++ y)
    merge (YArray x) (YArray y) = YArray (x ++ y)
    merge (YArray x) y = YArray (x ++ [y])
    merge x y = YArray [x, y]

    null :: Yaml
    null = YNull

instance Deserializer Yaml where
    getObject :: Yaml -> Either String [(String, Yaml)]
    getObject (YObject x) = Right x
    getObject _ = Left "Not an object"

    getNumber :: Yaml -> Either String Double
    getNumber (YNumber x) = Right x
    getNumber _ = Left "Not a number"

    getArray :: Yaml -> Either String [Yaml]
    getArray (YArray x) = Right x
    getArray _ = Left "Not an number"

    getString :: Yaml -> Either String String
    getString (YString x) = Right x
    getString _ = Left "Not a string"

    getBool :: Yaml -> Either String Bool
    getBool (YBool b) = Right b
    getBool _ = Left "Not a boolean"

    getNull :: Yaml -> Either String ()
    getNull YNull = Right ()
    getNull _ = Left "Not a null"