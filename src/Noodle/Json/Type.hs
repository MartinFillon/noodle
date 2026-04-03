{-# LANGUAGE InstanceSigs #-}

module Noodle.Json.Type (Json (..)) where

import Noodle.Deserializer (Deserializer (..))
import Noodle.Serializer (Serializer (..))

--- | The 'Json' data type represents the various types of values that can be found in a JSON document.
--- It includes constructors for objects (key-value pairs), strings, numbers, booleans, null values, and arrays.
--- This type serves as the core representation of JSON data within the library, allowing for easy manipulation and serialization/deserialization of JSON content.
data Json
    = JObject [(String, Json)]
    | JString String
    | JNumber Double
    | JBool Bool
    | JNull
    | JArray [Json]
    deriving (Show, Eq)

instance Serializer Json where
    object :: [(String, Json)] -> Json
    object = JObject

    string :: String -> Json
    string = JString

    number :: Double -> Json
    number = JNumber

    bool :: Bool -> Json
    bool = JBool

    array :: [Json] -> Json
    array = JArray

    merge :: Json -> Json -> Json
    merge (JObject x) (JObject y) = JObject (x ++ y)
    merge (JArray x) (JArray y) = JArray (x ++ y)
    merge (JArray x) y = JArray (y : x)
    merge x y = JArray [x, y]

    null :: Json
    null = JNull

instance Deserializer Json where
    getObject :: Json -> Either String [(String, Json)]
    getObject (JObject x) = Right x
    getObject _ = Left "Not an object"

    getNumber :: Json -> Either String Double
    getNumber (JNumber x) = Right x
    getNumber _ = Left "Not a number"

    getArray :: Json -> Either String [Json]
    getArray (JArray x) = Right x
    getArray _ = Left "Not an number"

    getString :: Json -> Either String String
    getString (JString x) = Right x
    getString _ = Left "Not a string"

    getBool :: Json -> Either String Bool
    getBool (JBool b) = Right b
    getBool _ = Left "Not a boolean"

    getNull :: Json -> Either String ()
    getNull JNull = Right ()
    getNull _ = Left "Not a null"
