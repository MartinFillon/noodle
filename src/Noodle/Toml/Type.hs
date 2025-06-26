module Noodle.Toml.Type (Toml (..)) where

import Noodle.Deserializer (Deserializer (..))
import Noodle.Serializer (Serializer (..))

data Toml
    = TObject [(String, Toml)]
    | TString String
    | TNumber Double
    | TBool Bool
    | TNull -- TOML doesn't really support null, this is placeholder
    | TArray [Toml]
    deriving (Show, Eq)

instance Serializer Toml where
    object :: [(String, Toml)] -> Toml
    object = TObject

    string :: String -> Toml
    string = TString

    number :: Double -> Toml
    number = TNumber

    bool :: Bool -> Toml
    bool = TBool

    array :: [Toml] -> Toml
    array = TArray

    merge :: Toml -> Toml -> Toml
    merge (TObject x) (TObject y) = TObject (x ++ y)
    merge (TArray x) (TArray y) = TArray (x ++ y)
    merge (TArray x) y = TArray (x ++ [y])
    merge x y = TArray [x, y]

    null :: Toml
    null = TNull -- TOML doesn't really support null, this is placeholder

instance Deserializer Toml where
    getObject :: Toml -> Either String [(String, Toml)]
    getObject (TObject x) = Right x
    getObject _ = Left "Not an object"

    getNumber :: Toml -> Either String Double
    getNumber (TNumber x) = Right x
    getNumber _ = Left "Not a number"

    getArray :: Toml -> Either String [Toml]
    getArray (TArray x) = Right x
    getArray _ = Left "Not an number"

    getString :: Toml -> Either String String
    getString (TString x) = Right x
    getString _ = Left "Not a string"

    getBool :: Toml -> Either String Bool
    getBool (TBool b) = Right b
    getBool _ = Left "Not a boolean"

    getNull :: Toml -> Either String ()
    getNull TNull = Right ()
    getNull _ = Left "Not a null"
