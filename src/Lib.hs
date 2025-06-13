{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import Data.Data
import Object (Object (..))
import Serialize (Serialize (..))
import Utils (maybeToEither)

data Json = O [(String, Json)] | S String | N Double | B Bool | Null | A [Json]
    deriving (Data)

instance Show Json where
    show :: Json -> String
    show (S s) = show s
    show (N d) = show d
    show (B False) = "false"
    show (B True) = "true"
    show Null = "null"
    show (A a) = show a
    show (O (x : xs)) = '{' : foldr (\n acc -> acc ++ ',' : disp n) (disp x) xs ++ "}"
      where
        disp (a, b) = show a ++ ':' : show b
    show (O []) = "{}"

instance Object Json where
    get :: String -> Json -> Either String Json
    get s (O n) = maybeToEither (s ++ " not found in object.") $ lookup s n
    get _ _ = Left "Object is not indexable by string."

    asBool :: Json -> Either String Bool
    asBool (B b) = Right b
    asBool _ = Left "Not a boolean"

    asDouble :: Json -> Either String Double
    asDouble (N d) = Right d
    asDouble _ = Left "Not a number"

    asString :: Json -> Either String String
    asString (S s) = Right s
    asString _ = Left "Not a string"

    asArray :: Json -> Either String [Json]
    asArray (A a) = Right a
    asArray _ = Left "Not an array"

    asObject :: Json -> Either String Json
    asObject (O o) = Right (O o)
    asObject _ = Left "Not an object"

    wrapInObject :: [(String, Json)] -> Json
    wrapInObject = O

    makeNumber :: Double -> Json
    makeNumber = N

    makeBool :: Bool -> Json
    makeBool = B

class Deserializable t where
    from :: Object f => f -> Either String t

data Test = Test
    { foo :: Double,
      bar :: Bool
    }
    deriving (Show, Data)

instance Deserializable Test where
    from :: Object f => f -> Either String Test
    from object = get "foo" object >>= (\j -> get "bar" object >>= construct j)
      where
        construct :: Object f => f -> f -> Either String Test
        construct d b = Test <$> asDouble d <*> asBool b

instance Serialize Test Json

main :: IO ()
main = do
    print (to (Test 1 False) :: Json)
