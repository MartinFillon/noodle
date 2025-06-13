{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}

module Lib where

import Data.Data
import Data.Generics.Aliases

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

class Typeable f => Object f where
    get :: String -> f -> Either String f

    asBool :: f -> Either String Bool

    asDouble :: f -> Either String Double

    asString :: f -> Either String String

    asArray :: f -> Either String [f]

    asObject :: f -> Either String f

    wrapInObject :: [(String, f)] -> f

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x Nothing = Left x
maybeToEither _ (Just x) = Right x

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

class Deserializable t where
    from :: Object f => f -> Either String t

class (Object f, Data t) => Serializable t f where
    to :: t -> f
    to = error "unimplemented"

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

instance Serializable Bool Json where
    to :: Bool -> Json
    to = B

instance {-# OVERLAPPABLE #-} Object f => Serializable Double f where
    to :: Double -> f
    to = error ""

instance {-# OVERLAPPABLE #-} Serializable Double Json where
    to :: Double -> Json
    to = N

double' :: Object f => Double -> f
double' = to

boolean' :: Object f => Bool -> f
boolean' = to

toObject :: (Object f, Data a) => a -> f
toObject =
    mkQ (error "unknown") id
        `extQ` double'
        `extQ` boolean'

getFieldsNameValue :: (Object f, Data a) => a -> [(String, f)]
getFieldsNameValue d = zip (constrFields $ toConstr d) (gmapQ toObject d)