{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import GHC.Generics
import Data.List

data Json = JObject [(String, Json)] | JString String | JNumber Double | JBool Bool | Null | JArray [Json]
    deriving (Show)

prettyPrintJson :: Json -> String
prettyPrintJson = prettyPrintJsonWithIndent 0

prettyPrintJsonWithIndent :: Int -> Json -> String
prettyPrintJsonWithIndent indent json = case json of
    JObject kvs ->
        let indentStr = replicate indent ' '
            nextIndent = indent + 2
            entries = [indentStr ++ "  " ++ show k ++ ": " ++ prettyPrintJsonWithIndent nextIndent v | (k, v) <- kvs]
        in "{\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "}"
    JArray xs ->
        let indentStr = replicate indent ' '
            nextIndent = indent + 2
            entries = [replicate nextIndent ' ' ++ prettyPrintJsonWithIndent nextIndent v | v <- xs]
        in "[\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "]"
    JString s -> show s
    JNumber n -> show n
    JBool b   -> if b then "true" else "false"
    Null      -> "null"

merge :: Json -> Json -> Json
merge (JObject x) (JObject y) = JObject (x ++ y)
merge (JArray x) (JArray y) = JArray (x ++ y)
merge (JArray x) y = JArray (y : x)
merge x y = JArray [x, y]

data Test = Test
    { foo :: Double,
      bar :: Bool
    }
    deriving (Show, Generic)

data Person = Person {
    age :: Age,
    name :: String,
    bar' :: Test
} deriving (Show, Generic)

data Age = Age Double deriving (Show, Generic)

data T2 = T2 deriving (Show, Generic)

data T3 deriving (Generic)

class Serialize a where
    toJson :: a -> Json

    default toJson :: (Generic a, GSerialize (Rep a)) => a -> Json
    toJson x = serialize (from x)

class GSerialize a where
    serialize :: a b -> Json

instance GSerialize V1 where
    serialize x = case x of {}

instance GSerialize U1 where
    serialize _ = Null

instance (GSerialize f) => GSerialize (M1 i t f) where
    serialize (M1 x) = serialize x

instance (GSerialize f, GSerialize g) => GSerialize (f :*: g) where
    serialize (f :*: g) = merge (serialize f) (serialize g)

instance (Selector s, GSerialize f) => GSerialize (M1 S s f) where
    serialize a@(M1 x)  | name /= [] = JObject [(name, serialize x)]
                        | otherwise = serialize x
                        where name = selName a

instance (Serialize c) => GSerialize (K1 i c) where
    serialize (K1 x) = toJson x

instance Serialize Double where
    toJson = JNumber

instance Serialize Bool where
    toJson = JBool

instance Serialize String where
    toJson = JString

instance Serialize T2
instance Serialize Age
instance Serialize Test
instance Serialize Person

main :: IO ()
main = do
    print "hello"
