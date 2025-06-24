module Noodle.Deserializer (Deserializer (..)) where

class Deserializer a where
    getObject :: a -> Either String [(String, a)]

    getNumber :: a -> Either String Double

    getArray :: a -> Either String [a]

    getString :: a -> Either String String

    getBool :: a -> Either String Bool

    getNull :: a -> Either String ()
