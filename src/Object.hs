module Object (Object(..)) where

import Data.Data (Typeable)

class Typeable f => Object f where
    get :: String -> f -> Either String f

    asBool :: f -> Either String Bool

    asDouble :: f -> Either String Double

    asString :: f -> Either String String

    asArray :: f -> Either String [f]

    asObject :: f -> Either String f

    wrapInObject :: [(String, f)] -> f

    makeNumber :: Double -> f

    makeBool :: Bool -> f