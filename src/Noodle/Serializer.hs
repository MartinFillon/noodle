module Noodle.Serializer (Serializer (..)) where

class Serializer s where
    object :: [(String, s)] -> s

    number :: Double -> s

    bool :: Bool -> s

    array :: [s] -> s

    string :: String -> s

    merge :: s -> s -> s

    null :: s

