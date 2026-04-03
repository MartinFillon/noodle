module Noodle.Serializer (Serializer (..)) where

--- | The 'Serializer' type class defines an interface for serializing data structures into a specific format.
--- This type class can be implemented for various serialization formats such as JSON, YAML, TOML, etc.
--- Each method corresponds to a different type of data that can be serialized, such as objects, numbers, booleans, arrays, strings, and null values.
--- The 'merge' method allows for combining two serialized values, which can be useful for merging objects or arrays.
class Serializer s where
    --- | Serialize an object represented as a list of key-value pairs.
    object :: [(String, s)] -> s

    --- | Serialize a number.
    number :: Double -> s

    --- | Serialize a boolean value.
    bool :: Bool -> s

    --- | Serialize an array of serialized values.
    array :: [s] -> s

    --- | Serialize a string.
    string :: String -> s

    --- | merge two serialized values, which can be useful for combining objects or arrays.
    merge :: s -> s -> s

    --- | Serialize a null value.
    null :: s
