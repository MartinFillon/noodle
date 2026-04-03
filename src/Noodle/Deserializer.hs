module Noodle.Deserializer (Deserializer (..)) where

--- | The 'Deserializer' type class defines an interface for deserializing data structures from a specific format.
--- This type class can be implemented for various deserialization formats such as JSON, YAML, TOML, etc.
--- Each method corresponds to a different type of data that can be deserialized, such as objects, numbers, booleans, arrays, strings, and null values.
--- The methods return an 'Either String' type, where 'Left String' represents an error message if the deserialization fails, and 'Right a' represents a successful deserialization of the expected type.
class Deserializer a where
    getObject :: a -> Either String [(String, a)]

    getNumber :: a -> Either String Double

    getArray :: a -> Either String [a]

    getString :: a -> Either String String

    getBool :: a -> Either String Bool

    getNull :: a -> Either String ()
