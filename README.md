# noodle

Noodle is a Haskell library for serializing and deserializing data to and from JSON and YAML formats. It provides a simple and flexible interface for working with these formats, allowing you to easily convert your Haskell data types to and from JSON and YAML.

## Example Usage :

```hs
{-# LANGUAGE DeriveGenerics #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import GHC.Generics (Generic)
import Noodle.Json (prettyPrintJson, Json)
import Noodle.Yaml (prettyPrintYaml, Yaml)
import Noodle.Serialize (Serialize (..))
import Noodle.Deserialize (Serialize (..))
import Noodle.Deserializer (Deserializer)

data Test = Test
    { foo :: Double,
      bar :: Bool,
      taz :: [Int]
    }
    deriving (Show, Generic)

data Person = Person {
    age :: Age,
    bar' :: Test,
    name :: String
} deriving (Show, Generic)

data Age = Age Double deriving (Show, Generic)

data T2 = T2 Int | T3 Bool deriving (Show, Generic)

instance Serializer f => Serialize T2 f
instance Serializer f => Serialize Age f
instance Serializer f => Serialize Test f
instance Serializer f => Serialize Person f

instance Deserializer f => Deserialize T2 f
instance Deserializer f => Deserialize Age f
instance Deserializer f => Deserialize Test f
instance Deserializer f => Deserialize Person f

testJson :: Serialize a Json => a -> IO ()
testJson = putStrLn . prettyPrintJson . serialize

testYaml :: Serialize a Yaml => a -> IO ()
testYaml = putStrLn . prettyPrintYaml . serialize
```

## Features

- **Serialization**: Convert Haskell data types to JSON and YAML formats.
- **Deserialization**: Convert JSON and YAML data back to Haskell data types.
- **Generic Support**: Automatically derive serialization and deserialization instances for data types using GHC generics.
- **Pretty Printing**: Easily format JSON and YAML output for readability.
- **Json Parsing**: Parse JSON strings into Haskell data types. With full support for parsing JSON objects, arrays, strings, numbers, booleans, and null values.
- **Yaml Parsing**: Parse YAML strings into Haskell data types. With support for parsing YAML mappings, sequences, scalars, and more complex YAML features.
- **Extensibility**: Easily extend the library to support custom data types and formats.

## Future Work

- **Full YAML Support**: Implement support for all YAML features, including anchors, aliases, and complex data structures.
- **Error Handling**: Improve error handling and reporting for both serialization and deserialization processes.
- **Performance Optimization**: Optimize the performance of serialization and deserialization, especially for large data structures.
- **Default Instances**: Provide default instances for common data types to simplify usage.

## Contributing

Contributions to Noodle are welcome! If you have an idea for a new feature, find a bug, or want to improve the documentation, please feel free to submit a pull request or open an issue on GitHub.

## License
Noodle is licensed under the MIT License. See the [LICENSE](LICENSE) file for more information.
