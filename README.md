# noodle

Example Usage :

```hs
{-# LANGUAGE DeriveGenerics #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import GHC.Generics (Generic)
import Noodle.Json (prettyPrintJson, Json)
import Noodle.Yaml (prettyPrintYaml, Yaml)
import Noodle.Serialize (Serialize (serialize))
import Noodle.Serializer (Serializer)

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

testJson :: Serialize a Json => a -> IO ()
testJson = putStrLn . prettyPrintJson . serialize

testYaml :: Serialize a Yaml => a -> IO ()
testYaml = putStrLn . prettyPrintYaml . serialize
```

Currently, only arrays, booleans, int, double, string are suported from prelude