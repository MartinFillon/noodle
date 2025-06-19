module Noodle where

import GHC.Generics
import Json (prettyPrintJson)
import Serialize (Serialize (toJson))

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

data T2 = T2 Int | T3 Bool deriving (Show, Generic)

data T3 deriving (Generic)

instance Serialize T2
instance Serialize Age
instance Serialize Test
instance Serialize Person

test :: Serialize a => a -> IO ()
test = putStrLn . prettyPrintJson . toJson
