module Noodle (
    module Noodle.Json,
    module Noodle.Yaml,
    module Noodle.Serialize,
    module Noodle.Serializer,
    module Noodle.Toml,
    module Noodle.Deserialize,
    module Noodle.Deserializer,
    T1 (..),
    T2 (..),
    T3 (..),
) where

import GHC.Generics (Generic)
import Noodle.Deserialize
import Noodle.Deserializer
import Noodle.Json
import Noodle.Serialize
import Noodle.Serializer
import Noodle.Toml
import Noodle.Yaml

data T1 = T1 deriving (Generic, Show)
data T2 = T2 Double deriving (Generic, Show)
data T3 = T3
    { a :: Double,
      b :: Double
    }
    deriving (Generic, Show)

instance Serializer f => Serialize T1 f
instance Deserializer f => Deserialize T1 f

instance Serializer f => Serialize T2 f
instance Deserializer f => Deserialize T2 f

instance Serializer f => Serialize T3 f
instance Deserializer f => Deserialize T3 f
