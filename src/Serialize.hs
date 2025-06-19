module Serialize(Serialize(..)) where

import GHC.Generics
import Json(Json (..), merge)

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

instance (GSerialize f, GSerialize g) => GSerialize (f :+: g) where
    serialize (L1 x) = serialize x
    serialize (R1 x) = serialize x

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

instance Serialize Int where
    toJson = JNumber . fromIntegral

instance Serialize a => Serialize [a] where
    toJson = JArray . map toJson