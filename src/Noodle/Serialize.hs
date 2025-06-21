module Noodle.Serialize (Serialize (..)) where

import GHC.Generics (
    Generic (..),
    K1 (..),
    M1 (..),
    S,
    Selector (..),
    U1 (..),
    V1,
    (:*:) (..),
    (:+:) (..),
 )
import Noodle.Serializer (Serializer (..))

class Serializer f => Serialize a f where
    serialize :: a -> f
    default serialize :: (Generic a, GSerialize (Rep a) f) => a -> f
    serialize x = gSerialize (from x)

class Serializer f => GSerialize a f where
    gSerialize :: a b -> f

instance Serializer f => GSerialize V1 f where
    gSerialize :: V1 b -> f
    gSerialize x = case x of {}

instance Serializer f => GSerialize U1 f where
    gSerialize :: U1 b -> f
    gSerialize _ = Noodle.Serializer.null

instance (Serializer a, GSerialize f a) => GSerialize (M1 i t f) a where
    gSerialize :: M1 i t f b -> a
    gSerialize (M1 x) = gSerialize x

instance (Serializer a, GSerialize f a, GSerialize g a) => GSerialize (f :*: g) a where
    gSerialize :: (:*:) f g b -> a
    gSerialize (f :*: g) = merge (gSerialize f) (gSerialize g)

instance (Serializer a, GSerialize f a, GSerialize g a) => GSerialize (f :+: g) a where
    gSerialize :: (:+:) f g b -> a
    gSerialize (L1 x) = gSerialize x
    gSerialize (R1 x) = gSerialize x

instance (Serializer a, Selector s, GSerialize f a) => GSerialize (M1 S s f) a where
    gSerialize :: M1 S s f b -> a
    gSerialize a@(M1 x)
        | name /= [] = object [(name, gSerialize x)]
        | otherwise = gSerialize x
      where
        name = selName a

instance (Serializer f, Serialize c f) => GSerialize (K1 i c) f where
    gSerialize :: K1 i c b -> f
    gSerialize (K1 x) = serialize x

instance Serializer f => Serialize Double f where
    serialize :: Double -> f
    serialize = number

instance Serializer f => Serialize Bool f where
    serialize :: Bool -> f
    serialize = bool

instance Serializer f => Serialize String f where
    serialize :: String -> f
    serialize = string

instance (Serializer f, Serialize a f) => Serialize [a] f where
    serialize :: [a] -> f
    serialize = array . map serialize

instance (Integral a, Serializer f) => Serialize a f where
    serialize :: a -> f
    serialize = number . fromIntegral

instance (Serializer f, Serialize a f, Serialize b f) => Serialize (Either a b) f
instance (Serializer f, Serialize a f) => Serialize (Maybe a) f
instance (Serializer f, Serialize a f, Serialize b f) => Serialize (a, b) f
instance
    (Serializer f, Serialize a f, Serialize b f, Serialize c f) =>
    Serialize (a, b, c) f
