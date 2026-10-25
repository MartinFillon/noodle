{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Noodle.Serialize (Serialize (..)) where

import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific (Scientific, toRealFloat)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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
import Numeric.Natural (Natural)

--- | The 'Serialize' type class defines an interface for serializing data structures into a specific format.
--- This type class is designed to work with any type that has a 'Generic' instance, allowing for automatic serialization of complex data types without requiring manual implementation for each type.
--- The 'serialize' method takes a value of type 'a' and produces a serialized representation of type 'f', where 'f' is an instance of the 'Serializer' type class.
class Serializer f => Serialize a f where
    --- | Serialize a value of type 'a' into a format represented by 'f'.
    serialize :: a -> f
    default serialize :: (Generic a, GSerialize (Rep a) f) => a -> f
    serialize x = gSerialize (from x)

--- | The 'GSerialize' type class is a helper type class used for generic serialization. It defines how to serialize the generic representation of a type.
--- This type class is used internally by the 'Serialize' type class to provide a default implementation
class Serializer f => GSerialize a f where
    gSerialize :: a b -> f

instance {-# OVERLAPPABLE #-} Serializer f => GSerialize V1 f where
    gSerialize :: V1 b -> f
    gSerialize x = case x of {}

instance {-# OVERLAPPABLE #-} Serializer f => GSerialize U1 f where
    gSerialize :: U1 b -> f
    gSerialize _ = Noodle.Serializer.null

instance {-# OVERLAPPABLE #-} (Serializer a, GSerialize f a) => GSerialize (M1 i t f) a where
    gSerialize :: M1 i t f b -> a
    gSerialize (M1 x) = gSerialize x

instance {-# OVERLAPPABLE #-} (Serializer a, GSerialize f a, GSerialize g a) => GSerialize (f :*: g) a where
    gSerialize :: (:*:) f g b -> a
    gSerialize (f :*: g) = merge (gSerialize f) (gSerialize g)

instance {-# OVERLAPPABLE #-} (Serializer a, GSerialize f a, GSerialize g a) => GSerialize (f :+: g) a where
    gSerialize :: (:+:) f g b -> a
    gSerialize (L1 x) = gSerialize x
    gSerialize (R1 x) = gSerialize x

instance {-# OVERLAPPABLE #-} (Serializer a, Selector s, GSerialize f a) => GSerialize (M1 S s f) a where
    gSerialize :: M1 S s f b -> a
    gSerialize a@(M1 x)
        | name /= [] = object [(name, gSerialize x)]
        | otherwise = gSerialize x
      where
        name = selName a

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize c f) => GSerialize (K1 i c) f where
    gSerialize :: K1 i c b -> f
    gSerialize (K1 x) = serialize x

instance {-# OVERLAPPABLE #-} Serializer f => Serialize Double f where
    serialize :: Double -> f
    serialize = number

instance {-# OVERLAPPABLE #-} Serializer f => Serialize Float f where
    serialize :: Float -> f
    serialize = number . realToFrac

instance {-# OVERLAPPABLE #-} Serializer f => Serialize Bool f where
    serialize :: Bool -> f
    serialize = bool

instance {-# OVERLAPPABLE #-} Serializer f => Serialize Char f where
    serialize :: Char -> f
    serialize c = string [c]

instance {-# OVERLAPPABLE #-} Serializer f => Serialize () f where
    serialize :: () -> f
    serialize () = Noodle.Serializer.null

instance {-# OVERLAPPABLE #-} Serializer f => Serialize String f where
    serialize :: String -> f
    serialize = string

instance {-# OVERLAPPABLE #-} Serializer f => Serialize Text f where
    serialize :: Text -> f
    serialize = string . T.unpack

instance {-# OVERLAPPABLE #-} Serializer f => Serialize Natural f where
    serialize :: Natural -> f
    serialize = number . fromIntegral

instance {-# OVERLAPPABLE #-} Serializer f => Serialize Scientific f where
    serialize :: Scientific -> f
    serialize = number . toRealFloat

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f) => Serialize [a] f where
    serialize :: [a] -> f
    serialize = array . map serialize

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f) => Serialize (NonEmpty a) f where
    serialize :: NonEmpty a -> f
    serialize = array . map serialize . NonEmpty.toList

instance {-# OVERLAPPABLE #-} (Integral a, Serializer f) => Serialize a f where
    serialize :: a -> f
    serialize = number . fromIntegral

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f) => Serialize (Maybe a) f where
    serialize :: Maybe a -> f
    serialize (Just x) = serialize x
    serialize Nothing = Noodle.Serializer.null

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f, Serialize b f) => Serialize (Either a b) f where
    serialize :: Either a b -> f
    serialize (Left x) = serialize x
    serialize (Right x) = serialize x

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f) => Serialize (Map String a) f where
    serialize :: Map String a -> f
    serialize = object . map (\(k, v) -> (k, serialize v)) . Map.toList

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f) => Serialize (Map Text a) f where
    serialize :: Map Text a -> f
    serialize = object . map (\(k, v) -> (T.unpack k, serialize v)) . Map.toList

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f) => Serialize (Set a) f where
    serialize :: Set a -> f
    serialize = array . map serialize . Set.toList

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f) => Serialize (Seq a) f where
    serialize :: Seq a -> f
    serialize = array . map serialize . Foldable.toList

instance {-# OVERLAPPABLE #-} (Serializer f, Serialize a f, Serialize b f) => Serialize (a, b) f where
    serialize :: (a, b) -> f
    serialize (a, b) = array [serialize a, serialize b]

instance
    {-# OVERLAPPABLE #-}
    (Serializer f, Serialize a f, Serialize b f, Serialize c f) =>
    Serialize (a, b, c) f
    where
    serialize :: (a, b, c) -> f
    serialize (a, b, c) = array [serialize a, serialize b, serialize c]

instance
    {-# OVERLAPPABLE #-}
    (Serializer f, Serialize a f, Serialize b f, Serialize c f, Serialize d f) =>
    Serialize (a, b, c, d) f
    where
    serialize :: (a, b, c, d) -> f
    serialize (a, b, c, d) = array [serialize a, serialize b, serialize c, serialize d]

instance
    {-# OVERLAPPABLE #-}
    ( Serializer f,
      Serialize a f,
      Serialize b f,
      Serialize c f,
      Serialize d f,
      Serialize e f
    ) =>
    Serialize (a, b, c, d, e) f
    where
    serialize :: (a, b, c, d, e) -> f
    serialize (a, b, c, d, e) = array [serialize a, serialize b, serialize c, serialize d, serialize e]
