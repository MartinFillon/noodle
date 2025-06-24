{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Noodle.Deserialize (Deserialize (..)) where

import GHC.Generics (
    Generic (..),
    K1 (..),
    M1 (..),
    S,
    Selector,
    U1 (..),
    V1,
    (:*:) (..),
    (:+:) (..),
 )

import Control.Monad.State (
    MonadState (..),
    MonadTrans (..),
    StateT,
    evalStateT,
 )
import Noodle.Deserializer (Deserializer (getNull, getNumber))

type DeserializeState f = StateT f (Either String)

class Deserializer f => Deserialize a f where
    deserialize :: f -> Either String a
    default deserialize ::
        (Generic a, GDeserialize (Rep a) f) => f -> Either String a
    deserialize x = to <$> evalStateT gDeserialize x

class Deserializer f => GDeserialize a f where
    gDeserialize :: DeserializeState f (a b)

-- instance Deserializer f => GDeserialize V1 f where
--     gDeserialize :: f -> Either String (V1 b)
--     gDeserialize x =

instance Deserializer f => GDeserialize U1 f where
    gDeserialize :: DeserializeState f (U1 b)
    gDeserialize = U1 <$ (get >>= (lift . getNull))

instance (Deserializer a, GDeserialize f a) => GDeserialize (M1 i t f) a where
    gDeserialize :: DeserializeState a (M1 i t f b)
    gDeserialize = M1 <$> gDeserialize

instance (Deserializer a, GDeserialize f a, GDeserialize g a) => GDeserialize (f :*: g) a where
    gDeserialize :: DeserializeState a ((:*:) f g b)
    gDeserialize = (:*:) <$> gDeserialize <*> gDeserialize

-- instance (Deserializer a, GDeserialize f a, GDeserialize g a) => GDeserialize (f :+: g) a where
--     gDeserialize ::
--         (Deserializer a, GDeserialize f a, GDeserialize g a) =>
--         DeserializeState a ((:+:) f g b)
--     gDeserialize =

instance (Deserializer a, Selector s, GDeserialize f a) => GDeserialize (M1 S s f) a where
    gDeserialize ::
        (Deserializer a, Selector s, GDeserialize f a) =>
        DeserializeState a (M1 S s f b)
    gDeserialize = _

instance (Deserializer f, Deserialize c f) => GDeserialize (K1 i c) f where
    gDeserialize ::
        (Deserializer f, Deserialize c f) => DeserializeState f (K1 i c b)
    gDeserialize = K1 <$> (get >>= (lift . deserialize))

instance Deserializer f => Deserialize Double f where
    deserialize :: f -> Either String Double
    deserialize = getNumber
