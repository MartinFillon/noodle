{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Noodle.Deserialize (Deserialize (..)) where

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

import GHC.Float (double2Int)
import Noodle.Deserializer (
    Deserializer (..),
 )

class Deserializer f => Deserialize a f where
    deserialize :: f -> Either String a
    default deserialize ::
        (Generic a, GDeserialize (Rep a) f) => f -> Either String a
    deserialize x = to <$> gDeserialize x

class Deserializer f => GDeserialize a f where
    gDeserialize :: f -> Either String (a b)

instance Deserializer f => GDeserialize V1 f where
    gDeserialize :: f -> Either String (V1 b)
    gDeserialize _ = Left "Cannot deserialize a data structure without constructor"

instance Deserializer f => GDeserialize U1 f where
    gDeserialize :: f -> Either String (U1 a)
    gDeserialize x = U1 <$ getNull x

instance (Deserializer a, GDeserialize f a, GDeserialize g a) => GDeserialize (f :*: g) a where
    gDeserialize ::
        a ->
        Either String ((:*:) f g b)
    gDeserialize x = (:*:) <$> gDeserialize x <*> gDeserialize x

instance (Deserializer a, GDeserialize f a, GDeserialize g a) => GDeserialize (f :+: g) a where
    gDeserialize ::
        a ->
        Either String ((:+:) f g b)
    gDeserialize x = case R1 <$> gDeserialize x of
        Left _ -> L1 <$> gDeserialize x
        Right x' -> Right x'

instance (Deserializer a, GDeserialize f a) => GDeserialize (M1 c t f) a where
    gDeserialize ::
        a -> Either String (M1 c t f b)
    gDeserialize x = M1 <$> gDeserialize x

instance (Deserializer a, Selector s, GDeserialize f a) => GDeserialize (M1 S s f) a where
    gDeserialize ::
        a ->
        Either String (M1 S s f b)
    gDeserialize x =
        fixProxy
            ( \proxy -> case selName proxy of
                [] -> M1 <$> gDeserialize x
                n ->
                    M1
                        <$> ( getObject x
                                >>= lookupE n
                                >>= gDeserialize
                            )
            )
      where
        fixProxy :: (a -> f a) -> f a
        fixProxy f = f undefined

lookupE :: (Ord k, Show k) => k -> [(k, v)] -> Either String v
lookupE k = maybe (Left $ "Key not found: " ++ show k) Right . lookup k

instance (Deserializer f, Deserialize c f) => GDeserialize (K1 i c) f where
    gDeserialize ::
        f -> Either String (K1 i c b)
    gDeserialize x = K1 <$> deserialize x

instance Deserializer f => Deserialize Double f where
    deserialize :: f -> Either String Double
    deserialize = getNumber

instance Deserializer f => Deserialize Bool f where
    deserialize :: f -> Either String Bool
    deserialize = getBool

instance Deserializer f => Deserialize String f where
    deserialize :: f -> Either String String
    deserialize = getString

instance (Deserializer f, Deserialize a f, Deserialize b f) => Deserialize (Either a b) f

instance (Deserializer f, Deserialize a f) => Deserialize [a] f where
    deserialize :: f -> Either String [a]
    deserialize x = getArray x >>= mapM deserialize

instance (Integral a, Deserializer f) => Deserialize a f where
    deserialize :: f -> Either String a
    deserialize x = fromIntegral . double2Int <$> getNumber x

instance (Deserializer f, Deserialize a f) => Deserialize (Maybe a) f
instance (Deserializer f, Deserialize a f, Deserialize b f) => Deserialize (a, b) f
instance
    (Deserializer f, Deserialize a f, Deserialize b f, Deserialize c f) =>
    Deserialize (a, b, c) f
