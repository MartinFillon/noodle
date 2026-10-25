{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Noodle.Deserialize (Deserialize (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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
import Noodle.Deserializer (
    Deserializer (..),
 )
import Numeric.Natural (Natural)

--- | The 'Deserialize' type class defines an interface for deserializing data structures from a specific format.
--- This type class is designed to work with any type that has a 'Generic' instance, allowing for automatic deserialization of complex data types without requiring manual implementation for each type.
--- The 'deserialize' method takes a value of type 'f' and produces either an error message (Left String) or a successfully deserialized value of type 'a' (Right a), where 'f' is an instance of the 'Deserializer' type class.
class Deserializer f => Deserialize a f where
    --- | Deserialize a value of type 'a' from a format represented by 'f'.
    deserialize :: f -> Either String a
    default deserialize ::
        (Generic a, GDeserialize (Rep a) f) => f -> Either String a
    deserialize x = to <$> gDeserialize x

class Deserializer f => GDeserialize a f where
    gDeserialize :: f -> Either String (a b)

instance {-# OVERLAPPABLE #-} Deserializer f => GDeserialize V1 f where
    gDeserialize :: f -> Either String (V1 b)
    gDeserialize _ = Left "Cannot deserialize a data structure without constructor"

instance {-# OVERLAPPABLE #-} Deserializer f => GDeserialize U1 f where
    gDeserialize :: f -> Either String (U1 a)
    gDeserialize x = U1 <$ getNull x

instance {-# OVERLAPPABLE #-} (Deserializer a, GDeserialize f a, GDeserialize g a) => GDeserialize (f :*: g) a where
    gDeserialize ::
        a ->
        Either String ((:*:) f g b)
    gDeserialize x = (:*:) <$> gDeserialize x <*> gDeserialize x

instance {-# OVERLAPPABLE #-} (Deserializer a, GDeserialize f a, GDeserialize g a) => GDeserialize (f :+: g) a where
    gDeserialize ::
        a ->
        Either String ((:+:) f g b)
    gDeserialize x = case R1 <$> gDeserialize x of
        Right x' -> Right x'
        Left errR -> case L1 <$> gDeserialize x of
            Right x' -> Right x'
            Left errL -> Left ("Could not deserialize sum type: " ++ errR ++ "; " ++ errL)

instance {-# OVERLAPPABLE #-} (Deserializer a, GDeserialize f a) => GDeserialize (M1 c t f) a where
    gDeserialize ::
        a -> Either String (M1 c t f b)
    gDeserialize x = M1 <$> gDeserialize x

class Deserializer a => GDeserializeRecordField f a where
    gDeserializeRecordField :: String -> [(String, a)] -> Either String (f b)

instance {-# OVERLAPPABLE #-} (Deserializer a, GDeserialize f a) => GDeserializeRecordField f a where
    gDeserializeRecordField n kvs = case lookup n kvs of
        Nothing -> Left ("Key not found: " ++ show n)
        Just v -> gDeserialize v

instance {-# OVERLAPPING #-} (Deserializer a, Deserialize c a) => GDeserializeRecordField (K1 i (Maybe c)) a where
    gDeserializeRecordField n kvs = case lookup n kvs of
        Nothing -> Right (K1 Nothing)
        Just v -> case getNull v of
            Right () -> Right (K1 Nothing)
            Left _ -> K1 . Just <$> deserialize v

instance
    {-# OVERLAPPABLE #-}
    (Deserializer a, Selector s, GDeserialize f a, GDeserializeRecordField f a) =>
    GDeserialize (M1 S s f) a
    where
    gDeserialize ::
        a ->
        Either String (M1 S s f b)
    gDeserialize x =
        case selName (undefined :: M1 S s f b) of
            [] -> M1 <$> gDeserialize x
            n -> M1 <$> (getObject x >>= gDeserializeRecordField n)

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize c f) => GDeserialize (K1 i c) f where
    gDeserialize ::
        f -> Either String (K1 i c b)
    gDeserialize x = K1 <$> deserialize x

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize Double f where
    deserialize :: f -> Either String Double
    deserialize = getNumber

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize Float f where
    deserialize :: f -> Either String Float
    deserialize x = realToFrac <$> getNumber x

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize Bool f where
    deserialize :: f -> Either String Bool
    deserialize = getBool

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize Char f where
    deserialize :: f -> Either String Char
    deserialize x =
        getString x >>= \case
            [c] -> Right c
            _ -> Left "Expected single-character string for Char"

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize () f where
    deserialize :: f -> Either String ()
    deserialize = getNull

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize String f where
    deserialize :: f -> Either String String
    deserialize = getString

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize Text f where
    deserialize :: f -> Either String Text
    deserialize x = T.pack <$> getString x

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize Natural f where
    deserialize :: f -> Either String Natural
    deserialize x = do
        n <- truncate <$> getNumber x
        if n < 0
            then Left "Expected non-negative integer for Natural"
            else Right (fromIntegral (n :: Integer))

instance {-# OVERLAPPABLE #-} Deserializer f => Deserialize Scientific f where
    deserialize :: f -> Either String Scientific
    deserialize x = fromFloatDigits <$> getNumber x

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f) => Deserialize [a] f where
    deserialize :: f -> Either String [a]
    deserialize x = getArray x >>= mapM deserialize

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f) => Deserialize (NonEmpty a) f where
    deserialize :: f -> Either String (NonEmpty a)
    deserialize x = do
        items <- deserialize x
        case NonEmpty.nonEmpty items of
            Just ne -> Right ne
            Nothing -> Left "Expected non-empty array"

instance {-# OVERLAPPABLE #-} (Integral a, Deserializer f) => Deserialize a f where
    deserialize :: f -> Either String a
    deserialize x = truncate <$> getNumber x

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f) => Deserialize (Maybe a) f where
    deserialize :: f -> Either String (Maybe a)
    deserialize x = case getNull x of
        Right () -> Right Nothing
        Left _ -> Just <$> deserialize x

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f, Deserialize b f) => Deserialize (Either a b) f where
    deserialize :: f -> Either String (Either a b)
    deserialize x = case (deserialize x :: Either String b) of
        Right r -> Right (Right r)
        Left _ -> case (deserialize x :: Either String a) of
            Right l -> Right (Left l)
            Left err -> Left ("Could not deserialize Either: " ++ err)

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f) => Deserialize (Map String a) f where
    deserialize :: f -> Either String (Map String a)
    deserialize x = do
        kvs <- getObject x
        Map.fromList <$> mapM (\(k, v) -> (k,) <$> deserialize v) kvs

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f) => Deserialize (Map Text a) f where
    deserialize :: f -> Either String (Map Text a)
    deserialize x = do
        kvs <- getObject x
        Map.fromList <$> mapM (\(k, v) -> (T.pack k,) <$> deserialize v) kvs

instance {-# OVERLAPPABLE #-} (Deserializer f, Ord a, Deserialize a f) => Deserialize (Set a) f where
    deserialize :: f -> Either String (Set a)
    deserialize x = Set.fromList <$> deserialize x

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f) => Deserialize (Seq a) f where
    deserialize :: f -> Either String (Seq a)
    deserialize x = Seq.fromList <$> deserialize x

instance {-# OVERLAPPABLE #-} (Deserializer f, Deserialize a f, Deserialize b f) => Deserialize (a, b) f where
    deserialize :: f -> Either String (a, b)
    deserialize x =
        getArray x >>= \case
            [a, b] -> (,) <$> deserialize a <*> deserialize b
            _ -> Left "Expected 2-element array for tuple (a, b)"

instance
    {-# OVERLAPPABLE #-}
    (Deserializer f, Deserialize a f, Deserialize b f, Deserialize c f) =>
    Deserialize (a, b, c) f
    where
    deserialize :: f -> Either String (a, b, c)
    deserialize x =
        getArray x >>= \case
            [a, b, c] -> (,,) <$> deserialize a <*> deserialize b <*> deserialize c
            _ -> Left "Expected 3-element array for tuple (a, b, c)"

instance
    {-# OVERLAPPABLE #-}
    ( Deserializer f,
      Deserialize a f,
      Deserialize b f,
      Deserialize c f,
      Deserialize d f
    ) =>
    Deserialize (a, b, c, d) f
    where
    deserialize :: f -> Either String (a, b, c, d)
    deserialize x =
        getArray x >>= \case
            [a, b, c, d] ->
                (,,,) <$> deserialize a <*> deserialize b <*> deserialize c <*> deserialize d
            _ -> Left "Expected 4-element array for tuple (a, b, c, d)"

instance
    {-# OVERLAPPABLE #-}
    ( Deserializer f,
      Deserialize a f,
      Deserialize b f,
      Deserialize c f,
      Deserialize d f,
      Deserialize e f
    ) =>
    Deserialize (a, b, c, d, e) f
    where
    deserialize :: f -> Either String (a, b, c, d, e)
    deserialize x =
        getArray x >>= \case
            [a, b, c, d, e] ->
                (,,,,)
                    <$> deserialize a
                    <*> deserialize b
                    <*> deserialize c
                    <*> deserialize d
                    <*> deserialize e
            _ -> Left "Expected 5-element array for tuple (a, b, c, d, e)"
