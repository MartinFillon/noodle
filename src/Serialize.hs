{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Serialize (Serialize (..)) where

import Data.Data (Data (..))
import Data.Generics (constrFields, extQ, mkQ)
import Object (Object (..))

class (Object f, Data t) => Serialize t f where
    to :: t -> f
    to = wrapInObject . getFieldsNameValue

instance Object f => Serialize Double f where
    to :: Double -> f
    to = makeNumber

instance Object f => Serialize Bool f where
    to :: Bool -> f
    to = makeBool

double' :: Object f => Double -> f
double' = to

boolean' :: Object f => Bool -> f
boolean' = to

toObject :: (Object f, Data a) => a -> f
toObject =
    mkQ (error "unknown") id
        `extQ` double'
        `extQ` boolean'

getFieldsNameValue :: (Object f, Data a) => a -> [(String, f)]
getFieldsNameValue d = zip (constrFields $ toConstr d) (gmapQ toObject d)
