{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib where

import Data.Generics
import Data.Generics.Builders

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Object = [(String, String)]

getNames :: Data object => object -> [String]
getNames = constrFields . toConstr

-- class Data a => Serialize a where
--     empty :: a
--     empty = (Data.Generics.Builders.empty :: a)

--     serialize :: Object -> Maybe String
--     serialize l = lookup field l
--       where
--         field = head $ getNames em

data TestData = T
    { foo :: Int, bar :: Int
    }
    deriving (Show, Data)
