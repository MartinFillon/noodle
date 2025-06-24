module Noodle.Toml (Toml (..), prettyPrintToml) where

import Data.List (intercalate)
import Noodle.Serializer (Serializer (..))

data Toml
    = TObject [(String, Toml)]
    | TString String
    | TNumber Double
    | TBool Bool
    | TNull -- TOML doesn't really support null, this is placeholder
    | TArray [Toml]
    deriving (Show, Eq)

instance Serializer Toml where
    object :: [(String, Toml)] -> Toml
    object = TObject

    string :: String -> Toml
    string = TString

    number :: Double -> Toml
    number = TNumber

    bool :: Bool -> Toml
    bool = TBool

    array :: [Toml] -> Toml
    array = TArray

    merge :: Toml -> Toml -> Toml
    merge (TObject x) (TObject y) = TObject (x ++ y)
    merge (TArray x) (TArray y) = TArray (x ++ y)
    merge (TArray x) y = TArray (x ++ [y])
    merge x y = TArray [x, y]

    null :: Toml
    null = TNull -- TOML doesn't really support null, this is placeholder

prettyPrintToml :: Toml -> String
prettyPrintToml = prettyPrintTomlWithPath []

prettyPrintTomlWithPath :: [String] -> Toml -> String
prettyPrintTomlWithPath path (TObject kvs) =
    let (scalars, objects) = partitionScalarsAndObjects kvs
        scalarLines = [key ++ " = " ++ prettyValue v | (key, v) <- scalars]
        objectLines = concatMap (\(k, v) -> prettyPrintTomlWithPath (path ++ [k]) v) objects
        tableHeader = if Prelude.null path then "" else "[" ++ intercalate "." path ++ "]\n"
     in tableHeader
            ++ intercalate "\n" scalarLines
            ++ (if Prelude.null scalarLines || Prelude.null objectLines then "" else "\n")
            ++ objectLines
  where
    partitionScalarsAndObjects ::
        [(String, Toml)] -> ([(String, Toml)], [(String, Toml)])
    partitionScalarsAndObjects =
        foldr
            ( \(k, v) (scalars, objects) ->
                case v of
                    TObject _ -> (scalars, (k, v) : objects)
                    _ -> ((k, v) : scalars, objects)
            )
            ([], [])
prettyPrintTomlWithPath _ (TArray xs) =
    "[" ++ intercalate ", " (map prettyValue xs) ++ "]"
prettyPrintTomlWithPath _ (TString s) = show s
prettyPrintTomlWithPath _ (TNumber n) = show n
prettyPrintTomlWithPath _ (TBool True) = "true"
prettyPrintTomlWithPath _ (TBool False) = "false"
prettyPrintTomlWithPath _ TNull = "null" -- TOML doesn't really support null, this is placeholder

prettyValue :: Toml -> String
prettyValue (TString s) = show s
prettyValue (TNumber n) = show n
prettyValue (TBool True) = "true"
prettyValue (TBool False) = "false"
prettyValue TNull = "null"
prettyValue (TArray xs) = "[" ++ intercalate ", " (map prettyValue xs) ++ "]"
prettyValue (TObject kvs) =
    "{ " ++ intercalate ", " [k ++ " = " ++ prettyValue v | (k, v) <- kvs] ++ " }"
