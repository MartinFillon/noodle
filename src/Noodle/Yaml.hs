module Noodle.Yaml (Yaml (..), prettyPrintYaml) where

import Data.List (intercalate)
import Noodle.Serializer (Serializer (..))

data Yaml
    = YObject [(String, Yaml)]
    | YString String
    | YNumber Double
    | YBool Bool
    | YNull
    | YArray [Yaml]
    deriving (Show, Eq)

instance Serializer Yaml where
    object :: [(String, Yaml)] -> Yaml
    object = YObject

    string :: String -> Yaml
    string = YString

    number :: Double -> Yaml
    number = YNumber

    bool :: Bool -> Yaml
    bool = YBool

    array :: [Yaml] -> Yaml
    array = YArray

    merge :: Yaml -> Yaml -> Yaml
    merge = merge'

    null :: Yaml
    null = YNull

prettyPrintYaml :: Yaml -> String
prettyPrintYaml = prettyPrintYamlWithIndent 0

prettyPrintYamlWithIndent :: Int -> Yaml -> String
prettyPrintYamlWithIndent indent (YObject kvs) =
    intercalate
        "\n"
        [ replicate indent ' ' ++ k ++ ": " ++ formatValue (v, indent + 2) | (k, v) <- kvs
        ]
  where
    formatValue (YObject o, i) = "\n" ++ prettyPrintYamlWithIndent i (YObject o)
    formatValue (YArray a, i) = "\n" ++ prettyPrintYamlWithIndent i (YArray a)
    formatValue (v, _) = prettyPrintYamlWithIndent 0 v
prettyPrintYamlWithIndent indent (YArray xs) =
    intercalate
        "\n"
        [replicate indent ' ' ++ "- " ++ formatValue (x, indent + 2) | x <- xs]
  where
    formatValue (YObject o, i) = "\n" ++ prettyPrintYamlWithIndent i (YObject o)
    formatValue (YArray a, i) = "\n" ++ prettyPrintYamlWithIndent i (YArray a)
    formatValue (v, _) = prettyPrintYamlWithIndent 0 v
prettyPrintYamlWithIndent _ (YString s) = show s
prettyPrintYamlWithIndent _ (YNumber n) = show n
prettyPrintYamlWithIndent _ (YBool True) = "true"
prettyPrintYamlWithIndent _ (YBool False) = "false"
prettyPrintYamlWithIndent _ YNull = "null"

merge' :: Yaml -> Yaml -> Yaml
merge' (YObject x) (YObject y) = YObject (x ++ y)
merge' (YArray x) (YArray y) = YArray (x ++ y)
merge' (YArray x) y = YArray (x ++ [y])
merge' x y = YArray [x, y]
