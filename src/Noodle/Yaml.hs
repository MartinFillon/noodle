module Noodle.Yaml (Yaml (..), prettyPrintYaml) where

import Data.List (intercalate)
import Noodle.Yaml.Type (Yaml (..))

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
