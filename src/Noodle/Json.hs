module Noodle.Json (Json (..), prettyPrintJson, parseJson) where

import Data.List (intercalate)
import Noodle.Json.Parser (parseJson)
import Noodle.Json.Type (Json (..))

--- | Pretty-print a 'Json' value as a string.
--- This function formats the JSON value with indentation for better readability.
--- >> let jsonValue = JObject [("name", JString "Alice"), ("age", JNumber 30), ("isStudent", JBool False)]
--- >> putStrLn (prettyPrintJson jsonValue)
--- {
---   "name": "Alice",
---   "age": 30,
---   "isStudent": false
--- }
prettyPrintJson :: Json -> String
prettyPrintJson = prettyPrintJsonWithIndent 0

prettyPrintJsonWithIndent :: Int -> Json -> String
prettyPrintJsonWithIndent indent (JObject kvs) = "{\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "}"
  where
    indentStr = replicate indent ' '
    nextIndent = indent + 2
    entries =
        [ indentStr ++ "  " ++ show k ++ ": " ++ prettyPrintJsonWithIndent nextIndent v
          | (k, v) <- kvs
        ]
prettyPrintJsonWithIndent indent (JArray xs) = "[\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "]"
  where
    indentStr = replicate indent ' '
    nextIndent = indent + 2
    entries =
        [replicate nextIndent ' ' ++ prettyPrintJsonWithIndent nextIndent v | v <- xs]
prettyPrintJsonWithIndent _ (JString s) = show s
prettyPrintJsonWithIndent _ (JNumber n) = show n
prettyPrintJsonWithIndent _ (JBool True) = "true"
prettyPrintJsonWithIndent _ (JBool False) = "true"
prettyPrintJsonWithIndent _ JNull = "null"
