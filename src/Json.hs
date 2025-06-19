module Json (Json (..), prettyPrintJson, merge) where

import Data.List (intercalate)

data Json = JObject [(String, Json)] | JString String | JNumber Double | JBool Bool | Null | JArray [Json]
    deriving (Show)

prettyPrintJson :: Json -> String
prettyPrintJson = prettyPrintJsonWithIndent 0

prettyPrintJsonWithIndent :: Int -> Json -> String
prettyPrintJsonWithIndent indent json = case json of
    JObject kvs ->
        let indentStr = replicate indent ' '
            nextIndent = indent + 2
            entries = [indentStr ++ "  " ++ show k ++ ": " ++ prettyPrintJsonWithIndent nextIndent v | (k, v) <- kvs]
        in "{\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "}"
    JArray xs ->
        let indentStr = replicate indent ' '
            nextIndent = indent + 2
            entries = [replicate nextIndent ' ' ++ prettyPrintJsonWithIndent nextIndent v | v <- xs]
        in "[\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "]"
    JString s -> show s
    JNumber n -> show n
    JBool b   -> if b then "true" else "false"
    Null      -> "null"

merge :: Json -> Json -> Json
merge (JObject x) (JObject y) = JObject (x ++ y)
merge (JArray x) (JArray y) = JArray (x ++ y)
merge (JArray x) y = JArray (y : x)
merge x y = JArray [x, y]