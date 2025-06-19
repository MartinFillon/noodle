module Json (Json (..), prettyPrintJson) where

import Data.List (intercalate)
import Serializer (Serializer (..))

data Json = JObject [(String, Json)] | JString String | JNumber Double | JBool Bool | Null | JArray [Json]
    deriving (Show)

instance Serializer Json where
    object :: [(String, Json)] -> Json
    object = JObject

    string :: String -> Json
    string = JString

    number :: Double -> Json
    number = JNumber

    bool :: Bool -> Json
    bool = JBool

    array :: [Json] -> Json
    array = JArray

    merge :: Json -> Json -> Json
    merge = merge'

    null :: Json
    null = Null

prettyPrintJson :: Json -> String
prettyPrintJson = prettyPrintJsonWithIndent 0

prettyPrintJsonWithIndent :: Int -> Json -> String
prettyPrintJsonWithIndent indent (JObject kvs) = "{\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "}"
    where 
        indentStr = replicate indent ' '
        nextIndent = indent + 2
        entries = [indentStr ++ "  " ++ show k ++ ": " ++ prettyPrintJsonWithIndent nextIndent v | (k, v) <- kvs]
prettyPrintJsonWithIndent indent (JArray xs) = "[\n" ++ intercalate ",\n" entries ++ "\n" ++ indentStr ++ "]"
        where
            indentStr = replicate indent ' '
            nextIndent = indent + 2
            entries = [replicate nextIndent ' ' ++ prettyPrintJsonWithIndent nextIndent v | v <- xs]
prettyPrintJsonWithIndent _ (JString s) = show s
prettyPrintJsonWithIndent _ (JNumber n) = show n
prettyPrintJsonWithIndent _ (JBool True) = "true"
prettyPrintJsonWithIndent _ (JBool False) = "true"
prettyPrintJsonWithIndent _ Null = "null"

merge' :: Json -> Json -> Json
merge' (JObject x) (JObject y) = JObject (x ++ y)
merge' (JArray x) (JArray y) = JArray (x ++ y)
merge' (JArray x) y = JArray (y : x)
merge' x y = JArray [x, y]
