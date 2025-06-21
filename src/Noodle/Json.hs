{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Json (Json (..), prettyPrintJson, parseJson, parseValue) where

import Data.List (intercalate)
import Noodle.Parser.Utils (
    Parser,
    ParserError,
    lexeme,
    parseBool,
    parseNumber,
    parseString,
    sc,
 )
import Noodle.Serializer (Serializer (..))
import Text.Megaparsec (MonadParsec (eof), between, choice, parse, sepBy)
import Text.Megaparsec.Char as MC

data Json
    = JObject [(String, Json)]
    | JString String
    | JNumber Double
    | JBool Bool
    | Null
    | JArray [Json]
    deriving (Show, Eq)

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
prettyPrintJsonWithIndent _ Null = "null"

merge' :: Json -> Json -> Json
merge' (JObject x) (JObject y) = JObject (x ++ y)
merge' (JArray x) (JArray y) = JArray (x ++ y)
merge' (JArray x) y = JArray (y : x)
merge' x y = JArray [x, y]

parseJsonNumber :: Parser Json
parseJsonNumber = lexeme $ JNumber <$> parseNumber

parseJsonString :: Parser Json
parseJsonString = lexeme $ JString <$> parseString

parseJsonNull :: Parser Json
parseJsonNull = lexeme $ MC.string "null" >> return Null

parseJsonBool :: Parser Json
parseJsonBool = lexeme $ JBool <$> parseBool

parseArray :: Parser Json
parseArray =
    JArray
        <$> lexeme
            ( between (char '[') (char ']') (parseValue `sepBy` lexeme ",")
            )

parseObjectItem :: Parser (String, Json)
parseObjectItem =
    lexeme (parseString >>= (\n -> lexeme $ char ':' >> parseObjectValue n))

parseObjectValue :: String -> Parser (String, Json)
parseObjectValue n = lexeme $ (n,) <$> lexeme parseValue

parseObject :: Parser Json
parseObject =
    JObject
        <$> lexeme (between (char '{') (char '}') (parseObjectItem `sepBy` lexeme ","))

parseValue :: Parser Json
parseValue =
    lexeme $
        choice
            [ parseObject,
              parseArray,
              parseJsonNumber,
              parseJsonString,
              parseJsonNull,
              parseJsonBool
            ]

parseJson :: String -> Either ParserError Json
parseJson = parse (between sc eof parseValue) ""
