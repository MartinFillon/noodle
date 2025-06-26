{-# LANGUAGE OverloadedStrings #-}

module Noodle.Json.Parser (parseJson) where

import Noodle.Json.Type (Json (..))
import Noodle.Parser.Utils (
    Parser,
    ParserError,
    lexeme,
    parseBool,
    parseNumber,
    parseString,
    sc,
 )
import Text.Megaparsec (MonadParsec (..), between, choice, parse, sepBy)
import Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

jsc :: Parser ()
jsc = sc (L.skipLineComment "//")

parseJsonNumber :: Parser Json
parseJsonNumber = lexeme jsc $ JNumber <$> parseNumber jsc

parseJsonString :: Parser Json
parseJsonString = lexeme jsc $ JString <$> parseString jsc

parseJsonNull :: Parser Json
parseJsonNull = lexeme jsc $ MC.string "null" >> return JNull

parseJsonBool :: Parser Json
parseJsonBool = lexeme jsc (JBool <$> parseBool jsc)

parseArray :: Parser Json
parseArray =
    lexeme jsc $
        JArray
            <$> lexeme
                jsc
                ( between
                    (lexeme jsc $ char '[')
                    (lexeme jsc $ char ']')
                    (lexeme jsc parseValue `sepBy` lexeme jsc ",")
                )

parseObjectItem :: Parser (String, Json)
parseObjectItem = try $ do
    n <- lexeme jsc $ parseString jsc
    _ <- lexeme jsc $ char ':'
    v <- parseObjectValue
    return (n, v)

parseObjectValue :: Parser Json
parseObjectValue = lexeme jsc parseValue

parseObject :: Parser Json
parseObject =
    JObject
        <$> lexeme
            jsc
            ( between
                (lexeme jsc $ char '{')
                (lexeme jsc $ char '}')
                (parseObjectItem `sepBy` lexeme jsc ",")
            )

parseValue :: Parser Json
parseValue =
    lexeme jsc $
        choice
            [ try parseObject,
              try parseArray,
              try parseJsonNumber,
              try parseJsonString,
              try parseJsonNull,
              try parseJsonBool
            ]

parseJson :: String -> Either ParserError Json
parseJson = parse (between jsc eof parseValue) ""
