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

parseJsonNumber :: Parser Json
parseJsonNumber = lexeme $ JNumber <$> parseNumber

parseJsonString :: Parser Json
parseJsonString = lexeme $ JString <$> parseString

parseJsonNull :: Parser Json
parseJsonNull = lexeme $ MC.string "null" >> return JNull

parseJsonBool :: Parser Json
parseJsonBool = lexeme (JBool <$> parseBool)

parseArray :: Parser Json
parseArray =
    lexeme $
        JArray
            <$> lexeme
                ( between
                    (lexeme $ char '[')
                    (lexeme $ char ']')
                    (lexeme parseValue `sepBy` lexeme ",")
                )

parseObjectItem :: Parser (String, Json)
parseObjectItem = try $ do
    n <- lexeme parseString
    _ <- lexeme (char ':')
    v <- parseObjectValue
    return (n, v)

parseObjectValue :: Parser Json
parseObjectValue = lexeme parseValue

parseObject :: Parser Json
parseObject =
    JObject
        <$> lexeme
            ( between
                (lexeme $ char '{')
                (lexeme $ char '}')
                (parseObjectItem `sepBy` lexeme ",")
            )

parseValue :: Parser Json
parseValue =
    lexeme $
        choice
            [ try parseObject,
              try parseArray,
              try parseJsonNumber,
              try parseJsonString,
              try parseJsonNull,
              try parseJsonBool
            ]

parseJson :: String -> Either ParserError Json
parseJson = parse (between sc eof parseValue) ""
