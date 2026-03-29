{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Yaml.Parser (parseYaml) where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (second))
import Debug.Trace (trace)
import Noodle.Parser.Utils (
    Parser,
    ParserError,
    lexeme,
    parseBool,
    parseEscapedChar,
    parseNumber,
    parseString,
    sc,
 )
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (
    MonadParsec (eof),
    between,
    choice,
    empty,
    noneOf,
    parse,
    some,
    try,
    (<|>),
 )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

comment :: Parser ()
comment = L.skipLineComment "#"

ysc :: Parser ()
ysc = sc comment

ysc' :: Parser ()
ysc' = L.space (void $ some (char ' ' <|> char '\t')) comment empty

parseYBoolean :: Parser () -> Parser Yaml
parseYBoolean x = lexeme x $ YBool <$> parseBool x

parseYNumber :: Parser () -> Parser Yaml
parseYNumber x = lexeme x $ YNumber <$> parseNumber x

parseYString :: Parser () -> Parser Yaml
parseYString x =
    lexeme x $
        YString
            <$> ( parseString x
                    <|> some (try parseEscapedChar <|> noneOf ("\n\t\r#" :: String))
                )

parseValue :: Parser () -> Parser Yaml
parseValue consumer =
    choice $
        map
            try
            [ parseYBoolean consumer,
              parseYNumber consumer,
              parseYString consumer
            ]

parseYArrayValue :: Parser () -> Parser Yaml
parseYArrayValue consumer = do
    _ <- lexeme consumer $ char '-'
    parseValue consumer

parseYArray :: Parser () -> Parser Yaml
parseYArray consumer =
    YArray
        <$> some (parseYArrayValue consumer)

parseObjectKey :: Parser String
parseObjectKey = lexeme ysc $ some (noneOf (":\n\r\t#" :: String))

parseObjectValue :: Parser () -> Parser (String, Yaml)
parseObjectValue consumer = do
    key <- parseObjectKey
    _ <- lexeme consumer $ char ':'
    (key,) <$> parseValue consumer

parseYObject :: Parser () -> Parser Yaml
parseYObject consumer = YObject <$> some (parseObjectValue consumer)

parseObjectDocument :: Parser Yaml
parseObjectDocument = YObject <$> some (L.nonIndented ysc parseObjectDocument')

parseObjectDocument' :: Parser (String, Yaml)
parseObjectDocument' = try (L.indentBlock ysc p) <|> parseObjectValue ysc
  where
    p = do
        key <- parseObjectKey
        _ <- lexeme ysc' $ char ':'
        return (L.IndentSome Nothing (return . (\x -> (key, head x))) (parseValue ysc'))

parseArrayDocument :: Parser Yaml
parseArrayDocument = parseYArray ysc

parseDocument :: Parser Yaml
parseDocument = parseObjectDocument

parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (between ysc eof parseDocument) "test"
