{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Yaml.Parser (parseYaml) where

import Control.Monad (void)
import Data.Bifunctor (second)
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
parseValue x =
    choice $
        map
            try
            [ parseYObject,
              parseYBoolean x,
              parseYNumber x,
              parseYString x
            ]

parseYArrayValue :: Parser Yaml
parseYArrayValue = do
    _ <- lexeme ysc $ char '-'
    parseValue ysc

parseYArray :: Parser Yaml
parseYArray =
    YArray
        <$> some parseYArrayValue

parseObjectKey :: Parser String
parseObjectKey = lexeme ysc $ some (noneOf (":\n\r\t#" :: String))

parseIndentedObjectValue :: String -> Parser (String, Yaml)
parseIndentedObjectValue key = second YArray <$> parseIndentedObjectValue' key

parseIndentedObjectValue' :: String -> Parser (String, [Yaml])
parseIndentedObjectValue' key = L.indentBlock ysc' $ do
    _ <- char ':'
    return $ L.IndentSome Nothing (return . (key,)) (parseValue ysc')

parseObjectValue :: Parser (String, Yaml)
parseObjectValue = do
    key <- parseObjectKey
    _ <- lexeme ysc $ char ':'
    try ((key,) <$> parseValue ysc) <|> parseIndentedObjectValue key

parseYObject :: Parser Yaml
parseYObject = YObject <$> some parseObjectValue

parseStart :: Parser Yaml
parseStart = L.nonIndented ysc' (try parseYArray <|> parseValue ysc)

parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (between ysc eof parseStart) "test"
