{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Yaml.Parser (parseYaml) where

import Control.Monad (void)
import Noodle.Parser.Utils (
    Parser,
    ParserError,
    lexeme,
    parseBool,
    parseEscapedChar,
    parseString,
    sc,
 )
import Noodle.Yaml.Parser.Numbers (parseYNumber)
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
            [ parseSubObject consumer,
              parseSubArray consumer,
              parseYBoolean consumer,
              parseYNumber consumer,
              parseYString consumer
            ]

parseYArrayValue :: Parser () -> Parser Yaml
parseYArrayValue consumer = do
    _ <- lexeme consumer $ char '-'
    parseValue consumer

parseObjectKey :: Parser String
parseObjectKey = lexeme ysc $ some (noneOf (":\n\r\t#" :: String))

parseObjectValue :: Parser () -> Parser (String, Yaml)
parseObjectValue consumer = do
    key <- parseObjectKey
    _ <- lexeme consumer $ char ':'
    (key,) <$> parseValue consumer

parseObjectDocument :: Parser Yaml
parseObjectDocument = YObject <$> some (L.nonIndented ysc parseObjectDocument')

parseObjectDocument' :: Parser (String, Yaml)
parseObjectDocument' = try (L.indentBlock ysc p) <|> parseObjectValue ysc
  where
    p = do
        key <- parseObjectKey
        _ <- lexeme ysc' $ char ':'
        return (L.IndentSome Nothing (keepFirstItem key) (parseValue ysc'))

parseSubObject :: Parser () -> Parser Yaml
parseSubObject consumer =
    YObject <$> some (try (L.indentBlock consumer p) <|> parseObjectValue consumer)
  where
    p = do
        key <- parseObjectKey
        _ <- lexeme consumer $ char ':'
        return
            (L.IndentSome Nothing (keepFirstItem key) (parseValue consumer))

keepFirstItem :: String -> [a] -> Parser (String, a)
keepFirstItem key (x : _) = return (key, x)
keepFirstItem _ [] = fail "Expected at least one item in the object"

head' :: [a] -> Parser a
head' (x : _) = return x
head' [] = fail "Expected at least one item in the array"

parseSubArray :: Parser () -> Parser Yaml
parseSubArray consumer =
    YArray <$> some (try (L.indentBlock consumer p) <|> parseYArrayValue consumer)
  where
    p = do
        _ <- lexeme consumer $ char '-'
        return (L.IndentSome Nothing head' (parseValue consumer))

parseArrayDocument :: Parser Yaml
parseArrayDocument = YArray <$> some (L.nonIndented ysc parseArrayDocument')

parseArrayDocument' :: Parser Yaml
parseArrayDocument' = try (L.indentBlock ysc p) <|> parseYArrayValue ysc
  where
    p = do
        _ <- lexeme ysc' $ char '-'
        return (L.IndentSome Nothing head' (parseValue ysc'))

parseDocument :: Parser Yaml
parseDocument = choice $ map try [parseObjectDocument, parseArrayDocument, parseValue ysc]

parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (between ysc eof parseDocument) "test"
