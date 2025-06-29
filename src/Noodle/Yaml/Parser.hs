{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Yaml.Parser (parseYaml) where

import Control.Arrow (Arrow (..))
import Control.Monad (void)
import Data.Functor (($>))
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
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

ysc :: Parser ()
ysc = sc (L.skipLineComment "#")

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

yscn :: Parser ()
yscn = L.space space1 (L.skipLineComment "#") empty

ysc' :: Parser ()
ysc' = L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "#") empty

parseKey :: Parser () -> Parser String
parseKey x =
    lexeme x $
        some
            (try parseEscapedChar <|> noneOf (":\n \t\r#" :: String))
            >>= (\key -> char ':' $> key)

parseValue :: Parser () -> Parser Yaml
parseValue x =
    choice $
        map
            try
            [ parseYArray',
              parseYBoolean x,
              parseYNumber x,
              parseYString x
            ]

-- parseObjectItem :: Parser (String, Yaml)
-- parseObjectItem = head <$> L.indentBlock yscn p
--   where
--     p = do
--         name <- parseKey ysc'
--         return
--             ( L.IndentSome
--                 Nothing
--                 return
--                 (parseValue name ysc')
--             )

-- parseObject :: String -> Parser (String, Yaml)
-- parseObject name = second YObject <$> L.indentBlock yscn p
--   where
--     p = do
--         return
--             ( L.IndentSome
--                 Nothing
--                 (return . (name,))
--                 parseObjectItem
--             )

-- parseObjectStart :: Parser (String, Yaml)
-- parseObjectStart = L.nonIndented yscn p
--   where
--     p = do
--         name <- parseKey ysc'
--         parseObject name

parseYArrayValue :: Parser Yaml
parseYArrayValue = do
    _ <- lexeme ysc' $ char '-'
    parseValue ysc'

parseYArray' :: Parser Yaml
parseYArray' =
    YArray
        <$> L.indentBlock yscn p
  where
    p = do
        return
            ( L.IndentMany
                Nothing
                return
                parseYArrayValue
            )
parseYArray :: Parser Yaml
parseYArray = L.nonIndented yscn parseYArray'

parseStart :: Parser Yaml
parseStart = parseYArray

parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (between ysc eof parseStart) "test"
