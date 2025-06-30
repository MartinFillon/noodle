{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Yaml.Parser (parseYaml) where

import Control.Arrow (Arrow (..))
import Control.Monad (void)
import Data.Functor (($>))
import Debug.Trace (trace, traceShow)
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
import Text.Megaparsec.Debug (dbg)

comment :: Parser ()
comment = L.skipLineComment "#"

ysc :: Parser ()
ysc = sc comment

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
yscn = L.space space1 comment empty

ysc' :: Parser ()
ysc' = L.space (void $ some (char ' ' <|> char '\t')) comment empty

-- parseKey :: Parser () -> Parser String
-- parseKey x =
--     lexeme x $
--         some
--             (try parseEscapedChar <|> noneOf (":\n \t\r#" :: String))
--             >>= (\key -> char ':' $> key)

parseValue :: Parser () -> Parser Yaml
parseValue x =
    choice $
        map
            try
            [ parseYBoolean x,
              parseYNumber x,
              parseYString x
            ]

parseYArrayValue :: Parser Yaml
parseYArrayValue = do
    _ <- dbg "parsing dash" $ lexeme ysc' $ char '-'
    dbg "parseArrayValue" $ parseValue ysc'

parseYArray' :: Parser Yaml
parseYArray' =
    YArray
        <$> L.indentBlock yscn p
  where
    p =
        return $
            L.IndentMany
                Nothing
                return
                parseYArrayValue

parseYArray :: Parser Yaml
parseYArray = YArray <$> L.nonIndented yscn (L.indentBlock yscn p)
  where
    p =
        do
            pos <- L.indentLevel
            v <- parseYArrayValue
            return
                ( L.IndentMany
                    (Just pos)
                    (return . (:) v)
                    parseYArrayValue
                )

parseStart :: Parser Yaml
parseStart = dbg "start" $ parseYArray

parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (between ysc eof parseStart) "test"
