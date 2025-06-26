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
    many,
    noneOf,
    parse,
    sepBy,
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

-- parseYArray :: Parser () -> Parser Yaml
-- parseYArray x =
--     YArray
--         <$> ( lexeme
--                 x
--                 (some (lexeme x (char '-') *> lexeme x parseValue))
--                 <|> jsonCase
--             )
--   where
--     jsonCase =
--         lexeme
--             ysc'
--             ( between (char '[') (char ']') (parseValue `sepBy` lexeme ysc' ",")
--             )

parseYOBject :: Parser Yaml
parseYOBject = YObject <$> lexeme ysc (some (lexeme ysc parseYKeyValue))

parseKey :: Parser () -> Parser String
parseKey x =
    lexeme x $
        some
            (try parseEscapedChar <|> noneOf (":\n \t\r#" :: String))
            >>= (\key -> char ':' *> return key)

parseYKeyValue :: Parser (String, Yaml)
parseYKeyValue =
    (,)
        <$> lexeme ysc (parseKey ysc)
        <*> lexeme ysc (lexeme ysc (char ':') *> parseValue ysc)

parseValue :: Parser () -> Parser Yaml
parseValue x =
    choice $
        map try [parseYBoolean x, parseYNumber x, parseYString x]

parseObjectItem :: Parser Yaml
parseObjectItem = L.indentBlock yscn p
  where
    p = do
        name <- parseKey ysc'
        return (L.IndentMany Nothing (return . (name,)) (parseValue ysc'))

parseObjectStart' :: Parser (String, [Yaml])
parseObjectStart' =
    L.nonIndented yscn (L.indentBlock yscn p)
  where
    p = do
        name <- parseKey ysc'
        return (L.IndentSome Nothing (return . (name,)) (parseObjectItem))

parseObjectStart :: Parser Yaml
parseObjectStart = do
    (name, kvs) <- parseObjectStart'
    case kvs of
        [] -> return $ YObject [(name, YNull)]
        _ -> return $ YObject [(name, YArray kvs)]

parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (between ysc eof parseObjectStart) "test"
