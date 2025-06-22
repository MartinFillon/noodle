module Noodle.Parser.Utils (
    Parser,
    sc,
    sce,
    lexeme,
    parseNumber,
    ParserError,
    parseString,
    parseBool,
) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (..),
    Parsec,
    between,
    choice,
    noneOf,
    (<?>),
 )
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

sc :: Parser ()
sc =
    L.space
        (void $ some (char ' ' <|> char '\t' <|> char '\n' <|> char '\r'))
        empty
        empty

sce :: Parser ()
sce = L.space empty empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseNumber :: Parser Double
parseNumber =(
    try (L.signed sce L.float) <|> fromIntegral
        <$> (L.signed sce L.decimal :: Parser Integer)) <?> "Number"

parseString :: Parser String
parseString =
    between
        (char '\"')
        (char '\"')
        ( (:)
            <$> (try parseEscapedChar <|> noneOf ("\"" :: [Char]))
            <*> many (try parseEscapedChar <|> noneOf ("\"" :: [Char]))
        )

parseEscapedChar :: Parser Char
parseEscapedChar =
    choice
        [ try (string "\\\"" >> return '\"'),
          try (string "\\n" >> return '\n'),
          try (string "\\r" >> return '\r'),
          try (string "\\t" >> return '\t'),
          try (string "\\\\" >> return '\\')
        ]

parseTrue :: Parser Bool
parseTrue = lexeme $ string "true" >> return True

parseFalse :: Parser Bool
parseFalse = lexeme $ string "false" >> return False

parseBool :: Parser Bool
parseBool =
    lexeme
        ( choice
            [ try parseTrue,
              try parseFalse
            ]
        )
        <?> "Boolean"
