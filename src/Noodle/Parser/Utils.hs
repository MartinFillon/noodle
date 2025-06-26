module Noodle.Parser.Utils (
    Parser,
    sc,
    sce,
    lexeme,
    parseNumber,
    ParserError,
    parseString,
    parseBool,
    parseEscapedChar,
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

sc :: Parser () -> Parser ()
sc lineComment =
    L.space
        (void $ some (char ' ' <|> char '\t' <|> char '\n' <|> char '\r'))
        lineComment
        empty

sce :: Parser ()
sce = L.space empty empty empty

lexeme :: Parser () -> Parser a -> Parser a
lexeme = L.lexeme

parseNumber :: Parser () -> Parser Double
parseNumber sc' =
    lexeme
        sc'
        ( try (L.signed sce L.float) <|> fromIntegral
            <$> (L.signed sce L.decimal :: Parser Integer)
        )
        <?> "Number"

parseString :: Parser () -> Parser String
parseString sc' =
    lexeme sc' $
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

parseTrue :: Parser () -> Parser Bool
parseTrue sc' = lexeme sc' $ string "true" >> return True

parseFalse :: Parser () -> Parser Bool
parseFalse sc' = lexeme sc' $ string "false" >> return False

parseBool :: Parser () -> Parser Bool
parseBool sc' =
    lexeme
        sc'
        ( choice
            [ try $ parseTrue sc',
              try $ parseFalse sc'
            ]
        )
        <?> "Boolean"
