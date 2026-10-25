module Noodle.Yaml.Parser.Numbers (
    parseYNumber,
    parsePlainNumber,
    parseSpecialFloat,
    parseHexNum,
    parseOctNum,
    parseInfinity,
    parseNaN,
    parseScientific,
) where

import Control.Applicative ((<|>))
import Data.Scientific (Scientific, toRealFloat)
import Noodle.Parser.Utils (Parser, lexeme, parseNumber)
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (choice, eof, runParser, try, (<?>))
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L

--- | Parse special float values: NaN, +inf, -inf
parseSpecialFloat :: Parser Double
parseSpecialFloat =
    choice
        [ try (choice [string ".nan", string ".NaN", string ".NAN"] *> return (0 / 0)),
          try
            ( choice
                [ string ".inf",
                  string ".Inf",
                  string ".INF",
                  string "+.inf",
                  string "+.Inf",
                  string "+.INF"
                ]
                *> return (1 / 0)
            ),
          try (choice [string "-.inf", string "-.Inf", string "-.INF"] *> return (-1 / 0))
        ]

--- | Parse hexadecimal integers (with optional sign)
parseHexNum :: Parser Double
parseHexNum =
    (\sign _ val -> fromIntegral (sign val))
        <$> choice [char '+' *> return id, char '-' *> return negate, return id]
        <*> choice [string "0x", string "0X"]
        <*> (L.hexadecimal :: Parser Integer)

--- | Parse octal integers (with optional sign)
parseOctNum :: Parser Double
parseOctNum =
    (\sign _ val -> fromIntegral (sign val))
        <$> choice [char '+' *> return id, char '-' *> return negate, return id]
        <*> choice [string "0o", string "0O", string "0"]
        <*> (L.octal :: Parser Integer)

--- | Parse a number from a plain scalar string
parsePlainNumber :: String -> Maybe Double
parsePlainNumber str =
    runParser' (parseSpecialFloat <* eof)
        <|> runParser' (parseHexNum <* eof)
        <|> runParser' (parseOctNum <* eof)
        <|> ( toRealFloat
                <$> runParser' ((L.signed (return ()) L.scientific :: Parser Scientific) <* eof)
            )
  where
    runParser' p = case runParser p "" str of
        Right val -> Just val
        Left _ -> Nothing

--- | Legacy parseYNumber combinators
parseHexadecimal :: Parser Yaml
parseHexadecimal = YNumber <$> parseHexNum <?> "Hexadecimal Number"

parseOctal :: Parser Yaml
parseOctal = YNumber <$> parseOctNum <?> "Octal Number"

baseMap :: [(String, Parser Yaml)]
baseMap =
    [ ("0x", parseHexadecimal),
      ("0X", parseHexadecimal),
      ("0o", parseOctal),
      ("0O", parseOctal),
      ("0", parseOctal)
    ]

parseNumberBase :: Parser () -> Parser Yaml
parseNumberBase x =
    lexeme x $
        choice [try (string prefix *> parser) | (prefix, parser) <- baseMap]

parseInfinity :: Parser () -> Parser Yaml
parseInfinity x =
    lexeme x (YNumber <$> parseSpecialFloat) <?> "Infinity"

parseNaN :: Parser () -> Parser Yaml
parseNaN x =
    lexeme x (YNumber <$> parseSpecialFloat) <?> "NaN"

parseScientific :: Parser () -> Parser Yaml
parseScientific x =
    lexeme
        x
        (YNumber . toRealFloat <$> (L.signed x L.scientific :: Parser Scientific))
        <?> "Scientific Notation"

parseYNumber' :: Parser () -> Parser Yaml
parseYNumber' x = lexeme x $ YNumber <$> parseNumber x

parseYNumber :: Parser () -> Parser Yaml
parseYNumber x =
    choice $
        map
            try
            [ parseNaN x,
              parseInfinity x,
              parseNumberBase x,
              parseScientific x,
              parseYNumber' x
            ]
