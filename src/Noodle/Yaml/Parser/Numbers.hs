module Noodle.Yaml.Parser.Numbers (parseYNumber) where

import Data.Scientific (toRealFloat)
import Noodle.Parser.Utils (Parser, lexeme, parseNumber)
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (choice, try, (<?>))
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L

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

parseHexadecimal :: Parser Yaml
parseHexadecimal =
    YNumber . fromIntegral
        <$> ( L.hexadecimal :: Parser Integer
            )
            <?> "Hexadecimal Number"

parseOctal :: Parser Yaml
parseOctal =
    YNumber . fromIntegral
        <$> ( L.octal :: Parser Integer
            )
            <?> "Octal Number"

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
        choice $
            [try (string prefix *> parser) | (prefix, parser) <- baseMap]

parseInfinity :: Parser () -> Parser Yaml
parseInfinity x =
    lexeme
        x
        ( choice
            [ try (string inf >> return (YNumber (1 / 0)))
              | inf <- [".inf", ".Inf", ".INF", "-.inf", "-.Inf", "-.INF"]
            ]
        )
        <?> "Infinity"

parseNaN :: Parser () -> Parser Yaml
parseNaN x =
    lexeme
        x
        ( choice
            [ try (string nan >> return (YNumber (0 / 0)))
              | nan <- [".nan", ".NaN", ".NAN"]
            ]
        )
        <?> "NaN"

parseScientific :: Parser () -> Parser Yaml
parseScientific x =
    lexeme
        x
        ( YNumber . toRealFloat
            <$> L.signed x L.scientific
        )
        <?> "Scientific Notation"
