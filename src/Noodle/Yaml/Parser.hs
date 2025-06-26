module Noodle.Yaml.Parser (parseYaml) where

import Noodle.Parser.Utils (Parser, ParserError, sc)
import Noodle.Yaml.Type (Yaml)
import Text.Megaparsec (MonadParsec (eof), between, parse)

parseValue :: Parser Yaml
parseValue = fail "Not Implemented"

parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (between sc eof parseValue) ""
