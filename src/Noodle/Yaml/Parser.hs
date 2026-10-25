module Noodle.Yaml.Parser (parseYaml) where

import Control.Monad (void)
import Control.Monad.State (evalStateT)
import qualified Data.Map as Map
import Noodle.Parser.Utils (ParserError)
import Noodle.Yaml.Parser.Items (parseIndentedBlock)
import Noodle.Yaml.Parser.Type (YamlParser)
import Noodle.Yaml.Parser.Utils (skipBlankLines, skipLineEnd)
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (
    choice,
    eof,
    lookAhead,
    many,
    optional,
    parse,
    takeWhileP,
    try,
 )
import Text.Megaparsec.Char (char, eol, string)

--- | Top-level document parser
parseDocument :: YamlParser Yaml
parseDocument =
    many
        (try (char '%' *> takeWhileP Nothing (\c -> c /= '\n' && c /= '\r') *> eol))
        *> skipBlankLines
        *> optional (try (string "---" *> skipLineEnd *> skipBlankLines))
        *> choice
            [ try
                ( lookAhead (choice [eof, void (try (string "..." *> skipLineEnd))])
                    *> return YNull
                ),
              (many (char ' ') >>= \spaces -> parseIndentedBlock (length spaces + 1))
                <* skipBlankLines
                <* optional (try (string "..." *> skipLineEnd))
                <* skipBlankLines
            ]

--- | Parse a YAML string into a 'Yaml' value.
--- It conforms to the YAML 1.2.2 specification:
--- - JSON-compatible flow mapping and sequence syntax
--- - Indentation-based block mappings and sequences
--- - YAML 1.2 Core Schema scalar resolution (null, boolean, integer, float, string)
--- - Double-quoted and single-quoted strings with escapes
--- - Literal and folded block scalars (| and >)
--- - Document start/end markers and directives (---, ..., %YAML)
--- - Anchors, aliases, and merge keys (&, *, <<)
---
--- >> parseYaml "name: Alice\nage: 30\nisStudent: false"
--- Right (YObject [("name",YString "Alice"),("age",YNumber 30.0),("isStudent",YBool False)])
parseYaml :: String -> Either ParserError Yaml
parseYaml = parse (evalStateT (parseDocument <* eof) Map.empty) ""
