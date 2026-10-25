module Noodle.Yaml.Parser.Flow (parseFlowSequence, parseFlowMapping) where

import Control.Applicative (some, (<|>))
import Control.Monad.State (lift)
import Data.Char (isSpace)
import Noodle.Yaml.Parser.Alias (parseAlias)
import Noodle.Yaml.Parser.Anchor (withAnchor)
import Noodle.Yaml.Parser.String (parseDoubleQuoted, parseFlowPlainString, parseSingleQuoted)
import Noodle.Yaml.Parser.Type (YamlParser)
import Noodle.Yaml.Parser.Utils (flowSpace, mergePairs, resolvePlainScalar, trim)
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (choice, satisfy, sepEndBy)
import Text.Megaparsec.Char (char)

parseFlowSequence :: YamlParser Yaml
parseFlowSequence =
  YArray
    <$> ( char '['
            *> flowSpace
            *> (parseFlowValue `sepEndBy` (char ',' <* flowSpace))
            <* flowSpace
            <* char ']'
        )

parseFlowMapping :: YamlParser Yaml
parseFlowMapping =
  YObject . mergePairs
    <$> ( char '{'
            *> flowSpace
            *> (parseFlowPair `sepEndBy` (char ',' <* flowSpace))
            <* flowSpace
            <* char '}'
        )

parseFlowPair :: YamlParser (String, Yaml)
parseFlowPair =
  (,)
    <$> parseFlowKey
    <*> (flowSpace *> char ':' *> flowSpace *> (parseFlowValue <|> return YNull))

parseFlowKey :: YamlParser String
parseFlowKey =
  choice
    [ lift parseDoubleQuoted,
      lift parseSingleQuoted,
      trim <$> some (satisfy (\c -> not (isSpace c) && not (c `elem` (":,[]{}" :: String))))
    ]

parseFlowValue :: YamlParser Yaml
parseFlowValue =
  withAnchor $
    choice
      [ parseFlowSequence,
        parseFlowMapping,
        parseAlias,
        YString <$> lift parseDoubleQuoted,
        YString <$> lift parseSingleQuoted,
        resolvePlainScalar <$> lift parseFlowPlainString
      ]