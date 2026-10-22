{-# LANGUAGE OverloadedStrings #-}

module Noodle.Yaml.Parser (parseYaml) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.State (evalStateT, get, lift, modify)
import Data.Char (isSpace)
import qualified Data.Map as Map
import Noodle.Parser.Utils (ParserError)
import Noodle.Yaml.Parser.Numbers (parsePlainNumber)
import Noodle.Yaml.Parser.String
  ( parseBlockPlainString,
    parseBlockScalar,
    parseDoubleQuoted,
    parseFlowPlainString,
    parseSingleQuoted,
  )
import Noodle.Yaml.Parser.Type (YamlParser)
import Noodle.Yaml.Parser.Utils (flowSpace, isSpaceOrNewline, skipBlankLines, skipHSpace, skipHSpace1, skipLineEnd)
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec
  ( choice,
    count,
    eof,
    lookAhead,
    many,
    notFollowedBy,
    optional,
    parse,
    satisfy,
    sepEndBy,
    some,
    takeWhile1P,
    takeWhileP,
    try,
  )
import Text.Megaparsec.Char (char, eol, string)

--- | Mapping keys
parseMappingKey :: YamlParser String
parseMappingKey =
  choice
    [ lift parseDoubleQuoted,
      lift parseSingleQuoted,
      parsePlainKey
    ]
  where
    parsePlainKey =
      (\c cs -> trim (c : cs))
        <$> satisfy (\x -> not (isSpace x) && not (x `elem` ("-?:,[]{}#&*!|>'\"%@`" :: String)))
        <*> many
          ( try (char ':' <* notFollowedBy (satisfy (\x -> isSpace x || x `elem` ("\n\r" :: String))))
              <|> satisfy (\x -> x /= ':' && x /= '\n' && x /= '\r' && x /= '#')
          )

isKeyFollowedByColon :: YamlParser ()
isKeyFollowedByColon =
  void $
    lookAhead $
      try $
        parseMappingKey
          *> skipHSpace
          *> char ':'
          *> (satisfy (\c -> isSpace c || c `elem` ("\n\r" :: String)) <|> (eof *> return ' '))

--- | Block Mapping
parseBlockMapping :: Int -> YamlParser [(String, Yaml)]
parseBlockMapping col =
  ((,) <$> (parseMappingKey <* skipHSpace <* char ':' <* skipHSpace) <*> parseValueAfterColon col)
    >>= \pair ->
      skipBlankLines
        *> choice
          [ try (count (col - 1) (char ' ') <* notFollowedBy (char ' ') <* lookAhead isKeyFollowedByColon)
              *> ((pair :) <$> parseBlockMapping col),
            return [pair]
          ]

--- | Parse value after colon
parseValueAfterColon :: Int -> YamlParser Yaml
parseValueAfterColon keyCol =
  withAnchor $
    choice
      [ try (lift (parseBlockScalar keyCol)),
        try parseFlowSequence <* skipLineEnd,
        try parseFlowMapping <* skipLineEnd,
        try parseAlias <* skipLineEnd,
        try (YString <$> lift parseDoubleQuoted <* skipLineEnd),
        try (YString <$> lift parseSingleQuoted <* skipLineEnd),
        try (resolvePlainScalar <$> lift parseBlockPlainString <* skipLineEnd),
        skipLineEnd *> skipBlankLines *> parseIndentedChild keyCol
      ]

--- | Parse indented child or null
parseIndentedChild :: Int -> YamlParser Yaml
parseIndentedChild parentCol =
  lookAhead (choice [Nothing <$ eof, Just . (+ 1) . length <$> many (char ' ')]) >>= parseIndentedChild' parentCol

parseIndentedChild' :: Int -> Maybe Int -> YamlParser Yaml
parseIndentedChild' parentCol (Just childCol) | childCol > parentCol = count (childCol - 1) (char ' ') *> parseIndentedBlock childCol
parseIndentedChild' _ _ = return YNull

--- | Parse indented block
parseIndentedBlock :: Int -> YamlParser Yaml
parseIndentedBlock childCol =
  withAnchor $
    choice
      [ try (YArray <$> (lookAhead (char '-' *> satisfy isSpaceOrNewline) *> parseBlockSequence childCol)),
        try (lookAhead isKeyFollowedByColon *> (YObject . mergePairs <$> parseBlockMapping childCol)),
        parseIndentedScalar childCol
      ]

--- | Block Sequence
parseBlockSequence :: Int -> YamlParser [Yaml]
parseBlockSequence col =
  char '-' *> parseSequenceItem col >>= \item ->
    skipBlankLines
      *> choice
        [ try (count (col - 1) (char ' ') <* lookAhead (char '-' *> satisfy isSpaceOrNewline))
            *> ((item :) <$> parseBlockSequence col),
          return [item]
        ]

parseSequenceItem :: Int -> YamlParser Yaml
parseSequenceItem col =
  withAnchor $
    choice
      [ try (skipHSpace1 *> isKeyFollowedByColon *> (YObject . mergePairs <$> parseBlockMapping (col + 2))),
        skipHSpace1
          *> choice
            [ try (lift (parseBlockScalar col)),
              try parseFlowSequence <* skipLineEnd,
              try parseFlowMapping <* skipLineEnd,
              try parseAlias <* skipLineEnd,
              try (YString <$> lift parseDoubleQuoted <* skipLineEnd),
              try (YString <$> lift parseSingleQuoted <* skipLineEnd),
              resolvePlainScalar <$> lift parseBlockPlainString <* skipLineEnd
            ],
        skipLineEnd *> skipBlankLines *> parseIndentedChild col
      ]

--- | Parse indented scalar
parseIndentedScalar :: Int -> YamlParser Yaml
parseIndentedScalar indent =
  choice
    [ lift (parseBlockScalar (indent - 1)),
      parseFlowSequence <* skipLineEnd,
      parseFlowMapping <* skipLineEnd,
      parseAlias <* skipLineEnd,
      YString <$> lift parseDoubleQuoted <* skipLineEnd,
      YString <$> lift parseSingleQuoted <* skipLineEnd,
      resolvePlainScalar <$> lift parseBlockPlainString <* skipLineEnd
    ]

--- | Top-level document parser
parseDocument :: YamlParser Yaml
parseDocument =
  many (try (char '%' *> takeWhileP Nothing (\c -> c /= '\n' && c /= '\r') *> eol))
    *> skipBlankLines
    *> optional (try (string "---" *> skipLineEnd *> skipBlankLines))
    *> choice
      [ try (lookAhead (choice [eof, void (try (string "..." *> skipLineEnd))]) *> return YNull),
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
