{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noodle.Yaml.Parser (parseYaml) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.State (StateT, evalStateT, get, lift, modify)
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as Map
import Noodle.Parser.Utils (Parser, ParserError)
import Noodle.Yaml.Parser.Numbers (parsePlainNumber)
import Noodle.Yaml.Parser.String (
    parseBlockPlainString,
    parseBlockScalar,
    parseDoubleQuoted,
    parseFlowPlainString,
    parseSingleQuoted,
 )
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (
    choice,
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

type YamlParser = StateT (Map String Yaml) Parser

--- | Consume horizontal spaces (spaces and tabs)
skipHSpace :: YamlParser ()
skipHSpace = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')

skipHSpace1 :: YamlParser ()
skipHSpace1 = void $ takeWhile1P (Just "horizontal space") (\c -> c == ' ' || c == '\t')

--- | Consume comment to the end of the line
skipComment :: YamlParser ()
skipComment = char '#' *> void (takeWhileP Nothing (\c -> c /= '\n' && c /= '\r'))

--- | Consume trailing spaces and optional comment on the line, then consume newline or eof
skipLineEnd :: YamlParser ()
skipLineEnd = skipHSpace *> optional skipComment *> (void eol <|> eof)

--- | Skip empty lines and comment-only lines
skipBlankLines :: YamlParser ()
skipBlankLines = void $ many (try (skipHSpace *> optional skipComment *> eol))

--- | Whitespace parser in flow context (skips spaces, tabs, newlines, and comments)
flowSpace :: YamlParser ()
flowSpace = void $ many (try (void (satisfy (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')) <|> skipComment))

isSpaceOrNewline :: Char -> Bool
isSpaceOrNewline c = isSpace c || c `elem` ("\n\r" :: String)

--- | Resolve an unquoted scalar string according to YAML 1.2 Core Schema
resolvePlainScalar :: String -> Yaml
resolvePlainScalar s
    | s `elem` ["~", "null", "Null", "NULL"] = YNull
    | s `elem` ["true", "True", "TRUE"] = YBool True
    | s `elem` ["false", "False", "FALSE"] = YBool False
    | Just n <- parsePlainNumber s = YNumber n
    | otherwise = YString s

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

--- | Flow Collections
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
parseFlowKey = choice
    [ lift parseDoubleQuoted
    , lift parseSingleQuoted
    , trim <$> some (satisfy (\c -> not (isSpace c) && not (c `elem` (":,[]{}" :: String))))
    ]

parseFlowValue :: YamlParser Yaml
parseFlowValue = withAnchor $ choice
    [ parseFlowSequence
    , parseFlowMapping
    , parseAlias
    , YString <$> lift parseDoubleQuoted
    , YString <$> lift parseSingleQuoted
    , resolvePlainScalar <$> lift parseFlowPlainString
    ]

--- | Anchor & Alias handling
parseAnchor :: YamlParser (Maybe String)
parseAnchor = optional (char '&' *> takeWhile1P (Just "anchor name") (\c -> not (isSpace c || c `elem` (",[]{}:" :: String))) <* skipHSpace)

parseAlias :: YamlParser Yaml
parseAlias =
    (char '*' *> takeWhile1P (Just "alias name") (\c -> not (isSpace c || c `elem` (",[]{}:" :: String)))) >>= \name ->
        get >>= \anchors ->
            maybe (fail $ "Undefined YAML alias: *" ++ name) return (Map.lookup name anchors)

registerAnchor :: Maybe String -> Yaml -> YamlParser Yaml
registerAnchor Nothing val = return val
registerAnchor (Just name) val = modify (Map.insert name val) *> return val

withAnchor :: YamlParser Yaml -> YamlParser Yaml
withAnchor p = parseAnchor >>= \mAnchor -> p >>= registerAnchor mAnchor

--- | Mapping keys
parseMappingKey :: YamlParser String
parseMappingKey = choice
    [ lift parseDoubleQuoted
    , lift parseSingleQuoted
    , parsePlainKey
    ]
  where
    parsePlainKey =
        (\c cs -> trim (c : cs))
            <$> satisfy (\x -> not (isSpace x) && not (x `elem` ("-?:,[]{}#&*!|>'\"%@`" :: String)))
            <*> many (try (char ':' <* notFollowedBy (satisfy (\x -> isSpace x || x `elem` ("\n\r" :: String))))
                    <|> satisfy (\x -> x /= ':' && x /= '\n' && x /= '\r' && x /= '#'))

isKeyFollowedByColon :: YamlParser ()
isKeyFollowedByColon = void $ lookAhead $ try $
    parseMappingKey
        *> skipHSpace
        *> char ':'
        *> (satisfy (\c -> isSpace c || c `elem` ("\n\r" :: String)) <|> (eof *> return ' '))

mergePairs :: [(String, Yaml)] -> [(String, Yaml)]
mergePairs [] = []
mergePairs (("<<", YObject fields) : rest) = fields ++ mergePairs rest
mergePairs (("<<", YArray objs) : rest) = concatMap extractFields objs ++ mergePairs rest
  where
    extractFields (YObject fs) = fs
    extractFields _ = []
mergePairs (kv : rest) = kv : mergePairs rest

--- | Block Mapping
parseBlockMapping :: Int -> YamlParser [(String, Yaml)]
parseBlockMapping col =
    ((,) <$> (parseMappingKey <* skipHSpace <* char ':' <* skipHSpace) <*> parseValueAfterColon col)
        >>= \pair ->
            skipBlankLines
                *> choice
                    [ try (count (col - 1) (char ' ') <* notFollowedBy (char ' ') <* lookAhead isKeyFollowedByColon)
                        *> ((pair :) <$> parseBlockMapping col)
                    , return [pair]
                    ]

--- | Parse value after colon
parseValueAfterColon :: Int -> YamlParser Yaml
parseValueAfterColon keyCol =
    withAnchor $
        choice
            [ try (lift (parseBlockScalar keyCol))
            , try parseFlowSequence <* skipLineEnd
            , try parseFlowMapping <* skipLineEnd
            , try parseAlias <* skipLineEnd
            , try (YString <$> lift parseDoubleQuoted <* skipLineEnd)
            , try (YString <$> lift parseSingleQuoted <* skipLineEnd)
            , try (resolvePlainScalar <$> lift parseBlockPlainString <* skipLineEnd)
            , skipLineEnd *> skipBlankLines *> parseIndentedChild keyCol
            ]

--- | Parse indented child or null
parseIndentedChild :: Int -> YamlParser Yaml
parseIndentedChild parentCol =
    lookAhead (choice [Nothing <$ eof, Just . (+ 1) . length <$> many (char ' ')]) >>= \case
        Just childCol | childCol > parentCol ->
            count (childCol - 1) (char ' ') *> parseIndentedBlock childCol
        _ -> return YNull

--- | Parse indented block
parseIndentedBlock :: Int -> YamlParser Yaml
parseIndentedBlock childCol =
    withAnchor $
        choice
            [ try (YArray <$> (lookAhead (char '-' *> satisfy isSpaceOrNewline) *> parseBlockSequence childCol))
            , try (lookAhead isKeyFollowedByColon *> (YObject . mergePairs <$> parseBlockMapping childCol))
            , parseIndentedScalar childCol
            ]

--- | Block Sequence
parseBlockSequence :: Int -> YamlParser [Yaml]
parseBlockSequence col =
    char '-' *> parseSequenceItem col >>= \item ->
        skipBlankLines
            *> choice
                [ try (count (col - 1) (char ' ') <* lookAhead (char '-' *> satisfy isSpaceOrNewline))
                    *> ((item :) <$> parseBlockSequence col)
                , return [item]
                ]

parseSequenceItem :: Int -> YamlParser Yaml
parseSequenceItem col =
    withAnchor $
        choice
            [ try (skipHSpace1 *> isKeyFollowedByColon *> (YObject . mergePairs <$> parseBlockMapping (col + 2)))
            , skipHSpace1
                *> choice
                    [ try (lift (parseBlockScalar col))
                    , try parseFlowSequence <* skipLineEnd
                    , try parseFlowMapping <* skipLineEnd
                    , try parseAlias <* skipLineEnd
                    , try (YString <$> lift parseDoubleQuoted <* skipLineEnd)
                    , try (YString <$> lift parseSingleQuoted <* skipLineEnd)
                    , resolvePlainScalar <$> lift parseBlockPlainString <* skipLineEnd
                    ]
            , skipLineEnd *> skipBlankLines *> parseIndentedChild col
            ]

--- | Parse indented scalar
parseIndentedScalar :: Int -> YamlParser Yaml
parseIndentedScalar indent = choice
    [ lift (parseBlockScalar (indent - 1))
    , parseFlowSequence <* skipLineEnd
    , parseFlowMapping <* skipLineEnd
    , parseAlias <* skipLineEnd
    , YString <$> lift parseDoubleQuoted <* skipLineEnd
    , YString <$> lift parseSingleQuoted <* skipLineEnd
    , resolvePlainScalar <$> lift parseBlockPlainString <* skipLineEnd
    ]

--- | Top-level document parser
parseDocument :: YamlParser Yaml
parseDocument =
    many (try (char '%' *> takeWhileP Nothing (\c -> c /= '\n' && c /= '\r') *> eol))
        *> skipBlankLines
        *> optional (try (string "---" *> skipLineEnd *> skipBlankLines))
        *> choice
            [ try (lookAhead (choice [eof, void (try (string "..." *> skipLineEnd))]) *> return YNull)
            , (many (char ' ') >>= \spaces -> parseIndentedBlock (length spaces + 1))
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
