module Noodle.Yaml.Parser.Utils (
    skipHSpace,
    skipHSpace1,
    skipComment,
    skipLineEnd,
    skipBlankLines,
    flowSpace,
    isSpaceOrNewline,
    resolvePlainScalar,
    trim,
    mergePairs,
) where

import Control.Monad (void)
import Data.Char (isSpace)
import Noodle.Yaml.Parser.Numbers (parsePlainNumber)
import Noodle.Yaml.Parser.Type (YamlParser)
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (
    MonadParsec (eof, takeWhile1P, takeWhileP, try),
    many,
    optional,
    satisfy,
    (<|>),
 )
import Text.Megaparsec.Char (char, eol)

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
flowSpace =
    void $
        many
            ( try
                ( void (satisfy (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r'))
                    <|> skipComment
                )
            )

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

mergePairs :: [(String, Yaml)] -> [(String, Yaml)]
mergePairs [] = []
mergePairs (("<<", YObject fields) : rest) = fields ++ mergePairs rest
mergePairs (("<<", YArray objs) : rest) = concatMap extractFields objs ++ mergePairs rest
  where
    extractFields (YObject fs) = fs
    extractFields _ = []
mergePairs (kv : rest) = kv : mergePairs rest
