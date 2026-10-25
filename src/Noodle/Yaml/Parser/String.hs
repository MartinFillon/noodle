{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Yaml.Parser.String (
    parseDoubleQuoted,
    parseSingleQuoted,
    parseBlockScalar,
    parseBlockPlainString,
    parseFlowPlainString,
    parseStringBlock,
    parseStringBlockCollapsed,
    parseYString,
    parseCollapsedIndented,
    parseStringBlockIndented,
) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (chr, isSpace)
import Data.List (intercalate)
import Noodle.Parser.Utils (Parser, lexeme, parseEscapedChar, parseString)
import Noodle.Yaml.Type (Yaml (..))
import Numeric (readHex)
import Text.Megaparsec (
    anySingle,
    between,
    choice,
    count,
    eof,
    lookAhead,
    many,
    noneOf,
    notFollowedBy,
    optional,
    satisfy,
    some,
    takeWhileP,
    try,
    (<?>),
 )
import Text.Megaparsec.Char (char, eol, hexDigitChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

--- | Double-quoted string escape characters (JSON and YAML 1.2 compatible)
parseDqChar :: Parser Char
parseDqChar =
    choice
        [ try (string "\\\"" >> return '\"'),
          try (string "\\\\" >> return '\\'),
          try (string "\\/" >> return '/'),
          try (string "\\b" >> return '\b'),
          try (string "\\f" >> return '\f'),
          try (string "\\n" >> return '\n'),
          try (string "\\r" >> return '\r'),
          try (string "\\t" >> return '\t'),
          try (string "\\0" >> return '\0'),
          try (string "\\a" >> return '\a'),
          try (string "\\v" >> return '\v'),
          try (string "\\e" >> return '\ESC'),
          try (string "\\ " *> return ' '),
          try parseHexEscape,
          try parseUnicode4Escape,
          try parseUnicode8Escape,
          try (char '\\' *> eol *> skipHSpace *> return ' '),
          noneOf ("\"\\\r\n" :: String),
          try (eol *> skipHSpace *> return ' ')
        ]
  where
    skipHSpace = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')
    parseHexEscape =
        string "\\x" *> count 2 hexDigitChar >>= \h ->
            case readHex h of
                [(n, "")] -> return (chr n)
                _ -> fail "Invalid hex escape"
    parseUnicode4Escape =
        string "\\u" *> count 4 hexDigitChar >>= \h ->
            case readHex h of
                [(n, "")] -> return (chr n)
                _ -> fail "Invalid unicode escape"
    parseUnicode8Escape =
        string "\\U" *> count 8 hexDigitChar >>= \h ->
            case readHex h of
                [(n, "")] -> return (chr n)
                _ -> fail "Invalid unicode escape"

--- | Parse double-quoted string
parseDoubleQuoted :: Parser String
parseDoubleQuoted = between (char '\"') (char '\"') (many parseDqChar)

--- | Parse single-quoted string (with '' escaping ')
parseSingleQuoted :: Parser String
parseSingleQuoted =
    between (char '\'') (char '\'') $
        let singleQuoteEscaped = try (string "''" *> return '\'')
            singleChar = noneOf ("\'\r\n" :: String)
            lineBreak = eol *> takeWhileP Nothing (\c -> c == ' ' || c == '\t') *> return ' '
         in many (singleQuoteEscaped <|> singleChar <|> try lineBreak)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

checkNonEmpty :: String -> String -> Parser String
checkNonEmpty msg res
    | null res = fail msg
    | otherwise = return res

--- | Parse plain scalar in block context
parseBlockPlainString :: Parser String
parseBlockPlainString =
    let firstChar =
            try
                ( satisfy (\x -> x `elem` ("-?:" :: String))
                    <* notFollowedBy
                        (satisfy (\nextChar -> isSpace nextChar || nextChar `elem` ("\n\r" :: String)))
                )
                <|> satisfy
                    (\x -> not (isSpace x) && not (x `elem` ("-?:,[]{}#&*!|>'\"%@`" :: String)))
        restChar =
            try
                ( char ':'
                    <* notFollowedBy (satisfy (\x -> isSpace x || x `elem` ("\n\r" :: String)))
                )
                <|> satisfy (\x -> x /= ':' && x /= '\n' && x /= '\r' && x /= '#')
     in ((\c cs -> trim (c : cs)) <$> firstChar <*> many restChar)
            >>= checkNonEmpty "Empty plain scalar"

--- | Parse plain scalar in flow context
parseFlowPlainString :: Parser String
parseFlowPlainString =
    let firstChar =
            try
                ( satisfy (\x -> x `elem` ("-?:" :: String))
                    <* notFollowedBy
                        ( satisfy
                            (\nextChar -> isSpace nextChar || nextChar `elem` (",[]{}\n\r" :: String))
                        )
                )
                <|> satisfy
                    (\x -> not (isSpace x) && not (x `elem` ("-?:,[]{}#&*!|>'\"%@`" :: String)))
        restChar =
            try
                ( char ':'
                    <* notFollowedBy (satisfy (\x -> isSpace x || x `elem` (",[]{}\n\r" :: String)))
                )
                <|> satisfy (\x -> not (x `elem` (",[]{}\n\r#" :: String)))
     in ((\c cs -> trim (c : cs)) <$> firstChar <*> many restChar)
            >>= checkNonEmpty "Empty flow plain scalar"

--- | Block scalar chomping
data Chomping = Clip | Strip | Keep deriving (Show, Eq)

--- | Parse block scalar (| or >)
parseBlockScalar :: Int -> Parser Yaml
parseBlockScalar parentIndent =
    (char '|' <|> char '>') >>= \style ->
        choice [char '-' *> return Strip, char '+' *> return Keep, return Clip] >>= \chomp ->
            optional (read . pure <$> satisfy (\c -> c >= '1' && c <= '9')) >>= \explicitIndent ->
                skipLineEnd
                    *> parseLines explicitIndent
                    >>= \linesContent ->
                        let joined = case style of
                                '|' -> intercalate "\n" linesContent
                                '>' -> foldFolded linesContent
                                _ -> ""
                            result = case chomp of
                                Strip -> dropWhileEndNewline joined
                                Keep -> joined ++ "\n"
                                Clip -> joined
                         in return (YString result)
  where
    skipHSpace = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')
    skipComment = char '#' *> void (takeWhileP Nothing (\c -> c /= '\n' && c /= '\r'))
    skipLineEnd = skipHSpace *> optional skipComment *> (void eol <|> eof)
    dropWhileEndNewline = reverse . dropWhile (== '\n') . reverse

    foldFolded [] = ""
    foldFolded [x] = x
    foldFolded (x : y : xs)
        | null x && null y = "\n" ++ foldFolded (y : xs)
        | null x = "\n" ++ foldFolded (y : xs)
        | null y = x ++ "\n" ++ foldFolded xs
        | otherwise = x ++ " " ++ foldFolded (y : xs)

    parseLines maybeExp =
        getFirstContentLine maybeExp >>= \case
            Nothing -> return []
            Just (indent, firstLine) -> (firstLine :) <$> getSubsequentLines indent

    getFirstContentLine maybeExp =
        many (try (skipHSpace *> eol *> return ("" :: String)))
            *> lookAhead (choice [True <$ eof, False <$ anySingle])
            >>= \case
                True -> return Nothing
                False ->
                    takeWhileP Nothing (== ' ') >>= \spaces ->
                        let col = length spaces + 1
                            isTooIndented = maybe (col <= parentIndent) (\e -> col < parentIndent + e) maybeExp
                         in case isTooIndented of
                                True -> return Nothing
                                False ->
                                    let indent = maybe col (parentIndent +) maybeExp
                                        extraSpaces = col - indent
                                     in (\line _ -> Just (indent, replicate extraSpaces ' ' ++ line))
                                            <$> takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
                                            <*> (void eol <|> eof)

    getSubsequentLines indent =
        let parseOne =
                lookAhead (choice [True <$ eof, False <$ anySingle]) >>= \case
                    True -> return Nothing
                    False ->
                        takeWhileP Nothing (== ' ') >>= \spaces ->
                            let col = length spaces + 1
                             in lookAhead (choice [True <$ eol, False <$ anySingle]) >>= \case
                                    True -> (void eol <|> eof) *> return (Just "")
                                    False -> case col < indent of
                                        True -> return Nothing
                                        False ->
                                            let extra = col - indent
                                             in (\line _ -> Just (replicate extra ' ' ++ line))
                                                    <$> takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
                                                    <*> (void eol <|> eof)
         in parseOne >>= \case
                Nothing -> return []
                Just l -> (l :) <$> getSubsequentLines indent

--- | Backward-compatible block scalar combinators
parseStringBlockHelper ::
    ([String] -> String) -> Parser () -> Char -> Parser Yaml
parseStringBlockHelper constructor consumer blockChar =
    L.indentBlock consumer $
        lexeme consumer (char blockChar)
            *> return
                ( L.IndentSome
                    Nothing
                    (return . listStringToString constructor)
                    (parseYString consumer <* char '\n')
                )
{-# INLINE parseStringBlockHelper #-}

parseStringBlock :: Parser () -> Parser Yaml
parseStringBlock consumer = parseStringBlockHelper (intercalate "\n") consumer '|'

parseStringBlockCollapsed :: Parser () -> Parser Yaml
parseStringBlockCollapsed consumer = parseStringBlockHelper unwords consumer '>'

parseBlockIndentendHelper ::
    String ->
    ([String] -> String) ->
    Parser () ->
    Char ->
    Parser (L.IndentOpt Parser (String, Yaml) Yaml)
parseBlockIndentendHelper key constructor consumer blockChar =
    lexeme consumer (char blockChar)
        *> return
            ( L.IndentSome
                Nothing
                (return . (key,) . listStringToString constructor)
                (parseYString consumer <* char '\n')
            )
{-# INLINE parseBlockIndentendHelper #-}

parseStringBlockIndented ::
    String -> Parser () -> Parser (L.IndentOpt Parser (String, Yaml) Yaml)
parseStringBlockIndented key consumer = parseBlockIndentendHelper key (intercalate "\n") consumer '|'

parseCollapsedIndented ::
    String -> Parser () -> Parser (L.IndentOpt Parser (String, Yaml) Yaml)
parseCollapsedIndented key consumer = parseBlockIndentendHelper key unwords consumer '>'

listStringToString :: ([String] -> String) -> [Yaml] -> Yaml
listStringToString f = YString . f . map (\case YString s -> s; _ -> error "Expected YString")

parseYString :: Parser () -> Parser Yaml
parseYString x =
    lexeme
        x
        ( YString
            <$> ( try (parseString x)
                    <|> some (try parseEscapedChar <|> noneOf ("\n\t\r#" :: String))
                )
        )
        <?> "Expected a string"
