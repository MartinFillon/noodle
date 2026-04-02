{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Noodle.Yaml.Parser.String (
    parseStringBlock,
    parseStringBlockCollapsed,
    parseYString,
    parseCollapsedIndented,
    parseStringBlockIndented,
) where

import Data.List (intercalate)
import Noodle.Parser.Utils (Parser, lexeme, parseEscapedChar, parseString)
import Noodle.Yaml.Type (Yaml (..))
import Text.Megaparsec (noneOf, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

parseStringBlockHelper ::
    ([String] -> String) -> Parser () -> Char -> Parser Yaml
parseStringBlockHelper constructor consumer blockChar = L.indentBlock consumer $ do
    _ <- lexeme consumer $ char blockChar
    return $
        L.IndentSome
            Nothing
            (return . listStringToString constructor)
            (parseYString consumer <* char '\n')
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
parseBlockIndentendHelper key constructor consumer blockChar = do
    _ <- lexeme consumer $ char blockChar
    return $
        L.IndentSome
            Nothing
            (return . (key,) . listStringToString constructor)
            (parseYString consumer <* char '\n')
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
