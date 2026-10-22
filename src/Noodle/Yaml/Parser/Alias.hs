module Noodle.Yaml.Parser.Alias (parseAlias) where

import Control.Monad.State (get)
import Data.Char (isSpace)
import qualified Data.Map as Map
import Noodle.Yaml.Parser.Type (YamlParser)
import Noodle.Yaml.Type (Yaml)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (char)

parseAlias :: YamlParser Yaml
parseAlias =
  (char '*' *> takeWhile1P (Just "alias name") (\c -> not (isSpace c || c `elem` (",[]{}:" :: String)))) >>= \name ->
    get >>= \anchors ->
      maybe (fail $ "Undefined YAML alias: *" ++ name) return (Map.lookup name anchors)
