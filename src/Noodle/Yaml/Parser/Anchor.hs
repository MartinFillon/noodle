module Noodle.Yaml.Parser.Anchor (parseAnchor, registerAnchor, withAnchor) where

import Noodle.Yaml.Parser.Type (YamlParser)
import Noodle.Yaml.Type (Yaml)
import Text.Megaparsec (takeWhile1P, optional)
import Data.Char (isSpace)
import Noodle.Yaml.Parser.Utils (skipHSpace)
import Text.Megaparsec.Char (char)
import Control.Monad.State (modify)
import qualified Data.Map as Map

parseAnchor :: YamlParser (Maybe String)
parseAnchor = optional (char '&' *> takeWhile1P (Just "anchor name") (\c -> not (isSpace c || c `elem` (",[]{}:" :: String))) <* skipHSpace)

registerAnchor :: Maybe String -> Yaml -> YamlParser Yaml
registerAnchor Nothing val = return val
registerAnchor (Just name) val = modify (Map.insert name val) *> return val

withAnchor :: YamlParser Yaml -> YamlParser Yaml
withAnchor p = parseAnchor >>= \mAnchor -> p >>= registerAnchor mAnchor
