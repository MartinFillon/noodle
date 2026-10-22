module Noodle.Yaml.Parser.Type (YamlParser) where

import Control.Monad.State (StateT)
import Data.Map (Map)
import Noodle.Parser.Utils (Parser)
import Noodle.Yaml.Type (Yaml)

type YamlParser = StateT (Map String Yaml) Parser
