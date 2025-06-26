module Noodle (
    module Noodle.Json,
    module Noodle.Yaml,
    module Noodle.Serialize,
    module Noodle.Serializer,
    module Noodle.Toml,
    module Noodle.Deserialize,
    module Noodle.Deserializer,
    test,
    t1,
    t2,
) where

import Noodle.Deserialize
import Noodle.Deserializer
import Noodle.Json
import Noodle.Parser.Utils (ParserError)
import Noodle.Serialize
import Noodle.Serializer
import Noodle.Toml
import Noodle.Yaml
import Text.Megaparsec (errorBundlePretty)

test :: Show a => String -> (String -> Either ParserError a) -> IO ()
test input parser = do
    case parser input of
        Left err -> putStr $ errorBundlePretty err
        Right result -> print result

t1 :: String
t1 = "- 1\n- 2\n- 3\n"

t2 :: String
t2 =
    "key1: value1\n"
    ++ "key2: value2\n"
    ++ "key3:\n"
    ++ "  - item1\n"
    ++ "  - item2\n"
    ++ "  - item3\n"
    ++ "key4:\n"
    ++ "  key5: value5\n"
    ++ "  key6: value6\n"
