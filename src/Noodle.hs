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
    t3,
    testFromFile,
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

testFromFile :: Show a => String -> (String -> Either ParserError a) -> IO ()
testFromFile file parser = do
    content <- readFile file
    test content parser

t1 :: String
t1 = "- t1\n- 2\n- 3\n"

t2 :: String
t2 =
    "key3:\n"
        ++ "  key7: \n"
        ++ "    caca: value7\n"

t3 :: String
t3 =
    "- 1\n"

-- ++ "- 2\n"
-- ++ "- 3\n"
-- ++ "- \n"
-- ++ "  - 4\n"
-- ++ "  - 5\n"