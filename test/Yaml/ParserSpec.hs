module Yaml.ParserSpec (spec) where

import Noodle (Yaml (..), parseYaml)
import Test.Hspec (Spec, describe, it, shouldBe)

tests :: [(String, Yaml)]
tests =
    [ ("object.yaml", YObject [("foo", YString "bar"), ("baz", YNumber 3.0)]),
      ("string.yaml", YString "hello"),
      ("number.yaml", YNumber 3.0),
      ("array.yaml", YArray [YNumber 1.0, YNumber 2.0, YNumber 3.0]),
      ("boolean.yaml", YBool True),
        ( "nested.yaml",
          YObject
            [   ( "test",
                  YObject [("foo", YNumber 1.0), ("bar", YNumber 2.0), ("baz", YBool True)]
                )
            ]
        ),
        ( "deep_nested.yaml",
          YObject
            [   ( "test",
                  YObject
                    [   ( "a",
                          YObject
                            [ ("b", YObject [("c", YNumber 1.0), ("d", YNumber 2.0)])
                            ]
                        )
                    ]
                )
            ]
        )
    ]

spec :: Spec
spec = do
    describe "Yaml Parser" $ do
        mapM_
            (\(file, result) -> testYaml ("test/Yaml/files/" ++ file) result)
            tests

testYaml :: String -> Yaml -> Spec
testYaml file result = do
    it ("should open " ++ file ++ " and parse " ++ show result) $ do
        content <- readFile file
        let parsed = parseYaml content
        parsed `shouldBe` Right result
