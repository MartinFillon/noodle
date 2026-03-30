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
        ),
        ( "huge_subobject.yaml",
          YObject
            [   ( "test",
                  YObject
                    [ ("depth", YNumber 100.0),
                      ("foo", YNumber 1.0),
                      ("bar", YNumber 2.0),
                      ("baz", YBool True),
                        ( "qux",
                          YObject
                            [   ( "quux",
                                  YObject
                                    [   ( "quuz",
                                          YObject [("quuux", YObject [("quuuux", YString "deeply nested value")])]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]
                )
            ]
        ),
        ( "object_array.yaml",
          YObject
            [   ( "a",
                  YObject
                    [ ("b", YArray [YNumber 1.0, YNumber 2.0, YObject [("baz", YBool True)]])
                    ]
                )
            ]
        )
    ]

numbersTests :: [(String, Yaml)]
numbersTests =
    [ ("integer.yaml", YNumber 12312.0),
      ("float.yaml", YNumber 312132.67),
        ( "hexadecimal.yaml",
          YObject [("lower", YNumber 0xab12), ("upper", YNumber 0xAB12)]
        ),
        ( "octal.yaml",
          YObject
            [ ("lower", YNumber 0o123),
              ("upper", YNumber 0O123),
              ("simple", YNumber 0o123)
            ]
        ),
        ( "scientific.yaml",
          YObject
            [ ("lower", YNumber 1.23456e+06),
              ("upper", YNumber 1.23456E+06)
            ]
        )
    ]

spec :: Spec
spec = do
    describe "Yaml Parser" $ do
        mapM_
            (\(file, result) -> testYaml ("test/Yaml/files/" ++ file) result)
            tests
        numbersSpec

numbersSpec :: Spec
numbersSpec = do
    describe "Numbers" $
        do
            mapM_
                (\(file, result) -> testYaml ("test/Yaml/files/numbers/" ++ file) result)
                numbersTests

testYaml :: String -> Yaml -> Spec
testYaml file result = do
    it file $ do
        content <- readFile file
        let parsed = parseYaml content
        parsed `shouldBe` Right result