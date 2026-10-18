module Yaml.ParserSpec (spec) where

import Noodle (Yaml (..), parseYaml)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

tests :: [(String, Yaml)]
tests =
    [ ("object.yaml", YObject [("foo", YString "bar"), ("baz", YNumber 3.0)]),
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

stringsTests :: [(String, Yaml)]
stringsTests =
    [   ( "block.yaml",
          YObject
            [   ( "string",
                  YString
                    "This is a block scalar with literal style.\nIt preserves newlines and spaces."
                )
            ]
        ),
        ( "collapsed.yaml",
          YObject
            [   ( "string",
                  YString
                    "This is a folded scalar with folded style. It replaces newlines with spaces."
                )
            ]
        ),
        ( "complex.yaml",
          YObject
            [   ( "string",
                  YString
                    "This is a folded scalar with folded style. It replaces newlines with spaces."
                ),
              ("test", YBool False)
            ]
        ),
      ("basic.yaml", YString "hello")
    ]

spec :: Spec
spec = do
    describe "Yaml Parser" $ do
        mapM_
            (\(file, result) -> testYaml ("test/Yaml/files/" ++ file) result)
            tests
        numbersSpec
        stringsSpec
        yaml12Spec

numbersSpec :: Spec
numbersSpec = do
    describe "Numbers" $
        do
            mapM_
                (\(file, result) -> testYaml ("test/Yaml/files/numbers/" ++ file) result)
                numbersTests

stringsSpec :: Spec
stringsSpec = do
    describe "Strings" $
        do
            mapM_
                (\(file, result) -> testYaml ("test/Yaml/files/strings/" ++ file) result)
                stringsTests

testYaml :: String -> Yaml -> Spec
testYaml file result = do
    it file $ do
        content <- readFile file
        let parsed = parseYaml content
        parsed `shouldBe` Right result

testParse :: String -> String -> Yaml -> Spec
testParse label input expected =
    it label $
        parseYaml input `shouldBe` Right expected

testNan :: String -> String -> Spec
testNan label input =
    it label $
        case parseYaml input of
            Right (YNumber d) | isNaN d -> return ()
            other -> expectationFailure $ "Expected NaN, got: " ++ show other

yaml12Spec :: Spec
yaml12Spec = describe "YAML 1.2.2 Features" $ do
    describe "Flow Collections" $ do
        testParse "empty flow sequence" "[]" (YArray [])
        testParse "flow sequence with scalars" "[1, \"two\", true, null]"
            (YArray [YNumber 1.0, YString "two", YBool True, YNull])
        testParse "nested flow sequences" "[[1, 2], [3, 4]]"
            (YArray [YArray [YNumber 1.0, YNumber 2.0], YArray [YNumber 3.0, YNumber 4.0]])
        testParse "flow sequence with trailing comma" "[1, 2,]"
            (YArray [YNumber 1.0, YNumber 2.0])
        testParse "empty flow mapping" "{}" (YObject [])
        testParse "flow mapping" "{\"a\": 1, \"b\": \"val\"}"
            (YObject [("a", YNumber 1.0), ("b", YString "val")])
        testParse "nested flow mapping" "{\"outer\": {\"inner\": true}}"
            (YObject [("outer", YObject [("inner", YBool True)])])
        testParse "flow mapping with trailing comma" "{\"x\": 10,}"
            (YObject [("x", YNumber 10.0)])
        testParse "flow mapping with flow sequence" "{\"items\": [1, 2]}"
            (YObject [("items", YArray [YNumber 1.0, YNumber 2.0])])

    describe "Core Schema Scalars" $ do
        testParse "null literal" "null" YNull
        testParse "Null capitalized" "Null" YNull
        testParse "NULL uppercase" "NULL" YNull
        testParse "tilde null" "~" YNull
        testParse "empty value after colon (implicit null)" "key:\n"
            (YObject [("key", YNull)])
        testParse "boolean true variants" "true" (YBool True)
        testParse "boolean True" "True" (YBool True)
        testParse "boolean TRUE" "TRUE" (YBool True)
        testParse "boolean false variants" "false" (YBool False)
        testParse "boolean False" "False" (YBool False)
        testParse "boolean FALSE" "FALSE" (YBool False)
        testParse "hex numbers" "0x1f" (YNumber 31.0)
        testParse "octal numbers" "0o77" (YNumber 63.0)
        testParse "scientific notation" "1.23e+2" (YNumber 123.0)
        testParse "positive infinity" ".inf" (YNumber (1/0))
        testParse "negative infinity" "-.inf" (YNumber (-1/0))
        testNan "nan lowercase" ".nan"
        testNan "nan mixed case" ".NaN"
        testNan "nan uppercase" ".NAN"
        testParse "single-quoted string with escaped quote" "'it''s fine'" (YString "it's fine")
        testParse "double-quoted string with escapes" "\"hello\\nworld\\t\\\"quoted\\\"\"" (YString "hello\nworld\t\"quoted\"")
        testParse "plain unquoted scalar" "just a string" (YString "just a string")

    describe "Directives, Documents, and Comments" $ do
        testParse "YAML directive and document start" "%YAML 1.2\n---\nfoo: bar"
            (YObject [("foo", YString "bar")])
        testParse "document start and document end" "---\nfoo: bar\n...\n"
            (YObject [("foo", YString "bar")])
        testParse "comments around and inline" "# header\nfoo: bar # inline\n# footer\n"
            (YObject [("foo", YString "bar")])

    describe "Anchors, Aliases, and Merge Keys" $ do
        testParse "basic scalar anchor and alias" "orig: &orig value\ncopy: *orig\n"
            (YObject [("orig", YString "value"), ("copy", YString "value")])
        testParse "flow anchor and alias" "[&val 42, *val]"
            (YArray [YNumber 42.0, YNumber 42.0])
        testParse "mapping merge key (<<)"
            "default: &default\n  adapter: postgres\n  host: localhost\ndevelopment:\n  <<: *default\n  database: dev_db\n"
            (YObject
                [ ("default", YObject [("adapter", YString "postgres"), ("host", YString "localhost")])
                , ("development", YObject [("adapter", YString "postgres"), ("host", YString "localhost"), ("database", YString "dev_db")])
                ])

    describe "Sequences with Inline Mappings" $ do
        testParse "sequence of inline mappings"
            "- id: 1\n  name: Alice\n- id: 2\n  name: Bob\n"
            (YArray
                [ YObject [("id", YNumber 1.0), ("name", YString "Alice")]
                , YObject [("id", YNumber 2.0), ("name", YString "Bob")]
                ])

