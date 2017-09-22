module BracketingTest exposing (..)

import Doc exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Bracketing"
        [ describe "enclose"
            [ test "it encloses given doc between 'left' and 'right' args" <|
                \_ ->
                    let
                        words =
                            "wrapped in stuff"
                    in
                    surround (char '[') (char ')') (string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "[wrapped in stuff)")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "squotes"
            [ test "it surrounds the given doc in single quotes" <|
                \_ ->
                    string "wrapped in single quotes"
                        |> squotes
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "'wrapped in single quotes'")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "dquotes"
            [ test "it surrounds the given doc in double quotes" <|
                \_ ->
                    let
                        expected =
                            "\"wrapped in double quotes\""
                    in
                    string "wrapped in double quotes"
                        |> dquotes
                        |> Doc.toString
                        |> Maybe.map (Expect.equal expected)
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "parens"
            [ test "it surrounds the given doc in parens" <|
                \_ ->
                    string "wrapped in parens"
                        |> parens
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "(wrapped in parens)")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "angles"
            [ test "it surrounds given doc in angle brackets" <|
                \_ ->
                    string "wrapped in angle brackets"
                        |> angles
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "<wrapped in angle brackets>")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "brackets"
            [ test "it surrounds given doc in square brackets" <|
                \_ ->
                    string "wrapped in square brackets"
                        |> brackets
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "[wrapped in square brackets]")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "braces"
            [ test "it surrounds given doc in braces" <|
                \_ ->
                    string "wrapped in braces"
                        |> braces
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "{wrapped in braces}")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        ]
