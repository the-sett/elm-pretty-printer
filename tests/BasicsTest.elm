module BasicsTest exposing (..)

import Doc exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Basic Combinators"
        [ describe "string"
            [ test "it concats chars using line for newline characters" <|
                \_ ->
                    let
                        input =
                            "hello\nfriend"
                    in
                    string input
                        |> Doc.toString
                        |> Maybe.map (Expect.equal input)
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "int"
            [ test "it turns a literal integer to a text object w/ integer" <|
                \_ ->
                    Doc.toString (int 102)
                        |> Maybe.map (Expect.equal "102")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "float"
            [ test "it turns a literal float to a text object w/ float" <|
                \_ ->
                    Doc.toString (float 32.144)
                        |> Maybe.map (Expect.equal "32.144")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "bool"
            [ test "it turns a literal bool to a text object w/ bool" <|
                \_ ->
                    let
                        input =
                            string "your answer was "
                                |+ bool False
                                |+ char '!'
                    in
                    string "your answer was "
                        |+ bool False
                        |+ char '!'
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "your answer was False!")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "|+"
            [ test "it combines 2 docs without a space" <|
                \_ ->
                    string "Porcu"
                        |+ string "pine"
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "Porcupine")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "nest"
            [ test "it renders doc with nested level set to given int" <|
                \_ ->
                    nest 2 (string "hello" |+ line |+ string "world")
                        |+ line
                        |+ char '!'
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "hello\n  world\n!")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "group"
            [ test "it moves all elements onto the same line by replacing breaks w/ space" <|
                \_ ->
                    string "how now"
                        |+ line
                        |+ string "brown cow?"
                        |> group
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "how now brown cow?")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            , test "it doesn't change anything if elements are already on same line" <|
                \_ ->
                    string "how now brown cow?"
                        |> group
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "how now brown cow?")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        ]
