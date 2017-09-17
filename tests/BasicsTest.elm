module BasicsTest exposing (..)

import Doc exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Basic Combinators"
        [ -- describe "string"
          --   [ test "it concats chars using line for newline characters" <|
          --       \_ ->
          --           let
          --               input =
          --                   "hello\nfriend"
          --           in
          --           string input
          --               |> Doc.toString
          --               |> Expect.equal input
          --   ]
          describe "int"
            [ test "it turns a literal integer to a text object w/ integer" <|
                \_ ->
                    Expect.equal "102" (Doc.toString (int 102))
            ]
        , describe "float"
            [ test "it turns a literal float to a text object w/ float" <|
                \_ ->
                    Expect.equal "32.144" (Doc.toString (float 32.144))
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
                    Expect.equal "your answer was False!" (Doc.toString input)
            ]
        , describe "|+"
            [ test "it combines 2 docs without a space" <|
                \_ ->
                    string "Porcu"
                        |+ string "pine"
                        |> Doc.toString
                        |> Expect.equal "Porcupine"
            ]
        , describe "nest"
            [ test "it renders doc with nested level set to given int" <|
                \_ ->
                    nest 2 (string "hello" |+ line |+ string "world")
                        |+ line
                        |+ char '!'
                        |> Doc.toString
                        |> Expect.equal "hello\n  world\n!"
            ]
        , describe "group"
            [ test "it moves all elements onto the same line by replacing breaks w/ space" <|
                \_ ->
                    string "how now"
                        |+ line
                        |+ string "brown cow?"
                        |> group
                        |> Doc.toString
                        |> Expect.equal "how now brown cow?"
            , test "it doesn't change anything if elements are already on same line" <|
                \_ ->
                    string "how now brown cow?"
                        |> group
                        |> Doc.toString
                        |> Expect.equal "how now brown cow?"
            ]
        ]
