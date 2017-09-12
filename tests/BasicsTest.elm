module BasicsTest exposing (..)

import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


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
                        |> Render.show
                        |> Expect.equal input
            ]
        , describe "int"
            [ test "it turns a literal integer to a text object w/ integer" <|
                \_ ->
                    Expect.equal "102" (Render.show (int 102))
            ]
        , describe "float"
            [ test "it turns a literal float to a text object w/ float" <|
                \_ ->
                    Expect.equal "32.144" (Render.show (float 32.144))
            ]
        , describe "bool"
            [ test "it turns a literal bool to a text object w/ bool" <|
                \_ ->
                    let
                        input =
                            text "your answer was"
                                <+> bool False
                                <> text "!"
                    in
                    Expect.equal "your answer was False!" (Render.show input)
            ]
        , describe "<>"
            [ test "it combines 2 docs without a space" <|
                \_ ->
                    let
                        doc1 =
                            text "Porcu"

                        doc2 =
                            text "pine"
                    in
                    (doc1 <> doc2)
                        |> Render.show
                        |> Expect.equal "Porcupine"
            ]
        , describe "nest"
            [ test "it renders doc with nested level set to given int" <|
                \_ ->
                    let
                        result =
                            nest 2 (text "hello" <$> text "world") <$> text "!"
                    in
                    result
                        |> Render.show
                        |> Expect.equal "hello\n  world\n!"
            ]
        , describe "group"
            [ test "it moves all elements onto the same line by replacing breaks w/ space" <|
                \_ ->
                    let
                        elts =
                            text "how now" <$> text "brown cow?"
                    in
                    group elts
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            , test "it doesn't change anything if elements are already on same line" <|
                \_ ->
                    let
                        sampleTxt =
                            "how now brown cow?"

                        elts =
                            text sampleTxt
                    in
                    group elts
                        |> Render.show
                        |> Expect.equal sampleTxt
            ]
        ]
