module LeijenTest exposing (..)

import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    describe "Leijen.Text"
        [ describe "<+>"
            [ test "it combines 2 docs with a space" <|
                \_ ->
                    let
                        doc1 =
                            text "Hello,"

                        doc2 =
                            text "World!"
                    in
                    (doc1 <+> doc2)
                        |> Render.show
                        |> Expect.equal "Hello, World!"
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
        , describe "fillSep"
            [ test "it concatenates documents horizontally with <+>" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    fillSep (List.map text words)
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            ]
        , describe "<$>"
            [ test "it separates elements with a newline" <|
                \_ ->
                    (text "hello" <$> text "world")
                        |> Render.show
                        |> Expect.equal "hello\nworld"
            ]
        , describe "align"
            [ test "it renders doc with nesting lvl set to current column" <|
                \_ ->
                    let
                        aligning left right =
                            align (left <$> right)

                        words =
                            text "hi" <+> aligning (text "nice") (text "world")
                    in
                    words
                        |> Render.show
                        |> Expect.equal "hi nice\n   world"
            ]
        , describe "hang"
            [ test "it implements hanging indentation" <|
                \_ ->
                    let
                        words =
                            "the hang combinator indents these words !"
                    in
                    words
                        |> String.words
                        |> List.map text
                        |> fillSep
                        |> hang 4
                        |> Render.show
                        |> Expect.equal "the hang combinator indents\n    these words !"
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
        ]
