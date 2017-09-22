module AlignmentTest exposing (..)

import Doc exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Alignment"
        [ describe "align"
            [ test "it renders doc with nesting lvl set to current column" <|
                \_ ->
                    string "nice"
                        |+ line
                        |+ string "world"
                        |> align
                        |> (|+) (string "hi ")
                        |> Doc.toString
                        |> Expect.equal "hi nice\n   world"
            ]
        , describe "hang"
            [ test "it implements hanging indentation" <|
                \_ ->
                    "the hang combinator indents these words !"
                        |> String.words
                        |> List.map string
                        |> join softline
                        |> hang 4
                        |> Doc.toString
                        |> Expect.equal "the hang combinator indents\n    these words !"
            ]
        , describe "indent"
            [ test "it indents entire document by given amount of spaces" <|
                \_ ->
                    let
                        elements =
                            "the indent combinator indents these words !"
                                |> String.words
                                |> List.map string
                                |> join softline

                        expected =
                            "    the indent combinator\n    indents these words !"
                    in
                    indent 4 elements
                        |> Doc.toString
                        |> Expect.equal expected
            ]
        , describe "encloseSep"
            [ test "it concats list of docs separated by sep and encloses result in left and right args" <|
                \_ ->
                    [ "one", "1", "1.0" ]
                        |> List.map string
                        |> surroundJoin (char '<') (char '>') (char '=')
                        |> Doc.toString
                        |> Expect.equal "<one=1=1.0>"
            , test "it aligns elements (with separator in front) if they cannot fit on one line" <|
                \_ ->
                    [ "a really long string", "another really long string", "a third really long string" ]
                        |> List.map string
                        |> surroundJoin (char '[') (char ']') (char ',')
                        |> (|+) (string "list ")
                        |> Doc.toString
                        |> Expect.equal "list [a really long string\n     ,another really long string\n     ,a third really long string]"
            ]
        , describe "list"
            [ test "it comma separates the docs and encloses them in square brackets" <|
                \_ ->
                    [ 10, 200, 3000 ]
                        |> List.map int
                        |> list
                        |> Doc.toString
                        |> Expect.equal "[10,200,3000]"
            ]
        , describe "tupled"
            [ test "it comma separates the docs and encloses them in parens" <|
                \_ ->
                    [ "apples", "bananas", "carrots" ]
                        |> List.map string
                        |> surroundJoin (char '(') (char ')') (char ',')
                        |> Doc.toString
                        |> Expect.equal "(apples,bananas,carrots)"
            ]
        , describe "semiBraces"
            [ test "it separates the docs with semicolons and encloses them in braces" <|
                \_ ->
                    [ "apples", "bananas", "carrots" ]
                        |> List.map string
                        |> surroundJoin (char '{') (char '}') (char ';')
                        |> Doc.toString
                        |> Expect.equal "{apples;bananas;carrots}"
            ]
        ]
