module ListCombinatorsTest exposing (..)

import Console
import Debugger as Bug
import Expect exposing (Expectation)
import Main exposing ((|+))
import Render
import Test exposing (..)


suite : Test
suite =
    describe "List Combinators"
        [ describe "hsep"
            [ test "it concatenates documents horizontally with a space - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    Main.join Main.space (List.map Main.text words)
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            ]
        , describe "fillSep"
            [ test "it concatenates documents horizontally with softline - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "where", "in", "the", "world", "is", "Carmen", "Sandiego?" ]
                    in
                    Main.join Main.softline (List.map Main.text words)
                        |> Render.show
                        |> Expect.equal "where in the world is Carmen\nSandiego?"
            ]
        , describe "vsep"
            [ test "it concats doc elements vertically with line - PUBLIC" <|
                \_ ->
                    let
                        someText =
                            "text to lay out"
                                |> String.words
                                |> List.map Main.text
                    in
                    Main.text "some"
                        |+ Main.space
                        |+ Main.join Main.line someText
                        |> Render.show
                        |> Expect.equal "some text\nto\nlay\nout"
            ]
        , describe "sep"
            [ test "it puts elements on same line, separated by space - PUBLIC" <|
                \_ ->
                    let
                        elements =
                            [ "how", "now", "brown", "cow?" ]
                                |> List.map Main.text
                    in
                    Main.join Main.line elements
                        |> Main.group
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            , test "if it cannot put them all on same line, it puts them on separate lines - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "where", "in", "the", "world", "is", "Carmen", "Sandiego?" ]
                    in
                    Main.join Main.line (List.map Main.text words)
                        |> Main.group
                        |> Render.show
                        |> Expect.equal (String.join "\n" words)
            ]
        , describe "hcat"
            [ test "it concats elements horizontally with no space - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    Main.concat (List.map Main.text words)
                        |> Render.show
                        |> Expect.equal "helloworld"
            ]
        , describe "vcat"
            [ test "it concats elements with linebreak - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    Main.join Main.linebreak (List.map Main.text words)
                        |> Render.show
                        |> Expect.equal (String.join "\n" words)
            ]
        , describe "fillCat"
            [ test "it concatenates with a softbreak (directly next to each other) - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    Main.join Main.softbreak (List.map Main.text words)
                        |> Render.show
                        |> Expect.equal "helloworld"
            , test "it fits as many as it can on one line - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "this is a long string", "another string", "third string", "banana" ]
                    in
                    Main.join Main.softbreak (List.map Main.text words)
                        |> Render.show
                        |> Expect.equal "this is a long string\nanother stringthird stringbanana"
            ]
        , describe "cat"
            [ test "concats horizontally if fits page - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    Main.join Main.linebreak (List.map Main.text words)
                        |> Main.group
                        |> Render.show
                        |> Expect.equal "helloworld"
            , test "concats vertically if it does not - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "what", "would", "you", "do", "if", "your", "son", "was", "at", "home?" ]
                    in
                    Main.join Main.linebreak (List.map Main.text words)
                        |> Main.group
                        |> Render.show
                        |> Expect.equal "what\nwould\nyou\ndo\nif\nyour\nson\nwas\nat\nhome?"
            ]
        , describe "punctuate"
            [ test "it concats all intersperses given document between elements - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    Main.join (Main.char ',') (List.map Main.text words)
                        |> Render.show
                        |> Expect.equal "how,now,brown,cow?"
            ]
        ]
