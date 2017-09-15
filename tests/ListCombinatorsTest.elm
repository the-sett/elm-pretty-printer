module ListCombinatorsTest exposing (..)

import Console
import Expect exposing (Expectation)
import Main
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    describe "List Combinators"
        [ describe "hsep"
            [ test "it concatenates documents horizontally with <+>" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    hsep (List.map text words)
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            , test "same results through public api" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    Main.concat Main.space (List.map text words)
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            ]
        , describe "fillSep"
            [ test "it concatenates documents horizontally with </>" <|
                \_ ->
                    let
                        words =
                            [ "this is a long string", "another string", "third string", "banana" ]
                    in
                    fillSep (List.map text words)
                        |> Render.show
                        |> Expect.equal "this is a long string\nanother string third string\nbanana"
            , test "same results through public api" <|
                \_ ->
                    let
                        words =
                            [ "this is a long string", "another string", "third string", "banana" ]
                    in
                    Main.concat Main.softline (List.map text words)
                        |> Render.show
                        |> Expect.equal "this is a long string\nanother string third string\nbanana"
            ]
        , describe "vsep"
            [ test "it concats doc elements vertically with <$>" <|
                \_ ->
                    let
                        someText =
                            "text to lay out"
                                |> String.words
                                |> List.map text
                    in
                    (text "some" <+> vsep someText)
                        |> Render.show
                        |> Expect.equal "some text\nto\nlay\nout"
            , test "it can be used in combination with align" <|
                \_ ->
                    let
                        someText =
                            "text to lay out"
                                |> String.words
                                |> List.map text
                    in
                    (text "some" <+> align (vsep someText))
                        |> Render.show
                        |> Expect.equal "some text\n     to\n     lay\n     out"
            ]
        , describe "sep"
            [ test "it takes elements on multiple lines and puts them on same line (separated by space)" <|
                \_ ->
                    let
                        elements =
                            [ "how", "now", "brown", "cow?" ]
                                |> List.map text
                    in
                    sep elements
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            , test "if it cannot put them all on same line, it puts them on separate lines" <|
                \_ ->
                    let
                        words =
                            [ "this is a long string", "another string", "third string", "banana" ]
                    in
                    sep (List.map text words)
                        |> Render.show
                        |> Expect.equal (String.join "\n" words)
            ]
        , describe "hcat"
            [ test "it concats elements horizontally with <>" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    hcat (List.map text words)
                        |> Render.show
                        |> Expect.equal "helloworld"
            ]
        , describe "vcat"
            [ test "it concats elements with <$$>" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    vcat (List.map text words)
                        |> Render.show
                        |> Expect.equal (String.join "\n" words)
            ]
        , describe "fillCat"
            [ test "it concatenates with a softbreak (directly next to each other)" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    fillCat (List.map text words)
                        |> Render.show
                        |> Expect.equal "helloworld"
            , test "it fits as many as it can on one line" <|
                \_ ->
                    let
                        words =
                            [ "this is a long string", "another string", "third string", "banana" ]
                    in
                    fillCat (List.map text words)
                        |> Render.show
                        |> Expect.equal "this is a long string\nanother stringthird stringbanana"
            ]
        , describe "cat"
            [ test "concats horizontally with <> if fits page" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    cat (List.map text words)
                        |> Render.show
                        |> Expect.equal "helloworld"
            , test "concats vertically with <$$> if it does not" <|
                \_ ->
                    let
                        words =
                            [ "this is a long string", "another string", "third string", "banana" ]
                    in
                    cat (List.map text words)
                        |> Render.show
                        |> Expect.equal "this is a long string\nanother string\nthird string\nbanana"
            ]
        , describe "punctuate"
            [ test "it concats all intersperses given document between elements" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    punctuate comma (List.map text words)
                        |> cat
                        |> Render.show
                        |> Expect.equal "how,now,brown,cow?"
            ]
        ]
