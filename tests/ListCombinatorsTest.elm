module ListCombinatorsTest exposing (..)

import Console
import Doc exposing (..)
import Expect exposing (Expectation)
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
                    join space (List.map string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "how now brown cow?")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "fillSep"
            [ test "it concatenates documents horizontally with softline - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "where", "in", "the", "world", "is", "Carmen", "Sandiego?" ]
                    in
                    join softline (List.map string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "where in the world is Carmen\nSandiego?")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "vsep"
            [ test "it concats doc elements vertically with line - PUBLIC" <|
                \_ ->
                    let
                        someText =
                            "string to lay out"
                                |> String.words
                                |> List.map string
                    in
                    string "some"
                        |+ space
                        |+ join line someText
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "some string\nto\nlay\nout")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "sep"
            [ test "it puts elements on same line, separated by space - PUBLIC" <|
                \_ ->
                    let
                        elements =
                            [ "how", "now", "brown", "cow?" ]
                                |> List.map string
                    in
                    join line elements
                        |> group
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "how now brown cow?")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            , test "if it cannot put them all on same line, it puts them on separate lines - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "where", "in", "the", "world", "is", "Carmen", "Sandiego?" ]
                    in
                    join line (List.map string words)
                        |> group
                        |> Doc.toString
                        |> Maybe.map (Expect.equal (String.join "\n" words))
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "hcat"
            [ test "it concats elements horizontally with no space - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    Doc.concat (List.map string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "helloworld")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "vcat"
            [ test "it concats elements with linebreak - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    join linebreak (List.map string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal (String.join "\n" words))
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "fillCat"
            [ test "it concatenates with a softbreak (directly next to each other) - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    join softbreak (List.map string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "helloworld")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            , test "it fits as many as it can on one line - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "this is a long string", "another string", "third string", "banana" ]
                    in
                    join softbreak (List.map string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "this is a long string\nanother stringthird stringbanana")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "cat"
            [ test "concats horizontally if fits page - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "hello", "world" ]
                    in
                    join linebreak (List.map string words)
                        |> group
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "helloworld")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            , test "concats vertically if it does not - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "what", "would", "you", "do", "if", "your", "son", "was", "at", "home?" ]
                    in
                    join linebreak (List.map string words)
                        |> group
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "what\nwould\nyou\ndo\nif\nyour\nson\nwas\nat\nhome?")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        , describe "punctuate"
            [ test "it concats all intersperses given document between elements - PUBLIC" <|
                \_ ->
                    let
                        words =
                            [ "how", "now", "brown", "cow?" ]
                    in
                    join (char ',') (List.map string words)
                        |> Doc.toString
                        |> Maybe.map (Expect.equal "how,now,brown,cow?")
                        |> Maybe.withDefault (Expect.fail "Failure in result")
            ]
        ]
