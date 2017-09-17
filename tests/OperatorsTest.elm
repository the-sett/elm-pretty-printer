module OperatorsTest exposing (..)

import Doc exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Operators"
        [ describe "<$> - line"
            [ test "it separates elements with a linebreak" <|
                \_ ->
                    string "hello"
                        |+ line
                        |+ string "world"
                        |> Doc.toString
                        |> Expect.equal "hello\nworld"
            , test "it puts a SPACE between elements when undone by group" <|
                \_ ->
                    string "hello"
                        |+ line
                        |+ string "world"
                        |> group
                        |> Doc.toString
                        |> Expect.equal "hello world"
            ]
        , describe "<$$> - linebreak"
            [ test "advances to next line" <|
                \_ ->
                    string "hello"
                        |+ linebreak
                        |+ string "world"
                        |> Doc.toString
                        |> Expect.equal "hello\nworld"
            , test "it puts elements right next to each other when undone by group" <|
                \_ ->
                    string "hello"
                        |+ linebreak
                        |+ string "world"
                        |> group
                        |> Doc.toString
                        |> Expect.equal "helloworld"
            ]
        , describe "</> - softline"
            [ test "it separates elements with a space if they can fit on same line" <|
                \_ ->
                    string "hello"
                        |+ softline
                        |+ string "world"
                        |> Doc.toString
                        |> Expect.equal "hello world"
            , test "it inserts a break if both will not fit on same line" <|
                \_ ->
                    string "a really long string that might"
                        |+ softline
                        |+ string "not fit on one line"
                        |> Doc.toString
                        |> Expect.equal "a really long string that might\nnot fit on one line"
            ]
        , describe "<//> - softbreak"
            [ test "it separates elements with nothing if they will fit on same line" <|
                \_ ->
                    string "hello"
                        |+ softbreak
                        |+ string "world"
                        |> Doc.toString
                        |> Expect.equal "helloworld"
            , test "it advances second element to next line if it will not fit on same line" <|
                \_ ->
                    string "a really long string that might"
                        |+ softbreak
                        |+ string "not fit on one line"
                        |> Doc.toString
                        |> Expect.equal "a really long string that might\nnot fit on one line"
            ]
        ]
