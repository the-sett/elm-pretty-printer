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
                        |> Result.map (Expect.equal "hello\nworld")
                        |> Result.withDefault (Expect.fail "Failure in result")
            , test "it puts a SPACE between elements when undone by group" <|
                \_ ->
                    string "hello"
                        |+ line
                        |+ string "world"
                        |> group
                        |> Doc.toString
                        |> Result.map (Expect.equal "hello world")
                        |> Result.withDefault (Expect.fail "Failure in result")
            ]
        , describe "<$$> - linebreak"
            [ test "advances to next line" <|
                \_ ->
                    string "hello"
                        |+ linebreak
                        |+ string "world"
                        |> Doc.toString
                        |> Result.map (Expect.equal "hello\nworld")
                        |> Result.withDefault (Expect.fail "Failure in result")
            , test "it puts elements right next to each other when undone by group" <|
                \_ ->
                    string "hello"
                        |+ linebreak
                        |+ string "world"
                        |> group
                        |> Doc.toString
                        |> Result.map (Expect.equal "helloworld")
                        |> Result.withDefault (Expect.fail "Failure in result")
            ]
        , describe "</> - softline"
            [ test "it separates elements with a space if they can fit on same line" <|
                \_ ->
                    string "hello"
                        |+ softline
                        |+ string "world"
                        |> Doc.toString
                        |> Result.map (Expect.equal "hello world")
                        |> Result.withDefault (Expect.fail "Failure in result")
            , test "it inserts a break if both will not fit on same line" <|
                \_ ->
                    string "a really long string that might"
                        |+ softline
                        |+ string "not fit on one line"
                        |> Doc.toString
                        |> Result.map (Expect.equal "a really long string that might\nnot fit on one line")
                        |> Result.withDefault (Expect.fail "Failure in result")
            ]
        , describe "<//> - softbreak"
            [ test "it separates elements with nothing if they will fit on same line" <|
                \_ ->
                    string "hello"
                        |+ softbreak
                        |+ string "world"
                        |> Doc.toString
                        |> Result.map (Expect.equal "helloworld")
                        |> Result.withDefault (Expect.fail "Failure in result")
            , test "it advances second element to next line if it will not fit on same line" <|
                \_ ->
                    string "a really long string that might"
                        |+ softbreak
                        |+ string "not fit on one line"
                        |> Doc.toString
                        |> Result.map (Expect.equal "a really long string that might\nnot fit on one line")
                        |> Result.withDefault (Expect.fail "Failure in result")
            ]
        , describe "hardline"
            [ test "it separates elements with a line break" <|
                \_ ->
                    string "hello"
                        |+ hardline
                        |+ string "world"
                        |> Doc.toString
                        |> Result.map (Expect.equal "hello\nworld")
                        |> Result.withDefault (Expect.fail "Failure in result")
            , test "it cannot be flattened with group" <|
                \_ ->
                    string "hello"
                        |+ hardline
                        |+ string "world"
                        |> group
                        |> Doc.toString
                        |> Result.map (Expect.equal "hello\nworld")
                        |> Result.withDefault (Expect.fail "Failure in result")
            ]
        ]
