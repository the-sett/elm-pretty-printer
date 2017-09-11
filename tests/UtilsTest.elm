module UtilsTest exposing (..)

import Char
import Expect exposing (Expectation)
import Test exposing (..)
import Utils exposing (..)


suite : Test
suite =
    describe "Utils"
        [ describe "takeWhile"
            [ test "it takes a pred and a string and returns string until false" <|
                \_ ->
                    let
                        str =
                            "hownowBROWNCOW?"
                    in
                    takeWhile Char.isLower str
                        |> Expect.equal "hownow"
            ]
        , describe "dropWhile"
            [ test "it takes a pred and a string and drops string until false" <|
                \_ ->
                    let
                        str =
                            "hownowBROWNCOW?"
                    in
                    dropWhile Char.isLower str
                        |> Expect.equal "BROWNCOW?"
            ]
        , describe "break"
            [ test "it splits string on char when pred is true" <|
                \_ ->
                    break ((==) '\n') "how now\nbrown cow?"
                        |> Expect.equal ( "how now", "\nbrown cow?" )
            ]
        ]
