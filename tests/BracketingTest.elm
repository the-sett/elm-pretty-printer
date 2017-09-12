module BracketingTest exposing (..)

import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    describe "Bracketing"
        [ describe "enclose"
            [ test "it encloses given doc between 'left' and 'right' args" <|
                \_ ->
                    let
                        words =
                            "wrapped in brackets"
                    in
                    enclose lbracket rbracket (text words)
                        |> Render.show
                        |> Expect.equal "[wrapped in brackets]"
            ]
        , describe "squotes"
            [ test "it surrounds the given doc in single quotes" <|
                \_ ->
                    squotes (text "wrapped in single quotes")
                        |> Render.show
                        |> Expect.equal "'wrapped in single quotes'"
            ]
        , describe "dquotes"
            [ test "it surrounds the given doc in double quotes" <|
                \_ ->
                    let
                        expected =
                            "\"wrapped in double quotes\""
                    in
                    dquotes (text "wrapped in double quotes")
                        |> Render.show
                        |> Expect.equal expected
            ]
        , describe "parens"
            [ test "it surrounds the given doc in parens" <|
                \_ ->
                    parens (text "wrapped in parens")
                        |> Render.show
                        |> Expect.equal "(wrapped in parens)"
            ]
        , describe "angles"
            [ test "it surrounds given doc in angle brackets" <|
                \_ ->
                    angles (text "wrapped in angle brackets")
                        |> Render.show
                        |> Expect.equal "<wrapped in angle brackets>"
            ]
        , describe "brackets"
            [ test "it surrounds given doc in square brackets" <|
                \_ ->
                    brackets (text "wrapped in square brackets")
                        |> Render.show
                        |> Expect.equal "[wrapped in square brackets]"
            ]
        , describe "braces"
            [ test "it surrounds given doc in braces" <|
                \_ ->
                    braces (text "wrapped in braces")
                        |> Render.show
                        |> Expect.equal "{wrapped in braces}"
            ]
        ]
