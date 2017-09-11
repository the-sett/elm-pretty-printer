module LeijenTest exposing (..)

import Console
import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    describe "Leijen.Text"
        [ describe "fillSep"
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
        ]
