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
        , describe "fillSep "
            [ test "it concatenates documents horizontally with <+>" <|
                \_ ->
                    let
                        docs =
                            [ text "how", text "now", text "brown", text "cow?" ]
                    in
                    fillSep docs
                        |> Debug.log "!!!"
                        |> Render.show
                        |> Expect.equal "how now brown cow?"
            ]
        ]
