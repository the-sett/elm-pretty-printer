module FillersTest exposing (..)

import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    describe "Filler Combinators"
        [ describe "fill"
            [ test "appends spaces until width is given int, nothing is appended if longer than int" <|
                \_ ->
                    let
                        types =
                            [ ( "empty", "Doc" )
                            , ( "nest", "Int -> Doc -> Doc" )
                            , ( "linebreak", "Doc" )
                            ]

                        ptype ( name, tipe ) =
                            fill 6 (text name) <+> text ":" <+> text tipe

                        expected =
                            "let empty  : Doc\n    nest   : Int -> Doc -> Doc\n    linebreak : Doc"
                    in
                    vcat (List.map ptype types)
                        |> align
                        |> (<+>) (text "let")
                        |> Render.show
                        |> Expect.equal expected
            ]
        , describe "fillBreak"
            [ test "same as fill, but inserts break and increase nesting to int if longer than int" <|
                \_ ->
                    let
                        types =
                            [ ( "empty", "Doc" )
                            , ( "nest", "Int -> Doc -> Doc" )
                            , ( "linebreak", "Doc" )
                            ]

                        ptype ( name, tipe ) =
                            fillBreak 6 (text name) <+> text ":" <+> text tipe

                        expected =
                            "let empty  : Doc\n    nest   : Int -> Doc -> Doc\n    linebreak\n           : Doc"
                    in
                    vcat (List.map ptype types)
                        |> align
                        |> (<+>) (text "let")
                        |> Render.show
                        |> Expect.equal expected
            ]
        ]
