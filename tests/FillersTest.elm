module FillersTest exposing (..)

import Doc exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


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
                            fill 6 (string name)
                                |+ space
                                |+ char ':'
                                |+ space
                                |+ string tipe

                        expected =
                            "let empty  : Doc\n    nest   : Int -> Doc -> Doc\n    linebreak : Doc"
                    in
                    string "let"
                        |+ space
                        |+ align (join linebreak (List.map ptype types))
                        |> Doc.toString
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
                            fillBreak 6 (string name)
                                |+ space
                                |+ char ':'
                                |+ space
                                |+ string tipe

                        expected =
                            "let empty  : Doc\n    nest   : Int -> Doc -> Doc\n    linebreak\n           : Doc"
                    in
                    string "let"
                        |+ space
                        |+ align (join linebreak (List.map ptype types))
                        |> Doc.toString
                        |> Expect.equal expected
            ]
        ]
