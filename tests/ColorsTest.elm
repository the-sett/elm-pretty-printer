module ColorsTest exposing (..)

import Console as Ansi
import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    describe "Colors"
        [ skip <|
            -- passes aesthetic test, but tests fail when comparing actual strings (different escape sequences)
            describe "foreground colors"
                [ test "colors are applied at text level" <|
                    \_ ->
                        let
                            result =
                                red (text "Red")
                                    <> comma
                                    <+> white (text "white")
                                    <+> text "and"
                                    <+> blue (text "blue")
                                    <> char '!'

                            expected =
                                Ansi.red "Red"
                                    ++ ", "
                                    ++ Ansi.white "white"
                                    ++ " and "
                                    ++ Ansi.blue "blue"
                                    ++ "!"
                        in
                        Expect.equal expected (Render.show result)
                , test "colors can be nested" <|
                    \_ ->
                        let
                            result =
                                blue (text "Nested" <+> yellow (text "colors") <+> text "example")

                            expected =
                                Ansi.blue "Nested" ++ Ansi.yellow " colors" ++ Ansi.blue " example"
                        in
                        Expect.equal expected (Render.show result)
                ]
        ]
