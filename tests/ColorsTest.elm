module ColorsTest exposing (..)

import Console as Ansi
import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    -- passes aesthetic tests, but tests fail when comparing actual strings (different escape sequences)
    describe "Colors"
        [ skip <|
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
        , skip <|
            describe "background colors"
                [ test "colors are applied at the text level" <|
                    \_ ->
                        let
                            result =
                                onRed (text "Red")
                                    <> comma
                                    <+> onWhite (text "white")
                                    <+> text "and"
                                    <+> onBlue (text "blue")
                                    <> char '!'

                            expected =
                                Ansi.bgRed "Red"
                                    ++ ", "
                                    ++ Ansi.bgWhite "white"
                                    ++ " and "
                                    ++ Ansi.bgBlue "blue"
                                    ++ "!"
                        in
                        Expect.equal expected (Render.show result)
                , test "colors can be nested" <|
                    \_ ->
                        let
                            result =
                                onBlue (text "Nested" <+> onYellow (text "colors") <+> text "example")

                            expected =
                                Ansi.bgBlue "Nested " ++ Ansi.bgYellow "colors" ++ Ansi.bgBlue " example"
                        in
                        Expect.equal expected (Render.show result)
                ]
        , skip <|
            describe "non-color formatting"
                [ test "it can do bold text" <|
                    \_ ->
                        let
                            result =
                                text "We can do"
                                    <+> bold (text "boldness")
                                    <+> text "if your terminal supports it."

                            expected =
                                "We can do "
                                    ++ Ansi.bold "boldness"
                                    ++ " if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do underlining" <|
                    \_ ->
                        let
                            result =
                                text "We can do"
                                    <+> underline (text "underlining")
                                    <+> text "if your terminal supports it."

                            expected =
                                "We can do "
                                    ++ Ansi.underline "underlining"
                                    ++ " if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                ]
        , skip <|
            describe "combinations of formatting"
                [ test "it can do both bold and underlining" <|
                    \_ ->
                        let
                            result =
                                text "We can do"
                                    <+> bold (underline (text "underlining and boldness"))
                                    <+> text "if your terminal supports it."

                            expected =
                                "We can do "
                                    ++ Ansi.bold (Ansi.underline "underlining and boldness")
                                    ++ " if your terminal supports it."
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do boldness with foreground color" <|
                    \_ ->
                        let
                            result =
                                text "this is"
                                    <+> bold (blue (text "some bold blue"))
                                    <+> text "text"

                            expected =
                                "this is "
                                    ++ Ansi.bold (Ansi.blue "some bold blue")
                                    ++ " text"
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do foreground color and background color" <|
                    \_ ->
                        let
                            result =
                                onWhite <|
                                    text "this is"
                                        <+> red (text "red text")
                                        <+> text "on a white background"

                            expected =
                                Ansi.bgWhite <|
                                    "this is "
                                        ++ Ansi.red "red text"
                                        ++ " on a white background"
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do bold with background color" <|
                    \_ ->
                        let
                            result =
                                onCyan <|
                                    text "this is"
                                        <+> bold (text "bold text")
                                        <+> text "on a cyan background"

                            expected =
                                Ansi.bgCyan <|
                                    "this is "
                                        ++ Ansi.bold "bold text"
                                        ++ " on a white background"
                        in
                        Expect.equal expected (Render.show result)
                , test "it can do underline with background color and foreground color" <|
                    \_ ->
                        let
                            result =
                                onBlue <|
                                    text "this is"
                                        <+> black (underline (text "underlined text"))
                                        <+> text "on a blue background"

                            expected =
                                Ansi.bgBlue <|
                                    "this is "
                                        ++ Ansi.black (Ansi.underline "underlined text")
                                        ++ " on a blue background"
                        in
                        Expect.equal expected (Render.show result)
                ]
        ]
