module PublicTest exposing (..)

import Expect exposing (Expectation)
import Main exposing (..)
import Render
import Test exposing (..)
import Text exposing (Doc(..))


{-| This test suite is for public facing functions that don't exist in the core
library. If a function here shares a namespace with one that already exists,
that's an indicator that the functionality is slightly different.
-}
suite : Test
suite =
    describe "Public API"
        [ describe "text"
            [ test "it is a simple Text element if no whitespace" <|
                \_ ->
                    Expect.equal (Text 5 "hello") (string "hello")
            , test "it knows how to handle empty inputs" <|
                \_ ->
                    Expect.equal Empty (string "")
            , test "it knows how to handle inputs with \n in middle" <|
                \_ ->
                    let
                        input =
                            "hello\nfriend"

                        expected =
                            Cat
                                (Text 5 "hello")
                                (Cat (FlatAlt Line (Char ' ')) (Cat (Text 6 "friend") Empty))
                    in
                    Expect.equal expected (string input)
            , test "it can properly handle inputs with trailing \n" <|
                \_ ->
                    let
                        input =
                            "hello\n"

                        expected =
                            Cat
                                (Text 5 "hello")
                                (Cat (FlatAlt Line (Char ' ')) Empty)
                    in
                    Expect.equal expected (string input)
            , test "it can properly handle inputs with leading \n" <|
                \_ ->
                    let
                        input =
                            "\nhello"

                        expected =
                            Cat
                                (FlatAlt Line (Char ' '))
                                (Cat (Text 5 "hello") Empty)
                    in
                    Expect.equal expected (string input)
            , test "it can properly format to a Doc if given trailing whitespace" <|
                \_ ->
                    let
                        input =
                            "howdy "

                        expected =
                            Cat
                                (Text 5 "howdy")
                                (Cat (Char ' ') Empty)
                    in
                    Expect.equal expected (string input)
            , test "it can properly format Doc if given leading whitespace" <|
                \_ ->
                    let
                        input =
                            " howdy"

                        expected =
                            Cat
                                (Char ' ')
                                (Cat (Text 5 "howdy") Empty)
                    in
                    Expect.equal expected (string input)
            , test "it can properly format Doc if whitespace is somewhere in middle" <|
                \_ ->
                    let
                        input =
                            "hello friend"

                        expected =
                            Cat
                                (Text 5 "hello")
                                (Cat (Char ' ') (Cat (Text 6 "friend") Empty))
                    in
                    Expect.equal expected (string input)
            , test "it can properly format Doc given input with all types of whitespace" <|
                \_ ->
                    let
                        input =
                            " hello\tnice\nfriend\n"

                        expected =
                            Cat
                                (Char ' ')
                                (Cat
                                    (Text 5 "hello")
                                    (Cat
                                        (Char ' ')
                                        (Cat
                                            (Char ' ')
                                            (Cat
                                                (Char ' ')
                                                (Cat
                                                    (Char ' ')
                                                    (Cat
                                                        (Text 4 "nice")
                                                        (Cat
                                                            (FlatAlt Line (Char ' '))
                                                            (Cat
                                                                (Text 6 "friend")
                                                                (Cat
                                                                    (FlatAlt Line (Char ' '))
                                                                    Empty
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                    in
                    Expect.equal expected (string input)
            ]
        ]
