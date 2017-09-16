module PublicTest exposing (..)

import Expect exposing (Expectation)
import Main exposing (..)
import Render
import Test exposing (..)
import Text exposing (Doc(..))


{-| This test suite is for functions in Main that are going to be
public. If a function shares a namespace with one that already exists,
that's an indicator that the functionality is slightly different.
-}
suite : Test
suite =
    describe "Public API"
        [ describe "text"
            [ test "it knows how to handle inputs with \n" <|
                \_ ->
                    let
                        input =
                            "hello\nfriend"

                        expected =
                            Cat
                                (Text 5 "hello")
                                (Cat (FlatAlt Line (Char ' ')) (Cat (Text 6 "friend") Empty))
                    in
                    Expect.equal expected (text input)
            ]
        ]
