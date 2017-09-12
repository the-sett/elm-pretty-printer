module OperatorsTest exposing (..)

import Expect exposing (Expectation)
import Render
import Test exposing (..)
import Text exposing (..)


suite : Test
suite =
    describe "Operators"
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
        , describe "<$>"
            [ test "it separates elements with a hardbreak" <|
                \_ ->
                    (text "hello" <$> text "world")
                        |> Render.show
                        |> Expect.equal "hello\nworld"
            ]
        , describe "</>"
            [ test "it concats with a softline (space if it will fit)" <|
                \_ ->
                    (text "hello" </> text "world")
                        |> Render.show
                        |> Expect.equal "hello world"
            , test "it concats with a softline (break if it will not fit on line)" <|
                \_ ->
                    (text "a really long string that might" </> text "not fit on one line")
                        |> Render.show
                        |> Expect.equal "a really long string that might\nnot fit on one line"
            ]
        , describe "<$$>"
            [ test "it concats with a linebreak in between - (advances to next line)" <|
                \_ ->
                    (text "hello" <$$> text "world")
                        |> Render.show
                        |> Expect.equal "hello\nworld"
            , test "it puts elements right next to each other when undone by group" <|
                \_ ->
                    (text "hello" <$$> text "world")
                        |> group
                        |> Render.show
                        |> Expect.equal "helloworld"
            ]
        , describe "<//>"
            [ test "it concats with a softbreak (directly next to each other if it fits)" <|
                \_ ->
                    (text "hello" <//> text "world")
                        |> Render.show
                        |> Expect.equal "helloworld"
            , test "it concats with a linebreak if it will not fit on line" <|
                \_ ->
                    (text "a really long string that might" <//> text "not fit on one line")
                        |> Render.show
                        |> Expect.equal "a really long string that might\nnot fit on one line"
            ]
        ]
