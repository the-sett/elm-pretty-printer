module Flatten exposing (flattenOfConcatIsCorrect)

import Expect
import Internals
import Pretty exposing (Doc)
import Test exposing (Test)


flattenOfConcatIsCorrect : Test
flattenOfConcatIsCorrect =
    Test.test "Flatten doesn't change the document" <|
        \_ ->
            let
                doc : Doc t
                doc =
                    Pretty.string "a"
                        |> Pretty.a (Pretty.string "b")
                        |> Pretty.a (Pretty.string "c")
                        |> Pretty.a (Pretty.string "d")
            in
            doc
                |> Internals.flatten
                |> Pretty.pretty 0
                |> Expect.equal "abcd"
