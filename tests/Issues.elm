module Issues exposing (..)

import Expect exposing (Expectation)
import Pretty exposing (Doc)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Github Issue Tests"
        [ flattenLongList () ]


flattenLongList : () -> Test
flattenLongList _ =
    let
        testList =
            List.repeat 10000 "test"

        longList _ =
            testList
                |> List.map Pretty.string
                |> Pretty.lines
                |> Pretty.group
                |> always Expect.pass
    in
    Test.test
        "#3 Check long list of appends can be grouped without overflowing the stack."
        longList
