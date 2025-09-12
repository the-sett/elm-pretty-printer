module Issues exposing (suite)

import Expect
import Pretty
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Github Issue Tests"
        [ flattenLongList ]


flattenLongList : Test
flattenLongList =
    Test.test "#3 Check long list of appends can be grouped without overflowing the stack." <|
        \_ ->
            Pretty.string "test"
                |> List.repeat 50000
                |> Pretty.lines
                |> Pretty.group
                |> always Expect.pass
