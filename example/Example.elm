port module Example exposing (..)

import Doc exposing ((|+), Doc)
import Platform


{-| TODO:

  - change onSomeBackgroundColor to bgSomeBackgroundColor

-}
port log : String -> Cmd msg


main =
    let
        _ =
            Doc.string "You can use "
                |+ Doc.bold (Doc.string "Debug.log")
                |+ Doc.string " to print text"
                |> Doc.toString
                |> Result.map (\doc -> Debug.log doc ())

        samples =
            [ divider
            , rwbSample
            , nestedColorsSample
            , rwbBgSample
            , nestedBgColorsSample
            , nonColorFormattingSample
            , divider
            ]

        doc =
            samples
                |> (::) (Doc.string "...or you can use ports for cleaner output!")
                |> Doc.join Doc.line
                |> Doc.toString
    in
    Platform.program
        { init =
            ( ()
            , Result.withDefault Cmd.none (Result.map log doc)
            )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


rwbSample : Doc
rwbSample =
    Doc.red (Doc.string "Red")
        |+ Doc.string ", "
        |+ Doc.white (Doc.string "white")
        |+ Doc.string " and "
        |+ Doc.blue (Doc.string "blue")
        |+ Doc.char '!'


nestedColorsSample : Doc
nestedColorsSample =
    Doc.string "Nested "
        |+ Doc.yellow (Doc.string "colors")
        |+ Doc.string " example"
        |> Doc.blue


rwbBgSample : Doc
rwbBgSample =
    Doc.onRed (Doc.string "Red")
        |+ Doc.string ", "
        |+ Doc.onWhite (Doc.string "white")
        |+ Doc.string " and "
        |+ Doc.onBlue (Doc.string "blue")
        |+ Doc.char '!'


nestedBgColorsSample : Doc
nestedBgColorsSample =
    Doc.string "Nested "
        |+ Doc.onYellow (Doc.string "colors")
        |+ Doc.string " example"
        |> Doc.onBlue


nonColorFormattingSample : Doc
nonColorFormattingSample =
    Doc.string "We can do "
        |+ Doc.bold (Doc.string "boldness")
        |+ Doc.string ", if your terminal supports it, and perhaps even "
        |+ Doc.underline (Doc.string "underlining")


divider : Doc
divider =
    String.repeat 80 "-"
        |> Doc.string
        |> Doc.cyan
