port module Example exposing (..)

import Doc exposing ((|+), Doc)
import Platform


port log : String -> Cmd msg


main : Program Never () msg
main =
    let
        _ =
            Doc.string "You can use "
                |+ Doc.bold (Doc.string "Debug.log")
                |+ Doc.string " to print text"
                |> Doc.append Doc.line
                |> Doc.append (dividerText "PRINTING")
                |> Doc.toString
                |> Result.map (\doc -> Debug.log doc ())

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


samples : List Doc
samples =
    [ dividerText "COLORS & FORMATTING"
    , rwbSample
    , nestedColorsSample
    , rwbBgSample
    , nestedBgColorsSample
    , nonColorFormattingSample
    , dividerText "ALIGNMENT"
    , jsonSample
    , dividerText "ENJOY!"
    ]


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
    Doc.bgRed (Doc.string "Red")
        |+ Doc.string ", "
        |+ Doc.bgWhite (Doc.string "white")
        |+ Doc.string " and "
        |+ Doc.bgBlue (Doc.string "blue")
        |+ Doc.char '!'


nestedBgColorsSample : Doc
nestedBgColorsSample =
    Doc.string "Nested "
        |+ Doc.bgYellow (Doc.string "colors")
        |+ Doc.string " example"
        |> Doc.bgBlue


nonColorFormattingSample : Doc
nonColorFormattingSample =
    Doc.string "We can do "
        |+ Doc.bold (Doc.string "boldness")
        |+ Doc.string ", if your terminal supports it, and perhaps even "
        |+ Doc.underline (Doc.string "underlining")


prettyKeyVal : ( String, String ) -> Doc
prettyKeyVal ( attr, value ) =
    let
        prettyAttr =
            Doc.string attr
                |> Doc.red
                |> Doc.bold
                |> Doc.dquotes
    in
    Doc.fill 12 prettyAttr
        |+ Doc.fill 4 (Doc.char ':')
        |+ Doc.dquotes (Doc.green (Doc.string value))


prettyJson : List ( String, String ) -> Doc
prettyJson data =
    List.map prettyKeyVal data
        |> Doc.join (Doc.char ',' |+ Doc.line)
        |> Doc.align
        |> Doc.indent 4
        |> wrapLines
        |> Doc.braces


jsonSample : Doc
jsonSample =
    [ sampleData1, sampleData2 ]
        |> List.map prettyJson
        |> Doc.join (wrapLines (Doc.char ','))
        |> Doc.indent 4
        |> wrapLines
        |> Doc.brackets


sampleData1 : List ( String, String )
sampleData1 =
    [ ( "full_name", "Bill Johnson" )
    , ( "address", "123 Fake St" )
    , ( "role", "guest" )
    ]


sampleData2 : List ( String, String )
sampleData2 =
    [ ( "full_name", "Jane Doe" )
    , ( "address", "1432 Westgreen Terrace" )
    , ( "role", "admin" )
    ]


wrapLines : Doc -> Doc
wrapLines =
    Doc.surround Doc.line Doc.line


dividerText : String -> Doc
dividerText txt =
    let
        room =
            floor
                (toFloat (String.length txt) / 2)

        divider =
            String.repeat (40 - room) "-"
                |> Doc.string
                |> Doc.cyan

        surrounders =
            [ Doc.space, divider, Doc.line ]
    in
    Doc.string txt
        |> Doc.yellow
        |> Doc.surround
            (Doc.concat <| List.reverse surrounders)
            (Doc.concat surrounders)