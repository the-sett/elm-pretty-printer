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
                |> flip Debug.log ()

        doc =
            samples
                |> (::) (Doc.string "...or you can use ports for cleaner output!")
                |> Doc.join Doc.hardline
                |> Doc.toString
    in
    Platform.program
        { init = ( (), log doc )
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
        withQuotes =
            Doc.dquotes << Doc.string
    in
    Doc.fill 12 (withQuotes attr)
        |+ Doc.fill 4 (Doc.char ':')
        |+ withQuotes value


prettyJson : List ( String, String ) -> Doc
prettyJson data =
    List.map prettyKeyVal data
        |> Doc.join (Doc.char ',' |+ Doc.line)
        |> Doc.align
        |> Doc.indent 4
        |> break
        |> Doc.braces


jsonSample : Doc
jsonSample =
    [ sampleData1, sampleData2 ]
        |> List.map prettyJson
        |> Doc.join (break (Doc.char ','))
        |> Doc.indent 4
        |> break
        |> Doc.brackets


break : Doc -> Doc
break =
    Doc.surround Doc.hardline Doc.hardline


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
            [ Doc.line, divider, Doc.space ]
    in
    Doc.string txt
        |> Doc.yellow
        |> Doc.surround
            (Doc.concat surrounders)
            (Doc.concat <| List.reverse surrounders)
