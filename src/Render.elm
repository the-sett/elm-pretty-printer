module Render exposing (show)

import Console as Ansi
import Text exposing (..)


type TextFormatter
    = WithColor ConsoleLayer Color
    | WithUnderline Underlining
    | Reset


type SimpleDoc
    = SFail
    | SEmpty
    | SChar Char SimpleDoc
    | SText Int String SimpleDoc
    | SLine Int SimpleDoc
    | SFormatted (List TextFormatter) SimpleDoc


type Docs
    = Nil
    | Cons Int Doc Docs


show : Doc -> String
show doc =
    display (renderPretty 0.4 80 doc)


renderPretty : Float -> Int -> Doc -> SimpleDoc
renderPretty =
    renderFits willFit1


renderFits :
    (Int -> Int -> Int -> SimpleDoc -> Bool)
    -> Float
    -> Int
    -> Doc
    -> SimpleDoc
renderFits doesItFit rfrac pageWidth doc =
    let
        -- https://github.com/ekmett/ansi-wl-pprint/blob/master/Text/PrettyPrint/ANSI/Leijen/Internal.hs#L1005
        ribbonWidth =
            32

        best indent currCol foregroundColor backgroundColor docs =
            case docs of
                Nil ->
                    SEmpty

                Cons n document documents ->
                    let
                        bestTypical indent currCol docs =
                            best indent currCol foregroundColor backgroundColor docs

                        dsRestore =
                            Cons n (RestoreFormat foregroundColor backgroundColor) documents
                    in
                    case document of
                        Fail ->
                            SFail

                        Empty ->
                            bestTypical indent currCol documents

                        Char char ->
                            SChar char (bestTypical indent (currCol + 1) documents)

                        Text l str ->
                            SText l str (bestTypical indent (currCol + l) documents)

                        Line ->
                            SLine n (bestTypical n n documents)

                        FlatAlt doc1 _ ->
                            bestTypical indent currCol (Cons n doc1 documents)

                        Cat doc1 doc2 ->
                            bestTypical indent currCol (Cons n doc1 (Cons n doc2 documents))

                        Nest num doc_ ->
                            bestTypical indent currCol (Cons (num + n) doc_ documents)

                        Union doc1 doc2 ->
                            nicest
                                indent
                                currCol
                                (bestTypical indent currCol (Cons n doc1 documents))
                                (bestTypical indent currCol (Cons n doc2 documents))

                        Column fn ->
                            bestTypical indent currCol (Cons n (fn currCol) documents)

                        Columns fn ->
                            bestTypical indent currCol (Cons n (fn (Just pageWidth)) documents)

                        Nesting fn ->
                            bestTypical indent currCol (Cons n (fn n) documents)

                        Color layer color doc ->
                            let
                                ( fgColor, bgColor ) =
                                    case layer of
                                        Background ->
                                            ( foregroundColor, Just color )

                                        Foreground ->
                                            ( Just color, backgroundColor )
                            in
                            SFormatted [ WithColor layer color ] (best indent currCol fgColor bgColor (Cons n doc dsRestore))

                        RestoreFormat fgColor bgColor ->
                            let
                                formats =
                                    Reset
                                        :: List.filterMap identity
                                            [ Maybe.map (WithColor Foreground) fgColor
                                            , Maybe.map (WithColor Background) bgColor
                                            ]
                            in
                            SFormatted formats (best indent currCol fgColor bgColor documents)

        nicest indent currCol doc1 doc2 =
            let
                width =
                    min (pageWidth - currCol) (ribbonWidth - currCol + indent)
            in
            if doesItFit pageWidth (min indent currCol) width doc1 then
                doc1
            else
                doc2
    in
    best 0 0 Nothing Nothing (Cons 0 doc Nil)



{--
  does 1 line lookahead
--}


willFit1 : Int -> Int -> Int -> SimpleDoc -> Bool
willFit1 pageWidth minNestingLvl firstLineWidth simpleDoc =
    if firstLineWidth < 0 then
        False
    else
        case simpleDoc of
            SFail ->
                False

            SEmpty ->
                True

            SChar char sDoc ->
                willFit1 pageWidth minNestingLvl (firstLineWidth - 1) sDoc

            SText width content sDoc ->
                willFit1 pageWidth minNestingLvl (firstLineWidth - width) sDoc

            SLine width sDoc ->
                True

            SFormatted _ sDoc ->
                willFit1 pageWidth minNestingLvl firstLineWidth sDoc


display : SimpleDoc -> String
display simpleDoc =
    case simpleDoc of
        SFail ->
            Debug.crash "SFail cannot appear uncaught in a rendered SimpleDoc"

        SEmpty ->
            ""

        SChar char sDoc ->
            display sDoc
                |> String.cons char

        SText _ content sDoc ->
            display sDoc
                |> String.append content

        SLine indents sDoc ->
            display sDoc
                |> String.append (String.cons '\n' (indentation indents))

        SFormatted formats sDoc ->
            formats
                |> List.map getFormatter
                |> List.foldr (<|) (display sDoc)


getFormatter : TextFormatter -> (String -> String)
getFormatter format =
    case format of
        Reset ->
            Ansi.plain

        WithColor layer color ->
            toColor color

        WithUnderline underlining ->
            toUnderline underlining


indentation : Int -> String
indentation n =
    if n <= 0 then
        ""
    else
        String.repeat n " "
