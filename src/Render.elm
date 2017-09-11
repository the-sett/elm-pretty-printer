module Render exposing (show)

import Console as Ansi
import Text exposing (..)
import Utils


type TextFormat
    = WithColor ConsoleLayer Color
    | WithUnderline Formatter
    | WithBold Formatter
    | Reset


type SimpleDoc
    = SFail
    | SEmpty
    | SChar Char SimpleDoc
    | SText Int String SimpleDoc
    | SLine Int SimpleDoc
    | SFormatted (List TextFormat) SimpleDoc


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
        ribbonWidth =
            round (toFloat pageWidth * rfrac)
                |> min pageWidth
                |> max 0

        best : Int -> Int -> Maybe Color -> Maybe Color -> Maybe Formatter -> Maybe Formatter -> Docs -> SimpleDoc
        best indent currCol foregroundColor backgroundColor boldFormatter underliner docs =
            case docs of
                Nil ->
                    SEmpty

                Cons n document documents ->
                    let
                        bestTypical indent currCol docs =
                            best indent currCol foregroundColor backgroundColor boldFormatter underliner docs

                        dsRestore =
                            Cons n
                                (RestoreFormat
                                    { fgColor = foregroundColor
                                    , bgColor = backgroundColor
                                    , bold = boldFormatter
                                    , underliner = underliner
                                    }
                                )
                                documents
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
                            SFormatted
                                [ WithColor layer color ]
                                (best indent currCol fgColor bgColor boldFormatter underliner (Cons n doc dsRestore))

                        Bold fn doc ->
                            SFormatted
                                [ WithBold fn ]
                                (best indent currCol foregroundColor backgroundColor (Just fn) underliner (Cons n doc dsRestore))

                        Underline fn doc ->
                            SFormatted
                                [ WithUnderline fn ]
                                (best indent currCol foregroundColor backgroundColor boldFormatter (Just fn) (Cons n doc dsRestore))

                        RestoreFormat { fgColor, bgColor, bold, underliner } ->
                            let
                                formats =
                                    Reset
                                        :: List.filterMap identity
                                            [ Maybe.map (WithColor Foreground) fgColor
                                            , Maybe.map (WithColor Background) bgColor
                                            , Maybe.map WithBold bold
                                            , Maybe.map WithUnderline underliner
                                            ]
                            in
                            SFormatted formats (best indent currCol fgColor bgColor boldFormatter underliner documents)

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
    best 0 0 Nothing Nothing Nothing Nothing (Cons 0 doc Nil)



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
            String.cons char (display sDoc)

        SText _ content sDoc ->
            String.append content (display sDoc)

        SLine indents sDoc ->
            display sDoc
                |> String.append (String.cons '\n' (Utils.spaces indents))

        SFormatted formats sDoc ->
            List.map getFormatter formats
                |> List.foldr (<|) (display sDoc)


getFormatter : TextFormat -> Formatter
getFormatter format =
    case format of
        Reset ->
            Ansi.plain

        WithColor layer color ->
            toColor color

        WithUnderline underliner ->
            underliner

        WithBold formatter ->
            formatter
