module Render exposing (show)

import Lazy exposing (Lazy)
import Text exposing (..)


type TextFormatter
    = SetColor Color
    | SetUnderlining Underlining


type SimpleDoc
    = SFail
    | SEmpty
    | SChar Char SimpleDoc
    | SText Int String SimpleDoc
    | SLine Int SimpleDoc


type Docs
    = Nil
    | Cons Int (Lazy Doc) Docs


show : Lazy Doc -> String
show doc =
    display (renderPretty 0.4 80 doc)


renderPretty : Float -> Int -> Lazy Doc -> SimpleDoc
renderPretty =
    renderFits fits1


renderFits :
    (Int -> Int -> Int -> SimpleDoc -> Bool)
    -> Float
    -> Int
    -> Lazy Doc
    -> SimpleDoc
renderFits doesItFit rfrac pageWidth doc =
    let
        -- https://github.com/ekmett/ansi-wl-pprint/blob/master/Text/PrettyPrint/ANSI/Leijen/Internal.hs#L1005
        ribbonWidth =
            32

        best indent currCol mb_fc mb_bc mb_in mb_it mb_un docs =
            case docs of
                Nil ->
                    SEmpty

                Cons n document documents ->
                    let
                        bestTypical indent_ currCol_ documents_ =
                            best indent_ currCol_ mb_fc mb_bc mb_in mb_it mb_un documents_
                    in
                    case Lazy.force document of
                        Fail ->
                            SFail

                        Empty ->
                            bestTypical indent currCol documents

                        Char char ->
                            SChar char (bestTypical indent (currCol + 1) documents)

                        Text l str ->
                            SText l str (bestTypical indent (currCol + 1) documents)

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
    best 0 0 Nothing Nothing Nothing Nothing Nothing (Cons 0 doc Nil)


fits1 : Int -> Int -> Int -> SimpleDoc -> Bool
fits1 pageWidth minNestingLvl firstLineWidth simpleDoc =
    if firstLineWidth < 0 then
        False
    else
        case simpleDoc of
            SFail ->
                False

            SEmpty ->
                True

            SChar char sDoc ->
                fits1 pageWidth minNestingLvl (firstLineWidth - 1) sDoc

            SText width content sDoc ->
                fits1 pageWidth minNestingLvl (firstLineWidth - width) sDoc

            SLine width sDoc ->
                True


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


indentation : Int -> String
indentation n =
    if n <= 0 then
        ""
    else
        String.repeat n " "
