module Pretty
    exposing
        ( Doc
        , (|+)
        , append
        , char
        , empty
        , string
        , line
        , group
        , surround
        , softline
        , join
        , space
        , indent
        , parens
        , braces
        , pretty
        )

{-| Pretty printer.
@docs Doc
-}


{-| The type of documents that can be pretty printed.
-}
type Doc
    = Empty
    | Concatenate Doc Doc
    | Nest Int Doc
    | Text String
    | Line
    | Union Doc Doc
    | Nesting (Int -> Doc)
    | Column (Int -> Doc)


type Normal
    = NNil
    | NText String Normal
    | NLine Int Normal



-- ==== Document constructors


empty : Doc
empty =
    Empty


append : Doc -> Doc -> Doc
append =
    Concatenate


infixr 6 |+
(|+) : Doc -> Doc -> Doc
(|+) =
    append


nest : Int -> Doc -> Doc
nest =
    Nest


string : String -> Doc
string =
    Text


char : Char -> Doc
char c =
    Text <| String.fromChar c


line : Doc
line =
    Line


group : Doc -> Doc
group doc =
    Union (flatten doc) doc


column : (Int -> Doc) -> Doc
column =
    Column


nesting : (Int -> Doc) -> Doc
nesting =
    Nesting



-- ==== Document helper functions


surround : Doc -> Doc -> Doc -> Doc
surround left right doc =
    left |+ doc |+ right


softline : Doc
softline =
    group Line


join : Doc -> List Doc -> Doc
join sep docs =
    List.intersperse sep docs
        |> List.foldr (\doc -> \remainder -> doc |+ remainder) empty


space : Doc
space =
    char ' '


parens : Doc -> Doc
parens doc =
    surround (char '(') (char ')') doc


braces : Doc -> Doc
braces doc =
    surround (char '{') (char '}') doc


align : Doc -> Doc
align doc =
    column
        (\currentColumn ->
            nesting
                (\indentLvl -> nest (currentColumn - indentLvl) doc)
        )


hang : Int -> Doc -> Doc
hang spaces doc =
    align (nest spaces doc)


indent : Int -> Doc -> Doc
indent spaces doc =
    string (copy spaces " ")
        |+ doc
        |> hang spaces



-- ==== Pretty printing


pretty : Int -> Doc -> String
pretty w doc =
    layout (best w 0 doc)



-- ==== Internals


flatten : Doc -> Doc
flatten doc =
    case doc of
        Concatenate doc doc2 ->
            Concatenate (flatten doc) (flatten doc2)

        Nest i doc ->
            Nest i <| flatten doc

        Union doc doc2 ->
            flatten doc

        Line ->
            Text " "

        x ->
            x


layout : Normal -> String
layout normal =
    case normal of
        NNil ->
            ""

        NText text normal ->
            text ++ layout normal

        NLine i normal ->
            "\n" ++ copy i " " ++ layout normal


copy : Int -> String -> String
copy i s =
    if i == 0 then
        ""
    else
        s ++ copy (i - 1) s


best : Int -> Int -> Doc -> Normal
best w k x =
    let
        be : Int -> Int -> List ( Int, Doc ) -> Normal
        be w k docs =
            case docs of
                [] ->
                    NNil

                ( i, Empty ) :: ds ->
                    be w k ds

                ( i, Concatenate doc doc2 ) :: ds ->
                    be w k <| ( i, doc ) :: ( i, doc2 ) :: ds

                ( i, Nest j doc ) :: ds ->
                    be w k <| ( i + j, doc ) :: ds

                ( i, Text text ) :: ds ->
                    NText text <| be w (k + (String.length text)) ds

                ( i, Line ) :: ds ->
                    NLine i <| be w i ds

                ( i, Union doc doc2 ) :: ds ->
                    better w
                        k
                        (be w k <| ( i, doc ) :: ds)
                        (\() -> (be w k <| ( i, doc2 ) :: ds))

                ( i, Nesting fn ) :: ds ->
                    be w k <| ( i, fn i ) :: ds

                ( i, Column fn ) :: ds ->
                    be w k <| ( i, fn k ) :: ds
    in
        be w k [ ( 0, x ) ]


better : Int -> Int -> Normal -> (() -> Normal) -> Normal
better w k doc doc2Fn =
    if fits (w - k) doc then
        doc
    else
        doc2Fn ()


fits : Int -> Normal -> Bool
fits w normal =
    if w < 0 then
        False
    else
        case normal of
            NNil ->
                True

            NText text normal ->
                fits (w - (String.length text)) normal

            NLine int normal ->
                True
