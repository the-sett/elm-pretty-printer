module Text exposing (..)


type Color
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White


type Underlining
    = SingleUnderline
    | NoUnderline



{--
  TODO:
    - support for Color and Underline
--}


type Doc
    = Fail
    | Empty
    | Char Char
    | Text Int String
    | Line
    | FlatAlt Doc Doc
    | Cat Doc Doc
    | Nest Int Doc
    | Union Doc Doc
    | Column (Int -> Doc)
    | Columns (Maybe Int -> Doc)
    | Nesting (Int -> Doc)


infixr 6 <>
(<>) : Doc -> Doc -> Doc
(<>) =
    Cat


infixr 6 <+>
(<+>) : Doc -> Doc -> Doc
(<+>) left right =
    left <> space <> right


infixr 5 <$$>
(<$$>) : Doc -> Doc -> Doc
(<$$>) left right =
    left <> linebreak <> right


infixr 5 <$>
(<$>) : Doc -> Doc -> Doc
(<$>) left right =
    left <> line <> right


infixr 5 </>
(</>) : Doc -> Doc -> Doc
(</>) doc1 doc2 =
    doc1 <> softline <> doc2


cat : List Doc -> Doc
cat =
    group << vcat


enclose : Doc -> Doc -> Doc -> Doc
enclose left right middle =
    left <> middle <> right


colon : Doc
colon =
    Char ':'


comma : Doc
comma =
    Char ','


equals : Doc
equals =
    Char '='


lbrace : Doc
lbrace =
    Char '{'


rbrace : Doc
rbrace =
    Char '}'


lparen : Doc
lparen =
    Char '('


rparen : Doc
rparen =
    Char ')'


parens : Doc -> Doc
parens =
    enclose lparen rparen


empty : Doc
empty =
    Empty


group : Doc -> Doc
group doc =
    Union (flatten doc) doc



-- yellow : Doc -> Doc
-- yellow =
--     Color Yellow


flatten : Doc -> Doc
flatten doc =
    case doc of
        FlatAlt doc1 doc2 ->
            doc2

        Cat doc1 doc2 ->
            Cat (flatten doc1) (flatten doc2)

        Nest n doc ->
            Nest n (flatten doc)

        Line ->
            Fail

        Union doc1 doc2 ->
            flatten doc1

        Column f ->
            Column (flatten << f)

        Columns f ->
            Columns (flatten << f)

        Nesting f ->
            Nesting (flatten << f)

        other ->
            other


sep : List Doc -> Doc
sep =
    group << vsep


vsep : List Doc -> Doc
vsep =
    fold (<$>)


vcat : List Doc -> Doc
vcat =
    fold (<$$>)


hsep : List Doc -> Doc
hsep =
    fold (<+>)


fillSep : List Doc -> Doc
fillSep =
    fold (</>)


fold : (Doc -> Doc -> Doc) -> List Doc -> Doc
fold fn docs =
    foldr1 fn docs
        |> Maybe.withDefault Empty


space : Doc
space =
    Char ' '


spaces : Int -> String
spaces n =
    if n <= 0 then
        ""
    else
        String.repeat n " "


char : Char -> Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Char input


column : (Int -> Doc) -> Doc
column fn =
    Column fn


nesting : (Int -> Doc) -> Doc
nesting fn =
    Nesting fn


nest : Int -> Doc -> Doc
nest n doc =
    Nest n doc


text : String -> Doc
text str =
    case str of
        "" ->
            Empty

        s ->
            Text (String.length s) s


line : Doc
line =
    FlatAlt Line space


softline : Doc
softline =
    group line


softbreak : Doc
softbreak =
    group linebreak


linebreak : Doc
linebreak =
    FlatAlt Line empty


hardline : Doc
hardline =
    Line



-- ALIGNMENT


indent : Int -> Doc -> Doc
indent n doc =
    hang n (text (spaces n) <> doc)


hang : Int -> Doc -> Doc
hang n doc =
    align (nest n doc)


align : Doc -> Doc
align doc =
    column
        (\currentColumn ->
            nesting (\indentLvl -> nest (currentColumn - indentLvl) doc)
        )


foldr1 : (a -> a -> a) -> List a -> Maybe a
foldr1 f xs =
    -- https://github.com/haskell-suite/base/blob/master/Data/Foldable.hs#L144
    let
        folding x m =
            m
                |> Maybe.map (f x)
                |> Maybe.withDefault x
                |> Just
    in
    List.foldr folding Nothing xs
