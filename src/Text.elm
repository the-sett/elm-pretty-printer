module Text exposing (..)

import Lazy as Lazy exposing (Lazy)


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
    | FlatAlt (Lazy Doc) (Lazy Doc)
    | Cat (Lazy Doc) (Lazy Doc)
    | Nest Int (Lazy Doc)
    | Union (Lazy Doc) (Lazy Doc)
    | Column (Int -> Lazy Doc)
    | Columns (Maybe Int -> Lazy Doc)
    | Nesting (Int -> Lazy Doc)


infixr 6 <>
(<>) : Doc -> Doc -> Lazy Doc
(<>) left right =
    Cat (lazify left) (lazify right)
        |> lazify


infixr 6 <+>
(<+>) : Doc -> Doc -> Lazy Doc
(<+>) left right =
    Lazy.andThen (flip (<>) right) space
        |> Lazy.andThen ((<>) left)


infixr 5 <$$>
(<$$>) : Doc -> Doc -> Lazy Doc
(<$$>) left right =
    (linebreak <> right)
        |> Lazy.andThen ((<>) left)


infixr 5 <$>
(<$>) : Doc -> Doc -> Lazy Doc
(<$>) left right =
    Lazy.andThen (flip (<>) right) space
        |> Lazy.andThen ((<>) left)


infixr 5 </>
(</>) : Doc -> Doc -> Lazy Doc
(</>) doc1 doc2 =
    Lazy.andThen (flip (<>) doc2) softline
        |> Lazy.andThen ((<>) doc1)


cat : List Doc -> Lazy Doc
cat =
    Lazy.andThen group << vcat


enclose : Doc -> Doc -> Doc -> Lazy Doc
enclose left right middle =
    (middle <> right)
        |> Lazy.andThen ((<>) left)


colon : Lazy Doc
colon =
    char ':'


comma : Lazy Doc
comma =
    char ','


equals : Lazy Doc
equals =
    char '='


lbrace : Lazy Doc
lbrace =
    char '{'


rbrace : Lazy Doc
rbrace =
    char '}'


lparen : Lazy Doc
lparen =
    char '('


rparen : Lazy Doc
rparen =
    char ')'


parens : Doc -> Lazy Doc
parens doc =
    Lazy.map2 enclose lparen rparen
        |> Lazy.andThen ((|>) doc)


empty : Lazy Doc
empty =
    lazify Empty


group : Doc -> Lazy Doc
group doc =
    Union (flatten doc) (lazify doc)
        |> lazify



-- yellow : Doc -> Doc
-- yellow =
--     Color Yellow


flatten : Doc -> Lazy Doc
flatten doc =
    case doc of
        FlatAlt doc1 doc2 ->
            doc2

        Cat doc1 doc2 ->
            Cat (Lazy.andThen flatten doc1) (Lazy.andThen flatten doc2)
                |> lazify

        Nest n doc ->
            Nest n (Lazy.andThen flatten doc)
                |> lazify

        Line ->
            lazify Fail

        Union doc1 doc2 ->
            Lazy.andThen flatten doc1

        Column f ->
            Column (Lazy.andThen flatten << f)
                |> lazify

        Columns f ->
            Columns (Lazy.andThen flatten << f)
                |> lazify

        Nesting f ->
            Nesting (Lazy.andThen flatten << f)
                |> lazify

        other ->
            lazify other


sep : List Doc -> Lazy Doc
sep =
    Lazy.andThen group << vsep


vsep : List Doc -> Lazy Doc
vsep =
    fold (<$>)


vcat : List Doc -> Lazy Doc
vcat =
    fold (<$$>)


hsep : List Doc -> Lazy Doc
hsep =
    fold (<+>)


fillSep : List Doc -> Lazy Doc
fillSep =
    fold (</>)


fold : (Doc -> Doc -> Lazy Doc) -> List Doc -> Lazy Doc
fold fn docs =
    let
        f : Doc -> Lazy Doc -> Lazy Doc
        f elt acc =
            Lazy.andThen (fn elt) acc
    in
    List.foldr f (lazify Empty) docs


space : Lazy Doc
space =
    char ' '


spaces : Int -> String
spaces n =
    if n <= 0 then
        ""
    else
        String.repeat n " "


char : Char -> Lazy Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Char input
                |> lazify


column : (Int -> Lazy Doc) -> Lazy Doc
column fn =
    lazify (Column fn)


nesting : (Int -> Lazy Doc) -> Doc
nesting fn =
    Nesting fn


nest : Int -> Doc -> Doc
nest n doc =
    Nest n (lazify doc)


text : String -> Doc
text str =
    case str of
        "" ->
            Empty

        s ->
            Text (String.length s) s


line : Lazy Doc
line =
    FlatAlt (lazify Line) space
        |> lazify


softline : Lazy Doc
softline =
    Lazy.andThen group line


softbreak : Lazy Doc
softbreak =
    group linebreak


linebreak : Doc
linebreak =
    FlatAlt hardline empty


hardline : Lazy Doc
hardline =
    lazify Line



-- ALIGNMENT


indent : Int -> Doc -> Lazy Doc
indent n doc =
    --hang n (text (spaces n) <> doc)
    (text (spaces n) <> doc)
        |> Lazy.andThen (hang n)


hang : Int -> Doc -> Lazy Doc
hang n doc =
    align (nest n doc)


align : Doc -> Lazy Doc
align doc =
    column
        (\k ->
            lazify (nesting (\i -> lazify (nest (k - i) doc)))
        )


lazify : a -> Lazy a
lazify elt =
    Lazy.lazy (\_ -> elt)
