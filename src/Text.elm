module Text exposing (..)

import Console as Ansi


type Color
    = Black (String -> String)
    | Red (String -> String)
    | Green (String -> String)
    | Yellow (String -> String)
    | Blue (String -> String)
    | Magenta (String -> String)
    | Cyan (String -> String)
    | White (String -> String)


type Underlining
    = SingleUnderline (String -> String)
    | NoUnderline (String -> String)


type ConsoleLayer
    = Foreground
    | Background


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
    | Color ConsoleLayer Color Doc
    | Column (Int -> Doc)
    | Columns (Maybe Int -> Doc)
    | Nesting (Int -> Doc)
    | RestoreFormat (Maybe Color) (Maybe Color)



-- OPERATORS


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



-- CHARS


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


space : Doc
space =
    Char ' '


parens : Doc -> Doc
parens =
    enclose lparen rparen



-- COMBINATORS


cat : List Doc -> Doc
cat =
    group << vcat


enclose : Doc -> Doc -> Doc -> Doc
enclose left right middle =
    left <> middle <> right


group : Doc -> Doc
group doc =
    Union (flatten doc) doc


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

        Color layer color doc ->
            Color layer color (flatten doc)

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



-- PUBLIC CONSTRUCTORS


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


empty : Doc
empty =
    Empty



-- COLORS


black : Doc -> Doc
black =
    color (Black Ansi.black)


red : Doc -> Doc
red =
    color (Red Ansi.red)


darkRed : Doc -> Doc
darkRed =
    color <| Red (Ansi.dark << Ansi.red)


green : Doc -> Doc
green =
    color (Green Ansi.green)


darkGreen : Doc -> Doc
darkGreen =
    color <| Green (Ansi.dark << Ansi.green)


yellow : Doc -> Doc
yellow =
    color (Yellow Ansi.yellow)


darkYellow : Doc -> Doc
darkYellow =
    color <| Yellow (Ansi.dark << Ansi.yellow)


blue : Doc -> Doc
blue =
    color (Blue Ansi.blue)


darkBlue : Doc -> Doc
darkBlue =
    color <| Blue (Ansi.dark << Ansi.blue)


magenta : Doc -> Doc
magenta =
    color (Magenta Ansi.magenta)


darkMagenta : Doc -> Doc
darkMagenta =
    color <| Magenta (Ansi.dark << Ansi.magenta)


cyan : Doc -> Doc
cyan =
    color (Cyan Ansi.cyan)


darkCyan : Doc -> Doc
darkCyan =
    color <| Cyan (Ansi.dark << Ansi.cyan)


white : Doc -> Doc
white =
    color (White Ansi.white)


darkWhite : Doc -> Doc
darkWhite =
    color <| White (Ansi.dark << Ansi.white)


color : Color -> Doc -> Doc
color =
    Color Foreground



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



-- UTIL


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


toColor : Color -> (String -> String)
toColor color =
    case color of
        Black toBlack ->
            toBlack

        Red toRed ->
            toRed

        Green toGreen ->
            toGreen

        Yellow toYellow ->
            toYellow

        Blue toBlue ->
            toBlue

        Magenta toMagenta ->
            toMagenta

        Cyan toCyan ->
            toCyan

        White toWhite ->
            toWhite


toUnderline : Underlining -> (String -> String)
toUnderline underline =
    case underline of
        SingleUnderline toUnderline_ ->
            toUnderline_

        NoUnderline removeUnderline ->
            -- placeholder, find fn that removes underline
            Ansi.plain
