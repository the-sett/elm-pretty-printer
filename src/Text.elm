module Text exposing (..)

import Console as Ansi
import Utils


-- BASIC COMBINATORS
--    empty, char, text, string, int, float, bool,
--    (<>), nest, line, linebreak, group, softline,
--    softbreak, hardline, flatAlt
--
-- ALIGNMENT COMBINATORS
--    align, hang, indent, encloseSep, list,
--    tupled, semiBraces
--
-- OPERATORS
--    (<+>), (<$>), (</>), (<$$>), (<//>)
--
-- LIST COMBINATORS
--    hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate
--
-- FILLER COMBINATORS
--    fill, fillBreak
--
-- BRACKETING COMBINATORS
--    enclose, squotes, dquotes, parens, angles, braces, brackets
--
-- NAMED CHARACTER COMBINATORS
--    lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
--    squote, dquote, semi, colon, comma, space, dot, backslash, equals
--
-- FORMATTING COMBINATORS
--    bold, debold, underline, deunderline
--
-- FORMATTING ELIMINATION COMBINATORS
--    plain
--
-- RENDERING
--    SimpleDoc(..), show
--
-- UNDOCUMENTED?
--    column, columns, nesting, width


type alias Formatter =
    String -> String


type Color
    = Black Formatter
    | Red Formatter
    | Green Formatter
    | Yellow Formatter
    | Blue Formatter
    | Magenta Formatter
    | Cyan Formatter
    | White Formatter


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
    | Bold Formatter Doc
    | Underline Formatter Doc
    | Column (Int -> Doc)
    | Columns (Maybe Int -> Doc)
    | Nesting (Int -> Doc)
    | RestoreFormat
        { fgColor : Maybe Color
        , bgColor : Maybe Color
        , bold : Maybe Formatter
        , underliner : Maybe Formatter
        }



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


infixr 5 <//>
(<//>) : Doc -> Doc -> Doc
(<//>) doc1 doc2 =
    doc1 <> softbreak <> doc2



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


langle : Doc
langle =
    Char '<'


rangle : Doc
rangle =
    Char '>'


lbracket : Doc
lbracket =
    Char '['


rbracket : Doc
rbracket =
    Char ']'


space : Doc
space =
    Char ' '


semi : Doc
semi =
    Char ';'


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

        Bold f doc ->
            Bold f (flatten doc)

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
    Utils.foldr1 fn docs
        |> Maybe.withDefault Empty



-- BASIC COMBINATORS


empty : Doc
empty =
    Empty


char : Char -> Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Char input


text : String -> Doc
text str =
    case str of
        "" ->
            Empty

        s ->
            Text (String.length s) s



-- string is like "text" but replaces '\n' by "line"
-- | The document @(string s)@ concatenates all characters in @s@
-- using @line@ for newline characters and @char@ for all other
-- characters. It is used instead of 'text' whenever the text contains
-- newline characters.


string : String -> Doc
string str =
    case String.uncons str of
        Nothing ->
            empty

        Just ( '\n', rest ) ->
            line <> string rest

        _ ->
            let
                ( xs, ys ) =
                    Utils.break ((==) '\n') str
            in
            text xs <> string ys


int : Int -> Doc
int =
    text << toString


float : Float -> Doc
float =
    text << toString


bool : Bool -> Doc
bool =
    text << toString


flatAlt : Doc -> Doc -> Doc
flatAlt =
    FlatAlt


column : (Int -> Doc) -> Doc
column fn =
    Column fn


nesting : (Int -> Doc) -> Doc
nesting fn =
    Nesting fn


nest : Int -> Doc -> Doc
nest n doc =
    Nest n doc


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


bgColor : Color -> Doc -> Doc
bgColor =
    Color Background


onRed : Doc -> Doc
onRed =
    bgColor (Red Ansi.bgRed)


onWhite : Doc -> Doc
onWhite =
    bgColor (White Ansi.bgWhite)


onBlue : Doc -> Doc
onBlue =
    bgColor (Blue Ansi.bgBlue)


onYellow : Doc -> Doc
onYellow =
    bgColor (Yellow Ansi.bgYellow)


onCyan : Doc -> Doc
onCyan =
    bgColor (Cyan Ansi.bgCyan)


onGreen : Doc -> Doc
onGreen =
    bgColor (Green Ansi.bgGreen)


onBlack : Doc -> Doc
onBlack =
    bgColor (Black Ansi.bgBlack)


onMagenta : Doc -> Doc
onMagenta =
    bgColor (Magenta Ansi.bgMagenta)



-- BOLD


bold : Doc -> Doc
bold =
    Bold Ansi.bold


debold : Doc -> Doc
debold doc =
    case doc of
        Bold formatter restOfDoc ->
            restOfDoc

        Union doc1 doc2 ->
            Union (debold doc1) (debold doc2)

        Cat doc1 doc2 ->
            Cat (debold doc1) (debold doc2)

        Color layer color doc ->
            Color layer color (debold doc)

        Underline formatter doc ->
            Underline formatter (debold doc)

        FlatAlt doc1 doc2 ->
            FlatAlt (debold doc1) (debold doc2)

        Nest nestingLvl doc ->
            Nest nestingLvl (debold doc)

        Column f ->
            Column (debold << f)

        Columns f ->
            Columns (debold << f)

        Nesting f ->
            Nesting (debold << f)

        _ ->
            doc



-- UNDERLINE


underline : Doc -> Doc
underline =
    Underline Ansi.underline


deunderline : Doc -> Doc
deunderline doc =
    case doc of
        Underline formatter restOfDoc ->
            restOfDoc

        Union doc1 doc2 ->
            Union (deunderline doc1) (deunderline doc2)

        Cat doc1 doc2 ->
            Cat (deunderline doc1) (deunderline doc2)

        Color layer color doc ->
            Color layer color (deunderline doc)

        Bold formatter doc ->
            Bold formatter (deunderline doc)

        FlatAlt doc1 doc2 ->
            FlatAlt (deunderline doc1) (deunderline doc2)

        Nest nestingLvl doc ->
            Nest nestingLvl (deunderline doc)

        Column docFromCurrCol ->
            Column (deunderline << docFromCurrCol)

        Columns docFromCurrCol ->
            Columns (deunderline << docFromCurrCol)

        Nesting docFromIndent ->
            Nesting (deunderline << docFromIndent)

        _ ->
            doc



-- ALIGNMENT


indent : Int -> Doc -> Doc
indent spaces doc =
    hang spaces (text (Utils.spaces spaces) <> doc)


hang : Int -> Doc -> Doc
hang n doc =
    align (nest n doc)


align : Doc -> Doc
align doc =
    column
        (\currentColumn ->
            nesting
                (\indentLvl -> nest (currentColumn - indentLvl) doc)
        )


encloseSep : Doc -> Doc -> Doc -> List Doc -> Doc
encloseSep left right sep docs =
    case docs of
        [] ->
            left <> right

        [ doc ] ->
            left <> doc <> right

        _ ->
            let
                separators =
                    List.repeat (List.length docs) sep
            in
            align
                (cat (List.map2 (<>) (left :: separators) docs) <> right)


list : List Doc -> Doc
list =
    encloseSep lbracket rbracket comma


tupled : List Doc -> Doc
tupled =
    encloseSep lparen rparen comma


semiBraces : List Doc -> Doc
semiBraces =
    encloseSep lbrace rbrace semi



-- OTHER


colorFormatter : Color -> Formatter
colorFormatter color =
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
