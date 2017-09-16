module Main
    exposing
        ( (|+)
        , align
        , angles
        , black
        , blue
        , bold
        , bool
        , braces
        , brackets
        , char
        , concat
        , cyan
        , darkBlue
        , darkCyan
        , darkGreen
        , darkMagenta
        , darkRed
        , darkWhite
        , darkYellow
        , debold
        , deunderline
        , dquotes
        , empty
        , fill
        , fillBreak
        , float
        , green
        , group
        , hang
        , indent
        , int
        , join
        , line
        , linebreak
        , list
        , magenta
        , nest
        , onBlack
        , onBlue
        , onCyan
        , onGreen
        , onMagenta
        , onRed
        , onWhite
        , onYellow
        , parens
        , plain
        , red
        , softbreak
        , softline
        , space
        , squotes
        , string
        , surround
        , surroundJoin
        , underline
        , white
        , yellow
        )

import Console as Ansi
import Regex
import Render
import Text exposing (Color(..), ConsoleLayer(..), Doc(..))
import Utils


infixr 6 |+
(|+) : Doc -> Doc -> Doc
(|+) =
    Cat


join : Doc -> List Doc -> Doc
join sep =
    concat << List.intersperse sep


concat : List Doc -> Doc
concat docs =
    Utils.foldr1 (|+) docs
        |> Maybe.withDefault Empty


group : Doc -> Doc
group doc =
    Union (flatten doc) doc



-- BASIC COMBINATORS


empty : Doc
empty =
    Empty


space : Doc
space =
    Char ' '


softline : Doc
softline =
    group line


softbreak : Doc
softbreak =
    group linebreak


line : Doc
line =
    FlatAlt Line space


linebreak : Doc
linebreak =
    FlatAlt Line Empty


string : String -> Doc
string str =
    case str of
        "" ->
            Empty

        s ->
            if Utils.hasWhitespace s then
                parseString s
            else
                Text (String.length s) s


char : Char -> Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Char input


int : Int -> Doc
int =
    string << toString


float : Float -> Doc
float =
    string << toString


bool : Bool -> Doc
bool =
    string << toString


{-| TODO:
find out what this does
-}
column : (Int -> Doc) -> Doc
column =
    Column


{-| TODO:
find out what this does
-}
columns : (Maybe Int -> Doc) -> Doc
columns =
    Columns


{-| TODO:
find out what this does
-}
nesting : (Int -> Doc) -> Doc
nesting =
    Nesting


{-| TODO:
find out what this does
-}
nest : Int -> Doc -> Doc
nest =
    Nest



-- ALIGNMENT


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
    string (Utils.spaces spaces)
        |+ doc
        |> hang spaces



-- BRACKETING


surround : Doc -> Doc -> Doc -> Doc
surround left right doc =
    left
        |+ doc
        |+ right


squotes : Doc -> Doc
squotes =
    surround (char '\'') (char '\'')


dquotes : Doc -> Doc
dquotes =
    surround (char '"') (char '"')


parens : Doc -> Doc
parens =
    surround (char '(') (char ')')


angles : Doc -> Doc
angles =
    surround (char '<') (char '>')


brackets : Doc -> Doc
brackets =
    surround (char '[') (char ']')


braces : Doc -> Doc
braces =
    surround (char '{') (char '}')


surroundJoin : Doc -> Doc -> Doc -> List Doc -> Doc
surroundJoin left right sep docs =
    case docs of
        [] ->
            left
                |+ right

        [ doc ] ->
            left
                |+ doc
                |+ right

        _ ->
            let
                separators =
                    List.repeat (List.length docs) sep
            in
            docs
                |> List.map2 (|+) (left :: separators)
                |> join linebreak
                |> group
                |> flip (|+) right
                |> align


list : List Doc -> Doc
list =
    surroundJoin (char '[') (char ']') (char ',')



-- FILLERS


fill : Int -> Doc -> Doc
fill spacesToAdd doc =
    let
        addSpaces textWidth =
            if textWidth >= spacesToAdd then
                empty
            else
                string (Utils.spaces (spacesToAdd - textWidth))
    in
    width doc addSpaces


fillBreak : Int -> Doc -> Doc
fillBreak spacesToAdd doc =
    let
        addSpaces textWidth =
            if textWidth > spacesToAdd then
                nest spacesToAdd linebreak
            else
                string (Utils.spaces (spacesToAdd - textWidth))
    in
    width doc addSpaces


{-| TODO:
find out whether to expose or not, if so find documentation
-}
width : Doc -> (Int -> Doc) -> Doc
width doc addSpaces =
    column
        (\currCol1 ->
            doc
                |+ column
                    (\currCol2 -> addSpaces (currCol2 - currCol1))
        )



-- COLORS


color : Color -> Doc -> Doc
color =
    Color Foreground


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



-- FORMATTING


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


plain : Doc -> Doc
plain doc =
    case doc of
        FlatAlt doc1 doc2 ->
            FlatAlt (plain doc1) (plain doc2)

        Cat doc1 doc2 ->
            Cat (plain doc1) (plain doc2)

        Nest n doc ->
            Nest n (plain doc)

        Union doc1 doc2 ->
            Union (plain doc1) (plain doc2)

        Color _ _ doc ->
            plain doc

        Bold _ doc ->
            plain doc

        Underline _ doc ->
            plain doc

        Column formDoc ->
            Column (plain << formDoc)

        Columns formDoc ->
            Columns (plain << formDoc)

        Nesting formDoc ->
            Nesting (plain << formDoc)

        _ ->
            doc



-- PRIVATE


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


parseString : String -> Doc
parseString str =
    let
        parse chunk doc =
            case chunk of
                "" ->
                    doc

                "\n" ->
                    line |+ doc

                "\t" ->
                    space |+ space |+ space |+ space |+ doc

                " " ->
                    space |+ doc

                _ ->
                    Text (String.length chunk) chunk
                        |+ doc
    in
    Utils.splitOnWhitespace str
        |> List.foldr parse Empty
