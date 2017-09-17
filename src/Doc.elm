module Doc
    exposing
        ( (|+)
        , Color(..)
        , Doc
        , Formatter
        , SimpleDoc(..)
        , TextFormat(..)
        , align
        , angles
        , black
        , blue
        , bold
        , bool
        , braces
        , brackets
        , char
        , column
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
        , display
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
        , renderPretty
        , softbreak
        , softline
        , space
        , squotes
        , string
        , surround
        , surroundJoin
        , toString
        , underline
        , white
        , yellow
        )

import Basics
import Console as Ansi
import Utils


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


{-| Type alias for a function that knows how to take a String and return a new String with some
sort of formatting. Right now formatting can either be color, bold, or underline.
-}
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
    string << Basics.toString


float : Float -> Doc
float =
    string << Basics.toString


bool : Bool -> Doc
bool =
    string << Basics.toString


{-| Creates a Doc from an `(Int -> Doc)` function where the `Int` is the current column, or the
rightmost position (in characters) on the current line. So `string "hello"` has a current column of `5`.

The Doc that gets returned from the given function will get placed at the current column.
If you don't need fine-grain control over where elements will be placed, `align` or `fill` would be a better alternative.

    string "hello"
        |+ column (\col -> indent col (string "from afar"))

    -- when rendered, will be printed as
    hello     from afar

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


{-| Creates a Doc with the current indentation level increased by the given `Int`.

    nest 2 (string "hello" |+ line |+ string "world")
        |+ line
        |+ char '!'

    -- when rendered, will be printed as
    hello
      world
    !

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


{-| Takes an Int and a Doc and appends spaces to the end of the Doc until the current
column is equal to the given Int.

    fill 12 (string "how now")
        |+ string "brown cow?"
    -- when printed, will be rendered as:
    how now     brown cow?

If current column is greater than the given Int, nothing is appended.

    fill 5 (string "how now brown cow?")
    -- when printed, will be rendered as:
    how now brown cow?

This can be especially useful with `align` to represent type signatures

    let
        types =
            [ ( "empty", "Doc" )
            , ( "nest", "Int -> Doc -> Doc" )
            , ( "linebreak", "Doc" )
            ]

        asAnnotation ( name, tipe ) =
            fill 6 (string name)
                |+ string " : "
                |+ string tipe
    in
        string "let "
            |+ align (join linebreak (List.map asAnnotation types))

    -- when printed, will be rendered as:
    let empty  : Doc
        nest   : Int -> Doc -> Doc
        linebreak : Doc

-}
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


{-| Works the same way as `fill`, except that if the current column is greater than the given `Int`,
then a linebreak is inserted and the nesting level is increased to given `Int`.

    let
        types =
            [ ( "empty", "Doc" )
            , ( "nest", "Int -> Doc -> Doc" )
            , ( "linebreak", "Doc" )
            ]

        asAnnotation ( name, tipe ) =
            fillBreak 6 (string name)
                |+ string " : "
                |+ string tipe
    in
        string "let "
            |+ align (join linebreak (List.map asAnnotation types))

    -- when printed, will be rendered as:
    let empty  : Doc
        nest   : Int -> Doc -> Doc
        linebreak
               : Doc

-}
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



-- RENDERERS


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


{-| Taks a Doc and converts it to a string with a column width of 80 and a ribbon width of 32
-}
toString : Doc -> String
toString doc =
    display (renderPretty 0.4 80 doc)


{-| Converts a Doc into a SimpleDoc, which is just the intermediate data structure between Doc and
String. This function also takes a ribbon width and a page width. Where the ribbon width indicates the
max percentage of non-indentation characters that should appear on a line, and the page width is the
max number of total characters that can be on a single line. Can be used in combination with `display` to
convert a Doc to a String with more customization on the width than using the default `toString` function.

    let
        doc =
          string "list"
              |+ list (List.map int [10, 200, 3000])
    in
        display (renderPretty 0.25 80 doc)
        -- when rendered, will render with a ribbon width of 20
        list [10,200,3000]


        display (renderPretty 0.2 80 doc)
        -- when rendered, will render with a ribbon width of 16
        list [10
             ,200
             ,3000]

-}
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

                        Text length str ->
                            SText length str (bestTypical indent (currCol + length) documents)

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


{-| Takes a SimpleDoc and converts it to a String
-}
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
            colorFormatter color

        WithUnderline underliner ->
            underliner

        WithBold formatter ->
            formatter



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
