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


{-| Append two Docs together

    string "hello "
        |+ string "world"
        |> Doc.toString
    -- when printed, will be rendered as
    hello world

-}
infixr 6 |+
(|+) : Doc -> Doc -> Doc
(|+) =
    Cat


{-| Puts many Docs together, separated by a given Doc.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join (char ' ')
        |> Doc.toString
    -- when printed, will be rendered as
    how now brown cow?

-}
join : Doc -> List Doc -> Doc
join sep =
    concat << List.intersperse sep


{-| Concatenates many Docs into one.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> concat
        |> Doc.toString
    -- when printed, will be rendered as
    hownowbrowncow?

-}
concat : List Doc -> Doc
concat docs =
    Utils.foldr1 (|+) docs
        |> Maybe.withDefault Empty


{-| Tries to put all elements of a Doc on the current line if it will fit
the width of the page. If everything cannot fit on the current line, then
no changes are made.
-}
group : Doc -> Doc
group doc =
    Union (flatten doc) doc



-- BASIC COMBINATORS


{-| An empty Doc element.

    Doc.toString empty == ""

-}
empty : Doc
empty =
    Empty


{-| Doc element that represents a space

    space =
        char ' '

-}
space : Doc
space =
    Char ' '


{-| Doc that, when combined with other Doc elements, advances to the next line.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join line
        |> Doc.toString
    -- when printed, will be rendered as:
    how
    now
    brown
    cow

When `group` is called on a Doc with a `line` element, it is replaced with spaces.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join line
        |> group
        |> Doc.toString
    -- when printed, will be rendered as:
    how now brown cow?

-}
line : Doc
line =
    FlatAlt Line space


{-| Works the same way as `line`, except when `group` is called on a Doc with a `linebreak`
element, the `linebreak` is replaced with `empty`

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join linebreak
        |> group
        |> Doc.toString
    -- when printed, will be rendered as:
    hownowbrowncow?

-}
linebreak : Doc
linebreak =
    FlatAlt Line Empty


{-| Doc that, when combined with other Doc elements, advances a single space if the current
line has room.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join softline
        |> Doc.toString
    -- when printed, will be rendered as:
    how now brown cow?

If the elements cannot fit on the same line, then it advances to the next line.

    string "a really long string that might"
        |+ softline
        |+ string "not fit on one line"
        |> Doc.toString
    -- when printed, will be rendered as
    a really long string that might
    not fit on one line

-}
softline : Doc
softline =
    group line


{-| Similar to `softline`, except it does not advance the current column if elements can
fit on the same line.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join softbreak
        |> Doc.toString
    -- when printed, will be rendered as:
    hownowbrowncow?

-}
softbreak : Doc
softbreak =
    group linebreak


{-| Creates a Doc from a String.
-}
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


{-| Creates a Doc from a Char.
-}
char : Char -> Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Char input


{-| Create a Doc from an Int.
-}
int : Int -> Doc
int =
    string << Basics.toString


{-| Create a Doc from a Float.
-}
float : Float -> Doc
float =
    string << Basics.toString


{-| Create a Doc from a Bool.
-}
bool : Bool -> Doc
bool =
    string << Basics.toString


{-| Creates a Doc from an `(Int -> Doc)` function where the `Int` is the current column, or the
rightmost position (in characters) on the current line. So `string "hello"` has a current column of `5`.

The Doc that gets returned from the given function will get placed at the current column.
If you don't need fine-grain control over where elements will be placed, `align` or `fill` would be a better alternative.

    string "hello"
        |+ column (\col -> indent col (string "from afar"))
        |> Doc.toString

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
        |> Doc.toString

    -- when rendered, will be printed as
    hello
      world
    !

-}
nest : Int -> Doc -> Doc
nest =
    Nest



-- ALIGNMENT


{-| Sets the nesting level of the given Doc equal to the current column. This vertically aligns the
Doc elements so that they move as a single column.

    string "old"
        |+ line
        |+ string "friend"
        |> align
        |> (|+) (string "hello ")
        |> Doc.toString
    -- when rendered, will be printed as
    hello old
          friend

-}
align : Doc -> Doc
align doc =
    column
        (\currentColumn ->
            nesting
                (\indentLvl -> nest (currentColumn - indentLvl) doc)
        )


{-| Applies hanging indentation equal to the given integer. Does not indent the first line.

    ["the", "hang", "combinator", "indents", "these", "words !"]
        |> List.map string
        |> join softline
        |> hang 4
        |> Doc.toString
    -- when rendered, will be printed as
    the hang combinator indents\n
        these words !

-}
hang : Int -> Doc -> Doc
hang spaces doc =
    align (nest spaces doc)


{-| Indents the entire Doc equal to the given number of spaces.

    ["the", "indent", "combinator", "indents", "these", "words !"]
        |> List.map string
        |> join softline
        |> indent 4
        |> Doc.toString
    -- when printed, will be rendered as
        the indent combinator
        indents these words !

-}
indent : Int -> Doc -> Doc
indent spaces doc =
    string (Utils.spaces spaces)
        |+ doc
        |> hang spaces



-- BRACKETING


{-| Surrounds a Doc with given Docs.

    surround (char '#') (char '?') (string "questionable")
        |> Doc.toString
    -- when printed, will be rendered as:
    #questionable?

-}
surround : Doc -> Doc -> Doc -> Doc
surround left right doc =
    left
        |+ doc
        |+ right


{-| Surrounds a Doc in single quotes
-}
squotes : Doc -> Doc
squotes =
    surround (char '\'') (char '\'')


{-| Surrounds a Doc in double quotes
-}
dquotes : Doc -> Doc
dquotes =
    surround (char '"') (char '"')


{-| Surrounds a Doc in parentheses
-}
parens : Doc -> Doc
parens =
    surround (char '(') (char ')')


{-| Surrounds a Doc in angle brackets
-}
angles : Doc -> Doc
angles =
    surround (char '<') (char '>')


{-| Surrounds a Doc in square brackets
-}
brackets : Doc -> Doc
brackets =
    surround (char '[') (char ']')


{-| Surrounds a Doc in curly braces
-}
braces : Doc -> Doc
braces =
    surround (char '{') (char '}')


{-| Joins a List of Docs together with a given separator and surrounds it with given Docs.

    List.map string ["some", "html", "element"]
        |> surroundJoin (char '<') (char '>') (char '-')
        |> Doc.toString
    -- when printed, will be rendered as
    <some-html-element>

Provides a bit of extra formatting help by aligning elements (separator in front) if they cannot
all fit on the same line.

    [ "a really long string", "another really long string", "a third really long string" ]
        |> List.map string
        |> surroundJoin (char '[') (char ']') (char ',')
        |> (|+) (string "list ")
        |> Doc.toString
    -- when printed, will be rendered as
    list [a really long string
         ,another really long string
         ,a third really long string]

-}
surroundJoin : Doc -> Doc -> Doc -> List Doc -> Doc
surroundJoin left right sep docs =
    case docs of
        [] ->
            left
                |+ right

        doc :: [] ->
            left
                |+ doc
                |+ right

        _ ->
            docs
                |> List.map2 (|+) (left :: List.repeat (List.length docs) sep)
                |> join linebreak
                |> group
                |> flip (|+) right
                |> align


{-| Render a list of Docs as a list.

    list =
      surroundJoin (char '[') (char ']') (char ',')

    List.map int [10, 200, 3000]
        |> list
        |> Doc.toString
    -- when printed, will be rendered as
    [10,200,3000]

-}
list : List Doc -> Doc
list =
    surroundJoin (char '[') (char ']') (char ',')



-- FILLERS


{-| Takes an Int and a Doc and appends spaces to the end of the Doc until the current
column is equal to the given Int.

    fill 12 (string "how now")
        |+ string "brown cow?"
        |> Doc.toString
    -- when printed, will be rendered as:
    how now     brown cow?

If current column is greater than the given Int, nothing is appended.

    fill 5 (string "how now")
        |+ string "brown cow?"
        |> Doc.toString
    -- when printed, will be rendered as:
    how nowbrown cow?

This can be especially useful with `align` to represent type signatures

    let
        types =
            [ ( "empty", "Doc" )
            , ( "nest", "Int -> Doc -> Doc" )
            , ( "linebreak", "Doc" )
            ]

        asAnnotation ( name, typeOf ) =
            fill 6 (string name)
                |+ string " : "
                |+ string typeOf
    in
    List.map asAnnotation types
        |> join linebreak
        |> align
        |> (|+) (string "let ")
        |> Doc.toString

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

        asAnnotation ( name, typeOf ) =
            fillBreak 6 (string name)
                |+ string " : "
                |+ string typeOf
    in
    List.map asAnnotation types
        |> join linebreak
        |> align
        |> (|+) (string "let ")
        |> Doc.toString

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


{-| Changes text color of Doc to black. May not be supported on all terminals.
-}
black : Doc -> Doc
black =
    color (Black Ansi.black)


{-| Changes text color of Doc to red. May not be supported on all terminals.
-}
red : Doc -> Doc
red =
    color (Red Ansi.red)


{-| Changes text color of Doc to dark red. May not be supported on all terminals.
-}
darkRed : Doc -> Doc
darkRed =
    color <| Red (Ansi.dark << Ansi.red)


{-| Changes text color of Doc to green. May not be supported on all terminals.
-}
green : Doc -> Doc
green =
    color (Green Ansi.green)


{-| Changes text color of Doc to dark green. May not be supported on all terminals.
-}
darkGreen : Doc -> Doc
darkGreen =
    color <| Green (Ansi.dark << Ansi.green)


{-| Changes text color of Doc to yellow. May not be supported on all terminals.
-}
yellow : Doc -> Doc
yellow =
    color (Yellow Ansi.yellow)


{-| Changes text color of Doc to dark yellow. May not be supported on all terminals.
-}
darkYellow : Doc -> Doc
darkYellow =
    color <| Yellow (Ansi.dark << Ansi.yellow)


{-| Changes text color of Doc to blue. May not be supported on all terminals.
-}
blue : Doc -> Doc
blue =
    color (Blue Ansi.blue)


{-| Changes text color of Doc to dark blue. May not be supported on all terminals.
-}
darkBlue : Doc -> Doc
darkBlue =
    color <| Blue (Ansi.dark << Ansi.blue)


{-| Changes text color of Doc to magenta. May not be supported on all terminals.
-}
magenta : Doc -> Doc
magenta =
    color (Magenta Ansi.magenta)


{-| Changes text color of Doc to dark magenta. May not be supported on all terminals.
-}
darkMagenta : Doc -> Doc
darkMagenta =
    color <| Magenta (Ansi.dark << Ansi.magenta)


{-| Changes text color of Doc to cyan. May not be supported on all terminals.
-}
cyan : Doc -> Doc
cyan =
    color (Cyan Ansi.cyan)


{-| Changes text color of Doc to dark cyan. May not be supported on all terminals.
-}
darkCyan : Doc -> Doc
darkCyan =
    color <| Cyan (Ansi.dark << Ansi.cyan)


{-| Changes text color of Doc to white. May not be supported on all terminals.
-}
white : Doc -> Doc
white =
    color (White Ansi.white)


{-| Changes text color of Doc to dark white. May not be supported on all terminals.
-}
darkWhite : Doc -> Doc
darkWhite =
    color <| White (Ansi.dark << Ansi.white)


bgColor : Color -> Doc -> Doc
bgColor =
    Color Background


{-| Changes background color of Doc to red. May not be supported on all terminals.
-}
onRed : Doc -> Doc
onRed =
    bgColor (Red Ansi.bgRed)


{-| Changes background color of Doc to white. May not be supported on all terminals.
-}
onWhite : Doc -> Doc
onWhite =
    bgColor (White Ansi.bgWhite)


{-| Changes background color of Doc to blue. May not be supported on all terminals.
-}
onBlue : Doc -> Doc
onBlue =
    bgColor (Blue Ansi.bgBlue)


{-| Changes background color of Doc to yellow. May not be supported on all terminals.
-}
onYellow : Doc -> Doc
onYellow =
    bgColor (Yellow Ansi.bgYellow)


{-| Changes background color of Doc to cyan. May not be supported on all terminals.
-}
onCyan : Doc -> Doc
onCyan =
    bgColor (Cyan Ansi.bgCyan)


{-| Changes background color of Doc to green. May not be supported on all terminals.
-}
onGreen : Doc -> Doc
onGreen =
    bgColor (Green Ansi.bgGreen)


{-| Changes background color of Doc to black. May not be supported on all terminals.
-}
onBlack : Doc -> Doc
onBlack =
    bgColor (Black Ansi.bgBlack)


{-| Changes background color of Doc to magenta. May not be supported on all terminals.
-}
onMagenta : Doc -> Doc
onMagenta =
    bgColor (Magenta Ansi.bgMagenta)



-- FORMATTING


{-| Takes a Doc and bolds all the text. May not be supported on all terminals.
-}
bold : Doc -> Doc
bold =
    Bold Ansi.bold


{-| Removes all bold formatting from a Doc while keeping other formatting.
-}
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


{-| Takes a Doc and underlines all the text. May not be supported on all terminals.
-}
underline : Doc -> Doc
underline =
    Underline Ansi.underline


{-| Removes all underlining from a Doc while keeping other formatting.
-}
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


{-| Removes all formatting from a Doc, including foreground (text) color, background color, underlining, and bold.
-}
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
