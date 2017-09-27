module Doc
    exposing
        ( (|+)
        , Color(..)
        , Doc
        , Formatter
        , NormalForm(..)
        , TextFormat(..)
        , align
        , angles
        , append
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

{-| A pretty printing library based off of the the [paper by Philip Wadler][paper].
Right now this library is geared to print to the terminal, so users must pass the content they wish to pretty print as
the first argument of `Debug.log` in order for the text to be rendered as expected. **The comments underneath each example
is what Debug.log will render after the Doc is converted to a String**. In the future I intend to add functionality that will allow pretty printing as Html.

[paper]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf


# Basics

@docs Doc, string, char, int, float, bool, space, empty


# Combining Docs

@docs append, (|+), join, concat


# Lines

@docs group, line, linebreak, softline, softbreak


# Bracketing

@docs surround, squotes, dquotes, parens, angles, brackets, braces, surroundJoin, list


# Alignment

@docs align, hang, indent, nest, column


# Fillers

@docs fill, fillBreak


# Colors

@docs Color, Formatter, black, red, darkRed, green, darkGreen, yellow, darkYellow, blue, darkBlue, magenta, darkMagenta, cyan, darkCyan, white, darkWhite, onRed, onWhite, onBlue, onYellow, onCyan, onGreen, onBlack, onMagenta


# Formatting

@docs bold, debold, underline, deunderline, plain


# Rendering

@docs NormalForm, TextFormat, renderPretty, toString, display

-}

import Basics
import Console as Ansi
import Utils


{-| Serves as a representation of what can be rendered. Can be combined, aligned, and formatted in many ways.
-}
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


{-| Different ANSI Colors that can be displayed. Many have dark variations as well. Colors may come out differently
depending on your terminal.
-}
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

    append (string "hello ") (string "world")
    -- hello world

-}
append : Doc -> Doc -> Doc
append =
    Cat


infixr 6 |+


{-| Infix version of `append`

    string "hello "
        |+ string "world"
    -- hello world

-}
(|+) : Doc -> Doc -> Doc
(|+) =
    Cat


{-| Puts many Docs together, separated by a given Doc.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join (char ' ')
    -- how now brown cow?

-}
join : Doc -> List Doc -> Doc
join sep =
    concat << List.intersperse sep


{-| Concatenates many Docs into one.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> concat
    -- hownowbrowncow?

-}
concat : List Doc -> Doc
concat docs =
    Utils.foldr1 (|+) docs
        |> Maybe.withDefault Empty


{-| Tries to put all elements of a Doc on the current line if it will fit
the width of the page. If everything cannot fit on the current line, then
no changes are made.

    string "how now"
        |+ char '\n'
        |+ string "brown cow?"
        |> group
    -- how now brown cow?

-}
group : Doc -> Doc
group doc =
    Union (flatten doc) doc



-- BASIC COMBINATORS


{-| An empty Doc element.

    empty
    -- ""

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
    -- how
    -- now
    -- brown
    -- cow

When `group` is called on a Doc separated by `line` elements, the `line`s are replaced with spaces.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join line
        |> group
    -- how now brown cow?

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
    -- hownowbrowncow?

-}
linebreak : Doc
linebreak =
    FlatAlt Line Empty


{-| Doc that, when combined with other Doc elements, advances a single space if the current
line has room.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join softline
    -- how now brown cow?

If the elements cannot fit on the same line, then it advances to the next line.

    string "a really long string that might"
        |+ softline
        |+ string "not fit on one line"
    -- a really long string that might
    -- not fit on one line

-}
softline : Doc
softline =
    group line


{-| Works the same way `softline`, except it separates with `empty` if elements can
fit on the same line.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join softbreak
    -- hownowbrowncow?

-}
softbreak : Doc
softbreak =
    group linebreak


{-| Creates a Doc from a String.

    string "hello, world!"
    -- hello, world!

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

    char '!'
    -- !

-}
char : Char -> Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Char input


{-| Create a Doc from an Int.

    int 3
    -- 3

-}
int : Int -> Doc
int =
    string << Basics.toString


{-| Create a Doc from a Float.

    float 12.3456
    -- 12.3456

-}
float : Float -> Doc
float =
    string << Basics.toString


{-| Create a Doc from a Bool.

    bool True
    -- True

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
    -- hello     from afar

-}
column : (Int -> Doc) -> Doc
column =
    Column


columns : (Maybe Int -> Doc) -> Doc
columns =
    Columns


nesting : (Int -> Doc) -> Doc
nesting =
    Nesting


{-| Creates a Doc with the current indentation level increased by the given `Int`.

    nest 2 (string "hello" |+ line |+ string "world")
        |+ line
        |+ char '!'
    -- hello
    --  world
    -- !

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
        |> append (string "hello ")
    -- hello old
    --       friend

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
    -- the hang combinator indents
    --     these words !

-}
hang : Int -> Doc -> Doc
hang spaces doc =
    align (nest spaces doc)


{-| Indents the entire Doc equal to the given number of spaces.

    ["the", "indent", "combinator", "indents", "these", "words !"]
        |> List.map string
        |> join softline
        |> indent 4
    --     the indent combinator
    --     indents these words !

-}
indent : Int -> Doc -> Doc
indent spaces doc =
    string (Utils.spaces spaces)
        |+ doc
        |> hang spaces



-- BRACKETING


{-| Surrounds a Doc with given Docs.

    surround (char '#') (char '?') (string "questionable")
    -- #questionable?

-}
surround : Doc -> Doc -> Doc -> Doc
surround left right doc =
    left
        |+ doc
        |+ right


{-| Surrounds a Doc in single quotes

    squotes (string "wrapped in single quotes")
    -- 'wrapped in single quotes'

-}
squotes : Doc -> Doc
squotes =
    surround (char '\'') (char '\'')


{-| Surrounds a Doc in double quotes

    dquotes (string "wrapped in double quotes")
    -- "wrapped in double quotes"

-}
dquotes : Doc -> Doc
dquotes =
    surround (char '"') (char '"')


{-| Surrounds a Doc in parentheses

    parens (string "wrapped in parens")
    -- (wrapped in parens)

-}
parens : Doc -> Doc
parens =
    surround (char '(') (char ')')


{-| Surrounds a Doc in angle brackets

    angles (string "wrapped in angles")
    -- <wrapped in angles>

-}
angles : Doc -> Doc
angles =
    surround (char '<') (char '>')


{-| Surrounds a Doc in square brackets

    brackets (string "wrapped in brackets")
    -- [wrapped in brackets]

-}
brackets : Doc -> Doc
brackets =
    surround (char '[') (char ']')


{-| Surrounds a Doc in curly braces

    braces (string "wrapped in braces")
    -- {wrapped in braces}

-}
braces : Doc -> Doc
braces =
    surround (char '{') (char '}')


{-| Joins a List of Docs together with a given separator and surrounds it with given Docs.

    List.map string ["some", "html", "element"]
        |> surroundJoin (char '<') (char '>') (char '-')
    -- <some-html-element>

Provides a bit of extra formatting help by aligning elements (separator in front) if they cannot
all fit on the same line.

    [ "a really long string", "another really long string", "a third really long string" ]
        |> List.map string
        |> surroundJoin (char '[') (char ']') (char ',')
        |> (|+) (string "list ")
    -- list [a really long string
    --      ,another really long string
    --      ,a third really long string]

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
    -- [10,200,3000]

-}
list : List Doc -> Doc
list =
    surroundJoin (char '[') (char ']') (char ',')



-- FILLERS


{-| Takes an Int and a Doc and appends spaces to the end of the Doc until the current
column is equal to the given Int.

    fill 12 (string "how now")
        |+ string "brown cow?"
    -- how now     brown cow?

If current column is greater than the given Int, nothing is appended.

    fill 5 (string "how now")
        |+ string "brown cow?"
    -- how nowbrown cow?

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
        |> append (string "let ")

    -- let empty  : Doc
    --     nest   : Int -> Doc -> Doc
    --     linebreak : Doc

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
        |> append (string "let ")

    -- let empty  : Doc
    --     nest   : Int -> Doc -> Doc
    --     linebreak
    --            : Doc

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


{-| Different formats that a text element can take on.
-}
type TextFormat
    = WithColor ConsoleLayer Color
    | WithUnderline Formatter
    | WithBold Formatter
    | Reset


{-| Intermediate data structure between Doc and String.
-}
type NormalForm
    = Failure
    | Blank
    | Character Char NormalForm
    | TextElement Int String NormalForm
    | Linebreak Int NormalForm
    | Formatted (List TextFormat) NormalForm


type Docs
    = Nil
    | Cons Int Doc Docs


{-| Takes a Doc and converts it to a string with a column width of 80 and a ribbon width of 32
-}
toString : Doc -> Result String String
toString doc =
    display (renderPretty 0.4 80 doc)


{-| Converts a Doc into a NormalForm, which is just the intermediate data structure between Doc and
String. This function also takes a ribbon width and a page width. Where the ribbon width indicates the
max _percentage_ of non-indentation characters that should appear on a line, and the page width is the
max number of _total_ characters that can be on a single line. Can be used in combination with `display` to
convert a Doc to a String with more customization on the width than using the default `toString` function.

    let
        doc =
          string "list"
              |+ list (List.map int [10, 200, 3000])
    in
        -- ribbon width of 20
        display (renderPretty 0.25 80 doc)
        -- list [10,200,3000]


        -- ribbon width of 16
        display (renderPretty 0.2 80 doc)
        -- list [10
        --      ,200
        --      ,3000]

-}
renderPretty : Float -> Int -> Doc -> NormalForm
renderPretty =
    renderFits willFit1


renderFits :
    (Int -> Int -> Int -> NormalForm -> Bool)
    -> Float
    -> Int
    -> Doc
    -> NormalForm
renderFits doesItFit rfrac pageWidth doc =
    let
        ribbonWidth =
            round (toFloat pageWidth * rfrac)
                |> min pageWidth
                |> max 0

        best : Int -> Int -> Maybe Color -> Maybe Color -> Maybe Formatter -> Maybe Formatter -> Docs -> NormalForm
        best indent currCol foregroundColor backgroundColor boldFormatter underliner docs =
            case docs of
                Nil ->
                    Blank

                Cons n document documents ->
                    let
                        recur indent currCol docs =
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
                            Failure

                        Empty ->
                            recur indent currCol documents

                        Char char ->
                            Character char (recur indent (currCol + 1) documents)

                        Text length str ->
                            TextElement length str (recur indent (currCol + length) documents)

                        Line ->
                            Linebreak n (recur n n documents)

                        FlatAlt doc1 _ ->
                            recur indent currCol (Cons n doc1 documents)

                        Cat doc1 doc2 ->
                            recur indent currCol (Cons n doc1 (Cons n doc2 documents))

                        Nest num doc_ ->
                            recur indent currCol (Cons (num + n) doc_ documents)

                        Union doc1 doc2 ->
                            nicest
                                indent
                                currCol
                                (recur indent currCol (Cons n doc1 documents))
                                (recur indent currCol (Cons n doc2 documents))

                        Column fn ->
                            recur indent currCol (Cons n (fn currCol) documents)

                        Columns fn ->
                            recur indent currCol (Cons n (fn (Just pageWidth)) documents)

                        Nesting fn ->
                            recur indent currCol (Cons n (fn n) documents)

                        Color layer color doc ->
                            let
                                ( fgColor, bgColor ) =
                                    case layer of
                                        Background ->
                                            ( foregroundColor, Just color )

                                        Foreground ->
                                            ( Just color, backgroundColor )
                            in
                            Formatted
                                [ WithColor layer color ]
                                (best indent currCol fgColor bgColor boldFormatter underliner (Cons n doc dsRestore))

                        Bold fn doc ->
                            Formatted
                                [ WithBold fn ]
                                (best indent currCol foregroundColor backgroundColor (Just fn) underliner (Cons n doc dsRestore))

                        Underline fn doc ->
                            Formatted
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
                            Formatted formats (best indent currCol fgColor bgColor bold underliner documents)

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


willFit1 : Int -> Int -> Int -> NormalForm -> Bool
willFit1 pageWidth minNestingLvl firstLineWidth simpleDoc =
    if firstLineWidth < 0 then
        False
    else
        case simpleDoc of
            Failure ->
                False

            Blank ->
                True

            Character char sDoc ->
                willFit1 pageWidth minNestingLvl (firstLineWidth - 1) sDoc

            TextElement width content sDoc ->
                willFit1 pageWidth minNestingLvl (firstLineWidth - width) sDoc

            Linebreak width sDoc ->
                True

            Formatted _ sDoc ->
                willFit1 pageWidth minNestingLvl firstLineWidth sDoc


{-| Takes a NormalForm and converts it to a `Result String String`
-}
display : NormalForm -> Result String String
display simpleDoc =
    case simpleDoc of
        Failure ->
            Err "Failure cannot appear in NormalForm"

        Blank ->
            Ok ""

        Character char sDoc ->
            Result.map (String.cons char) (display sDoc)

        TextElement _ content sDoc ->
            Result.map (String.append content) (display sDoc)

        Linebreak indents sDoc ->
            display sDoc
                |> Result.map (String.append (String.cons '\n' (Utils.spaces indents)))

        Formatted formats sDoc ->
            List.map getFormatter formats
                |> List.foldr Result.map (display sDoc)


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
