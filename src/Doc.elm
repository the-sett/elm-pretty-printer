module Doc
    exposing
        ( (|+)
        , Doc
        , NormalForm(..)
        , align
        , angles
        , append
        , bool
        , braces
        , brackets
        , char
        , column
        , concat
        , display
        , dquotes
        , empty
        , fill
        , fillBreak
        , float
        , group
        , hang
        , hardline
        , indent
        , int
        , join
        , line
        , linebreak
        , nest
        , parens
        , renderPretty
        , softbreak
        , softline
        , space
        , squotes
        , string
        , surround
        , surroundJoin
        , toString
        )

{-| Functions for combining, formatting, and printing text. Because this library uses the terminal, the content you wish to print
must be passed as the first argument of `Debug.log` (or `console.log` via [Ports][interop]) in order to render as expected.
**The comments underneath each example are what will be rendered to the terminal after the Doc is converted to a String**.

[interop]: https://guide.elm-lang.org/interop/javascript.html


## API Reference

  - [Basics](#basics)
  - [Combining Docs](#combining-docs)
  - [Lines](#lines)
  - [Bracketing](#bracketing)
  - [Alignment](#alignment)
  - [Fillers](#fillers)
  - [Colors](#colors)
  - [Formatting](#formatting)
  - [Rendering](#rendering)


## Basics

@docs Doc, string, char, int, float, bool, space, empty


## Combining Docs

@docs append, (|+), join, concat


## Lines

@docs group, line, linebreak, hardline, softline, softbreak


## Bracketing

@docs surround, squotes, dquotes, parens, angles, brackets, braces, surroundJoin


## Alignment

@docs align, nest, hang, indent, column


## Fillers

@docs fill, fillBreak


## Rendering

@docs NormalForm, renderPretty, toString, display

-}

import Basics
import Console as Ansi
import Utils


{-| Data structure that wraps the text you eventually wish to print. Docs can be conveniently
combined, formatted, and aligned in many ways.
-}
type Doc
    = Empty
    | Character Char
    | Txt Int String
    | Break
    | FlatAlt Doc Doc
    | Cat Doc Doc
    | Nest Int Doc
    | Union Doc Doc
    | Column (Int -> Doc)
    | Columns (Maybe Int -> Doc)
    | Nesting (Int -> Doc)


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
        |> join space
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


{-| Tries to put all elements of a Doc on a single line if it will fit
the width of the page. If everything cannot fit on one line, then
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


{-| An empty Doc element. The equivalent of `""` when converted to a `String`
-}
empty : Doc
empty =
    Empty


{-| Convenience function for representing a space.

    space =
        char ' '

-}
space : Doc
space =
    Character ' '


{-| Doc that advances to the next line when combined with other Doc elements.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join line
    -- how
    -- now
    -- brown
    -- cow?

When `group` is called on a Doc containing `line` elements, the `line`s are replaced with spaces.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join line
        |> group
    -- how now brown cow?

-}
line : Doc
line =
    FlatAlt Break space


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
    FlatAlt Break Empty


{-| Works the same way as `line` and `linebreak`, except when `group` is called on a Doc with
a `hardline`, it is not replaced with anything.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join hardline
        |> group
    -- how
    -- now
    -- brown
    -- cow?

-}
hardline : Doc
hardline =
    Break


{-| Doc that advances a single space when combined with other Doc elements, but only if the
line has room.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join softline
    -- how now brown cow?

If the elements cannot fit on the same line, then it advances to the next line. Since the basic
`string` function is not equipped for proper lookahead, this Doc should be used if there is a chance
of overflow and you'd like to keep consistent column lengths.

    -- NOT GREAT

    string "this string may or may not fit on one line"
    -- this string may or may not fit on one line

    -- MUCH BETTER

    "this string may or may not fit on one line"
        |> String.words
        |> List.map string
        |> join softline
    -- this string may or may not fit
    -- on one line

-}
softline : Doc
softline =
    group line


{-| Works the same way as `softline`, except it separates with `empty` if elements can
fit on the same line.

    ["how", "now", "brown", "cow?"]
        |> List.map string
        |> join softbreak
    -- hownowbrowncow?

-}
softbreak : Doc
softbreak =
    group linebreak


{-| Creates a Doc from a `String`.

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
                Txt (String.length s) s


{-| Creates a Doc from a `Char`.

    char '!'
    -- !

-}
char : Char -> Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Character input


{-| Create a Doc from an `Int`.

    int 3
    -- 3

-}
int : Int -> Doc
int =
    string << Basics.toString


{-| Create a Doc from a `Float`.

    float 12.3456
    -- 12.3456

-}
float : Float -> Doc
float =
    string << Basics.toString


{-| Create a Doc from a `Bool`.

    bool True
    -- True

-}
bool : Bool -> Doc
bool =
    string << Basics.toString


{-| Creates a Doc from a function where the first argument is the current column of the Doc.
Since `hello` will take up the first 5 columns when rendered, the following snippet will
indent `from afar...` 5 spaces.

    string "hello"
        |+ column (\col -> indent col (string "from afar..."))
    -- hello     from afar...

This is useful if you need to know the current column in order to do some sort of combination.

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


{-| Increases the number of spaces that all nested lines are indented.

    string "pretty printing in Elm"
        |+ line
        |+ string "can be a lot of fun?"
        |> nest 2
        |> append (string "Did you know that ")
    -- Did you know that pretty printing in Elm
    --   can be a lot of fun?

-}
nest : Int -> Doc -> Doc
nest =
    Nest



-- ALIGNMENT


{-| Sets the indentation of all *nested lines*, or lines that are placed underneath another line, equal to the current column.
This vertically aligns the Doc so that it moves as a single column.

    string "old"
        |+ line
        |+ string "friend"
        |> align
        |> append (string "hello ")
    -- hello old
    --       friend

In the above example, `string "hello "` has a current column of 6, so when it is prepended to the beginning of our aligned doc,
the nested line is indented by 6 spaces.

-}
align : Doc -> Doc
align doc =
    column
        (\currentColumn ->
            nesting
                (\indentLvl -> nest (currentColumn - indentLvl) doc)
        )


{-| Works similar to `nest`, but this function also aligns the nested lines with the top line.

    string "pretty printing in Elm"
        |+ line
        |+ string "can be a lot of fun?"
        |> hang 2
        |> append (string "Did you know that ")
    -- Did you know that pretty printing in Elm
    --                     can be a lot of fun?

-}
hang : Int -> Doc -> Doc
hang spaces doc =
    align (nest spaces doc)


{-| Indents the entire Doc by a given number of spaces.

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


{-| Joins a List of Docs together with a given separator, then surrounds the result with the first two arguments.

    List.map string ["some", "html", "element"]
        |> surroundJoin (char '<') (char '>') (char '-')
    -- <some-html-element>

Provides a bit of extra formatting help by aligning elements (separator in front) if they cannot all fit on the
same line.

    [ "a really long string", "another really long string", "a third really long string" ]
        |> List.map string
        |> List.map (surround space space)
        |> surroundJoin (char '[') (char ']') (char ',')
        |> append (string "list ")
    -- list [ a really long string
    --      , another really long string
    --      , a third really long string ]

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



-- FILLERS


{-| Appends spaces to the end of the Doc until the current column is equal to the given `Int`.

    fill 12 (string "how now")
        |+ string "brown cow?"
    -- how now     brown cow?

Since `how now` takes up the first 7 columns in the above example and we passed 12 to `fill`,
we can expect there to be 5 spaces between `how now` and `brown cow`. If the current column
is greater than the given `Int`, nothing is appended.

    fill 5 (string "how now")
        |+ string "brown cow?"
    -- how nowbrown cow?

When used with `align`, this function can be particularly helpful for pretty printing different types of data.

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
then a linebreak is inserted and the indentation of all nested lines is increased to given `Int`.

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



-- RENDERERS


{-| Intermediate data structure between `Doc` and `String`. Can be useful during situations where
greater control during the rendering process is preferred.
-}
type NormalForm
    = Blank
    | Char Char NormalForm
    | Text Int String NormalForm
    | Line Int NormalForm


type Docs
    = Nil
    | Cons Int Doc Docs


{-| Converts a Doc to a `String` with a column width of 80 and a ribbon width of 32.
This function is useful if you don't care about custom widths and just want to print your Doc.
-}
toString : Doc -> String
toString doc =
    display (renderPretty 0.4 80 doc)


{-| Convert a `Doc` into `NormalForm` by specifying both the ribbon width and the page width.
The **ribbon width** indicates the max *percentage* of non-indentation characters that should
appear on a line, and the **page width** is the max number of *total* characters that can be on a
single line. Can be used in combination with `display` to convert a `Doc` to a `String` with more
customization on the width than using the default `toString` function.

    let
        doc =
          [10, 200, 3000]
              |> List.map int
              |> surroundJoin (char '[') (char ']') (char ',')
              |> append (string "list ")
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
    renderFits willFit


renderFits :
    (Int -> Int -> Int -> NormalForm -> Bool)
    -> Float
    -> Int
    -> Doc
    -> NormalForm
renderFits doesItFit ribbonPct pageWidth doc =
    let
        ribbonWidth =
            round (toFloat pageWidth * ribbonPct)
                |> min pageWidth
                |> max 0

        best : Int -> Int -> Docs -> NormalForm
        best indent currCol docs =
            case docs of
                Nil ->
                    Blank

                Cons n document documents ->
                    case document of
                        Empty ->
                            best indent currCol documents

                        Character char ->
                            Char char (best indent (currCol + 1) documents)

                        Txt length str ->
                            Text length str (best indent (currCol + length) documents)

                        Break ->
                            Line n (best n n documents)

                        FlatAlt doc1 _ ->
                            best indent currCol (Cons n doc1 documents)

                        Cat doc1 doc2 ->
                            best indent currCol (Cons n doc1 (Cons n doc2 documents))

                        Nest num doc_ ->
                            best indent currCol (Cons (num + n) doc_ documents)

                        Union doc1 doc2 ->
                            nicest
                                indent
                                currCol
                                (best indent currCol (Cons n doc1 documents))
                                (best indent currCol (Cons n doc2 documents))

                        Column fn ->
                            best indent currCol (Cons n (fn currCol) documents)

                        Columns fn ->
                            best indent currCol (Cons n (fn (Just pageWidth)) documents)

                        Nesting fn ->
                            best indent currCol (Cons n (fn n) documents)

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
        best 0 0 (Cons 0 doc Nil)


willFit : Int -> Int -> Int -> NormalForm -> Bool
willFit pageWidth minNestingLvl remainingLineWidth simpleDoc =
    if remainingLineWidth < 0 then
        False
    else
        case simpleDoc of
            Blank ->
                True

            Char char sDoc ->
                willFit pageWidth minNestingLvl (remainingLineWidth - 1) sDoc

            Text width content sDoc ->
                willFit pageWidth minNestingLvl (remainingLineWidth - width) sDoc

            Line width sDoc ->
                True


{-| Intermediate step for converting `NormalForm` to a `String`. Where `toString` converts a `Doc`
all the way to a `String`, this function allows for greater control during the rendering process.
-}
display : NormalForm -> String
display simpleDoc =
    case simpleDoc of
        Blank ->
            ""

        Char char sDoc ->
            String.cons char (display sDoc)

        Text _ content sDoc ->
            String.append content (display sDoc)

        Line indents sDoc ->
            display sDoc
                |> String.append (String.cons '\n' (Utils.spaces indents))



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
                    Txt (String.length chunk) chunk
                        |+ doc
    in
        Utils.splitOnWhitespace str
            |> List.foldr parse Empty
