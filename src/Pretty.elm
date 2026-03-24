module Pretty exposing
    ( Doc
    , pretty, prettyWithRibbon
    , empty, space, string, taggedString, char
    , append, a, join, lines, separators, softlines, words, fold
    , group, line, tightline, softline, flatAlt, ifFlat, ifBreak
    , align, nest, hang, indent
    , surround, parens, braces, brackets
    , setTag, updateTag
    )

{-| A Wadler/Leijen-style pretty printer. Build documents with combinators,
then render them to fit a page width.

@docs Doc


# Rendering

@docs pretty, prettyWithRibbon


# Primitives

@docs empty, space, string, taggedString, char


# Combining Documents

@docs append, a, join, lines, separators, softlines, words, fold


# Layout Control

@docs group, line, tightline, softline, flatAlt, ifFlat, ifBreak


# Indentation

@docs align, nest, hang, indent


# Delimiters

@docs surround, parens, braces, brackets


# Tagging

@docs setTag, updateTag

-}

import Internals exposing (..)


{-| A document represents possible layouts. The `t` parameter allows tagging
strings for syntax highlighting (see `Pretty.Renderer`).

    doc = group (string "hello" |> a line |> a (string "world"))

    pretty 20 doc  -- "hello world"
    pretty 5 doc   -- "hello\nworld"

-}
type alias Doc t =
    Internals.Doc t



-- Document constructors -------------------------------------------------------


{-| An empty document that produces no output.

Unlike `string ""`, empties are filtered out by `join`, `lines`, `words`, etc.
This prevents double separators when some items are conditionally absent:

    words [ string "a", empty, string "b" ] -- "a b", not "a  b"

    -- Conditional content:
    words [ string "hello", if showName then string name else empty ]

-}
empty : Doc t
empty =
    Empty


{-| Concatenates two documents. The first argument comes before the second.

    append (string "hello") (string "world") -- "helloworld"

Usually it's more convenient to use `a` for pipelines, or `join`/`words`/`lines`
for lists.

-}
append : Doc t -> Doc t -> Doc t
append doc1 doc2 =
    Concatenate doc1 doc2


{-| Adds indentation to all line breaks inside a document. The first line is
NOT indented—only lines after breaks.

    string "{" |> a (nest 4 (line |> a (string "body"))) |> a line |> a (string "}")
    -- "{\n    body\n}"

Common mistake—nest around content without a line break inside:

    nest 4 (string "no effect")  -- just "no effect", no indentation

The line break must be INSIDE the nest for indentation to apply.

-}
nest : Int -> Doc t -> Doc t
nest depth doc =
    Nest depth doc


{-| Creates a document from a string. This is the primary way to add text content.

    pretty 80 (string "hello") == "hello"

    string "let" |> a space |> a (string "x = 1")  -- "let x = 1"

-}
string : String -> Doc t
string val =
    Text val Nothing


{-| Creates a tagged string for syntax highlighting. Use with `Pretty.Renderer`
to produce HTML or other formatted output.

    type Tag = Keyword | Literal

    taggedString "let" Keyword |> a space |> a (taggedString "42" Literal)

-}
taggedString : String -> t -> Doc t
taggedString val tag =
    Text val (Just tag)


{-| Creates a document from a single character.

    char '(' |> a (string "x") |> a (char ')')  -- "(x)"

-}
char : Char -> Doc t
char c =
    Text (String.fromChar c) Nothing


{-| A line break that becomes a space when flattened by `group`.

    -- Without group: always breaks
    string "a" |> a line |> a (string "b")  -- "a\nb"

    -- With group: breaks only if needed
    group (string "a" |> a line |> a (string "b"))
    -- Wide: "a b"    (line becomes space)
    -- Narrow: "a\nb" (line breaks)

Use `tightline` if you want no space when flattened.

-}
line : Doc t
line =
    Line " " ""


{-| A line break that becomes nothing when flattened by `group`.

Use this before closing delimiters to avoid unwanted spaces:

    -- Array formatting
    group (
        char '[' |> a tightline
            |> a (string "items")
            |> a tightline |> a (char ']')
    )
    -- Wide:   "[items]"  (no spaces)
    -- Narrow: "[\nitems\n]"

Compare with `line` which would produce "[ items ]" when flat.

-}
tightline : Doc t
tightline =
    Line "" ""


separator : String -> String -> Doc t
separator hsep vsep =
    Line hsep vsep


{-| Try to lay out a document on one line; if it doesn't fit, break it.

This is the key combinator for flexible layouts. It creates a choice between
the flattened version (line breaks become spaces) and the original.

    items = string "one" |> a line |> a (string "two") |> a line |> a (string "three")

    pretty 20 (group items) == "one two three"  -- fits
    pretty 10 (group items) == "one\ntwo\nthree" -- doesn't fit, breaks

Nested groups break outside-in: the outer group breaks first, inner groups
may still fit on their lines.

-}
group : Doc t -> Doc t
group doc =
    Union (flatten doc) doc


{-| Allows a document to be created from the current column position.
-}
column : (Int -> Doc t) -> Doc t
column =
    Column


{-| Allows a document to be created from the current indentation degree.
-}
nesting : (Int -> Doc t) -> Doc t
nesting =
    Nesting



-- Document helper functions ---------------------------------------------------


{-| Flip of `append` for use in pipelines. Read as "and":

    string "if"
        |> a space
        |> a (string "x > 0")
        |> a space
        |> a (string "then")
    -- "if x > 0 then"

-}
a : Doc t -> Doc t -> Doc t
a l r =
    append r l


{-| Wraps a document with left and right delimiters.

    surround (string "<<") (string ">>") (string "content")  -- "<<content>>"

See also `parens`, `braces`, `brackets` for common cases.

-}
surround : Doc t -> Doc t -> Doc t -> Doc t
surround left right doc =
    append (append left doc) right


{-| A line break that tries to be a space. Equivalent to `group line`.

Unlike `line` inside a `group`, each `softline` makes its own independent
break decision:

    string "a" |> a softline |> a (string "b") |> a softline |> a (string "c")
    -- Might render as: "a b\nc" if "a b" fits but "a b c" doesn't

-}
softline : Doc t
softline =
    group line


{-| Choose between two documents based on flat vs broken mode.

    group (flatAlt (string "flat") (string "broken"))
    -- Wide: "flat"  |  Narrow: "broken"

Without a surrounding `group`, always uses the broken (second) argument.

Unlike `line`/`tightline` which vary a single token, `flatAlt` allows entirely
different document structures in each mode:

    -- Different paren spacing: "(x)" when flat, "( x\n)" when broken
    group (flatAlt
        (char '(' |> a x |> a (char ')'))
        (string "( " |> a x |> a line |> a (char ')'))
    )

-}
flatAlt : Doc t -> Doc t -> Doc t
flatAlt flatDoc brokenDoc =
    FlatAlt flatDoc brokenDoc


{-| Includes a document only in flat mode; becomes empty when broken.

    group (string "items" |> a (ifFlat (string " (inline)")))
    -- Wide: "items (inline)"
    -- Narrow: "items"

Equivalent to `flatAlt doc empty`.

-}
ifFlat : Doc t -> Doc t
ifFlat doc =
    FlatAlt doc Empty


{-| Includes a document only when broken; becomes empty in flat mode.

    -- Trailing comma only in multi-line mode
    group (lines [ string "a" |> a (ifBreak (char ',')), string "b" ])
    -- Wide: "a b"
    -- Narrow: "a,\nb"

Equivalent to `flatAlt empty doc`.

-}
ifBreak : Doc t -> Doc t
ifBreak doc =
    FlatAlt Empty doc


{-| Joins documents with a separator between each pair. Empty documents are
filtered out to prevent double separators.

    join (string ", ") [ string "a", string "b", string "c" ]  -- "a, b, c"

    join (string ", ") [ string "a", empty, string "c" ]  -- "a, c" (not "a, , c")

See also `words` (join with space), `lines` (join with line), `separators`.

-}
join : Doc t -> List (Doc t) -> Doc t
join sep docs =
    case docs of
        [] ->
            empty

        Empty :: ds ->
            join sep ds

        d :: ds ->
            let
                step x rest =
                    case x of
                        Empty ->
                            rest

                        doc ->
                            append sep (append doc rest)

                spersed =
                    List.foldr step empty ds
            in
            append d spersed


{-| Joins documents with `line` between each. Equivalent to `join line`.

    lines [ string "one", string "two", string "three" ]
    -- "one\ntwo\nthree"

Wrap in `group` for flexible breaking:

    group (lines [ string "a", string "b" ])
    -- Wide: "a b"  |  Narrow: "a\nb"

-}
lines : List (Doc t) -> Doc t
lines =
    join line


{-| Joins documents with a separator that appears both when flat and when broken.
Useful for leading-comma or leading-pipe style:

    separators ", " [ string "a", string "b", string "c" ] |> group
    -- Wide:   "a, b, c"
    -- Narrow: "a\n, b\n, c"

    separators "| " [ string "A", string "B" ] |> group
    -- Wide:   "A| B"
    -- Narrow: "A\n| B"

-}
separators : String -> List (Doc t) -> Doc t
separators sep =
    Line sep sep |> join


{-| Joins documents with `softline` (each break decides independently).

    softlines [ string "one", string "two", string "three" ]
    -- Each softline independently becomes space or newline based on fit

Unlike `group (lines [...])` where all breaks happen together, here each
break point makes its own decision.

-}
softlines : List (Doc t) -> Doc t
softlines =
    join softline


{-| Joins documents with spaces. Equivalent to `join space`.

    words [ string "hello", string "world" ]  -- "hello world"

Empty docs are filtered out, preventing double spaces.

-}
words : List (Doc t) -> Doc t
words =
    join space


{-| Left fold over a list, starting from `empty`.

    fold (\item acc -> acc |> a (string item)) [ "a", "b", "c" ]
    -- "abc"

-}
fold : (a -> Doc t -> Doc t) -> List a -> Doc t
fold f =
    List.foldl f empty


{-| A single space character. Equivalent to `char ' '`.

    string "hello" |> a space |> a (string "world")  -- "hello world"

-}
space : Doc t
space =
    char ' '


{-| Wraps a document in parentheses.

    parens (string "x + y")  -- "(x + y)"

-}
parens : Doc t -> Doc t
parens doc =
    surround (char '(') (char ')') doc


{-| Wraps a document in curly braces.

    braces (string "x: 1")  -- "{x: 1}"

-}
braces : Doc t -> Doc t
braces doc =
    surround (char '{') (char '}') doc


{-| Wraps a document in square brackets.

    brackets (string "1, 2, 3")  -- "[1, 2, 3]"

-}
brackets : Doc t -> Doc t
brackets =
    surround (char '[') (char ']')


{-| Aligns subsequent lines to the current column position. Unlike `nest` which
adds a fixed indent, `align` indents relative to where you are now.

    string "let " |> a (align (lines [ string "x = 1", string "y = 2" ]))
    -- "let x = 1\n    y = 2"
    --     ^--- aligned to column 4 (after "let ")

Useful for hanging indentation that adapts to context.

-}
align : Doc t -> Doc t
align doc =
    column
        (\currentColumn ->
            nesting
                (\indentLvl -> nest (currentColumn - indentLvl) doc)
        )


{-| Combines `align` with additional nesting. Equivalent to `align (nest n doc)`.

    string "fn(" |> a (hang 2 (lines [ string "arg1", string "arg2" ])) |> a (string ")")
    -- "fn(arg1\n     arg2)"
    --    ^--- aligned to column 3, plus 2 more = column 5

-}
hang : Int -> Doc t -> Doc t
hang spaces doc =
    align (nest spaces doc)


{-| Indents the first line AND all subsequent lines by the given amount.

    indent 4 (lines [ string "one", string "two" ])
    -- "    one\n    two"

Compare with `nest` which only affects lines after breaks, not the first line.

-}
indent : Int -> Doc t -> Doc t
indent spaces doc =
    append (string (String.repeat spaces " ")) doc
        |> hang spaces


{-| Tags all strings in a document for syntax highlighting.

    type Tag = Error

    setTag Error (words [ string "undefined", string "variable" ])
    -- Both strings will be tagged as Error

-}
setTag : t -> Doc t -> Doc t
setTag tag =
    updateTag (\_ _ -> Just tag)


{-| Conditionally update tags based on string content and current tag.

    -- Tag numbers as literals
    updateTag (\s _ ->
        if String.all Char.isDigit s then Just Literal else Nothing
    ) doc

    -- Remove all tags
    updateTag (\_ _ -> Nothing) doc

-}
updateTag : (String -> Maybe t -> Maybe t) -> Doc t -> Doc t
updateTag =
    Internals.updateTag



-- Pretty printing -------------------------------------------------------------


{-| Renders a document to a string, fitting it within the given column width.

The algorithm makes greedy left-to-right decisions: when a `group` fits on the
remaining line, it stays flat; otherwise it breaks.

    doc = group (string "aaa" |> a line |> a (string "bbb"))

    pretty 10 doc == "aaa bbb"   -- fits in 10 columns
    pretty 5 doc  == "aaa\nbbb"  -- too wide, breaks

-}
pretty : Int -> Doc t -> String
pretty w doc =
    layout (best w 0 doc)


{-| Renders a document with both page width and ribbon width constraints.

The ribbon width limits non-indentation characters per line. At indentation
level `i`, the effective width is `min(pageWidth, i + ribbonWidth)`.

    prettyWithRibbon { pageWidth = 80, ribbonWidth = 40 } doc

This keeps deeply nested code compact. At indent 50 with ribbon 40, lines
break at column 80 (not 90), preventing rightward drift.

If `ribbonWidth >= pageWidth`, behaves identically to `pretty pageWidth`.

-}
prettyWithRibbon : { pageWidth : Int, ribbonWidth : Int } -> Doc t -> String
prettyWithRibbon { pageWidth, ribbonWidth } doc =
    let
        effectiveRibbon =
            min ribbonWidth pageWidth
    in
    layout (bestWithRibbon pageWidth effectiveRibbon 0 doc)
