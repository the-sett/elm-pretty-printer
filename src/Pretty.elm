module Pretty exposing
    ( Doc
    , pretty
    , empty, space, string, taggedString, char
    , append, a, join, lines, separators, softlines, words, fold
    , group, line, tightline, softline
    , align, nest, hang, indent
    , surround, parens, braces, brackets
    , setTag, updateTag
    )

{-| Wadler's Pretty printer. Use the constructor functions to build up a `Doc` and
lay it out to fit a page width using the `pretty` function.

@docs Doc


# Pretty printing documents

@docs pretty


# Building documents from string data

@docs empty, space, string, taggedString, char


# Joining documents together

@docs append, a, join, lines, separators, softlines, words, fold


# Fitting documents onto lines

@docs group, line, tightline, softline


# Indenting and alinging documents

@docs align, nest, hang, indent


# Putting things around documents

@docs surround, parens, braces, brackets


# Updating tags in documents

@docs setTag, updateTag

-}

import Basics.Extra exposing (flip)
import Internals exposing (..)


{-| The type of documents that can be pretty printed.
-}
type alias Doc t =
    Internals.Doc t



-- Document constructors -------------------------------------------------------


{-| Creates an empty document. Empties are discarded during pretty printing.

Note that the `join`, `lines`, `softlines` and `words` functions also filter
out empties. So if a list of `Docs` are joined by spaces any that are empty will
be dircarded and not result in a double space in the result. For this reason
empty is not the same as `string ""`.

    pretty 10 empty == ""

-}
empty : Doc t
empty =
    Empty


{-| Appends two documents together.
-}
append : Doc t -> Doc t -> Doc t
append doc1 doc2 =
    Concatenate (\() -> doc1) (\() -> doc2)


{-| Adds an indent of the given number of spaces to all line breakss in the document.
The first line will not be indented, only subsequent nested lines will be.
-}
nest : Int -> Doc t -> Doc t
nest depth doc =
    Nest depth (\() -> doc)


{-| Creates a document from a string.
-}
string : String -> Doc t
string val =
    Text val Nothing


{-| Creates a document from a string and tags it.

Later on the tag can be used to change how the string is displayed. For example
you might tag something as a `Keyword` then use a layout handler to show keywords
in bold, and so on.

This is intended as a way of tagging strings in a Doc for the purpose of syntax
highlighting.

-}
taggedString : String -> t -> Doc t
taggedString val tag =
    Text val (Just tag)


{-| Creates a document from a character.
-}
char : Char -> Doc t
char c =
    Text (String.fromChar c) Nothing


{-| Creates a hard line break. This creates a new line, with subsequent text
at the current indentation level.

Note that a line break can be undone, when it sits beneath a `group` operation.
If this happens and the text after the line break is printed on the same line
then the line break will be replaced by a space character.

-}
line : Doc t
line =
    Line " " ""


{-| Creates a hard line break. This creates a new line, with subsequent text
at the current indentation level.

Note that a line break can be undone, when it sits beneath a `group` operation.
If this happens and the text after the line break is printed on the same line
then this kind of line break will be replaced by an empty string; text before
the break will flow directly into text after with no space added between.

This is sometimes useful where you wan an end delimiter such as '}', ']' or ')'
to appear on a new line when the document is broken over multiple lines, but with
no space before it when the document is rendered on a single line. For example:

    long (function and args) -- Note the bracket has no space before it.

    versus

    long
        (function
            and
            args
        )

-}
tightline : Doc t
tightline =
    Line "" ""


separator : String -> String -> Doc t
separator hsep vsep =
    Line hsep vsep


{-| Tries to fit a document on a single line, replacing line breaks with single spaces
where possible to achieve this.
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


{-| Short hand notation for append.
Usefull when appending multiple parts together:

    string "Hello"
        |> a space
        |> a "World"
        |> a (char '!')
        |> a line

-}
a : Doc t -> Doc t -> Doc t
a =
    flip append


{-| Places a document inside left and right book ends.

    pretty 100 (surround (char '\') (char '/') string "hello")
      == "\hello/"

-}
surround : Doc t -> Doc t -> Doc t -> Doc t
surround left right doc =
    append (append left doc) right


{-| Creates a line break that will render to a single space if the documents it
separates can be fitted onto one line, or a line break otherwise.
-}
softline : Doc t
softline =
    group line


{-| Concatenates a list of documents together interspersed with a separator document.

Any `empty` docs in the list are dropped, so that multiple separators will not be
placed together with nothing in between them. If this behaviour is intended use
`string ""` instead of `empty`.

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


{-| Concatenate a list of documents together interspersed with lines.
Very convenient when laying out lines after another:

    lines
      [ string "Heading"
      , words [string "First", string "paragraph"]
      ...
      ]

    ==

    string "Heading"
      |> a line
      |> a (string "First")
      |> a space
      |> a (string "paragraph")
      ...

Any empty docs in the list are dropped, so multiple lines will not be inserted
around any empties.

See also `words`.

-}
lines : List (Doc t) -> Doc t
lines =
    join line


{-| Concatenates a list of documents together interspersed with lines and
separator strings. This is convenient when laying out lines where each line
begins with a separator, for example if commas are to go on the start rather
than the ends of lines:

    separators ", "
      [ string "Heading"
      , words [string "First", string "paragraph"]
      ...
      ]

    ==

    string "Heading"
      |> a line
      |> a (string ", ")
      |> a (string "First")
      |> a space
      |> a (string "paragraph")
      ...

The separator string is kept with the line break. If lines built in this way
are placed into a `group`, then the inline version of the group will include
the separators. The broken version of the group will have the separators after
any indentation but otherwise at the start of each line.

    separators ", "
      [ string "One"
      , string "Two"
      ...
      ]
      |> group

Can render as:

      One, Two, ...

Or

      One
      , Two
      , ...

Any empty docs in the list are dropped, so multiple lines will not be inserted
around any empties.

See also `words`.

-}
separators : String -> List (Doc t) -> Doc t
separators sep =
    Line sep sep |> join


{-| Like `lines` but uses `softline` instead.

Any empty docs in the list are dropped, so multiple lines will not be inserted
around any empties.

-}
softlines : List (Doc t) -> Doc t
softlines =
    join softline


{-| Concatenate a list of documents together interspersed with spaces.
Very convenient when laying out words after another.

See also `lines`.

Any empty docs in the list are dropped, so multiple spaces will not be inserted
around any empties.

-}
words : List (Doc t) -> Doc t
words =
    join space


{-| Fold a list of documents from left to right using a given function.

    fold f == List.foldl f empty

-}
fold : (a -> Doc t -> Doc t) -> List a -> Doc t
fold f =
    List.foldl f empty


{-| Creates a document consisting of a single space.
-}
space : Doc t
space =
    char ' '


{-| Wraps a document in parnethesese
-}
parens : Doc t -> Doc t
parens doc =
    surround (char '(') (char ')') doc


{-| Wraps a document in braces.
-}
braces : Doc t -> Doc t
braces doc =
    surround (char '{') (char '}') doc


{-| Wraps a document in brackets.
-}
brackets : Doc t -> Doc t
brackets =
    surround (char '[') (char ']')


{-| Adds an indent of the current column position to all line breaks in the document.
The first line will not be indented, only subsequent nested lines will be.
-}
align : Doc t -> Doc t
align doc =
    column
        (\currentColumn ->
            nesting
                (\indentLvl -> nest (currentColumn - indentLvl) doc)
        )


{-| Adds an indent of the current column position to all line breaks in the document and
a further indent of the specified number of columns.
The first line will not be indented, only subsequent nested lines will be.
-}
hang : Int -> Doc t -> Doc t
hang spaces doc =
    align (nest spaces doc)


{-| Indents a whole document by a given number of spaces.
-}
indent : Int -> Doc t -> Doc t
indent spaces doc =
    append (string (copy spaces " ")) doc
        |> hang spaces


{-| Set the tag of every string in the document.
-}
setTag : t -> Doc t -> Doc t
setTag tag =
    updateTag (\_ _ -> Just tag)


{-| Conditionally update the tag of every string in the document.

The update function is called with the string and its current tag and should
return a new tag for the string or `Nothing` to remove the current tag.
-}
updateTag : (String -> Maybe t -> Maybe t) -> Doc t -> Doc t
updateTag =
    Internals.updateTag



-- Pretty printing -------------------------------------------------------------


{-| Pretty prints a document trying to fit it as best as possible to the specified
column width of the page.
-}
pretty : Int -> Doc t -> String
pretty w doc =
    layout (best w 0 doc)
