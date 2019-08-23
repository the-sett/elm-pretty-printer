module Pretty exposing
    ( Doc
    , empty, space, string, char
    , append, a, join, lines, softlines, words, fold
    , group, line, softline
    , align, nest, hang, indent
    , surround, parens, braces, brackets
    , pretty
    )

{-| Pretty printer.

@docs Doc


# Building documents from string data

@docs empty, space, string, char


# Joining documents together

@docs append, a, join, lines, softlines, words, fold


# Fitting documents onto lines

@docs group, line, softline


# Indenting and alinging documents

@docs align, nest, hang, indent


# Putting things around documents

@docs surround, parens, braces, brackets


# Pretty printing documents

@docs pretty

-}

import Basics.Extra exposing (flip)


{-| The type of documents that can be pretty printed.
-}
type Doc
    = Empty
    | Concatenate Doc Doc
    | Nest Int Doc
    | Text String
    | Line
    | Union Doc Doc
    | Nesting (Int -> Doc)
    | Column (Int -> Doc)


type Normal
    = NNil
    | NText String (() -> Normal)
    | NLine Int (() -> Normal)



-- Document constructors -------------------------------------------------------


{-| Creates an empty document. Empties are discarded during pretty printing.

Note that the `join`, `lines`, `softlines` and `words` functions also filter
out empties. So if a list of `Docs` are joined by spaces any that are empty will
be dircarded and not result in a double space in the result. For this reason
empty is not the same as `string ""`.

    pretty 10 empty == ""

-}
empty : Doc
empty =
    Empty


{-| Appends two documents together.
-}
append : Doc -> Doc -> Doc
append =
    Concatenate


{-| Adds an indent of the given number of spaces to all line breakss in the document.
The first line will not be indented, only subsequent nested lines will be.
-}
nest : Int -> Doc -> Doc
nest =
    Nest


{-| Creates a document from a string.
-}
string : String -> Doc
string =
    Text


{-| Creates a document from a character.
-}
char : Char -> Doc
char c =
    Text <| String.fromChar c


{-| Creates a hard line break. This always creates a new line, with subsequent text
at the current indentation level.
-}
line : Doc
line =
    Line


{-| Tries to fit a document on a single line, replacing line breaks with single spaces
where possible to achieve this.
-}
group : Doc -> Doc
group doc =
    Union (flatten doc) doc


{-| Allows a document to be created from the current column position.
-}
column : (Int -> Doc) -> Doc
column =
    Column


{-| Allows a document to be created from the current indentation degree.
-}
nesting : (Int -> Doc) -> Doc
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
a : Doc -> Doc -> Doc
a =
    flip append


{-| Places a document inside left and right book ends.

    pretty 100 (surround (char '\') (char '/') string "hello")
      == "\hello/"

-}
surround : Doc -> Doc -> Doc -> Doc
surround left right doc =
    append (append left doc) right


{-| Creates a line break that will render to a single space if the documents it
separtes can be fitted onto one line, or a line break otherwise.
-}
softline : Doc
softline =
    group Line


{-| Concatenates a list of documents together interspersed with a separator document.

Any `empty` docs in the list are dropped, so that multiple separators will not be
placed together with nothing in between them. If this behaviour is intended use
`string ""` instead of `empty`.

-}
join : Doc -> List Doc -> Doc
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
      , empty
      , words [string "First", string "paragraph"]
      ...
      ]

    ==

    string "Heading"
      |> a line
      |> a line
      |> a (string "First")
      |> a space
      |> a (string "paragraph")
      ...

Any empty docs in the list are dropped, so multiple lines will not be inserted
around any empties.

See also `words`.

-}
lines : List Doc -> Doc
lines =
    join line


{-| Like `lines` but uses `softline` instead.

Any empty docs in the list are dropped, so multiple lines will not be inserted
around any empties.

-}
softlines : List Doc -> Doc
softlines =
    join softline


{-| Concatenate a list of documents together interspersed with spaces.
Very convenient when laying out words after another.

See also `lines`.

Any empty docs in the list are dropped, so multiple spaces will not be inserted
around any empties.

-}
words : List Doc -> Doc
words =
    join space


{-| Fold a list of documents from left to right using a given function.

    fold f == List.foldl f empty

-}
fold : (a -> Doc -> Doc) -> List a -> Doc
fold f =
    List.foldl f empty


{-| Creates a document consisting of a single space.
-}
space : Doc
space =
    char ' '


{-| Wraps a document in parnethesese
-}
parens : Doc -> Doc
parens doc =
    surround (char '(') (char ')') doc


{-| Wraps a document in braces.
-}
braces : Doc -> Doc
braces doc =
    surround (char '{') (char '}') doc


{-| Wraps a document in brackets.
-}
brackets : Doc -> Doc
brackets =
    surround (char '[') (char ']')


{-| Adds an indent of the current column position to all line breaks in the document.
The first line will not be indented, only subsequent nested lines will be.
-}
align : Doc -> Doc
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
hang : Int -> Doc -> Doc
hang spaces doc =
    align (nest spaces doc)


{-| Indents a whole document by a given number of spaces.
-}
indent : Int -> Doc -> Doc
indent spaces doc =
    append (string (copy spaces " ")) doc
        |> hang spaces



-- Pretty printing -------------------------------------------------------------


{-| Pretty prints a document trying to fit it as best as possible to the specified
column width of the page.
-}
pretty : Int -> Doc -> String
pretty w doc =
    layout (best w 0 doc)



-- Internals -------------------------------------------------------------------


flatten : Doc -> Doc
flatten doc =
    case doc of
        Concatenate doc1 doc2 ->
            Concatenate (flatten doc1) (flatten doc2)

        Nest i doc1 ->
            Nest i (flatten doc1)

        Union doc1 doc2 ->
            flatten doc1

        Line ->
            Text " "

        x ->
            x


layout : Normal -> String
layout normal =
    let
        layoutInner : Normal -> List String -> List String
        layoutInner normal2 acc =
            case normal2 of
                NNil ->
                    acc

                NText text innerNormal ->
                    layoutInner (innerNormal ()) (text :: acc)

                NLine i innerNormal ->
                    layoutInner (innerNormal ()) (("\n" ++ copy i " ") :: acc)
    in
    layoutInner normal []
        |> List.reverse
        |> String.concat


copy : Int -> String -> String
copy i s =
    if i == 0 then
        ""

    else
        s ++ copy (i - 1) s


best : Int -> Int -> Doc -> Normal
best width startCol x =
    let
        be : Int -> Int -> List ( Int, Doc ) -> Normal
        be w k docs =
            case docs of
                [] ->
                    NNil

                ( i, Empty ) :: ds ->
                    be w k ds

                ( i, Concatenate doc doc2 ) :: ds ->
                    be w k (( i, doc ) :: ( i, doc2 ) :: ds)

                ( i, Nest j doc ) :: ds ->
                    be w k (( i + j, doc ) :: ds)

                ( i, Text text ) :: ds ->
                    NText text (\() -> be w (k + String.length text) ds)

                ( i, Line ) :: ds ->
                    NLine i (\() -> be w i ds)

                ( i, Union doc doc2 ) :: ds ->
                    better w
                        k
                        (be w k (( i, doc ) :: ds))
                        (\() -> be w k (( i, doc2 ) :: ds))

                ( i, Nesting fn ) :: ds ->
                    be w k (( i, fn i ) :: ds)

                ( i, Column fn ) :: ds ->
                    be w k (( i, fn k ) :: ds)
    in
    be width startCol [ ( 0, x ) ]


better : Int -> Int -> Normal -> (() -> Normal) -> Normal
better w k doc doc2Fn =
    if fits (w - k) doc then
        doc

    else
        doc2Fn ()


fits : Int -> Normal -> Bool
fits w normal =
    if w < 0 then
        False

    else
        case normal of
            NNil ->
                True

            NText text innerNormal ->
                fits (w - String.length text) (innerNormal ())

            NLine int innerNormal ->
                True
