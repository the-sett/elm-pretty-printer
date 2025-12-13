module PropertyTests exposing (suite)

{-| Property-based tests for elm-pretty-printer using elm-explorations/test fuzz testing.
-}

import Expect
import Fuzz exposing (Fuzzer)
import Pretty exposing (Doc)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "Pretty Printer Property Tests"
        [ emptyDocumentProperties
        , stringCharProperties
        , appendProperties
        , lineBreakProperties
        , nestProperties
        , groupProperties
        , joinProperties
        , surroundProperties
        , widthBehaviorProperties
        , indentProperties
        , alignProperties
        , foldProperties
        , spaceProperties
        , tagProperties
        , advancedGroupProperties
        , advancedNestProperties
        , determinismProperties
        , edgeCaseProperties
        ]



-- FUZZERS


{-| Fuzzer for reasonable page widths (10 to 200 characters)
-}
widthFuzzer : Fuzzer Int
widthFuzzer =
    Fuzz.intRange 10 200


{-| Fuzzer for small page widths to force line breaks
-}
smallWidthFuzzer : Fuzzer Int
smallWidthFuzzer =
    Fuzz.intRange 1 20


{-| Fuzzer for large page widths to avoid line breaks
-}
largeWidthFuzzer : Fuzzer Int
largeWidthFuzzer =
    Fuzz.intRange 500 2000


{-| Fuzzer for non-empty single-line strings (no newlines)
-}
singleLineStringFuzzer : Fuzzer String
singleLineStringFuzzer =
    Fuzz.string
        |> Fuzz.map (String.filter (\c -> c /= '\n' && c /= '\u{000D}'))
        |> Fuzz.map
            (\s ->
                if String.isEmpty s then
                    "x"

                else
                    s
            )


{-| Fuzzer for short single-line strings (good for testing with limited widths)
-}
shortStringFuzzer : Fuzzer String
shortStringFuzzer =
    Fuzz.stringOfLengthBetween 1 10
        |> Fuzz.map (String.filter (\c -> c /= '\n' && c /= '\u{000D}'))
        |> Fuzz.map
            (\s ->
                if String.isEmpty s then
                    "x"

                else
                    s
            )


{-| Fuzzer for printable ASCII characters
-}
printableCharFuzzer : Fuzzer Char
printableCharFuzzer =
    Fuzz.intRange 32 126
        |> Fuzz.map Char.fromCode


{-| Fuzzer for indentation levels
-}
indentFuzzer : Fuzzer Int
indentFuzzer =
    Fuzz.intRange 0 10


{-| Fuzzer for a simple Doc (just a string)
-}
simpleDocFuzzer : Fuzzer (Doc t)
simpleDocFuzzer =
    shortStringFuzzer
        |> Fuzz.map Pretty.string


{-| Fuzzer for a list of simple docs
-}
simpleDocListFuzzer : Fuzzer (List (Doc t))
simpleDocListFuzzer =
    Fuzz.listOfLengthBetween 0 5 simpleDocFuzzer



-- HELPER FUNCTIONS


{-| Count the number of lines in output
-}
countLines : String -> Int
countLines s =
    if String.isEmpty s then
        0

    else
        List.length (String.split "\n" s)


{-| Check if output contains a newline
-}
hasNewline : String -> Bool
hasNewline s =
    String.contains "\n" s



-- 1. EMPTY DOCUMENT PROPERTIES


emptyDocumentProperties : Test
emptyDocumentProperties =
    describe "Empty Document Properties"
        [ fuzz widthFuzzer "empty renders to empty string" <|
            \w ->
                Pretty.pretty w Pretty.empty
                    |> Expect.equal ""
        , fuzz2 widthFuzzer simpleDocFuzzer "empty is left identity for append" <|
            \w doc ->
                Pretty.pretty w (Pretty.append Pretty.empty doc)
                    |> Expect.equal (Pretty.pretty w doc)
        , fuzz2 widthFuzzer simpleDocFuzzer "empty is right identity for append" <|
            \w doc ->
                Pretty.pretty w (Pretty.append doc Pretty.empty)
                    |> Expect.equal (Pretty.pretty w doc)
        ]



-- 2. STRING/CHAR PROPERTIES


stringCharProperties : Test
stringCharProperties =
    describe "String and Char Properties"
        [ fuzz2 widthFuzzer singleLineStringFuzzer "string preserves content" <|
            \w s ->
                Pretty.pretty w (Pretty.string s)
                    |> Expect.equal s
        , fuzz2 widthFuzzer printableCharFuzzer "char preserves character" <|
            \w c ->
                Pretty.pretty w (Pretty.char c)
                    |> Expect.equal (String.fromChar c)
        , fuzz widthFuzzer "empty string renders to empty" <|
            \w ->
                Pretty.pretty w (Pretty.string "")
                    |> Expect.equal ""
        , fuzz2 widthFuzzer singleLineStringFuzzer "taggedString ignores tag in plain pretty" <|
            \w s ->
                Pretty.pretty w (Pretty.taggedString s "tag")
                    |> Expect.equal s
        ]



-- 3. APPEND PROPERTIES


appendProperties : Test
appendProperties =
    describe "Append Properties"
        [ fuzz3 widthFuzzer shortStringFuzzer shortStringFuzzer "append is associative in output" <|
            \w s1 s2 ->
                let
                    doc1 =
                        Pretty.string s1

                    doc2 =
                        Pretty.string s2

                    doc3 =
                        Pretty.string "c"

                    leftAssoc =
                        Pretty.append (Pretty.append doc1 doc2) doc3

                    rightAssoc =
                        Pretty.append doc1 (Pretty.append doc2 doc3)
                in
                Pretty.pretty w leftAssoc
                    |> Expect.equal (Pretty.pretty w rightAssoc)
        , fuzz2 shortStringFuzzer shortStringFuzzer "multiple appends concatenate correctly" <|
            \s1 s2 ->
                Pretty.string s1
                    |> Pretty.a (Pretty.string s2)
                    |> Pretty.pretty 80
                    |> Expect.equal (s1 ++ s2)
        ]



-- 4. LINE BREAK PROPERTIES


lineBreakProperties : Test
lineBreakProperties =
    describe "Line Break Properties"
        [ test "grouped line becomes space" <|
            \_ ->
                Pretty.group Pretty.line
                    |> Pretty.pretty 80
                    |> Expect.equal " "
        , test "grouped tightline becomes empty" <|
            \_ ->
                Pretty.group Pretty.tightline
                    |> Pretty.pretty 80
                    |> Expect.equal ""
        , fuzz largeWidthFuzzer "softline in wide context is space" <|
            \w ->
                Pretty.string "a"
                    |> Pretty.a Pretty.softline
                    |> Pretty.a (Pretty.string "b")
                    |> Pretty.pretty w
                    |> Expect.equal "a b"
        , test "line without group forces newline" <|
            \_ ->
                Pretty.string "a"
                    |> Pretty.a Pretty.line
                    |> Pretty.a (Pretty.string "b")
                    |> Pretty.pretty 80
                    |> hasNewline
                    |> Expect.equal True
        ]



-- 5. NEST PROPERTIES


nestProperties : Test
nestProperties =
    describe "Nest Properties"
        [ fuzz2 widthFuzzer simpleDocFuzzer "nest 0 is identity" <|
            \w doc ->
                Pretty.pretty w (Pretty.nest 0 doc)
                    |> Expect.equal (Pretty.pretty w doc)
        , fuzz2 widthFuzzer shortStringFuzzer "nest doesn't affect flat content" <|
            \w s ->
                let
                    doc =
                        Pretty.string s
                in
                Pretty.pretty w (Pretty.nest 4 doc)
                    |> Expect.equal (Pretty.pretty w doc)
        , fuzz indentFuzzer "nested lines have correct indentation" <|
            \n ->
                let
                    doc =
                        Pretty.nest n
                            (Pretty.string "a"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "b")
                            )

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ _, second ] ->
                        String.left n second
                            |> Expect.equal (String.repeat n " ")

                    _ ->
                        Expect.fail "Expected exactly two lines"
        ]



-- 6. GROUP PROPERTIES


groupProperties : Test
groupProperties =
    describe "Group Properties"
        [ fuzz2 widthFuzzer simpleDocFuzzer "group is idempotent" <|
            \w doc ->
                Pretty.pretty w (Pretty.group (Pretty.group doc))
                    |> Expect.equal (Pretty.pretty w (Pretty.group doc))
        , fuzz2 widthFuzzer shortStringFuzzer "group on flat doc is identity" <|
            \w s ->
                let
                    doc =
                        Pretty.string s
                in
                Pretty.pretty w (Pretty.group doc)
                    |> Expect.equal (Pretty.pretty w doc)
        , fuzz largeWidthFuzzer "group tries to fit on one line with large width" <|
            \w ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "a"
                                , Pretty.string "b"
                                , Pretty.string "c"
                                ]
                            )
                in
                Pretty.pretty w doc
                    |> hasNewline
                    |> Expect.equal False
        ]



-- 7. JOIN PROPERTIES


joinProperties : Test
joinProperties =
    describe "Join Properties"
        [ fuzz widthFuzzer "join empty list is empty" <|
            \w ->
                Pretty.join Pretty.space []
                    |> Pretty.pretty w
                    |> Expect.equal ""
        , fuzz2 widthFuzzer shortStringFuzzer "join singleton is identity" <|
            \w s ->
                let
                    doc =
                        Pretty.string s
                in
                Pretty.join Pretty.space [ doc ]
                    |> Pretty.pretty w
                    |> Expect.equal (Pretty.pretty w doc)
        , test "words joins with spaces" <|
            \_ ->
                Pretty.words
                    [ Pretty.string "hello"
                    , Pretty.string "world"
                    , Pretty.string "test"
                    ]
                    |> Pretty.pretty 80
                    |> Expect.equal "hello world test"
        , test "lines joins with newlines" <|
            \_ ->
                Pretty.lines
                    [ Pretty.string "a"
                    , Pretty.string "b"
                    , Pretty.string "c"
                    ]
                    |> Pretty.pretty 80
                    |> Expect.equal "a\nb\nc"
        , fuzz widthFuzzer "join filters out empties" <|
            \w ->
                Pretty.words
                    [ Pretty.string "a"
                    , Pretty.empty
                    , Pretty.string "b"
                    ]
                    |> Pretty.pretty w
                    |> Expect.equal "a b"
        , test "softlines joins with softlines" <|
            \_ ->
                Pretty.softlines
                    [ Pretty.string "a"
                    , Pretty.string "b"
                    ]
                    |> Pretty.pretty 1000
                    |> Expect.equal "a b"
        , test "separators adds separator strings" <|
            \_ ->
                Pretty.separators ", "
                    [ Pretty.string "a"
                    , Pretty.string "b"
                    , Pretty.string "c"
                    ]
                    |> Pretty.group
                    |> Pretty.pretty 1000
                    |> Expect.equal "a, b, c"
        ]



-- 8. SURROUND PROPERTIES


surroundProperties : Test
surroundProperties =
    describe "Surround and Wrapping Properties"
        [ fuzz2 widthFuzzer shortStringFuzzer "parens wraps with parentheses" <|
            \w s ->
                Pretty.parens (Pretty.string s)
                    |> Pretty.pretty w
                    |> Expect.equal ("(" ++ s ++ ")")
        , fuzz2 widthFuzzer shortStringFuzzer "braces wraps with braces" <|
            \w s ->
                Pretty.braces (Pretty.string s)
                    |> Pretty.pretty w
                    |> Expect.equal ("{" ++ s ++ "}")
        , fuzz2 widthFuzzer shortStringFuzzer "brackets wraps with brackets" <|
            \w s ->
                Pretty.brackets (Pretty.string s)
                    |> Pretty.pretty w
                    |> Expect.equal ("[" ++ s ++ "]")
        , fuzz2 widthFuzzer shortStringFuzzer "surround adds delimiters" <|
            \w s ->
                Pretty.surround (Pretty.string "<") (Pretty.string ">") (Pretty.string s)
                    |> Pretty.pretty w
                    |> Expect.equal ("<" ++ s ++ ">")
        ]



-- 9. WIDTH BEHAVIOR PROPERTIES


widthBehaviorProperties : Test
widthBehaviorProperties =
    describe "Width Behavior Properties"
        [ fuzz2 widthFuzzer (Fuzz.intRange 1 50) "larger width never increases line count" <|
            \baseWidth delta ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "hello"
                                , Pretty.string "world"
                                , Pretty.string "test"
                                ]
                            )

                    narrowOutput =
                        Pretty.pretty baseWidth doc

                    wideOutput =
                        Pretty.pretty (baseWidth + delta) doc
                in
                countLines wideOutput
                    |> Expect.atMost (countLines narrowOutput)
        , fuzz largeWidthFuzzer "very large width minimizes lines for grouped content" <|
            \w ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "a"
                                , Pretty.string "b"
                                ]
                            )
                in
                Pretty.pretty w doc
                    |> countLines
                    |> Expect.equal 1
        ]



-- 10. INDENT PROPERTIES


indentProperties : Test
indentProperties =
    describe "Indent Properties"
        [ fuzz2 indentFuzzer shortStringFuzzer "indent adds leading spaces" <|
            \n s ->
                let
                    output =
                        Pretty.indent n (Pretty.string s)
                            |> Pretty.pretty 80
                in
                String.left n output
                    |> Expect.equal (String.repeat n " ")
        , fuzz shortStringFuzzer "indent 0 is identity for simple strings" <|
            \s ->
                Pretty.indent 0 (Pretty.string s)
                    |> Pretty.pretty 80
                    |> Expect.equal s
        , fuzz indentFuzzer "hang preserves content after indentation" <|
            \n ->
                let
                    doc =
                        Pretty.hang n
                            (Pretty.string "a"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "b")
                            )

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ first, _ ] ->
                        first |> Expect.equal "a"

                    _ ->
                        Expect.fail "Expected exactly two lines"
        ]



-- 11. ALIGN PROPERTIES


alignProperties : Test
alignProperties =
    describe "Align Properties"
        [ test "align aligns subsequent lines to current column" <|
            \_ ->
                let
                    doc =
                        Pretty.string "prefix: "
                            |> Pretty.a
                                (Pretty.align
                                    (Pretty.lines
                                        [ Pretty.string "line1"
                                        , Pretty.string "line2"
                                        , Pretty.string "line3"
                                        ]
                                    )
                                )

                    output =
                        Pretty.pretty 80 doc

                    outputLines =
                        String.split "\n" output
                in
                case outputLines of
                    [ first, second, third ] ->
                        Expect.all
                            [ \_ -> first |> Expect.equal "prefix: line1"
                            , \_ -> String.left 8 second |> Expect.equal "        "
                            , \_ -> String.left 8 third |> Expect.equal "        "
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected exactly three lines"
        , test "align with single line is identity" <|
            \_ ->
                Pretty.align (Pretty.string "test")
                    |> Pretty.pretty 80
                    |> Expect.equal "test"
        ]



-- 12. FOLD PROPERTIES


foldProperties : Test
foldProperties =
    describe "Fold Properties"
        [ fuzz widthFuzzer "fold empty list produces empty" <|
            \w ->
                Pretty.fold (\s doc -> Pretty.append doc (Pretty.string s)) []
                    |> Pretty.pretty w
                    |> Expect.equal ""
        , test "fold concatenates strings correctly" <|
            \_ ->
                Pretty.fold (\s doc -> Pretty.append doc (Pretty.string s)) [ "a", "b", "c" ]
                    |> Pretty.pretty 80
                    |> Expect.equal "abc"
        , test "fold with custom combiner works" <|
            \_ ->
                let
                    combiner s doc =
                        if Pretty.pretty 80 doc == "" then
                            Pretty.string s

                        else
                            doc |> Pretty.a Pretty.space |> Pretty.a (Pretty.string s)
                in
                Pretty.fold combiner [ "hello", "world" ]
                    |> Pretty.pretty 80
                    |> Expect.equal "hello world"
        ]



-- 13. SPACE PROPERTIES


spaceProperties : Test
spaceProperties =
    describe "Space Properties"
        [ fuzz widthFuzzer "space renders to single space" <|
            \w ->
                Pretty.pretty w Pretty.space
                    |> Expect.equal " "
        , fuzz2 widthFuzzer shortStringFuzzer "space separates strings" <|
            \w s ->
                Pretty.string s
                    |> Pretty.a Pretty.space
                    |> Pretty.a (Pretty.string s)
                    |> Pretty.pretty w
                    |> Expect.equal (s ++ " " ++ s)
        ]



-- 14. TAG PROPERTIES


tagProperties : Test
tagProperties =
    describe "Tag Properties"
        [ fuzz2 widthFuzzer shortStringFuzzer "setTag doesn't affect plain pretty output" <|
            \w s ->
                Pretty.string s
                    |> Pretty.setTag "mytag"
                    |> Pretty.pretty w
                    |> Expect.equal s
        , fuzz2 widthFuzzer shortStringFuzzer "updateTag doesn't affect plain pretty output" <|
            \w s ->
                Pretty.string s
                    |> Pretty.updateTag (\_ _ -> Just "newtag")
                    |> Pretty.pretty w
                    |> Expect.equal s
        , test "setTag on complex doc preserves structure" <|
            \_ ->
                Pretty.lines
                    [ Pretty.string "a"
                    , Pretty.string "b"
                    ]
                    |> Pretty.setTag "tag"
                    |> Pretty.pretty 80
                    |> Expect.equal "a\nb"
        ]



-- 15. ADVANCED GROUP PROPERTIES


advancedGroupProperties : Test
advancedGroupProperties =
    describe "Advanced Group Properties"
        [ test "group breaks when content too wide" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "this is a long line"
                                , Pretty.string "another long line"
                                ]
                            )
                in
                Pretty.pretty 10 doc
                    |> hasNewline
                    |> Expect.equal True
        , test "nested groups work correctly" <|
            \_ ->
                let
                    inner =
                        Pretty.group
                            (Pretty.lines [ Pretty.string "a", Pretty.string "b" ])

                    outer =
                        Pretty.group
                            (Pretty.lines [ inner, Pretty.string "c" ])
                in
                Pretty.pretty 1000 outer
                    |> Expect.equal "a b c"
        , test "nested groups break correctly when narrow" <|
            \_ ->
                let
                    inner =
                        Pretty.group
                            (Pretty.lines [ Pretty.string "aa", Pretty.string "bb" ])

                    outer =
                        Pretty.group
                            (Pretty.lines [ inner, Pretty.string "cc" ])
                in
                Pretty.pretty 3 outer
                    |> hasNewline
                    |> Expect.equal True
        , fuzz smallWidthFuzzer "softline breaks at small widths" <|
            \w ->
                let
                    doc =
                        Pretty.string "hello"
                            |> Pretty.a Pretty.softline
                            |> Pretty.a (Pretty.string "world")
                in
                if w < 11 then
                    Pretty.pretty w doc
                        |> hasNewline
                        |> Expect.equal True

                else
                    Expect.pass
        ]



-- 16. ADVANCED NEST PROPERTIES


advancedNestProperties : Test
advancedNestProperties =
    describe "Advanced Nest Properties"
        [ fuzz2 indentFuzzer indentFuzzer "double nesting adds indentation" <|
            \n1 n2 ->
                let
                    doc =
                        Pretty.nest n1
                            (Pretty.string "a"
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.nest n2
                                        (Pretty.string "b"
                                            |> Pretty.a Pretty.line
                                            |> Pretty.a (Pretty.string "c")
                                        )
                                    )
                            )

                    output =
                        Pretty.pretty 80 doc

                    outputLines =
                        String.split "\n" output
                in
                case outputLines of
                    [ _, second, third ] ->
                        Expect.all
                            [ \_ ->
                                String.left n1 second
                                    |> Expect.equal (String.repeat n1 " ")
                            , \_ ->
                                String.left (n1 + n2) third
                                    |> Expect.equal (String.repeat (n1 + n2) " ")
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected exactly three lines"
        , test "nest inside group respects flattening" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.nest 4
                                (Pretty.lines
                                    [ Pretty.string "a"
                                    , Pretty.string "b"
                                    ]
                                )
                            )
                in
                Pretty.pretty 1000 doc
                    |> Expect.equal "a b"
        , test "group inside nest preserves nesting on break" <|
            \_ ->
                let
                    doc =
                        Pretty.nest 4
                            (Pretty.string "x"
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "aaaaaaaaaa"
                                            , Pretty.string "bbbbbbbbbb"
                                            ]
                                        )
                                    )
                            )

                    output =
                        Pretty.pretty 15 doc

                    outputLines =
                        String.split "\n" output
                in
                case outputLines of
                    [ _, second, third ] ->
                        Expect.all
                            [ \_ -> String.left 4 second |> Expect.equal "    "
                            , \_ -> String.left 4 third |> Expect.equal "    "
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected three lines"
        ]



-- 17. DETERMINISM PROPERTIES


determinismProperties : Test
determinismProperties =
    describe "Determinism Properties"
        [ fuzz2 widthFuzzer simpleDocFuzzer "same doc produces same output" <|
            \w doc ->
                Pretty.pretty w doc
                    |> Expect.equal (Pretty.pretty w doc)
        , fuzz3 widthFuzzer shortStringFuzzer shortStringFuzzer "complex doc is deterministic" <|
            \w s1 s2 ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string s1
                                , Pretty.nest 2 (Pretty.string s2)
                                ]
                            )
                in
                Pretty.pretty w doc
                    |> Expect.equal (Pretty.pretty w doc)
        ]



-- 18. EDGE CASE PROPERTIES


edgeCaseProperties : Test
edgeCaseProperties =
    describe "Edge Case Properties"
        [ test "zero width still produces output" <|
            \_ ->
                Pretty.string "test"
                    |> Pretty.pretty 0
                    |> Expect.equal "test"
        , test "very narrow width forces all breaks" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "abc"
                                , Pretty.string "def"
                                ]
                            )
                in
                Pretty.pretty 1 doc
                    |> hasNewline
                    |> Expect.equal True
        , fuzz widthFuzzer "multiple consecutive empties are filtered" <|
            \w ->
                Pretty.words
                    [ Pretty.empty
                    , Pretty.empty
                    , Pretty.string "a"
                    , Pretty.empty
                    , Pretty.empty
                    , Pretty.string "b"
                    , Pretty.empty
                    ]
                    |> Pretty.pretty w
                    |> Expect.equal "a b"
        , test "tightline disappears in group" <|
            \_ ->
                Pretty.string "a"
                    |> Pretty.a (Pretty.group Pretty.tightline)
                    |> Pretty.a (Pretty.string "b")
                    |> Pretty.pretty 80
                    |> Expect.equal "ab"
        , test "separators work in multiline context" <|
            \_ ->
                Pretty.separators ", "
                    [ Pretty.string "one"
                    , Pretty.string "two"
                    , Pretty.string "three"
                    ]
                    |> Pretty.pretty 80
                    |> Expect.equal "one\n, two\n, three"
        , test "long chain of appends works" <|
            \_ ->
                let
                    doc =
                        List.range 1 100
                            |> List.map (\i -> Pretty.string (String.fromInt i))
                            |> Pretty.words
                in
                Pretty.pretty 1000 doc
                    |> String.words
                    |> List.length
                    |> Expect.equal 100
        , test "deeply nested structure works" <|
            \_ ->
                let
                    nestLevel n innerDoc =
                        if n <= 0 then
                            innerDoc

                        else
                            Pretty.nest 1 (nestLevel (n - 1) innerDoc)
                in
                nestLevel 50 (Pretty.string "deep")
                    |> Pretty.pretty 80
                    |> Expect.equal "deep"
        ]
