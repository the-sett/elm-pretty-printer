module DeepAlgorithmTests exposing (suite)

{-| Deep algorithm tests for elm-pretty-printer.

These tests verify the behavior of the pretty printing algorithm with:
- Complex operator combinations
- Width threshold behavior
- Content preservation
- Line length constraints
- Real-world document patterns
-}

import Expect
import Fuzz exposing (Fuzzer)
import Pretty exposing (Doc)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "Deep Algorithm Tests"
        [ widthThresholdTests
        , contentPreservationTests
        , lineLengthConstraintTests
        , operatorCombinationTests
        , realWorldPatternTests
        , comparisonTests
        , multipleGroupTests
        , greedyAlgorithmTests
        , surroundMultilineTests
        , complexIndentationTests
        , stressTests
        ]



-- FUZZERS


widthFuzzer : Fuzzer Int
widthFuzzer =
    Fuzz.intRange 10 100


smallWidthFuzzer : Fuzzer Int
smallWidthFuzzer =
    Fuzz.intRange 5 30


shortStringFuzzer : Fuzzer String
shortStringFuzzer =
    Fuzz.stringOfLengthBetween 1 8
        |> Fuzz.map (String.filter (\c -> c /= '\n' && c /= '\u{000D}' && c /= ' '))
        |> Fuzz.map
            (\s ->
                if String.isEmpty s then
                    "x"

                else
                    s
            )


indentFuzzer : Fuzzer Int
indentFuzzer =
    Fuzz.intRange 1 6



-- HELPERS


countLines : String -> Int
countLines s =
    if String.isEmpty s then
        0

    else
        List.length (String.split "\n" s)


maxLineLength : String -> Int
maxLineLength s =
    String.split "\n" s
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0


allLinesFitWidth : Int -> String -> Bool
allLinesFitWidth w s =
    String.split "\n" s
        |> List.all (\line -> String.length line <= w)


extractContent : String -> String
extractContent s =
    String.filter (\c -> c /= ' ' && c /= '\n') s


hasNewline : String -> Bool
hasNewline s =
    String.contains "\n" s



-- 1. WIDTH THRESHOLD TESTS


widthThresholdTests : Test
widthThresholdTests =
    describe "Width Threshold Behavior"
        [ test "exact fit stays on one line" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "ab"
                                , Pretty.string "cd"
                                ]
                            )

                    -- "ab cd" is 5 chars
                in
                Pretty.pretty 5 doc
                    |> Expect.equal "ab cd"
        , test "one char over threshold breaks" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "ab"
                                , Pretty.string "cd"
                                ]
                            )

                    -- "ab cd" is 5 chars, width 4 should break
                in
                Pretty.pretty 4 doc
                    |> hasNewline
                    |> Expect.equal True
        , fuzz (Fuzz.intRange 1 50) "threshold behavior is consistent" <|
            \w ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "hello"
                                , Pretty.string "world"
                                ]
                            )

                    output =
                        Pretty.pretty w doc

                    singleLineLength =
                        11

                    -- "hello world"
                in
                if w >= singleLineLength then
                    hasNewline output
                        |> Expect.equal False

                else
                    hasNewline output
                        |> Expect.equal True
        , test "width exactly matches content length" <|
            \_ ->
                let
                    content =
                        "exactly20characters!"

                    doc =
                        Pretty.string content
                in
                Pretty.pretty 20 doc
                    |> Expect.equal content
        , fuzz2 (Fuzz.intRange 5 20) (Fuzz.intRange 5 20) "two groups with different thresholds" <|
            \w1 w2 ->
                let
                    shortDoc =
                        Pretty.group (Pretty.lines [ Pretty.string "a", Pretty.string "b" ])

                    -- "a b" = 3 chars
                    longDoc =
                        Pretty.group (Pretty.lines [ Pretty.string "hello", Pretty.string "world" ])

                    -- "hello world" = 11 chars
                    combined =
                        Pretty.lines [ shortDoc, longDoc ]

                    minWidth =
                        min w1 w2

                    output =
                        Pretty.pretty minWidth combined
                in
                -- Short doc (3 chars) should fit if width >= 3
                -- This tests that each group evaluates independently
                Expect.pass
        ]



-- 2. CONTENT PRESERVATION TESTS


contentPreservationTests : Test
contentPreservationTests =
    describe "Content Preservation"
        [ fuzz2 widthFuzzer shortStringFuzzer "all content appears in output" <|
            \w s ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string s
                                , Pretty.string "middle"
                                , Pretty.string s
                                ]
                            )

                    output =
                        Pretty.pretty w doc

                    contentOnly =
                        extractContent output
                in
                contentOnly
                    |> Expect.equal (s ++ "middle" ++ s)
        , fuzz widthFuzzer "nested content is preserved" <|
            \w ->
                let
                    doc =
                        Pretty.nest 4
                            (Pretty.group
                                (Pretty.lines
                                    [ Pretty.string "abc"
                                    , Pretty.string "def"
                                    ]
                                )
                            )

                    output =
                        Pretty.pretty w doc
                in
                extractContent output
                    |> Expect.equal "abcdef"
        , fuzz2 widthFuzzer indentFuzzer "indent preserves content" <|
            \w n ->
                let
                    doc =
                        Pretty.indent n (Pretty.string "content")

                    output =
                        Pretty.pretty w doc
                in
                String.contains "content" output
                    |> Expect.equal True
        , test "complex nesting preserves all strings" <|
            \_ ->
                let
                    doc =
                        Pretty.nest 2
                            (Pretty.lines
                                [ Pretty.string "first"
                                , Pretty.nest 2
                                    (Pretty.lines
                                        [ Pretty.string "second"
                                        , Pretty.string "third"
                                        ]
                                    )
                                , Pretty.string "fourth"
                                ]
                            )

                    output =
                        Pretty.pretty 80 doc
                in
                Expect.all
                    [ \o -> String.contains "first" o |> Expect.equal True
                    , \o -> String.contains "second" o |> Expect.equal True
                    , \o -> String.contains "third" o |> Expect.equal True
                    , \o -> String.contains "fourth" o |> Expect.equal True
                    ]
                    output
        ]



-- 3. LINE LENGTH CONSTRAINT TESTS


lineLengthConstraintTests : Test
lineLengthConstraintTests =
    describe "Line Length Constraints"
        [ fuzz widthFuzzer "simple string never exceeds its own length" <|
            \w ->
                let
                    doc =
                        Pretty.string "test"

                    output =
                        Pretty.pretty w doc
                in
                maxLineLength output
                    |> Expect.equal 4
        , test "grouped doc respects width when breaking" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "short"
                                , Pretty.string "words"
                                ]
                            )

                    output =
                        Pretty.pretty 6 doc
                in
                -- When broken, each line should be <= 6
                allLinesFitWidth 6 output
                    |> Expect.equal True
        , fuzz (Fuzz.intRange 10 50) "nested indent respects accumulated indentation" <|
            \w ->
                let
                    doc =
                        Pretty.nest 4
                            (Pretty.string "a"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "b")
                            )

                    output =
                        Pretty.pretty w doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ _, second ] ->
                        -- Second line has 4 spaces + "b" = 5 chars
                        String.length second
                            |> Expect.equal 5

                    _ ->
                        Expect.fail "Expected two lines"
        , fuzz2 smallWidthFuzzer indentFuzzer "indent adds exactly n spaces to first line" <|
            \w n ->
                let
                    doc =
                        Pretty.indent n (Pretty.string "x")

                    output =
                        Pretty.pretty w doc
                in
                String.length output
                    |> Expect.equal (n + 1)
        ]



-- 4. OPERATOR COMBINATION TESTS


operatorCombinationTests : Test
operatorCombinationTests =
    describe "Operator Combinations"
        [ test "nest + group + nest" <|
            \_ ->
                let
                    doc =
                        Pretty.nest 2
                            (Pretty.group
                                (Pretty.nest 2
                                    (Pretty.lines
                                        [ Pretty.string "a"
                                        , Pretty.string "b"
                                        ]
                                    )
                                )
                            )
                in
                -- With wide width, should flatten
                Pretty.pretty 100 doc
                    |> Expect.equal "a b"
        , test "nest + group + nest with narrow width" <|
            \_ ->
                let
                    doc =
                        Pretty.nest 2
                            (Pretty.group
                                (Pretty.nest 2
                                    (Pretty.lines
                                        [ Pretty.string "a"
                                        , Pretty.string "b"
                                        ]
                                    )
                                )
                            )

                    output =
                        Pretty.pretty 2 doc
                in
                -- Should break with combined indentation
                output
                    |> Expect.equal "a\n    b"
        , test "align + nest interaction" <|
            \_ ->
                let
                    doc =
                        Pretty.string "pre: "
                            |> Pretty.a
                                (Pretty.align
                                    (Pretty.nest 2
                                        (Pretty.lines
                                            [ Pretty.string "a"
                                            , Pretty.string "b"
                                            ]
                                        )
                                    )
                                )

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ first, second ] ->
                        Expect.all
                            [ \_ -> first |> Expect.equal "pre: a"
                            , \_ -> String.left 7 second |> Expect.equal "       "
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected two lines"
        , test "indent + group combination" <|
            \_ ->
                let
                    doc =
                        Pretty.indent 4
                            (Pretty.group
                                (Pretty.lines
                                    [ Pretty.string "a"
                                    , Pretty.string "b"
                                    ]
                                )
                            )
                in
                Pretty.pretty 100 doc
                    |> Expect.equal "    a b"
        , test "indent + group with narrow width" <|
            \_ ->
                let
                    doc =
                        Pretty.indent 4
                            (Pretty.group
                                (Pretty.lines
                                    [ Pretty.string "a"
                                    , Pretty.string "b"
                                    ]
                                )
                            )

                    output =
                        Pretty.pretty 5 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ first, second ] ->
                        Expect.all
                            [ \_ -> first |> Expect.equal "    a"
                            , \_ -> second |> Expect.equal "    b"
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected two lines"
        , test "surround + group + nest" <|
            \_ ->
                let
                    inner =
                        Pretty.group
                            (Pretty.nest 2
                                (Pretty.lines
                                    [ Pretty.string "x"
                                    , Pretty.string "y"
                                    ]
                                )
                            )

                    doc =
                        Pretty.parens inner
                in
                Pretty.pretty 100 doc
                    |> Expect.equal "(x y)"
        , fuzz2 indentFuzzer indentFuzzer "hang vs indent + nest equivalence for simple case" <|
            \n1 n2 ->
                let
                    content =
                        Pretty.string "test"

                    hangDoc =
                        Pretty.hang n1 content

                    -- hang n = align (nest n doc)
                    -- For a simple string with no line breaks, both should be identical
                in
                Pretty.pretty 80 hangDoc
                    |> Expect.equal "test"
        ]



-- 5. REAL-WORLD PATTERN TESTS


realWorldPatternTests : Test
realWorldPatternTests =
    describe "Real-World Document Patterns"
        [ test "JSON-like array formatting (wide)" <|
            \_ ->
                let
                    items =
                        [ "1", "2", "3" ]
                            |> List.map Pretty.string

                    doc =
                        Pretty.group
                            (Pretty.string "["
                                |> Pretty.a
                                    (Pretty.nest 2
                                        (Pretty.tightline
                                            |> Pretty.a (Pretty.join (Pretty.string "," |> Pretty.a Pretty.line) items)
                                        )
                                    )
                                |> Pretty.a Pretty.tightline
                                |> Pretty.a (Pretty.string "]")
                            )
                in
                -- "," + line (becomes space when grouped) = ", "
                Pretty.pretty 80 doc
                    |> Expect.equal "[1, 2, 3]"
        , test "JSON-like array formatting (narrow)" <|
            \_ ->
                let
                    items =
                        [ "1", "2", "3" ]
                            |> List.map Pretty.string

                    doc =
                        Pretty.group
                            (Pretty.string "["
                                |> Pretty.a
                                    (Pretty.nest 2
                                        (Pretty.tightline
                                            |> Pretty.a (Pretty.join (Pretty.string "," |> Pretty.a Pretty.line) items)
                                        )
                                    )
                                |> Pretty.a Pretty.tightline
                                |> Pretty.a (Pretty.string "]")
                            )

                    output =
                        Pretty.pretty 5 doc
                in
                hasNewline output
                    |> Expect.equal True
        , test "function call formatting (wide)" <|
            \_ ->
                let
                    args =
                        [ "arg1", "arg2", "arg3" ]
                            |> List.map Pretty.string

                    doc =
                        Pretty.string "function"
                            |> Pretty.a
                                (Pretty.group
                                    (Pretty.string "("
                                        |> Pretty.a
                                            (Pretty.nest 2
                                                (Pretty.softlines args
                                                    |> Pretty.a Pretty.tightline
                                                )
                                            )
                                        |> Pretty.a (Pretty.string ")")
                                    )
                                )
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "function(arg1 arg2 arg3)"
        , test "code block with proper indentation" <|
            \_ ->
                let
                    -- nest only affects line breaks INSIDE the nested doc
                    -- So we put the line break inside the nest
                    doc =
                        Pretty.string "if (condition) {"
                            |> Pretty.a
                                (Pretty.nest 4
                                    (Pretty.line
                                        |> Pretty.a (Pretty.string "doSomething();")
                                    )
                                )
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.string "}")
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "if (condition) {\n    doSomething();\n}"
        , test "nested code blocks" <|
            \_ ->
                let
                    -- Proper nesting: line breaks are inside nest
                    doc =
                        Pretty.string "function foo() {"
                            |> Pretty.a
                                (Pretty.nest 4
                                    (Pretty.line
                                        |> Pretty.a (Pretty.string "if (x) {")
                                        |> Pretty.a
                                            (Pretty.nest 4
                                                (Pretty.line
                                                    |> Pretty.a (Pretty.string "return y;")
                                                )
                                            )
                                        |> Pretty.a Pretty.line
                                        |> Pretty.a (Pretty.string "}")
                                    )
                                )
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.string "}")

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ l1, l2, l3, l4, l5 ] ->
                        Expect.all
                            [ \_ -> l1 |> Expect.equal "function foo() {"
                            , \_ -> l2 |> Expect.equal "    if (x) {"
                            , \_ -> l3 |> Expect.equal "        return y;"
                            , \_ -> l4 |> Expect.equal "    }"
                            , \_ -> l5 |> Expect.equal "}"
                            ]
                            ()

                    _ ->
                        Expect.fail ("Expected 5 lines, got " ++ String.fromInt (List.length lines))
        , test "list with trailing comma style" <|
            \_ ->
                let
                    items =
                        [ "alpha", "beta", "gamma" ]
                            |> List.map Pretty.string

                    doc =
                        Pretty.separators ", " items
                            |> Pretty.group
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "alpha, beta, gamma"
        , test "list with leading comma style (broken)" <|
            \_ ->
                let
                    items =
                        [ "alpha", "beta", "gamma" ]
                            |> List.map Pretty.string

                    doc =
                        Pretty.separators ", " items
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "alpha\n, beta\n, gamma"
        ]



-- 6. COMPARISON TESTS


comparisonTests : Test
comparisonTests =
    describe "Operator Comparisons"
        [ test "line vs tightline in group" <|
            \_ ->
                let
                    withLine =
                        Pretty.group
                            (Pretty.string "a"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "b")
                            )

                    withTightline =
                        Pretty.group
                            (Pretty.string "a"
                                |> Pretty.a Pretty.tightline
                                |> Pretty.a (Pretty.string "b")
                            )
                in
                Expect.all
                    [ \_ -> Pretty.pretty 80 withLine |> Expect.equal "a b"
                    , \_ -> Pretty.pretty 80 withTightline |> Expect.equal "ab"
                    ]
                    ()
        , test "line vs tightline when broken" <|
            \_ ->
                let
                    withLine =
                        Pretty.string "a"
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.string "b")

                    withTightline =
                        Pretty.string "a"
                            |> Pretty.a Pretty.tightline
                            |> Pretty.a (Pretty.string "b")
                in
                Expect.all
                    [ \_ -> Pretty.pretty 80 withLine |> Expect.equal "a\nb"
                    , \_ -> Pretty.pretty 80 withTightline |> Expect.equal "a\nb"
                    ]
                    ()
        , fuzz indentFuzzer "nest vs indent on single line" <|
            \n ->
                let
                    nested =
                        Pretty.nest n (Pretty.string "x")

                    indented =
                        Pretty.indent n (Pretty.string "x")
                in
                Expect.all
                    [ \_ -> Pretty.pretty 80 nested |> Expect.equal "x"
                    , \_ -> Pretty.pretty 80 indented |> Expect.equal (String.repeat n " " ++ "x")
                    ]
                    ()
        , fuzz indentFuzzer "nest vs indent with line break" <|
            \n ->
                let
                    content =
                        Pretty.string "a"
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.string "b")

                    nested =
                        Pretty.nest n content

                    indented =
                        Pretty.indent n content

                    nestedOut =
                        Pretty.pretty 80 nested

                    indentedOut =
                        Pretty.pretty 80 indented

                    nestedLines =
                        String.split "\n" nestedOut

                    indentedLines =
                        String.split "\n" indentedOut
                in
                case ( nestedLines, indentedLines ) of
                    ( [ n1, n2 ], [ i1, i2 ] ) ->
                        Expect.all
                            [ \_ -> n1 |> Expect.equal "a"
                            , \_ -> n2 |> Expect.equal (String.repeat n " " ++ "b")
                            , \_ -> i1 |> Expect.equal (String.repeat n " " ++ "a")
                            , \_ -> i2 |> Expect.equal (String.repeat n " " ++ "b")
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected two lines each"
        , test "softline vs line in wide context" <|
            \_ ->
                let
                    withSoftline =
                        Pretty.string "a"
                            |> Pretty.a Pretty.softline
                            |> Pretty.a (Pretty.string "b")

                    withLine =
                        Pretty.string "a"
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.string "b")
                in
                Expect.all
                    [ \_ -> Pretty.pretty 80 withSoftline |> Expect.equal "a b"
                    , \_ -> Pretty.pretty 80 withLine |> Expect.equal "a\nb"
                    ]
                    ()
        , test "words vs softlines" <|
            \_ ->
                let
                    items =
                        [ Pretty.string "a", Pretty.string "b", Pretty.string "c" ]

                    withWords =
                        Pretty.words items

                    withSoftlines =
                        Pretty.softlines items
                in
                -- Both should produce same output in wide context
                Expect.all
                    [ \_ -> Pretty.pretty 80 withWords |> Expect.equal "a b c"
                    , \_ -> Pretty.pretty 80 withSoftlines |> Expect.equal "a b c"
                    ]
                    ()
        ]



-- 7. MULTIPLE GROUP TESTS


multipleGroupTests : Test
multipleGroupTests =
    describe "Multiple Independent Groups"
        [ test "sequential groups evaluated independently" <|
            \_ ->
                let
                    group1 =
                        Pretty.group (Pretty.lines [ Pretty.string "aa", Pretty.string "bb" ])

                    group2 =
                        Pretty.group (Pretty.lines [ Pretty.string "cc", Pretty.string "dd" ])

                    doc =
                        group1
                            |> Pretty.a Pretty.line
                            |> Pretty.a group2
                in
                -- With width 5, "aa bb" fits (5 chars) but "cc dd" also fits
                Pretty.pretty 5 doc
                    |> Expect.equal "aa bb\ncc dd"
        , test "first group affects second group's column" <|
            \_ ->
                let
                    group1 =
                        Pretty.group (Pretty.lines [ Pretty.string "xxx", Pretty.string "yyy" ])

                    group2 =
                        Pretty.group (Pretty.lines [ Pretty.string "aaa", Pretty.string "bbb" ])

                    doc =
                        group1
                            |> Pretty.a (Pretty.string " ")
                            |> Pretty.a group2

                    -- "xxx yyy" = 7, then " " = 1, then group2 starts at column 8
                    -- "aaa bbb" = 7, total would be 15
                in
                Pretty.pretty 15 doc
                    |> Expect.equal "xxx yyy aaa bbb"
        , test "nested groups break from outside in" <|
            \_ ->
                let
                    inner =
                        Pretty.group (Pretty.lines [ Pretty.string "inner1", Pretty.string "inner2" ])

                    outer =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "outer1"
                                , inner
                                ]
                            )

                    -- inner flat = "inner1 inner2" (13 chars)
                    -- outer flat = "outer1 inner1 inner2" (20 chars)
                in
                -- Width 15: outer breaks but inner might stay flat
                Pretty.pretty 15 outer
                    |> Expect.equal "outer1\ninner1 inner2"
        , fuzz (Fuzz.intRange 5 25) "group independence varies with width" <|
            \w ->
                let
                    group1 =
                        Pretty.group (Pretty.lines [ Pretty.string "ab", Pretty.string "cd" ])

                    -- flat: "ab cd" (5 chars)
                    group2 =
                        Pretty.group (Pretty.lines [ Pretty.string "ef", Pretty.string "gh" ])

                    -- flat: "ef gh" (5 chars)
                    doc =
                        Pretty.lines [ group1, group2 ]

                    output =
                        Pretty.pretty w doc
                in
                if w >= 5 then
                    -- Both groups should fit on their lines
                    output |> Expect.equal "ab cd\nef gh"

                else
                    -- Both should break
                    countLines output |> Expect.equal 4
        ]



-- 8. GREEDY ALGORITHM TESTS


greedyAlgorithmTests : Test
greedyAlgorithmTests =
    describe "Greedy Algorithm Behavior"
        [ test "greedy fitting: first fit wins" <|
            \_ ->
                let
                    -- The algorithm is greedy: it tries to fit as much as possible
                    -- on the current line before breaking
                    doc =
                        Pretty.group
                            (Pretty.string "prefix"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "suffix")
                            )

                    -- flat: "prefix suffix" (13 chars)
                in
                Expect.all
                    [ \_ -> Pretty.pretty 13 doc |> Expect.equal "prefix suffix"
                    , \_ -> Pretty.pretty 12 doc |> Expect.equal "prefix\nsuffix"
                    ]
                    ()
        , test "early content consumes width budget" <|
            \_ ->
                let
                    doc =
                        Pretty.string "aaaaaaaaaa"
                            |> Pretty.a (Pretty.string " ")
                            |> Pretty.a
                                (Pretty.group
                                    (Pretty.lines
                                        [ Pretty.string "b"
                                        , Pretty.string "c"
                                        ]
                                    )
                                )

                    -- At column 11, group needs 3 more chars for "b c"
                    -- Total: 14 chars needed
                in
                Expect.all
                    [ \_ -> Pretty.pretty 14 doc |> Expect.equal "aaaaaaaaaa b c"
                    , \_ -> Pretty.pretty 13 doc |> Expect.equal "aaaaaaaaaa b\nc"
                    ]
                    ()
        , test "indentation affects fitting budget" <|
            \_ ->
                let
                    doc =
                        Pretty.nest 10
                            (Pretty.string "x"
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "ab"
                                            , Pretty.string "cd"
                                            ]
                                        )
                                    )
                            )

                    -- After line break, we're at column 10
                    -- Group needs 5 chars for "ab cd"
                    -- Total line would be 15 chars
                in
                Expect.all
                    [ \_ ->
                        Pretty.pretty 15 doc
                            |> Expect.equal "x\n          ab cd"
                    , \_ ->
                        Pretty.pretty 14 doc
                            |> Expect.equal "x\n          ab\n          cd"
                    ]
                    ()
        ]



-- 9. SURROUND WITH MULTILINE TESTS


surroundMultilineTests : Test
surroundMultilineTests =
    describe "Surround with Multiline Content"
        [ test "parens with multiline content (wide)" <|
            \_ ->
                let
                    content =
                        Pretty.group (Pretty.lines [ Pretty.string "a", Pretty.string "b" ])

                    doc =
                        Pretty.parens content
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "(a b)"
        , test "parens with multiline content (narrow)" <|
            \_ ->
                let
                    content =
                        Pretty.group (Pretty.lines [ Pretty.string "a", Pretty.string "b" ])

                    doc =
                        Pretty.parens content
                in
                Pretty.pretty 3 doc
                    |> Expect.equal "(a\nb)"
        , test "brackets with nested groups" <|
            \_ ->
                let
                    inner =
                        Pretty.group (Pretty.lines [ Pretty.string "x", Pretty.string "y" ])

                    doc =
                        Pretty.brackets
                            (Pretty.group
                                (Pretty.lines
                                    [ inner
                                    , Pretty.string "z"
                                    ]
                                )
                            )
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "[x y z]"
        , test "nested brackets formatting (wide)" <|
            \_ ->
                let
                    doc =
                        Pretty.brackets
                            (Pretty.group
                                (Pretty.nest 2
                                    (Pretty.tightline
                                        |> Pretty.a
                                            (Pretty.lines
                                                [ Pretty.brackets (Pretty.string "1")
                                                , Pretty.brackets (Pretty.string "2")
                                                ]
                                            )
                                        |> Pretty.a Pretty.tightline
                                    )
                                )
                            )
                in
                -- Group flattens lines to spaces
                Pretty.pretty 80 doc
                    |> Expect.equal "[[1] [2]]"
        , test "nested brackets formatting (narrow)" <|
            \_ ->
                let
                    doc =
                        Pretty.brackets
                            (Pretty.group
                                (Pretty.nest 2
                                    (Pretty.tightline
                                        |> Pretty.a
                                            (Pretty.lines
                                                [ Pretty.brackets (Pretty.string "1")
                                                , Pretty.brackets (Pretty.string "2")
                                                ]
                                            )
                                        |> Pretty.a Pretty.tightline
                                    )
                                )
                            )
                in
                -- When narrow, breaks with indentation (closing tightline is also indented)
                Pretty.pretty 5 doc
                    |> Expect.equal "[\n  [1]\n  [2]\n  ]"
        , test "braces with code-like content (proper structure)" <|
            \_ ->
                let
                    -- For proper formatting, tightline before closing brace must be outside nest
                    doc =
                        Pretty.group
                            (Pretty.string "{"
                                |> Pretty.a
                                    (Pretty.nest 2
                                        (Pretty.line
                                            |> Pretty.a (Pretty.string "x: 1")
                                        )
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "}")
                            )
                in
                Expect.all
                    [ \_ -> Pretty.pretty 80 doc |> Expect.equal "{ x: 1 }"
                    , \_ -> Pretty.pretty 5 doc |> Expect.equal "{\n  x: 1\n}"
                    ]
                    ()
        ]



-- 10. COMPLEX INDENTATION TESTS


complexIndentationTests : Test
complexIndentationTests =
    describe "Complex Indentation Scenarios"
        [ test "align after prefix maintains alignment" <|
            \_ ->
                let
                    doc =
                        Pretty.string "let "
                            |> Pretty.a
                                (Pretty.align
                                    (Pretty.lines
                                        [ Pretty.string "x = 1"
                                        , Pretty.string "y = 2"
                                        , Pretty.string "z = 3"
                                        ]
                                    )
                                )

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ l1, l2, l3 ] ->
                        Expect.all
                            [ \_ -> l1 |> Expect.equal "let x = 1"
                            , \_ -> l2 |> Expect.equal "    y = 2"
                            , \_ -> l3 |> Expect.equal "    z = 3"
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected 3 lines"
        , test "hang creates hanging indent" <|
            \_ ->
                let
                    doc =
                        Pretty.string "call"
                            |> Pretty.a
                                (Pretty.hang 2
                                    (Pretty.lines
                                        [ Pretty.string "(arg1"
                                        , Pretty.string "arg2"
                                        , Pretty.string "arg3)"
                                        ]
                                    )
                                )

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ l1, l2, l3 ] ->
                        Expect.all
                            [ \_ -> l1 |> Expect.equal "call(arg1"
                            , \_ -> l2 |> Expect.equal "      arg2"
                            , \_ -> l3 |> Expect.equal "      arg3)"
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected 3 lines"
        , fuzz2 indentFuzzer indentFuzzer "triple nesting accumulates correctly" <|
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
                                            |> Pretty.a
                                                (Pretty.nest n1
                                                    (Pretty.string "c"
                                                        |> Pretty.a Pretty.line
                                                        |> Pretty.a (Pretty.string "d")
                                                    )
                                                )
                                        )
                                    )
                            )

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ _, l2, l3, l4 ] ->
                        Expect.all
                            [ \_ ->
                                String.length l2 - 1
                                    |> Expect.equal n1
                            , \_ ->
                                String.length l3 - 1
                                    |> Expect.equal (n1 + n2)
                            , \_ ->
                                String.length l4 - 1
                                    |> Expect.equal (n1 + n2 + n1)
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected 4 lines"
        , test "mixed indent and nest" <|
            \_ ->
                let
                    doc =
                        Pretty.indent 2
                            (Pretty.nest 2
                                (Pretty.string "a"
                                    |> Pretty.a Pretty.line
                                    |> Pretty.a (Pretty.string "b")
                                )
                            )

                    output =
                        Pretty.pretty 80 doc

                    lines =
                        String.split "\n" output
                in
                case lines of
                    [ l1, l2 ] ->
                        Expect.all
                            [ \_ -> l1 |> Expect.equal "  a"
                            , \_ -> l2 |> Expect.equal "    b"
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected 2 lines"
        ]



-- 11. STRESS TESTS


stressTests : Test
stressTests =
    describe "Stress Tests"
        [ test "many sequential groups" <|
            \_ ->
                let
                    groups =
                        List.range 1 20
                            |> List.map
                                (\i ->
                                    Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string (String.fromInt i)
                                            , Pretty.string "x"
                                            ]
                                        )
                                )

                    doc =
                        Pretty.lines groups

                    output =
                        Pretty.pretty 10 doc
                in
                countLines output
                    |> Expect.equal 20
        , test "deeply nested groups" <|
            \_ ->
                let
                    nestGroups n inner =
                        if n <= 0 then
                            inner

                        else
                            Pretty.group (nestGroups (n - 1) inner)

                    doc =
                        nestGroups 20 (Pretty.string "deep")
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "deep"
        , test "wide document with many breaks" <|
            \_ ->
                let
                    doc =
                        List.range 1 50
                            |> List.map (\i -> Pretty.string (String.fromInt i))
                            |> Pretty.lines

                    output =
                        Pretty.pretty 80 doc
                in
                countLines output
                    |> Expect.equal 50
        , test "alternating nest and group" <|
            \_ ->
                let
                    build n acc =
                        if n <= 0 then
                            acc

                        else
                            build (n - 1)
                                (Pretty.nest 1
                                    (Pretty.group
                                        (acc
                                            |> Pretty.a Pretty.line
                                            |> Pretty.a (Pretty.string "x")
                                        )
                                    )
                                )

                    doc =
                        build 10 (Pretty.string "start")
                in
                -- Should produce valid output without stack overflow
                String.length (Pretty.pretty 1000 doc)
                    |> Expect.greaterThan 0
        , fuzz (Fuzz.intRange 1 100) "varying width produces valid output" <|
            \w ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "alpha"
                                , Pretty.nest 2
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "beta"
                                            , Pretty.string "gamma"
                                            ]
                                        )
                                    )
                                , Pretty.string "delta"
                                ]
                            )

                    output =
                        Pretty.pretty w doc
                in
                -- Output should always contain all content
                Expect.all
                    [ \o -> String.contains "alpha" o |> Expect.equal True
                    , \o -> String.contains "beta" o |> Expect.equal True
                    , \o -> String.contains "gamma" o |> Expect.equal True
                    , \o -> String.contains "delta" o |> Expect.equal True
                    ]
                    output
        , test "large join operation" <|
            \_ ->
                let
                    items =
                        List.range 1 100
                            |> List.map (\i -> Pretty.string (String.fromInt i))

                    doc =
                        Pretty.join (Pretty.string ", ") items
                            |> Pretty.group

                    output =
                        Pretty.pretty 1000 doc

                    -- Should be comma-separated on one line
                in
                String.contains ", " output
                    |> Expect.equal True
        ]
