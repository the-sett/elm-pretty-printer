module FlatAltTests exposing (suite)

{-| Tests for the flatAlt, ifFlat, and ifBreak primitives.

These primitives allow documents to render differently depending on whether
a surrounding group is flattened (single-line) or broken (multi-line).
-}

import Expect
import Fuzz exposing (Fuzzer)
import Pretty exposing (Doc)
import Test exposing (Test, describe, fuzz, fuzz2, test)



-- SUITE


suite : Test
suite =
    describe "FlatAlt Tests"
        [ basicBehaviorTests
        , nestingCompositionTests
        , realWorldPatternTests
        , edgeCaseTests
        , propertyTests
        ]



-- HELPERS


hasNewline : String -> Bool
hasNewline s =
    String.contains "\n" s


extractContent : String -> String
extractContent s =
    String.filter (\c -> c /= ' ' && c /= '\n') s



-- 1. BASIC BEHAVIOR TESTS


basicBehaviorTests : Test
basicBehaviorTests =
    describe "Basic Behavior"
        [ test "flatAlt chooses flat when group fits" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt (Pretty.string "FLAT") (Pretty.string "BROKEN"))
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "FLAT"
        , test "flatAlt chooses broken when group doesn't fit" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt (Pretty.string "FLAT-TOO-LONG") (Pretty.string "BROKE"))
                in
                -- Width 5 means "FLAT-TOO-LONG" (13 chars) won't fit
                Pretty.pretty 5 doc
                    |> Expect.equal "BROKE"
        , test "flatAlt without group always uses broken" <|
            \_ ->
                let
                    doc =
                        Pretty.flatAlt (Pretty.string "FLAT") (Pretty.string "BROKEN")
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "BROKEN"
        , test "ifFlat includes content when flat" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.string "hello"
                                |> Pretty.a (Pretty.ifFlat (Pretty.string "!"))
                            )
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "hello!"
        , test "ifFlat excludes content when broken" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.string "hello"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "world")
                                |> Pretty.a (Pretty.ifFlat (Pretty.string "!"))
                            )
                in
                -- Width 5 forces break
                Pretty.pretty 5 doc
                    |> String.contains "!"
                    |> Expect.equal False
        , test "ifBreak excludes content when flat" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.string "hello"
                                |> Pretty.a (Pretty.ifBreak (Pretty.string "?"))
                            )
                in
                Pretty.pretty 80 doc
                    |> Expect.equal "hello"
        , test "ifBreak includes content when broken" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.string "hello"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "world")
                                |> Pretty.a (Pretty.ifBreak (Pretty.string "?"))
                            )
                in
                -- Width 5 forces break
                Pretty.pretty 5 doc
                    |> String.contains "?"
                    |> Expect.equal True
        ]



-- 2. NESTING AND COMPOSITION TESTS


nestingCompositionTests : Test
nestingCompositionTests =
    describe "Nesting and Composition"
        [ test "nested flatAlt within flat alternative" <|
            \_ ->
                -- Outer group fits, so outer flatAlt chooses its flat branch
                -- Inner group also fits, so inner flatAlt chooses "A"
                let
                    inner =
                        Pretty.group (Pretty.flatAlt (Pretty.string "A") (Pretty.string "B"))

                    outer =
                        Pretty.group (Pretty.flatAlt inner (Pretty.string "C"))
                in
                Pretty.pretty 80 outer
                    |> Expect.equal "A"
        , test "flatAlt inside nest - indentation applies correctly" <|
            \_ ->
                let
                    content =
                        Pretty.flatAlt
                            (Pretty.string "flat")
                            (Pretty.string "line1"
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "line2")
                            )

                    doc =
                        Pretty.string "{"
                            |> Pretty.a (Pretty.nest 4 (Pretty.line |> Pretty.a content))
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.string "}")
                in
                -- No group, so broken mode - should have indentation on line2
                Pretty.pretty 80 doc
                    |> Expect.equal "{\n    line1\n    line2\n}"
        , test "flatAlt combined with line" <|
            \_ ->
                -- Test the exposing list pattern from the issues doc
                let
                    content =
                        Pretty.string "Foo, Bar"

                    flatVersion =
                        Pretty.char '(' |> Pretty.a content |> Pretty.a (Pretty.char ')')

                    brokenVersion =
                        Pretty.string "( "
                            |> Pretty.a content
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.char ')')

                    doc =
                        Pretty.group (Pretty.flatAlt flatVersion brokenVersion)
                in
                -- Wide: should use flat version
                Expect.all
                    [ \_ ->
                        Pretty.pretty 80 doc
                            |> Expect.equal "(Foo, Bar)"
                    , \_ ->
                        Pretty.pretty 5 doc
                            |> Expect.equal "( Foo, Bar\n)"
                    ]
                    ()
        , test "multiple flatAlt in same group use same mode" <|
            \_ ->
                let
                    doc =
                        Pretty.group
                            (Pretty.flatAlt (Pretty.string "A") (Pretty.string "a")
                                |> Pretty.a (Pretty.flatAlt (Pretty.string "B") (Pretty.string "b"))
                            )
                in
                Expect.all
                    [ \_ ->
                        Pretty.pretty 80 doc
                            |> Expect.equal "AB"
                    , \_ ->
                        Pretty.pretty 1 doc
                            |> Expect.equal "ab"
                    ]
                    ()
        , test "flatAlt in different groups independently decide" <|
            \_ ->
                let
                    group1 =
                        Pretty.group (Pretty.flatAlt (Pretty.string "FLAT1") (Pretty.string "BROKE1"))

                    group2 =
                        Pretty.group (Pretty.flatAlt (Pretty.string "FLAT2-LONGER") (Pretty.string "B2"))

                    doc =
                        group1 |> Pretty.a (Pretty.string " ") |> Pretty.a group2
                in
                -- Width 10: "FLAT1" (5) + " " (1) + "FLAT2-LONGER" (12) = 18, too long
                -- But group1 fits on its own (5), group2 doesn't (12)
                -- So group1 stays flat, group2 breaks
                Pretty.pretty 10 doc
                    |> Expect.equal "FLAT1 B2"
        ]



-- 3. REAL-WORLD PATTERN TESTS


realWorldPatternTests : Test
realWorldPatternTests =
    describe "Real-World Patterns"
        [ test "exposing list parens pattern" <|
            \_ ->
                let
                    -- Flat: (Foo, Bar)
                    -- Broken: ( Foo
                    --         , Bar
                    --         )
                    items =
                        Pretty.separators ", " [ Pretty.string "Foo", Pretty.string "Bar" ]

                    flatVersion =
                        Pretty.char '(' |> Pretty.a items |> Pretty.a (Pretty.char ')')

                    brokenVersion =
                        Pretty.string "( "
                            |> Pretty.a items
                            |> Pretty.a Pretty.tightline
                            |> Pretty.a (Pretty.char ')')

                    doc =
                        Pretty.group (Pretty.flatAlt flatVersion brokenVersion)
                in
                Expect.all
                    [ \_ ->
                        Pretty.pretty 80 doc
                            |> Expect.equal "(Foo, Bar)"
                    , \_ ->
                        Pretty.pretty 5 doc
                            |> Expect.equal "( Foo\n, Bar\n)"
                    ]
                    ()
        , test "backward pipe operator placement pattern" <|
            \_ ->
                let
                    -- Flat: foo <| bar
                    -- Broken: foo
                    --         <| bar
                    pipeLeft lhs rhs =
                        Pretty.group
                            (lhs
                                |> Pretty.a
                                    (Pretty.flatAlt
                                        (Pretty.string " <| ")
                                        (Pretty.line |> Pretty.a (Pretty.string "<| "))
                                    )
                                |> Pretty.a rhs
                            )

                    doc =
                        pipeLeft (Pretty.string "foo") (Pretty.string "bar")
                in
                Expect.all
                    [ \_ ->
                        Pretty.pretty 80 doc
                            |> Expect.equal "foo <| bar"
                    , \_ ->
                        Pretty.pretty 5 doc
                            |> Expect.equal "foo\n<| bar"
                    ]
                    ()
        , test "trailing comma pattern" <|
            \_ ->
                let
                    -- Items have trailing comma only when multi-line
                    item s =
                        Pretty.string s |> Pretty.a (Pretty.ifBreak (Pretty.char ','))

                    doc =
                        Pretty.group
                            (Pretty.char '['
                                |> Pretty.a (Pretty.nest 2 (Pretty.line |> Pretty.a (Pretty.lines [ item "a", item "b" ])))
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char ']')
                            )
                in
                Expect.all
                    [ \_ ->
                        Pretty.pretty 80 doc
                            |> Expect.equal "[ a b ]"
                    , \_ ->
                        Pretty.pretty 5 doc
                            |> Expect.equal "[\n  a,\n  b,\n]"
                    ]
                    ()
        , test "record brace spacing pattern" <|
            \_ ->
                let
                    -- Flat: { foo = 1 }
                    -- Broken: { foo = 1
                    --         , bar = 2
                    --         }
                    fields =
                        Pretty.lines [ Pretty.string "foo = 1", Pretty.string "bar = 2" ]

                    flatVersion =
                        Pretty.string "{ "
                            |> Pretty.a fields
                            |> Pretty.a (Pretty.string " }")

                    brokenVersion =
                        Pretty.string "{ "
                            |> Pretty.a fields
                            |> Pretty.a Pretty.line
                            |> Pretty.a (Pretty.char '}')

                    doc =
                        Pretty.group (Pretty.flatAlt flatVersion brokenVersion)
                in
                Expect.all
                    [ \_ ->
                        Pretty.pretty 80 doc
                            |> Expect.equal "{ foo = 1 bar = 2 }"
                    , \_ ->
                        Pretty.pretty 10 doc
                            |> Expect.equal "{ foo = 1\nbar = 2\n}"
                    ]
                    ()
        ]



-- 4. EDGE CASE TESTS


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge Cases"
        [ test "empty document in flat position" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt Pretty.empty (Pretty.string "broken"))
                in
                Pretty.pretty 80 doc
                    |> Expect.equal ""
        , test "empty document in broken position" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt (Pretty.string "flat") Pretty.empty)
                in
                -- Width 1 forces break, broken version is empty
                Pretty.pretty 1 doc
                    |> Expect.equal ""
        , test "width exactly at threshold - fits" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt (Pretty.string "12345") (Pretty.string "broke"))
                in
                -- "12345" is exactly 5 chars, width is 5
                Pretty.pretty 5 doc
                    |> Expect.equal "12345"
        , test "width exactly at threshold - doesn't fit" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt (Pretty.string "123456") (Pretty.string "broke"))
                in
                -- "123456" is 6 chars, width is 5
                Pretty.pretty 5 doc
                    |> Expect.equal "broke"
        , test "flatAlt with ribbon width" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt (Pretty.string "FLAT-CONTENT") (Pretty.string "B"))
                in
                -- Page width 80, ribbon 5: effective = min(80, 0+5) = 5
                -- "FLAT-CONTENT" (12 chars) > 5
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 5 } doc
                    |> Expect.equal "B"
        ]



-- 5. PROPERTY TESTS


propertyTests : Test
propertyTests =
    describe "Property Tests"
        [ fuzz2 shortStringFuzzer shortStringFuzzer "content preservation - flat or broken content preserved" <|
            \flatStr brokenStr ->
                let
                    doc =
                        Pretty.group (Pretty.flatAlt (Pretty.string flatStr) (Pretty.string brokenStr))

                    resultWide =
                        Pretty.pretty 1000 doc

                    -- Width 0 ensures flat version never fits
                    resultNarrow =
                        Pretty.pretty 0 doc
                in
                Expect.all
                    [ \_ ->
                        resultWide
                            |> Expect.equal flatStr
                    , \_ ->
                        resultNarrow
                            |> Expect.equal brokenStr
                    ]
                    ()
        , fuzz shortStringFuzzer "tag propagation via setTag" <|
            \s ->
                -- Tags should be applied to both alternatives
                let
                    doc =
                        Pretty.setTag () (Pretty.flatAlt (Pretty.string s) (Pretty.string s))

                    -- If setTag didn't traverse FlatAlt, this would crash or behave wrong
                    -- We just verify it compiles and renders correctly
                    result =
                        Pretty.pretty 80 doc
                in
                -- In broken mode (no group), should render the broken version
                result
                    |> Expect.equal s
        ]



-- FUZZERS


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
