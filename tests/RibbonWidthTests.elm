module RibbonWidthTests exposing (suite)

{-| Tests for the ribbon width feature.

Ribbon width limits the number of non-indentation characters per line.
For a line with indentation `i`, the effective line limit is `min(pageWidth, i + ribbonWidth)`.
-}

import Expect
import Fuzz exposing (Fuzzer)
import Pretty exposing (Doc)
import Test exposing (Test, describe, fuzz, fuzz2, test)



-- SUITE


suite : Test
suite =
    describe "Ribbon Width Tests"
        [ basicRibbonBehaviorTests
        , ribbonWithIndentationTests
        , ribbonVsPageWidthTests
        , ribbonPropertyTests
        , ribbonEdgeCaseTests
        , ribbonComplexStructureTests
        ]



-- FUZZERS


widthFuzzer : Fuzzer Int
widthFuzzer =
    Fuzz.intRange 10 200


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



-- HELPERS


countLines : String -> Int
countLines s =
    if String.isEmpty s then
        0

    else
        List.length (String.split "\n" s)


hasNewline : String -> Bool
hasNewline s =
    String.contains "\n" s


extractContent : String -> String
extractContent s =
    String.filter (\c -> c /= ' ' && c /= '\n') s



-- 1. BASIC RIBBON BEHAVIOR TESTS


basicRibbonBehaviorTests : Test
basicRibbonBehaviorTests =
    describe "Basic Ribbon Behavior"
        [ test "ribbon width breaks line before page width limit" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "hello", Pretty.string "world" ])

                    -- "hello world" = 11 chars
                in
                -- Page width 80, ribbon 10: effective = min(80, 0+10) = 10
                -- 11 > 10, so should break
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 10 } doc
                    |> hasNewline
                    |> Expect.equal True
        , test "content fitting ribbon width stays on one line" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "ab", Pretty.string "cd" ])

                    -- "ab cd" = 5 chars
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 10 } doc
                    |> Expect.equal "ab cd"
        , test "exact ribbon width threshold - fits" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "ab", Pretty.string "cd" ])
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 5 } doc
                    |> Expect.equal "ab cd"
        , test "exact ribbon width threshold - breaks" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "ab", Pretty.string "cd" ])
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 4 } doc
                    |> hasNewline
                    |> Expect.equal True
        ]



-- 2. RIBBON WITH INDENTATION TESTS


ribbonWithIndentationTests : Test
ribbonWithIndentationTests =
    describe "Ribbon with Indentation"
        [ test "ribbon applies relative to indentation" <|
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

                    -- After line break at indent 10, group needs 5 chars
                    -- Effective width = min(80, 10+20) = 30
                    -- At column 10, remaining = 20, "ab cd" (5) fits
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 20 } doc
                    |> Expect.equal "x\n          ab cd"
        , test "deep indentation with small ribbon forces break" <|
            \_ ->
                let
                    doc =
                        Pretty.nest 20
                            (Pretty.string "x"
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "hello"
                                            , Pretty.string "world"
                                            ]
                                        )
                                    )
                            )

                    -- At indent 20, effective = min(80, 20+10) = 30
                    -- "hello world" = 11, at column 20, needs 31 total
                    -- 31 > 30, breaks
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 10 } doc
                    |> String.contains "hello\n"
                    |> Expect.equal True
        , test "ribbon respects align-adjusted column" <|
            \_ ->
                let
                    doc =
                        Pretty.string "prefix: "
                            |> Pretty.a
                                (Pretty.align
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "alpha"
                                            , Pretty.string "beta"
                                            ]
                                        )
                                    )
                                )

                    -- "prefix: " = 8 chars, then align sets indent to 8
                    -- "alpha beta" = 10 chars, at column 8, needs 18 total
                    -- Ribbon 15: effective = min(80, 8+15) = 23, fits
                    -- Ribbon 8: effective = min(80, 8+8) = 16, 18 > 16, breaks
                in
                Expect.all
                    [ \_ ->
                        Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 15 } doc
                            |> Expect.equal "prefix: alpha beta"
                    , \_ ->
                        Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 8 } doc
                            |> hasNewline
                            |> Expect.equal True
                    ]
                    ()
        ]



-- 3. RIBBON VS PAGE WIDTH INTERACTION TESTS


ribbonVsPageWidthTests : Test
ribbonVsPageWidthTests =
    describe "Ribbon vs Page Width Interaction"
        [ test "ribbon width larger than page width is silently capped" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "hello", Pretty.string "world" ])
                in
                -- Ribbon 100 > page 11, so effective = 11, "hello world" fits exactly
                Pretty.prettyWithRibbon { pageWidth = 11, ribbonWidth = 100 } doc
                    |> Expect.equal "hello world"
        , fuzz widthFuzzer "ribbon equals page width matches standard pretty" <|
            \w ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "hello"
                                , Pretty.string "world"
                                ]
                            )
                in
                Pretty.prettyWithRibbon { pageWidth = w, ribbonWidth = w } doc
                    |> Expect.equal (Pretty.pretty w doc)
        , test "page width is hard limit even with large ribbon" <|
            \_ ->
                let
                    doc =
                        Pretty.nest 70
                            (Pretty.string "x"
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "hello"
                                            , Pretty.string "world"
                                            ]
                                        )
                                    )
                            )

                    -- Indent 70, ribbon 50: effective = min(80, 70+50) = 80
                    -- "hello world" = 11, at column 70, needs 81
                    -- 81 > 80, breaks
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 50 } doc
                    |> String.contains "hello\n"
                    |> Expect.equal True
        ]



-- 4. PROPERTY-BASED TESTS


ribbonPropertyTests : Test
ribbonPropertyTests =
    describe "Ribbon Property Tests"
        [ fuzz2 widthFuzzer (Fuzz.intRange 1 50) "larger ribbon never increases line count" <|
            \pageWidth delta ->
                let
                    baseRibbon =
                        max 10 (pageWidth // 2)

                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "hello"
                                , Pretty.string "world"
                                ]
                            )

                    narrowOutput =
                        Pretty.prettyWithRibbon { pageWidth = pageWidth, ribbonWidth = baseRibbon } doc

                    wideOutput =
                        Pretty.prettyWithRibbon { pageWidth = pageWidth, ribbonWidth = baseRibbon + delta } doc
                in
                countLines wideOutput
                    |> Expect.atMost (countLines narrowOutput)
        , fuzz2 widthFuzzer (Fuzz.intRange 1 20) "smaller ribbon never decreases line count" <|
            \pageWidth delta ->
                let
                    baseRibbon =
                        max (delta + 5) (pageWidth // 2)

                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "alpha"
                                , Pretty.string "beta"
                                , Pretty.string "gamma"
                                ]
                            )

                    wideRibbonOutput =
                        Pretty.prettyWithRibbon { pageWidth = pageWidth, ribbonWidth = baseRibbon } doc

                    narrowRibbonOutput =
                        Pretty.prettyWithRibbon { pageWidth = pageWidth, ribbonWidth = baseRibbon - delta } doc
                in
                countLines narrowRibbonOutput
                    |> Expect.atLeast (countLines wideRibbonOutput)
        , fuzz2 widthFuzzer (Fuzz.intRange 5 100) "ribbon width preserves content" <|
            \pageWidth ribbonWidth ->
                let
                    doc =
                        Pretty.group
                            (Pretty.lines
                                [ Pretty.string "abc"
                                , Pretty.string "def"
                                ]
                            )

                    output =
                        Pretty.prettyWithRibbon { pageWidth = pageWidth, ribbonWidth = ribbonWidth } doc
                in
                extractContent output
                    |> Expect.equal "abcdef"
        ]



-- 5. EDGE CASE TESTS


ribbonEdgeCaseTests : Test
ribbonEdgeCaseTests =
    describe "Ribbon Edge Cases"
        [ test "ribbon width zero forces all breaks" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "a", Pretty.string "b" ])
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 0 } doc
                    |> hasNewline
                    |> Expect.equal True
        , test "ribbon width one allows single character per line" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "a", Pretty.string "b" ])

                    -- "a b" = 3 chars, ribbon 1, breaks
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 1 } doc
                    |> hasNewline
                    |> Expect.equal True
        , test "small page width dominates large ribbon" <|
            \_ ->
                let
                    doc =
                        Pretty.group (Pretty.lines [ Pretty.string "hello", Pretty.string "world" ])
                in
                -- Page 5, ribbon 100: effective = min(5, 0+100) = 5
                -- "hello world" = 11 > 5, breaks
                Pretty.prettyWithRibbon { pageWidth = 5, ribbonWidth = 100 } doc
                    |> hasNewline
                    |> Expect.equal True
        ]



-- 6. COMPLEX STRUCTURE TESTS


ribbonComplexStructureTests : Test
ribbonComplexStructureTests =
    describe "Ribbon with Complex Structures"
        [ test "sequential groups each respect ribbon at their position" <|
            \_ ->
                let
                    group1 =
                        Pretty.group (Pretty.lines [ Pretty.string "aa", Pretty.string "bb" ])

                    -- 5 chars flat
                    group2 =
                        Pretty.group (Pretty.lines [ Pretty.string "cc", Pretty.string "dd" ])

                    -- 5 chars flat
                    doc =
                        Pretty.lines [ group1, group2 ]
                in
                -- Ribbon 10: both groups fit independently
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 10 } doc
                    |> Expect.equal "aa bb\ncc dd"
        , test "nested groups with ribbon break from outside in" <|
            \_ ->
                let
                    inner =
                        Pretty.group (Pretty.lines [ Pretty.string "inner1", Pretty.string "inner2" ])

                    -- 13 flat
                    outer =
                        Pretty.group (Pretty.lines [ Pretty.string "outer", inner ])

                    -- 19+ flat
                in
                -- Ribbon 15: outer breaks (19 > 15), inner stays flat (13 < 15)
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 15 } outer
                    |> Expect.equal "outer\ninner1 inner2"
        , test "hang respects ribbon for continuation lines - fits" <|
            \_ ->
                let
                    doc =
                        Pretty.string "call"
                            |> Pretty.a
                                (Pretty.hang 4
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "arg1"
                                            , Pretty.string "arg2"
                                            ]
                                        )
                                    )
                                )

                    -- "call" at column 0, then hang sets indent to column 4
                    -- group "arg1 arg2" = 9 chars, at column 4 = 13 total
                    -- Ribbon 15: effective = min(80, 4+15) = 19, 13 fits
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 15 } doc
                    |> hasNewline
                    |> Expect.equal False
        , test "hang respects ribbon for continuation lines - breaks" <|
            \_ ->
                let
                    doc =
                        Pretty.string "call"
                            |> Pretty.a
                                (Pretty.hang 4
                                    (Pretty.group
                                        (Pretty.lines
                                            [ Pretty.string "arg1"
                                            , Pretty.string "arg2"
                                            ]
                                        )
                                    )
                                )

                    -- "call" at column 0, then hang 4 = align (nest 4 ...)
                    -- align at column 4 with indent 0 creates nest 4, plus the nest 4 from hang = indent 8
                    -- group "arg1 arg2" = 9 chars, at column 4
                    -- Ribbon 4: effective = min(80, 8+4) = 12, remaining = 12-4 = 8, 9 > 8, breaks
                in
                Pretty.prettyWithRibbon { pageWidth = 80, ribbonWidth = 4 } doc
                    |> hasNewline
                    |> Expect.equal True
        ]
