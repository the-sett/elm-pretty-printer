# THEORY.md

This document captures the essential insights and design rationale for the elm-pretty-printer library. It is written for an engineer joining the project who wants to quickly build a working understanding of how the system thinks, not just how it works.

## Origins

This library implements Philip Wadler's "A Prettier Printer" algorithm, ported from the Haskell Wadler/Leijen pretty-printer. The core insight is that pretty printing is about making choices: when content fits on one line, keep it together; when it doesn't, break it across multiple lines with proper indentation.

## The Central Abstraction: Doc

A `Doc t` is not a string—it's a *description of possible layouts*. When you build a `Doc`, you're not deciding how it will look; you're describing what choices the printer can make. The `t` parameter allows tagging strings for syntax highlighting.

```elm
type Doc t
    = Empty
    | Concatenate (Doc t) (Doc t)
    | Nest Int (Doc t)
    | Text String (Maybe t)
    | Line String String        -- (hsep, vsep)
    | Union (Doc t) (Doc t)     -- key to the algorithm
    | Nesting (Int -> Doc t)
    | Column (Int -> Doc t)
```

The `Union` constructor is the heart of the algorithm. It represents a choice between two layouts—typically "flattened" (single line) vs "broken" (multiple lines).

## The Two-Phase Model

**Phase 1: Document Construction**
Build an abstract `Doc` tree using combinators. This is lazy—nothing renders yet. You're assembling a specification of layout possibilities.

**Phase 2: Rendering (`pretty w doc`)**
The algorithm walks the `Doc`, making greedy choices about which `Union` branch to take based on whether content fits in the remaining width.

## Key Insight: `nest` vs `indent`

This is the most commonly misunderstood distinction:

- **`nest n doc`**: Adds `n` spaces to all line breaks *inside* `doc`. Does NOT indent the first line.
- **`indent n doc`**: Adds `n` spaces to the first line AND all subsequent lines.

```elm
-- nest: first line unchanged, subsequent lines indented
nest 4 (string "a" |> a line |> a (string "b"))
-- Output:
-- a
--     b

-- indent: everything shifted right
indent 4 (string "a" |> a line |> a (string "b"))
-- Output:
--     a
--     b
```

**Correct pattern for code blocks:**
```elm
-- WRONG: nest around content after lines
lines [ string "{", nest 4 (string "body"), string "}" ]
-- Output: {\nbody\n}  (no indentation!)

-- RIGHT: line break inside the nest
string "{"
    |> a (nest 4 (line |> a (string "body")))
    |> a line
    |> a (string "}")
-- Output: {\n    body\n}
```

## Key Insight: `line` vs `tightline`

Both create hard line breaks, but differ when flattened by `group`:

- **`line`**: Becomes a space when grouped and fits
- **`tightline`**: Becomes empty string when grouped and fits

```elm
group (string "a" |> a line |> a (string "b"))      -- "a b"
group (string "a" |> a tightline |> a (string "b")) -- "ab"
```

Use `tightline` for closing delimiters that should have no space:
```elm
-- Want: "foo(x, y)" or "foo(\n  x,\n  y\n)"
-- Use tightline before closing paren
```

## Key Insight: `group` and the Greedy Algorithm

`group doc` creates a `Union (flatten doc) doc`—it tries the flattened version first, falls back to broken if it doesn't fit.

The algorithm is **greedy**: it makes decisions left-to-right without backtracking. Once it commits to flattening a group, it's committed. This means:

1. **Early content affects later layout**: If you consume width early, later groups have less room.
2. **Width threshold is exact**: Content of length N fits in width N; length N+1 breaks.
3. **Nested groups break outside-in**: Outer group breaks first; inner groups may still fit.

```elm
-- At width 15, outer breaks but inner stays flat
outer = group (lines [ string "outer1", inner ])
inner = group (lines [ string "a", string "b" ])
-- Output: "outer1\na b"
```

## Key Insight: `align` and Column-Relative Indentation

`align doc` indents subsequent lines to the *current column position*, not a fixed amount. This enables hanging indentation:

```elm
string "let " |> a (align (lines [ string "x = 1", string "y = 2" ]))
-- Output:
-- let x = 1
--     y = 2
```

`hang n doc` combines align with additional nesting: `align (nest n doc)`.

## The `join` Family and Empty Filtering

All join functions (`join`, `lines`, `words`, `softlines`, `separators`) filter out `Empty` documents. This prevents double separators:

```elm
words [ string "a", empty, string "b" ]  -- "a b", not "a  b"
```

**Important**: `empty` is not the same as `string ""`. Empty documents are discarded; empty strings are preserved.

## Separators: Leading vs Trailing Style

`separators sep docs` uses a special `Line sep sep` that renders the separator both when flattened and when broken:

```elm
separators ", " [ string "a", string "b", string "c" ] |> group
-- Wide:   "a, b, c"
-- Narrow: "a\n, b\n, c"  (leading comma style)
```

## Performance Considerations

1. **Lazy evaluation**: The `Normal` type uses thunks (`LText`, `LLine`) to defer computation.
2. **Stack safety**: The `flatten` function uses a queue-based algorithm to handle very long concatenation chains without stack overflow.
3. **Greedy fitting**: The `fits` function short-circuits as soon as it knows the answer.

## Algebraic Properties

These properties hold and are verified by tests:

1. **`empty` is identity for `append`**: `append empty doc == doc == append doc empty`
2. **`append` is associative**: `append (append a b) c == append a (append b c)` (in output)
3. **`group` is idempotent**: `group (group doc) == group doc` (in output)
4. **`nest 0` is identity**: `nest 0 doc == doc` (in output)
5. **Width monotonicity**: Larger width never increases line count
6. **Content preservation**: All text content appears in output regardless of width

## Custom Rendering

The `Pretty.Renderer` module enables output beyond plain strings:

```elm
type alias Renderer t a b =
    { init : a
    , tagged : t -> String -> a -> a
    , untagged : String -> a -> a
    , newline : a -> a
    , outer : a -> b
    }
```

This allows building HTML with syntax highlighting, or any accumulator-based output format.

## Common Patterns

**JSON-like arrays:**
```elm
brackets
    (group
        (nest 2
            (tightline
                |> a (join (string "," |> a line) items)
            )
            |> a tightline
        )
    )
-- Wide:   "[1, 2, 3]"
-- Narrow: "[\n  1,\n  2,\n  3\n]"
```

**Function calls:**
```elm
string "function"
    |> a (parens (group (softlines args)))
-- Wide:   "function(a b c)"
-- Narrow: "function(a\nb\nc)"
```

**Code blocks with proper indentation:**
```elm
string "if (cond) {"
    |> a (nest 4 (line |> a (string "body")))
    |> a line
    |> a (string "}")
```

## Mental Model

Think of building a `Doc` as describing a flexible container:
- `string` creates rigid content
- `line` creates a breakable joint (space when flat, newline when broken)
- `nest` says "if you break here, indent the pieces"
- `group` says "try to keep this together if possible"
- `align` says "line up with where I am now"

The printer then pours this flexible structure into a fixed-width container, letting it break at the joints as needed.
