# THEORY.md

This document captures the essential insights and design rationale for the elm-pretty-printer library. It is written for an engineer joining the project who wants to quickly build a working understanding of how the system thinks, not just how it works.

## Origins

This library implements Philip Wadler's "A Prettier Printer" algorithm, extended with features from the Wadler/Leijen pretty-printer tradition. The core insight is that pretty printing is about making choices: when content fits on one line, keep it together; when it doesn't, break it across multiple lines with proper indentation.

**What comes from Wadler's original paper:**
- The `Doc` tree structure with `Empty`, `Concatenate`, `Nest`, `Text`, `Line`, `Union`
- The `group`/`flatten` mechanism for creating layout choices
- The greedy "best" layout algorithm

**What comes from Wadler/Leijen extensions:**
- `Column (Int -> Doc)` and `Nesting (Int -> Doc)` — query current layout context
- `align`, `hang`, `indent` — derived from Column/Nesting
- The tagging system (`Maybe t` on Text) for syntax highlighting

**What differs from both:**
- `Line String String` carries two separator strings rather than just being a newline (see below)

## The Central Abstraction: Doc

A `Doc t` is not a string—it's a *description of possible layouts*. When you build a `Doc`, you're not deciding how it will look; you're describing what choices the printer can make. The `t` parameter allows tagging strings for syntax highlighting.

```elm
type Doc t
    = Empty                     -- nothing
    | Concatenate (Doc t) (Doc t)  -- sequencing
    | Nest Int (Doc t)          -- add indentation for line breaks
    | Text String (Maybe t)     -- literal text, optionally tagged
    | Line String String        -- (hsep, vsep) - see below
    | Union (Doc t) (Doc t)     -- choice: left is flatter (INVARIANT)
    | Nesting (Int -> Doc t)    -- Wadler/Leijen: query indent level
    | Column (Int -> Doc t)     -- Wadler/Leijen: query column position
```

**The `Line` constructor** differs from Wadler's original. In his paper, `Line` is just a newline. Here, `Line hsep vsep` carries two strings:
- `hsep` (horizontal separator): used when flattened onto one line
- `vsep` (vertical separator): printed after the newline when broken

This enables `line` (hsep=" ", vsep="") and `tightline` (hsep="", vsep=""), as well as the `separators` combinator where both are the same string (e.g., `", "`).

**The `Union` constructor** is the heart of the algorithm. It represents a choice between two layouts. Crucially, it has an **invariant**: the left branch must always be "flatter" than the right (see below).

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

**How `flatten` works:**
- `Line hsep _` → `Text hsep` (replaces line break with horizontal separator)
- `Union left _` → `flatten left` (always takes the flatter branch)
- `Nesting fn` → `flatten (fn 0)` (assumes indent level 0)
- `Column fn` → `flatten (fn 0)` (assumes column 0)

The last two are necessary because when flattening, we're computing a hypothetical single-line layout where indentation and column context don't apply in the same way.

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

## Key Insight: The Union Invariant

For the greedy algorithm to work correctly, every `Union left right` must satisfy:

> **The first line of `left` is at least as long as the first line of `right`.**

This invariant is maintained by `group`, which constructs `Union (flatten doc) doc`. Since `flatten` replaces line breaks with spaces (or removes them), the flattened version always has a longer (or equal) first line.

**Why this matters:** The layout algorithm checks only whether the left branch fits. If it does, left is chosen (longer first line is preferred when both fit). If it doesn't fit, right is chosen (must break). The invariant guarantees this greedy choice is always correct—we never need to backtrack.

```elm
-- In Internals.elm, the algorithm:
( i, Union doc1 doc2 ) :: ds ->
    better w k
        (be w k (( i, doc1 ) :: ds))       -- try left (flatter)
        (\_ -> be w k (( i, doc2 ) :: ds)) -- fallback to right

-- 'better' just checks if left fits:
better w k norm thunk =
    if fits (w - k) norm then norm else thunk ()
```

If you manually construct a `Union` that violates this invariant, you may get incorrect layouts.

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

### The Normal Type (Lazy Evaluation)

Wadler's algorithm has two representations:
- `Doc t`: the tree you build with combinators (may contain `Union` choices)
- `Normal t`: the linearized output after layout decisions are made (no choices remain)

```elm
type Normal t
    = NNil
    | NText String (Normal t) (Maybe t)    -- eager: rest already computed
    | LText String (() -> Normal t) (Maybe t)  -- lazy: rest is a thunk
    | NLine Int String (Normal t)
    | LLine Int String (() -> Normal t)
```

The `L*` variants (LText, LLine) defer computation using thunks. This is critical: when checking if the left branch of a `Union` fits, we don't want to fully evaluate the right branch. The `better` function takes the right branch as `(() -> Normal t)`, only forcing it if the left doesn't fit.

### Stack Safety

The `flatten` function uses a queue-based algorithm to handle very long concatenation chains without stack overflow. Instead of recursing into `Concatenate` nodes, it collects them into a work queue.

### Greedy Fitting

The `fits` function short-circuits as soon as it knows the answer:
- Returns `True` immediately when it hits a `Line` (line break means it fits)
- Returns `False` immediately when width goes negative

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
