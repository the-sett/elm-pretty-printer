module Debugger exposing (showTheDoc)

import Text exposing (..)


showTheDoc : Int -> Int -> Doc -> String
showTheDoc n curr doc =
    -- Pretty print package to color coordinate children
    case doc of
        FlatAlt x y ->
            "FlatAlt\n" ++ String.repeat n " " ++ showTheDoc (n + 1) curr x ++ String.repeat n " " ++ showTheDoc (n + 1) curr y

        Cat x y ->
            "Cat\n" ++ String.repeat n " " ++ showTheDoc (n + 1) curr x ++ String.repeat n " " ++ showTheDoc (n + 1) curr y

        Nest x y ->
            "Nest\n" ++ String.repeat n " " ++ showTheDoc (n + 1) curr y

        Line ->
            "Line\n"

        Union x y ->
            "Union\n" ++ String.repeat n " " ++ showTheDoc (n + 1) curr x ++ String.repeat n " " ++ showTheDoc (n + 1) curr y

        Column f ->
            "Column\n" ++ String.repeat n " " ++ showTheDoc (n + 1) curr (f curr)

        Columns f ->
            "Columns\n" ++ String.repeat n " " ++ showTheDoc (n + 1) curr (f (Just curr))

        Nesting f ->
            "Nesting\n" ++ String.repeat n " " ++ showTheDoc (n + 1) curr (f curr)

        Empty ->
            "Empty\n"

        Char c ->
            "Char (" ++ String.cons c ")\n"

        Text _ s ->
            "Text (" ++ s ++ ")\n"

        Fail ->
            "FAIL\n"

        _ ->
            Debug.crash "FORMATTING"
