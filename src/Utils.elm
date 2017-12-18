module Utils
    exposing
        ( foldr1
        , hasWhitespace
        , spaces
        , splitOnWhitespace
        )

import Regex


spaces : Int -> String
spaces n =
    if n <= 0 then
        ""
    else
        String.repeat n " "


foldr1 : (a -> a -> a) -> List a -> Maybe a
foldr1 f xs =
    let
        foldMaybes elt acc =
            acc
                |> Maybe.map (f elt)
                |> Maybe.withDefault elt
                |> Just
    in
        List.foldr foldMaybes Nothing xs


splitOnWhitespace : String -> List String
splitOnWhitespace =
    Regex.split Regex.All (Regex.regex regexWhitespace)


hasWhitespace : String -> Bool
hasWhitespace =
    Regex.contains (Regex.regex regexWhitespace)


regexWhitespace : String
regexWhitespace =
    whitespace
        |> List.map ((++) "\\")
        |> String.join "|"
        |> (++) "("
        |> flip (++) ")"


whitespace : List String
whitespace =
    [ "\n", "\t", " " ]
