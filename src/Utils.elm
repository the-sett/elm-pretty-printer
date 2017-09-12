module Utils exposing (..)


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


break : (Char -> Bool) -> String -> ( String, String )
break pred str =
    let
        toSplit =
            not << pred
    in
    ( takeWhile toSplit str, dropWhile toSplit str )


takeWhile : (Char -> Bool) -> String -> String
takeWhile shouldTake str =
    let
        take1 ( head, rest ) =
            if shouldTake head then
                String.cons head (takeWhile shouldTake rest)
            else
                ""
    in
    String.uncons str
        |> Maybe.map take1
        |> Maybe.withDefault ""


dropWhile : (Char -> Bool) -> String -> String
dropWhile shouldDrop str =
    let
        drop1 ( head, rest ) =
            if shouldDrop head then
                dropWhile shouldDrop rest
            else
                String.cons head rest
    in
    String.uncons str
        |> Maybe.map drop1
        |> Maybe.withDefault ""
