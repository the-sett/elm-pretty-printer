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
