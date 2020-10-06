module Pretty.Renderer exposing (..)

import Internals exposing (Doc(..), Normal(..))



-- Pretty printing -------------------------------------------------------------


{-| Pretty prints a document trying to fit it as best as possible to the specified
column width of the page.
-}
pretty : Int -> Renderer t a -> Doc t -> a
pretty w handler doc =
    layout handler (Internals.best w 0 doc)


type alias Renderer t a =
    { tagged : t -> String -> List a -> List a
    , untagged : String -> List a -> List a
    , outer : List a -> a
    }


layout : Renderer t a -> Normal t -> a
layout handler normal =
    let
        layoutInner : Normal t -> List a -> List a
        layoutInner normal2 acc =
            case normal2 of
                NNil ->
                    acc

                NText text innerNormal maybeTag ->
                    case maybeTag of
                        Just tag ->
                            layoutInner (innerNormal ()) (handler.tagged tag text acc)

                        Nothing ->
                            layoutInner (innerNormal ()) (handler.untagged text acc)

                NLine i sep innerNormal ->
                    let
                        norm =
                            innerNormal ()
                    in
                    case norm of
                        NLine _ _ _ ->
                            layoutInner (innerNormal ()) (handler.untagged ("\n" ++ sep) acc)

                        _ ->
                            layoutInner (innerNormal ()) (handler.untagged ("\n" ++ Internals.copy i " " ++ sep) acc)
    in
    layoutInner normal []
        |> List.reverse
        |> handler.outer
