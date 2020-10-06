module Pretty.Renderer exposing (..)

import Internals exposing (Doc(..), Normal(..))



-- Pretty printing -------------------------------------------------------------


{-| Pretty prints a document trying to fit it as best as possible to the specified
column width of the page.
-}
pretty : Int -> Renderer t a b -> Doc t -> b
pretty w handler doc =
    layout handler (Internals.best w 0 doc)


type alias Renderer t a b =
    { init : a
    , tagged : t -> String -> a -> a
    , untagged : String -> a -> a
    , newline : a -> a
    , outer : a -> b
    }


layout : Renderer t a b -> Normal t -> b
layout handler normal =
    let
        layoutInner : Normal t -> a -> a
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
                            case sep of
                                "" ->
                                    layoutInner (innerNormal ())
                                        (handler.newline acc)

                                _ ->
                                    layoutInner (innerNormal ())
                                        (handler.untagged sep (handler.newline acc))

                        _ ->
                            layoutInner (innerNormal ())
                                (handler.untagged (Internals.copy i " " ++ sep) (handler.newline acc))
    in
    layoutInner normal handler.init
        --|> List.reverse
        |> handler.outer
