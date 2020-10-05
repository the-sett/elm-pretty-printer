module Pretty.Html exposing (..)

import Css
import Html.Styled as Html exposing (Html)
import Internals exposing (Doc(..), Normal(..))



-- Pretty printing -------------------------------------------------------------


{-| Pretty prints a document trying to fit it as best as possible to the specified
column width of the page.
-}
pretty : Int -> HtmlHandler t msg -> Doc t -> Html msg
pretty w handler doc =
    layout handler (Internals.best w 0 doc)


type alias LayoutHandler t a =
    { tagged : t -> String -> List a -> List a
    , untagged : String -> List a -> List a
    , outer : List a -> a
    }


type alias HtmlHandler t msg =
    LayoutHandler t (Html msg)


type alias StringHandler t =
    LayoutHandler t String


layout : LayoutHandler t a -> Normal t -> a
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
