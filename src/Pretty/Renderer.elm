module Pretty.Renderer exposing (pretty, prettyWithRibbon, Renderer)

{-| Custom rendering for syntax highlighting or structured output.

Use `Pretty.pretty` for plain text. Use this module to:

  - Render to HTML with syntax highlighting
  - Handle tagged strings differently based on their tags
  - Produce any accumulator-based output format

@docs Renderer, pretty, prettyWithRibbon

-}

import Internals exposing (Doc(..), Normal(..))



-- Pretty printing -------------------------------------------------------------


{-| Renders a document using a custom `Renderer`.

    pretty 80 htmlRenderer doc

-}
pretty : Int -> Renderer t a b -> Doc t -> b
pretty w handler doc =
    layout handler (Internals.best w 0 doc)


{-| Pretty prints with both page width and ribbon width constraints.

The ribbon width limits non-indentation characters per line. At indentation
level `i`, the effective width is `min(pageWidth, i + ribbonWidth)`.

-}
prettyWithRibbon : { pageWidth : Int, ribbonWidth : Int } -> Renderer t a b -> Doc t -> b
prettyWithRibbon { pageWidth, ribbonWidth } handler doc =
    let
        effectiveRibbon =
            min ribbonWidth pageWidth
    in
    layout handler (Internals.bestWithRibbon pageWidth effectiveRibbon 0 doc)


{-| Accumulator-based renderer, similar to `foldl`.

  - `init`: Starting accumulator value
  - `tagged`: Handle tagged text (for syntax highlighting)
  - `untagged`: Handle untagged text and indentation
  - `newline`: Handle line breaks (add `\n` here if needed)
  - `outer`: Transform final accumulator to output type

Text is processed left-to-right; reverse lists if accumulating in order.

    htmlRenderer : Renderer Tag (List (Html msg)) (Html msg)
    htmlRenderer =
        { init = []
        , tagged = \tag text acc -> styledSpan tag text :: acc
        , untagged = \text acc -> Html.text text :: acc
        , newline = \acc -> Html.br [] [] :: acc
        , outer = \acc -> Html.pre [] (List.reverse acc)
        }

-}
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

                NText text inner maybeTag ->
                    case maybeTag of
                        Just tag ->
                            layoutInner inner (handler.tagged tag text acc)

                        Nothing ->
                            layoutInner inner (handler.untagged text acc)

                LText text thunk maybeTag ->
                    case maybeTag of
                        Just tag ->
                            layoutInner (thunk ()) (handler.tagged tag text acc)

                        Nothing ->
                            layoutInner (thunk ()) (handler.untagged text acc)

                NLine i sep norm ->
                    case norm of
                        NLine _ _ _ ->
                            case sep of
                                "" ->
                                    layoutInner norm
                                        (handler.newline acc)

                                _ ->
                                    layoutInner norm
                                        (handler.untagged sep (handler.newline acc))

                        _ ->
                            layoutInner norm
                                (handler.untagged (String.repeat i " " ++ sep) (handler.newline acc))

                LLine i sep thunk ->
                    let
                        norm =
                            thunk ()
                    in
                    case norm of
                        NLine _ _ _ ->
                            case sep of
                                "" ->
                                    layoutInner norm
                                        (handler.newline acc)

                                _ ->
                                    layoutInner norm
                                        (handler.untagged sep (handler.newline acc))

                        _ ->
                            layoutInner norm
                                (handler.untagged (String.repeat i " " ++ sep) (handler.newline acc))
    in
    layoutInner normal handler.init
        |> handler.outer
