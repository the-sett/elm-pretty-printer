module Internals exposing (..)

type Doc t
    = Empty
    | Concatenate (Doc t) (Doc t)
    | Nest Int (Doc t)
    | Text String (Maybe t)
    | Line String String
    | Union (Doc t) (Doc t)
    | Nesting (Int -> Doc t)
    | Column (Int -> Doc t)

-- Normal form of documents
-- N* constructors have continuations that are already evaluated 
-- L* constructors have continuations that are delayed (thunks)
type Normal t
    = NNil
    | NText String (Normal t) (Maybe t)
    | LText String (() -> Normal t) (Maybe t)
      --
    | NLine Int String (Normal t)
    | LLine Int String (() -> Normal t)
      
     
-- Internals -------------------------------------------------------------------


updateTag : (String -> Maybe t -> Maybe t) -> Doc t -> Doc t
updateTag updateFn doc =
    case doc of
        Concatenate doc1 doc2 ->
            Concatenate (updateTag updateFn doc1) (updateTag updateFn doc2)

        Nest i doc1 ->
            Nest i (updateTag updateFn doc1)

        Text text maybeTag ->
            Text text (updateFn text maybeTag)

        Union doc1 doc2 ->
            Union (updateTag updateFn doc1) (updateTag updateFn doc2)

        Nesting fn ->
            Nesting (\i -> updateTag updateFn (fn i))

        Column fn ->
            Column (\i -> updateTag updateFn (fn i))

        x ->
            x


flatten : Doc t -> Doc t
flatten doc =
    case doc of
        Concatenate doc1 doc2 ->
            Concatenate (flatten doc1) (flatten doc2)

        Nest i doc1 ->
            Nest i (flatten doc1)

        Union doc1 _ ->
            flatten doc1

        Line hsep _ ->
            Text hsep Nothing

        Nesting fn ->
            flatten (fn 0)

        Column fn ->
            flatten (fn 0)

        x ->
            x


layout : Normal t -> String
layout normal =
    let
        layoutInner : Normal t -> List String -> List String
        layoutInner normal2 acc =
            case normal2 of
                NNil ->
                    acc

                NText text norm _ ->
                    layoutInner norm (text :: acc)
                        
                LText text thunk _ ->
                    let
                        norm = thunk ()
                    in
                        layoutInner norm (text :: acc)

                NLine i sep norm ->
                    case norm of
                        NLine _ _ _ ->
                            layoutInner norm (("\n" ++ sep) :: acc)

                        _ ->
                            layoutInner norm (("\n" ++ String.repeat i " " ++ sep) :: acc)
                    
                LLine i sep thunk ->
                    let norm = thunk ()
                    in 
                        case norm of
                            NLine _ _ _ ->
                                layoutInner norm (("\n" ++ sep) :: acc)

                            _ ->
                                layoutInner norm (("\n" ++ String.repeat i " " ++ sep) :: acc)
    in
    layoutInner normal []
        |> List.reverse
        |> String.concat



best : Int -> Int -> Doc t -> Normal t
best width startCol x = be width startCol [ ( 0, x ) ]

be : Int -> Int -> List ( Int, Doc t ) -> Normal t
be w k docs
    = case docs of
        [] ->
            NNil

        (i, Empty) :: ds  ->
            be w k ds

        (i, Concatenate doc1 doc2) :: ds ->
            be w k ((i, doc1) :: (i, doc2) :: ds)

        (i, Nest j doc) :: ds ->
            be w k ((i + j, doc) :: ds)

        (i, Text text maybeTag) :: ds ->
            LText text (\_ -> be w (k + String.length text) ds) maybeTag

        (i, Line _ vsep) :: ds ->
            LLine i vsep (\_ -> be w (i + String.length vsep) ds)
                                         
        (i, Union doc1 doc2) :: ds ->
            better w
                   k
                   (be w k ((i,doc1)::ds))
                   (\_ -> be w k ((i,doc2)::ds))

        (i, Nesting fn) :: ds ->
             be w k (( i, fn i ) :: ds)

        (i, Column fn) :: ds ->
             be w k (( i, fn k ) :: ds)


better : Int -> Int -> Normal t -> (()->Normal t) -> Normal t
better w k norm thunk =
    case fits (w - k) norm of
        (True, norm1) ->
            norm1
        _ ->
            thunk ()

-- this returns a new normal form to avoid recomputations
fits : Int -> Normal t -> (Bool, Normal t)
fits w norm =
    if w < 0 then
        (False, norm)
    else
        case norm of
            NNil ->
                (True, norm)

            NText text norm1 tag ->
                let
                    (b, rest) = fits (w-String.length text) norm1
                in
                    (b, NText text rest tag)
            LText text thunk tag ->
                let
                    norm1 = thunk ()
                    (b, rest) = fits (w-String.length text) norm1
                in
                    (b, NText text rest tag)

            NLine _ _ _ ->
                (True, norm)
                    
            LLine _ _ _ ->
                (True, norm)
