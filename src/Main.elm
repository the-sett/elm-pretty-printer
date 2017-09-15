module Main exposing (..)

import Render
import Text exposing (Doc(..))
import Utils


append : Doc -> Doc -> Doc
append doc1 doc2 =
    Cat doc1 doc2


concat : Doc -> List Doc -> Doc
concat sep docs =
    let
        appending doc acc =
            append doc (append sep acc)
    in
    Utils.foldr1 appending docs
        |> Maybe.withDefault Empty


space : Doc
space =
    Char ' '


softline : Doc
softline =
    group line


group : Doc -> Doc
group doc =
    Union (Text.flatten doc) doc


line : Doc
line =
    FlatAlt Line space
