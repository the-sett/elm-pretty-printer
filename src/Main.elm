module Main exposing (..)

import Render
import Text exposing (Doc(..))
import Utils


infixr 6 |+
(|+) : Doc -> Doc -> Doc
(|+) =
    Cat


join : Doc -> List Doc -> Doc
join sep =
    concat << List.intersperse sep


concat : List Doc -> Doc
concat docs =
    Utils.foldr1 (|+) docs
        |> Maybe.withDefault Empty


group : Doc -> Doc
group doc =
    Union (Text.flatten doc) doc


space : Doc
space =
    Char ' '


softline : Doc
softline =
    group line


softbreak : Doc
softbreak =
    group linebreak


line : Doc
line =
    FlatAlt Line space


linebreak : Doc
linebreak =
    FlatAlt Line Empty


text : String -> Doc
text str =
    case str of
        "" ->
            Empty

        s ->
            if String.contains "\n" s then
                Text.string s
            else
                Text (String.length s) s


char : Char -> Doc
char input =
    case input of
        '\n' ->
            line

        _ ->
            Char input



-- potential ideas for joining elts
--   withSpace
--   withLine
--   withSoftline etc.
--
--  unword
--  unline
--
--
-- union
