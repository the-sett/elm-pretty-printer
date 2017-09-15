module Main exposing (..)

import Render
import Text exposing (Doc(..))
import Utils


-- TODO:
--    Find way to discourage users from doing stuff like:
--      append (text "hello\n") (text "world")
--    Make it difficult for people to do things like that
--    or provide a convenient way to append with lines


append : Doc -> Doc -> Doc
append doc1 doc2 =
    Cat doc1 doc2


(|+) : Doc -> Doc -> Doc
(|+) =
    append


join : Doc -> List Doc -> Doc
join sep =
    concat << List.intersperse sep


concat : List Doc -> Doc
concat docs =
    Utils.foldr1 append docs
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
