Hey all, I'm in the process of writing an Elm implementation of Haskell's Wadler/Leijen Pretty Print Library and
am looking for help on a few design decisions. The library itself is similar to a combinator library, allowing
the user to build and combine Doc elements before converting into a String.

I'd like to discourage users from using the newline character like so:
```
append (text "hello\n") (text "world")
```
where `text` is just a function that converts a String to a Doc.

Alternatively, I'd like to encourage users to use the appropriate Doc elements if they want to combine
in a particular way
```
append (append (text "hello") softline) (text "world")
```

This is because the library is structured to use specific Doc elements like `softline`, `softbreak`, `line`,
and `linebreak` so that text can get rendered nicely when all is said and done, but newline characters throw
off the library's ability to do proper lookahead.


The Haskell library provides convenient infix functions to combine Docs with the appropriate elements. So
the above snippet in Haskell's version would look like:
```
(text "hello") </> (text "world")
```

I'm trying to avoid using infix operators here as they aren't really intuitive, but can't think of a better
way to provide enough convenience to combine Doc's in this way that will discourage users from using the
`\n` character.
