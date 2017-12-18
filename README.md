# elm-pretty-printer

A pretty printing library based on ['A Prettier Printer' by Philip Wadler](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

This version follows Wadler's paper closely, except for the addition of Column
and Nesting constructors in the document type, which make for easier and more
flexible indentation.

This implementation is sufficiently lazy and tail-recursive to perform well under
Elm.
