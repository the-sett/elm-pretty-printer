# elm-pretty-printer

A pretty printing library based off of the [paper by Philip Wadler](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

```
$ cd example
$ elm-make --output=elm.js Example.elm && node run.js
```


# Releases
### 1.0.0

Initial release.

### 2.0.0

Change rendering functions to return "success" value instead of a Result type. Add `hardline`.
