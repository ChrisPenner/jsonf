# jsonf

Exposes a simple JSON equivalent structure `JSON` with a compnanion datatype `JSONF` where the recursion has been
factored out into a type parameter. This allows `JSONF` to be used with recursion schemes.

This library favours simplicity and teachability over performance and uses Strings, Maps, and Doubles instead of more
complex data-structures.

See [`Data.Aeson.Extra.Recursive`](http://hackage.haskell.org/package/aeson-extra-0.4.1.1/docs/Data-Aeson-Extra-Recursive.html)
for more performant versions.

`JSON` and `Fix JSONF` both implement `ToJSON`/`FromJSON` so you can deserialize json objects from text for easier
usage.
