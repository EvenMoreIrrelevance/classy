## Support for annotations
Test on public fields; todo: forcibly remove them from the impls' bodies.

## AOT
We now generate an opaque name for the impl class, with the unique part consisting of a UUID and a gensym. I consider this to be sufficient.

## Refine Implementation
We went with the hacks on top of more hacks, for the time being. As much of a hack this may be, the current impl is a drop-in replacement for deftype in almost all circumstances.
What we mostly lose out on is performance, but there are avenues for optimization (e.g. use a single instance of the impl on the class if the impl has no state (e.g. `defsubclass` with no private fields - DONE!).

If it's reaasonable to get Clojure to emit the methods' code for us, we could look into doing just that, which would more or less solve all our problems without doing stuff like force-compiling the class and reading it back with alterations. We'd just emit the scaffolding, tap into the compiler for the bodies, and come away with something clean.
