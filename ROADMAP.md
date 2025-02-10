## AOT
We now generate an opaque name for the impl class, with the unique part consisting of a UUID drawn at startup and a gensym drawn at macroexpand time. I consider this to be sufficient.

## Refine Implementation
We went with the hacks on top of more hacks, for the time being. As much of a hack this may be, the current impl is a drop-in replacement for deftype in almost all circumstances.
What we mostly lose out on is performance due to having to incur an allocation for the visitor.

If it's reaasonable to get Clojure to emit the methods' code for us, we could look into doing just that, which would more or less solve all our problems without doing stuff like force-compiling the class and reading it back with alterations. We'd just emit the scaffolding, tap into the compiler for the bodies, and come away with something clean.

## ASM dependency
Three options here:
- depend on `clojure.asm`: avoids pulling in another copy of `asm`, but technically `clojure.asm` is an implementation detail. Clojure itself releases pretty slowly and I don't believe its `asm` version will change much if at all (last change was 7 years ago!), so manually looking for changes to `clojure.asm` for versions beyond `1.11.4` looks very feasible to me.
- depend on `org.ow2.asm`: simple change, but pulls in extra dependency, and can create issues w/ other transitive `asm` dependencies. Actually looks like the least appealing option.
- depend on our own shaded `io.github.evenmoreirrelevance.asm`: cleanest option in terms of not messing with other packages, but potentially requires the most maintainance from my part. Pulls in extra dependency.
