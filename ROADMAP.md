## Support for annotations
Like in `deftype`. It's very doable for class and methods and achievable for the immutable fields as well, but likely not for the private, mutable fields unless we go through the route of recompilation.

## AOT
All bytecode compilation already happens as a side effect of macro-expansion (common practice in Clojure code that defines classes), so we should be safe in that regard. However due to how helper class caching currently works, there is a scenario where one could AOT compile an `instance` of `Foo` + `IBar`, load the repl, and subsequently define another; this would result in two classes with the same name, though of course with different loaders. We limit the scenario to the likelihood of a UUID collision by appending a random salt to the helper class names.

## Refine Implementation
Consider switching from the current quick-and-dirty visitor approach to a recompilation approach in which we intercept the bytecode of the underlying `reify`/`deftype` and recompile it. 

For one, this allows us to expose the immutable `defsubclass` fields in a clean way, but it needs to happen before alpha or we risk breaking people that do chain inheritance (it's currently feasible by moving the immutable fields only away from the visitor and into the shell -under the asssumption that the rule stays as simple as ^:*-mutable -> private- but it requires dumping them into the environment of the impl bodies, another hack on top of a pile of hacks).

The main wins however are that we needn't break encapsulation to access super methods and that it makes for a clear performance win -though the recompilation time needs to be taken into account in non-AOT scenarios-.

Also consider partially replicating the Clojure compiler's logic: ideally we would get it to compile the method bodies for us (doing it ourselves is likely out of scope) and go from there; this would definitely be the cleanest approach, with the least amount of helper classes emitted, but it also requires figuring the inner workings of the compiler.