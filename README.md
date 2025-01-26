# CLASSY
More -terrible- Java inheritance support.

[![Clojars Project](https://img.shields.io/clojars/v/io.github.evenmoreirrelevance/classy.svg)](https://clojars.org/io.github.evenmoreirrelevance/classy)

## Rationale
Although most of the time you won't miss inheritance at all, one of the strengths of Clojure is its ease of interoperation with existing JVM libraries, and some simply require you define subclasses.

Dropping down to Java is what I recommend for all but the simplest of cases: the language plainly has a lot more features supporting the case than my library can realistically aspire to, and given that this kind of code usually lives at the edge between Java and Clojure the extra performance can be worthwhile; tools like `virgil` can keep the experience REPL friendly as well.

However, I believe theat there are cases where staying in Clojure is a good idea, and for these cases I find `proxy` and `gen-class` not to be adequate, both due to their implementation which makes assumptions that don't pan out most cases, and due to their feel which is very different from similar constructs like `reify`, and somewhat unpolished to boot. This library tries to address these cases.

## Features
Only documented vars in the `io.github.evenmoreirrelevance.classy.core` namespace are considered part of the public API.

- `instance` covers similar use cases to `reify` and `proxy`.
- `defsubclass` covers similar use cases to `gen-class` and `deftype`.
- `super-call` lets one access overridden methods.

The design will deliberately be kept similar to `reify` and `deftype`: no new method may be defined in a subclass unless more interfaces are added, class initialization is kept as simple as possible, and by default the output of `defsubclass` isn't open to further extension.
