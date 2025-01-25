# CLASSY
More -terrible- Java inheritance support.

## Rationale
Clojure support for inheritance is... there.

I understand that the idea is that for all but the simplest cases you're better served with dropping to Java, but I consider `proxy` not to be an adequate tool for a lot of these cases, not only due to its quite unoptimized implementation but also due to how clunky calling super methods interface methods is.

## Features
The library exposes two macros: `instance` and `defsubclass`. Just read the docs for these; `super-call`
is used in order to access super methods inside their bodies.

The macros lean heavily on `reify` and `deftype`, and indeed share most of their behavior; all classes emitted by these macros have stable names, so there should be no caveats in terms of AOT compilation.