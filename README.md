Graph of Hackage packages
=========================

The goal of this package is making the graph of Hackage package dependencies
available.

The program is very much hacked together with only that goal only in mind, so
the code is fairly ugly (but short enough so it doesn’t matter).

Taking only the dependencies of the latest version of each package are taken
into account and ignoring packages with two or less dependencies and `base`,
here is what Hackage looked like at the time of creation:

![](out/hackage-3in-small.png)

A higher resolution that includes package names image is available in the
[`out`](out/) directory (18 MiB PNG), as well as the raw [`dot`][dot] file to
create it. (Visualization made with [Gephi][gephi].)

Since there are *lots* of dependencies going all over the place, the graph
is mostly a giant blob. To extract useful information from it probably requires
special focus in an appropriate program.



Usage
-----

The rather program reads the locally stored package database `00-index.tar` from
a hardcoded location (in [`Main.hs`][main]), and generates data in [`dot`][dot]
format to represent all packages and which packages they depend on. The result
is written to STDOUT, ready to be plotted with GraphViz compatible tools such as
[Gephi][gephi].

(There is another program to include a giant `.cabal` file that makes the core of
the `acme-everything` package, but that’s just a joke and nobody should ever use
it.)

To build and run from scratch, proceed as usual:

```
stack build --exec hackage-graph-as-dot > output.dot
```



[cabal-db]: http://hackage.haskell.org/package/cabal-db
[dot]:      https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29
[gephi]:    https://gephi.org/
[travis]:   https://travis-ci.org/quchen/hackage-graph
[main]:     src/Main.hs
