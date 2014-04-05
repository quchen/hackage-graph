Hackage graph
=============

Inspired by the recent release of [cabal-db][cabal-db], I thought I might as
well yank the graph drawing up to eleven and have a look at the entirety of
Hackage.

The current graph (April 2014) consists of around 6000 nodes with 35000 edges.
Only the dependencies of the latest version of each package are taken into
account; in addition, since virtually everything depends on `base`, `Main.hs`
has a list of packages to ignore.

Ignoring packages with two or less dependencies, here is what the graph looks
like:

[!][out/hackage-3in-small.png]

A higher resolution (18 MiB!) image is available in the [`out`][out/] directory,
as well as the raw file to create it.

Usage
-----

The rather hacky program reads the locally stored package database
`00-index.tar` from a hardcoded location (in `Main.hs`), and generates data in
`.dot` format to represent all packages and which packages they depend on.
The result is written to STDOUT, ready to be plotted with GraphViz compatible
tools such as [Gephi][gephi].

To build and run from scratch, use the usual:
```
cabal sandbox init && cabal install --only-dependencies
cabal run > output.dot
```



[cabal-db]: http://hackage.haskell.org/package/cabal-db
[gephi]: https://gephi.org/