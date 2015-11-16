### lrpath

`lrpath` is a little tool that takes a path and in parallel parses
and prints the `LC_RPATH` entries (OS X) or `RPATH` and `RUNPATH` entries
(Linux) that it finds in all object files in the directory tree.

Usage:

    lrpath <path to file or directory>

##### OS X
Runs `otool -l` from your Xcode installation and parses all `LC_RPATH` sections,
so you need a working Xcode installation with working command line tools.

##### Linux
Runs `chrpath -l` and parses its output. You need to have `chrpath` installed.

##### Building
The easiest is to install [Haskell Platform](https://www.haskell.org/platform/)
and run `cabal build` in the source directory. The resulting binary is located
at `dist/build/lrpath/lrpath`.
