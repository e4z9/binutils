### lrpath

`lrpath` is a little tool that takes a path and in parallel parses
and prints the `RPATH` entries that it finds in all object files in
the directory tree.
Currently only works on OS X.

Usage:

    lrpath <path to file or directory>

It internally runs `otool -l` from your Xcode installation and parses all
`LC_RPATH` sections, so you need a working Xcode installation with working
command line tools.
