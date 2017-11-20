Arete is an implementation of the Scheme programming language in C++ and itself.

# Why?

Arete was written mostly for fun. I also made it as a platform for experimenting with more radical programming language
design and implementation, but for the moment it's essentially a vanilla Scheme. 

# Usage

Currently:

    make heap.boot
    bin/arete --load-image heap.boot

Will bootstrap the expander and compiler, then save them into a loadable image. Prepending --load-image heap.boot will
make the files in examples/ and REPL work as expected.

    make test-all

Will run all tests. Since Arete has not been tested on a wide variety of systems, doing this before
attempting to use it is probably a good idea.

For development, scheme/expand.scm, scheme/syntax.scm and scheme/compiler.scm must be loaded in order. See the files
for details on the various side-effecting things they do in order to bootstrap the system.

# Dependencies

Arete uses the C++ STL, and optionally the linenoise-ng library from ArangoDB.

# R?RS/SRFI compliance.

Arete currently supports some features of R5RS and R7RS. Full compliance is not the primary goal at present, but I have
tried to hew to the standards. If you miss something, open an issue :)

