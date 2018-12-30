Arete is an implementation of the Scheme programming language in C++ and itself.

# Why?

Arete was written mostly for fun. I also made it as a platform for experimenting with more radical programming language
design and implementation, but for the moment it's essentially a vanilla Scheme. 

# Usage

Currently:

    make heap.boot
    bin/arete --save-image heap.boot

Will bootstrap the expander and compiler, then save them into a loadable image. Prepending heap.boot will make the
files in examples/ and REPL work as expected, for example:

    bin/arete heap.boot --repl

Will run the read-eval-print loop.

    bin/arete heap.boot examples/life.scm 

Will run a simple Conway's Game of Life simulation.

    make test-all

Will run all tests. Since Arete has not been tested on a wide variety of systems, doing this before
attempting to use it is probably a good idea.

# Dependencies

Arete uses the C++ STL, and optionally
- linenoise-ng from ArangoDB (for nice cross-platform REPL, included in the repository)
- DynASM (for native compilation, included in the repository)
- SDL2
- Python (for building & testing only)

# R?RS/SRFI compliance.

Arete currently supports some features of R5RS and R7RS. Full compliance is not the primary goal at present, but I have tried to hew to the standards. If you miss something, open an issue :)

## Noted issues

- equal? does not handle shared structure
- Continuations not supported
- eval does not support environment arguments
