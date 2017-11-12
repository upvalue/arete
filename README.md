Arete is an implementation of the Scheme programming language in C++ and itself.

# Why?

Arete was written mostly for fun. I also made it as a platform for experimenting with more radical programming language
design and implementation, but for the moment it's essentially a vanilla Scheme. 

# Usage

Currently:

    make arete
    ./arete bootstrap.scm 

Will load the macro-expander and base library (scheme/syntax.scm) and the compiler (scheme/compiler.scm), then compile
all functions, at which point you can load other files (such as the ones in the examples directory) or play with the
REPL.

# Dependencies

Arete uses the C++ STL, and optionally the linenoise-ng library from ArangoDB.

# R?RS/SRFI compliance.

Arete currently aims at R5RS compliance minus continuations. It is probable that R7RS modules will be added in the near
future. 
