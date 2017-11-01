Arete is an implementation of the Scheme programming language in C++ and itself.

# Why?

Arete was written mostly for fun. I also made it as a platform for experimenting with more radical programming language
design and implementation, but for the moment it's essentially a vanilla Scheme. 

But here are some things that may differentiate it from other programming language:

- Distributable. Arete can be boiled down to two C++ files which can then be trivially statically linked into a
  program. 

- Embeddable. Arete can be controlled and interacted with from C++ code. Unlike other embeddable languages, Arete's 
  full command line interface is available to C++ programs, making it easier to modify the behavior of compiled
  programs on the fly.

- Helpful error messages. 

- Hygienic macros

- Reasonably fast. Current performance goal is "VM fast" eg. Arete should be competitive with Lua and other
  lightweight VM-based languages.

- Precise garbage collection. Choose between compacting (Cheney) or incremental (lazy sweep).

- Somewhat self-hosting: Arete is bootstrapped on a simple, highly minimal interpreted subset of Scheme, which is
  replaced with a bytecode compiler written in Scheme before user programs are run.

- Windows/MSVC support (not always a given for lisps)

# Dependencies

Arete uses the C++ STL, and optionally the linenoise-ng library from ArangoDB.

# R?RS/SRFI compliance.

Arete currently aims at R5RS compliance minus continuations. It is probable that R7RS modules will be added in the near
future. 

# Future

- Optimizing native code compilation *without* LLVM or large hairy dependencies.
- Generational garbage collection
- Gradual typing, static typing, a new programming language

