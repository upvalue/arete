# Arete Scheme

Arete is a simple, embeddable Scheme interpreter written in C++ and itself. It's written with the following goals in mind:

- Cross-platform
- Distills down to two C++ files for ease of inclusion in other projects
- Incremental garbage collector and speed suitable for writing games
- Hygienic macros and modules
- Highly practical feature set (type-checking, generics and methods)

But mostly just for fun!

# Running

`make test` makes the test suite and `./test` runs it. It's a good idea to do this before using Arete as it hasn't been tested on a wide variety of systems yet.