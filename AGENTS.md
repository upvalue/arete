# arete

arete is a simple Scheme implementation written in C++.

# Basics

Arete is a self-bootstrapping implementation where the initial layer is written
in a limited, interpreted dialect of Scheme, and this is used to write the
macro expander, a bytecode compiler and much more of the library.

To build the base system

> make bin/arete

To bootstrap

bin/arete boot.scm

To run other code

bin/arete heap.boot other-code.scm

# Tickets

Use `ticket --help` to learn about the ticketing subsystem and `ticket list` to
see existing tickets.

# Wiki

In docs/ there is a markdown wiki. Use [[WikiLinks]] to link out to other
documentation and you may list the directory and read the files if any seem
relevant. The wiki should be for evergreen content and intended to be read by
agents -- include information
