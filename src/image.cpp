// image.cpp - saving and loading of heap images

#include "arete.hpp"

namespace arete {

static const char MAGIC_STRING[] = "ARETE-IMAGE";

struct ImageHeader {
  char magic[sizeof(MAGIC_STRING)];
  // symbol table
  // globals
  // cfunction pointer addresses
};

}