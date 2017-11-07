// image.cpp - saving and loading of heap images

#include <assert.h>

#include "arete.hpp"

// Image structure:

// Magic string
// Heap image -- we iterate over the entire heap, replacing pointers with
//   offsets into the file
// After that
// Globals
// Symbol table (pair of strings and pointers)

namespace arete {

static const char MAGIC_STRING[] = "ARETE-IMAGE\n";

struct ImageHeader {
  char magic[sizeof(MAGIC_STRING)];
};

struct CString {
  size_t bytes;
  char data[1];
};

static void write_header(State& state, FILE* f, HeapValue* v) {

}

static void write_object_postfix(State& state, HeapValue* v, size_t offset) {

}

static void serialize_value(State& state, size_t heap_begin, FILE* f, HeapValue* v) {
  // Write object header.

  // Size of non pointer containing postfix, e.g. sourcelocation for pairs, code for VMFunctions
  size_t postfix_size = 0;
  switch(v->get_type()) {
    case PAIR:


      break;


  }
}

void State::save_image(const std::string& path) {
  FILE *f = fopen(path.c_str(), "wb");
  assert(f);

  ImageHeader hdr;
  strncpy((char*) &hdr.magic, (char*)MAGIC_STRING, sizeof(MAGIC_STRING));

  size_t heap_begin = sizeof(ImageHeader);
  fwrite(&hdr, sizeof(ImageHeader), 1, f);


  fclose(f);
}

}