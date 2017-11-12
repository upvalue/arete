// image.cpp - saving and loading of heap images

#include <assert.h>

#include "arete.hpp"

#define AR_IMG_LOG(expr) ARETE_LOG(ARETE_LOG_TAG_IMAGE, "img", expr)

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
  size_t heap_size;
};

struct CString {
  size_t bytes;
  char data[1];
};

// Updates pointers as they are read or written
struct PointerUpdater {
  size_t begin, offset;

  PointerUpdater(size_t begin_, size_t offset_): begin(begin_), offset(offset_) {}
  PointerUpdater() {}
  ~PointerUpdater() {}

  HeapValue* update_heapvalue(HeapValue* v) {
    char* ptr = (char*) v;
    ptr -= (size_t) begin;
    return (HeapValue*)(offset + ptr);
  }

  Value update_value(Value v) {
    if(v.immediatep()) return v;
    Value nv(update_heapvalue(v.heap));
    return nv;
  }

  void update_pointers(HeapValue* heap) {
    switch(heap->get_type()) {
      case STRING: 
        break;
      case PAIR: {
        static_cast<Pair*>(heap)->data_car = update_value(static_cast<Pair*>(heap)->data_car);
        static_cast<Pair*>(heap)->data_cdr = update_value(static_cast<Pair*>(heap)->data_cdr);
        break;
      }
      case VECTOR:
        static_cast<Vector*>(heap)->storage = update_value(static_cast<Vector*>(heap)->storage);
        break;
      case VECTOR_STORAGE:
        for(size_t i = 0; i != static_cast<VectorStorage*>(heap)->length; i++) {
          static_cast<VectorStorage*>(heap)->data[i] =
            update_value(static_cast<VectorStorage*>(heap)->data[i]);
        }
        break;
      case SYMBOL:
        static_cast<Symbol*>(heap)->name = update_value(static_cast<Symbol*>(heap)->name);
        static_cast<Symbol*>(heap)->value = update_value(static_cast<Symbol*>(heap)->value);

        break;
      default:
        std::cerr << "don't know how to write type " << (Type) heap->get_type() << std::endl;
        AR_ASSERT(!"image writer/reader doesn't know how to write type");
        break;
    }
  }
};

struct ImageWriter {
  ImageWriter(State& state_, FILE* f_): state(state_), f(f_) {}
  ~ImageWriter() {}

  void write_header() {

  }

  HeapValue* update_heapvalue(HeapValue* v) {
    char* ptr = (char*) v;
    ptr -= (size_t)state.gc.active->data;
    return (HeapValue*)(heap_offset + ptr);
  }

  Value update_value(Value v) {
    if(v.immediatep()) {
      return v;
    } 
    Value nv(update_heapvalue(v.heap));
    AR_IMG_LOG("write ptr " << ((size_t) v.heap) << " as " << ((size_t) nv.heap));
    return nv;
  }

  void serialize_value(HeapValue* heap) {
    updater.update_pointers(heap);
    fwrite(heap, heap->size, 1, f);
  }

  void walk_heap() {
    char* sweep = state.gc.active->data;
    while(sweep != (state.gc.active->data + state.gc.block_cursor)) {
      HeapValue* v = (HeapValue*) sweep;
      serialize_value(v);
      sweep += v->size;
    }
  }

  /** Offset of heap data in the file */
  PointerUpdater updater;
  size_t heap_offset;
  State& state;
  FILE* f;
};

struct ImageReader {
  ImageReader(State& state_, FILE* f_): state(state_), f(f_) {}
  ~ImageReader() {}

  HeapValue* update_heapvalue(HeapValue* v) {
    char* ptr = (char*) v;
    ptr -= heap_offset;
    return (HeapValue*)(state.gc.active->data + (size_t)ptr);
    //return (HeapValue*)(state.gc.active->data + (size_t)(((char*) v) - heap_offset));
  }

  Value update_value(Value v) {
    if(v.immediatep()) return v;
    Value nv(update_heapvalue(v.heap));
    AR_IMG_LOG("read " << ((size_t) v.heap) << " to " << ((size_t) nv.heap));
    return nv;
  }

  void walk_heap() {
    char* sweep = state.gc.active->data;
    while(state.gc.block_cursor != heap_size) {
      int place = ftell(f);

      // allocating a HeapValue on the stack...how naughty
      // Read in the header which contains the object's size
      HeapValue v;
      fread(&v, sizeof(HeapValue), 1, f);
      fseek(f, place, SEEK_SET);

      HeapValue* heap = (HeapValue*) sweep;

      fread(heap, v.size, 1, f);

      updater.update_pointers(heap);

      Value vug(heap);
      std::cout << vug << std::endl;

      state.gc.block_cursor += v.size;
      sweep += v.size;
    }
  }

  PointerUpdater updater;
  size_t heap_size;
  size_t heap_offset;
  State& state;
  FILE* f;
};

void State::save_image(const std::string& path) {
  FILE *f = fopen(path.c_str(), "wb");
  assert(f);

  ImageHeader hdr;
  strncpy((char*) &hdr.magic, (char*)MAGIC_STRING, sizeof(MAGIC_STRING));
  hdr.heap_size = gc.block_cursor;

  size_t heap_begin = sizeof(ImageHeader);
  fwrite(&hdr, sizeof(ImageHeader), 1, f);

  ImageWriter writer(*this, f);
  writer.heap_offset = heap_begin;
  writer.updater.begin = (size_t)gc.active->data;
  writer.updater.offset = heap_begin;
  writer.walk_heap();

  fclose(f);
}

const char* State::load_image(const std::string& path) {
  FILE* f = fopen(path.c_str(), "rb");

  AR_IMG_LOG("loading " << path);

  assert(f);

  ImageHeader hdr;
  fread(&hdr, sizeof(ImageHeader), 1, f);

  if(strncmp(hdr.magic, MAGIC_STRING, 13) != 0) {
    return "failed to read image header";
  }

  AR_IMG_LOG("heap size " << hdr.heap_size);

  ImageReader reader(*this, f);

  reader.heap_size = hdr.heap_size;
  reader.heap_offset = sizeof(ImageHeader);
  reader.updater.begin = sizeof(ImageHeader);
  reader.updater.offset = (size_t)gc.active->data;
  reader.walk_heap();

  fclose(f);

  return 0;
}

}