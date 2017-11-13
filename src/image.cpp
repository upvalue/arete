// image.cpp - saving and loading of heap images

#include <assert.h>

#include "arete.hpp"

#define AR_IMG_LOG(expr) ARETE_LOG(ARETE_LOG_TAG_IMAGE, "img", expr)

// Image structure:

// Magic string
// Heap image -- we iterate over the entire heap, replacing pointers with
//   offsets into the file

// Symbol table is constructed after reading

namespace arete {

static const char MAGIC_STRING[] = "ARETE-IMAGE\n";

struct ImageHeader {
  char magic[sizeof(MAGIC_STRING)];
  size_t heap_size;
  bool is_64_bit;
  size_t global_count;
};

struct CString {
  size_t bytes;
  char data[1];
};

// Updates pointers as they are read or written
struct PointerUpdater {
  size_t begin, offset;
  bool reading;

  PointerUpdater(size_t begin_, size_t offset_): begin(begin_), offset(offset_), reading(true) {}
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
    //AR_IMG_LOG("updated pointer from " << (size_t)v.heap << " to " << (size_t) nv.heap);
    return nv;
  }
  
  void update_pointers(HeapValue* heap) {
    switch(heap->get_type()) {
      case FLONUM: case STRING: case CHARACTER: case BLOB: 
        break;

      case PAIR: {
        static_cast<Pair*>(heap)->data_car = update_value(static_cast<Pair*>(heap)->data_car);
        static_cast<Pair*>(heap)->data_cdr = update_value(static_cast<Pair*>(heap)->data_cdr);
        break;
      }

      case VECTOR:
        static_cast<Vector*>(heap)->storage = update_value(static_cast<Vector*>(heap)->storage);
        break;

      case FUNCTION:
        static_cast<Function*>(heap)->name = update_value(static_cast<Function*>(heap)->name);
        static_cast<Function*>(heap)->parent_env = update_value(static_cast<Function*>(heap)->name);
        static_cast<Function*>(heap)->arguments = update_value(static_cast<Function*>(heap)->name);
        static_cast<Function*>(heap)->rest_arguments = update_value(static_cast<Function*>(heap)->name);
        static_cast<Function*>(heap)->body = update_value(static_cast<Function*>(heap)->name);
        break;

      case FILE_PORT:
        static_cast<FilePort*>(heap)->path = C_FALSE;
        static_cast<FilePort*>(heap)->input_handle = 0;
        static_cast<FilePort*>(heap)->reader = 0;
        break;
      case CFUNCTION: 
        static_cast<CFunction*>(heap)->name = C_FALSE;
        static_cast<CFunction*>(heap)->addr = 0;

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
        std::cerr << "don't know how to update pointers for type " << (Type) heap->get_type() << " " 
          << (size_t) heap->get_type()  << std::endl;
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

  void serialize_value(HeapValue* heap) {
    updater.update_pointers(heap);
    fwrite(heap, heap->size, 1, f);
  }

  void write_globals() {
    for(size_t i = 0; i != state.globals.size(); i++) {
      Value v = updater.update_value(state.globals[i]);
      fwrite(&v.heap, sizeof(HeapValue*), 1, f);
      AR_IMG_LOG("writing global " << i);
      //fwrite(&globals[i], sizeof(ptrdiff_t), 1, f);
    }
    AR_IMG_LOG("wrote " << state.globals.size() << " globals");
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


  void read_globals() {
    for(size_t i = 0; i != hdr.global_count; i++) {
      Value ptr;
      fread(&ptr.heap, sizeof(HeapValue*), 1, f);
      ptr = updater.update_value(ptr);
      state.globals.push_back(ptr);
    }
  }

  void walk_heap() {
    char* sweep = state.gc.active->data;
    while(state.gc.block_cursor != hdr.heap_size) {
      int place = ftell(f);

      // allocating a HeapValue on the stack...how naughty
      // Read in the header which contains the object's size
      HeapValue v;
      fread(&v, sizeof(HeapValue), 1, f);
      fseek(f, place, SEEK_SET);

      switch(v.get_type()) {
        case FILE_PORT: 
        case CFUNCTION: {
          HeapValue* heap = (HeapValue*) sweep;
          heap->header = BLOB;
          heap->size = v.size;

          sweep += v.size;
          state.gc.block_cursor += v.size;
          fseek(f, v.size, SEEK_CUR);
          continue;
        }
      }

      HeapValue* heap = (HeapValue*) sweep;

      fread(heap, v.size, 1, f);

      updater.update_pointers(heap);

      Value valu(heap);

      state.gc.block_cursor += v.size;
      sweep += v.size;
    }

    // Rebuild symbol table
    sweep = state.gc.active->data;
    size_t symbols_loaded = 0;
    while(sweep != (state.gc.active->data + state.gc.block_cursor)) {
      Value valu((HeapValue*) sweep);
      if(valu.heap_type_equals(SYMBOL)) {
        std::string key(valu.symbol_name_data());
        state.symbol_table->insert(std::make_pair(key, valu.as<Symbol>()));
        symbols_loaded++;
      }
      sweep += valu.heap->size;
    }

    AR_IMG_LOG(symbols_loaded << " symbols loaded");
  }

  ImageHeader hdr;
  PointerUpdater updater;
  State& state;
  FILE* f;
};

void State::save_image(const std::string& path) {
  FILE *f = fopen(path.c_str(), "wb");
  assert(f);

  ImageHeader hdr;
  memset(&hdr, 0, sizeof(ImageHeader));
  strncpy((char*) &hdr.magic, (char*)MAGIC_STRING, sizeof(MAGIC_STRING));
  hdr.heap_size = gc.block_cursor;
  hdr.is_64_bit = ARETE_64_BIT;
  hdr.global_count = globals.size();

  size_t heap_begin = sizeof(ImageHeader);
  fwrite(&hdr, sizeof(ImageHeader), 1, f);

  ImageWriter writer(*this, f);
  writer.updater.begin = (size_t)gc.active->data;
  writer.updater.offset = ftell(f); // + (globals.size() * sizeof(size_t));
  //writer.write_globals();
  AR_IMG_LOG("writing heap beginning at " << writer.updater.offset);
  writer.walk_heap();

  fclose(f);
}

const char* State::load_image(const std::string& path) {
  FILE* f = fopen(path.c_str(), "rb");

  AR_IMG_LOG("loading " << path);

  assert(f);

  ImageHeader hdr;
  fread(&hdr, sizeof(ImageHeader), 1, f);

  AR_IMG_LOG((hdr.is_64_bit ? "64-bit" : "32-bit") << " image with " << hdr.global_count
    << " globals");

  if(strncmp(hdr.magic, MAGIC_STRING, 13) != 0) {
    return "failed to read image header";
  }

  AR_IMG_LOG("heap size " << hdr.heap_size);

  ImageReader reader(*this, f);

  reader.hdr = hdr;
  reader.updater.offset = (size_t)gc.active->data;
  reader.updater.begin = ftell(f); // + (hdr.global_count * sizeof(size_t));
  AR_IMG_LOG("reading heap beginning at " << reader.updater.begin << " into " << (size_t)gc.active->data);
  // reader.read_globals();
  reader.walk_heap();

  fclose(f);

  return 0;
}

}