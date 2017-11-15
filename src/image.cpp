// image.cpp - saving and loading of heap images

// TODO: Save sources

// TODO: Cross-platformity

// TODO: Support for non-semispace collectors. We need to be able to walk the entire heap, which
// is more complex for the incremental collector. Since we're dumping everything, one option
// might be to just create a GCSemispace and compact the entire heap once. However, this would
// still require the semispace collector to walk the incremental heap; it would also be ideal
// if we had a way to compact incremental and possibly inefficient allocations. However, we don't
// always know what the correct size for an object should be.

#include <assert.h>

#include "arete.hpp"

#define AR_IMG_LOG(expr) ARETE_LOG(ARETE_LOG_TAG_IMAGE, "img", expr)

// Image structure:

// Magic string
// Heap image -- we iterate over the entire heap, replacing pointers with
//   offsets into the file

// Symbol table is reconstructed when the reader iterates over the heap

namespace arete {

#if ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE

static const char MAGIC_STRING[] = "ARETE-IMAGE\n";

struct ImageHeader {
  char magic[sizeof(MAGIC_STRING)] AR_ALIGN;
  size_t heap_size AR_ALIGN;
  bool is_64_bit AR_ALIGN;
  size_t global_count AR_ALIGN;
  size_t gensym_counter AR_ALIGN;
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
    if(v == 0) return 0;
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

  template <class T>
  T update_function_pointer(T addr) {
    if(addr == 0) return (T) 0;
    if(reading) {
      // if(id_to_function.size() )
      //AR_ASSERT("function id out of bounds; did you forget to recompile?" &&
      //  id_to_function.size() > (size_t) addr);
      return (T) function_id_to_ptr((size_t) addr);
      //return (T)(((char*) aslr_address) + ((size_t) addr));
    } else {
      /*
      khiter_t k = kh_get(ar_fn_table, function_to_id3, ((ptrdiff_t) addr));
      if(k == kh_end(function_to_id3)) {
        AR_IMG_LOG("failed to find function id " << (ptrdiff_t) addr);
        AR_ASSERT("failed to find function id; did you forget a Defun?" && k != kh_end(function_to_id3));
      }
      */
      //auto i = function_to_id2->find((ptrdiff_t) addr);
      //AR_ASSERT("failed to find function id; did you forget a Defun?" && i != function_to_id2->end());
      return (T) function_ptr_to_id((size_t) addr);
      //return (T) i->second;
      //return (T)(((char*) addr) - ((size_t) aslr_address));
    }
  }
  
  void update_pointers(HeapValue* heap) {
    switch(heap->get_type()) {
      case FLONUM: case STRING: case CHARACTER: case BLOB: 
        break;

      // One pointer
      case TABLE:
      case VECTOR:
        static_cast<Vector*>(heap)->storage = update_value(static_cast<Vector*>(heap)->storage);
        break;

      // Two pointers
      case PAIR: {
        static_cast<Pair*>(heap)->data_car = update_value(static_cast<Pair*>(heap)->data_car);
        static_cast<Pair*>(heap)->data_cdr = update_value(static_cast<Pair*>(heap)->data_cdr);
        break;
      }

      case RENAME: {
        static_cast<Rename*>(heap)->env = update_value(static_cast<Rename*>(heap)->env);
        static_cast<Rename*>(heap)->expr = update_value(static_cast<Rename*>(heap)->expr);
        static_cast<Rename*>(heap)->gensym = update_value(static_cast<Rename*>(heap)->gensym);
        break;
      }

      case RECORD_TYPE: {
        if(!reading) {
          AR_IMG_LOG(static_cast<RecordType*>(heap)->name);
        }
        static_cast<RecordType*>(heap)->apply = update_value(static_cast<RecordType*>(heap)->apply);
        static_cast<RecordType*>(heap)->print = update_value(static_cast<RecordType*>(heap)->print);
        static_cast<RecordType*>(heap)->name = update_value(static_cast<RecordType*>(heap)->name);
        static_cast<RecordType*>(heap)->parent = update_value(static_cast<RecordType*>(heap)->parent);
        static_cast<RecordType*>(heap)->field_names = update_value(static_cast<RecordType*>(heap)->field_names);
        static_cast<RecordType*>(heap)->finalizer = update_function_pointer(static_cast<RecordType*>(heap)->finalizer);
        break;
      }

      case FUNCTION:
        static_cast<Function*>(heap)->name = update_value(static_cast<Function*>(heap)->name);
        static_cast<Function*>(heap)->parent_env = update_value(static_cast<Function*>(heap)->parent_env);
        static_cast<Function*>(heap)->arguments = update_value(static_cast<Function*>(heap)->arguments);
        static_cast<Function*>(heap)->rest_arguments = update_value(static_cast<Function*>(heap)->rest_arguments);
        static_cast<Function*>(heap)->body = update_value(static_cast<Function*>(heap)->body);
        break;

      case VMFUNCTION: {
        static_cast<VMFunction*>(heap)->name = update_value(static_cast<VMFunction*>(heap)->name);
        static_cast<VMFunction*>(heap)->constants = (VectorStorage*)update_heapvalue(static_cast<VMFunction*>(heap)->constants);
        static_cast<VMFunction*>(heap)->macro_env = update_value(static_cast<VMFunction*>(heap)->macro_env);
        static_cast<VMFunction*>(heap)->sources = (Blob*)update_heapvalue(static_cast<VMFunction*>(heap)->sources);
        static_cast<VMFunction*>(heap)->free_variables = (Blob*)update_heapvalue(static_cast<VMFunction*>(heap)->free_variables);
        break;
      }

      case RECORD: {
        RecordType* rt = static_cast<Record*>(heap)->type;

        if(reading) {
          static_cast<Record*>(heap)->type =
            (RecordType*) update_heapvalue(static_cast<Record*>(heap)->type);
          rt = static_cast<Record*>(heap)->type;
        }
        size_t fc = rt->field_count;
        if(!reading) {
          static_cast<Record*>(heap)->type =
            (RecordType*) update_heapvalue(static_cast<Record*>(heap)->type);
        }

        for(size_t i = 0; i != fc; i++) {
          static_cast<Record*>(heap)->fields[i] =
            update_value(static_cast<Record*>(heap)->fields[i]);
        }
        break;
      }
      case FILE_PORT:
        static_cast<FilePort*>(heap)->path = C_FALSE;
        static_cast<FilePort*>(heap)->input_handle = 0;
        static_cast<FilePort*>(heap)->reader = 0;
        break;

      case CFUNCTION:  {
        static_cast<CFunction*>(heap)->addr = update_function_pointer(static_cast<CFunction*>(heap)->addr);
        static_cast<CFunction*>(heap)->name = update_value(static_cast<CFunction*>(heap)->name);
        static_cast<CFunction*>(heap)->closure = update_value(static_cast<CFunction*>(heap)->closure);
        break;
      }

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
    if(heap->header == BLOB) {
      //std::cout << "Writing blob of size " << heap->size << std::endl;
    }
    fwrite(heap, heap->size, 1, f);
  }

  void write_globals() {
    for(size_t i = 0; i != state.globals.size(); i++) {
      Value v = updater.update_value(state.globals[i]);
      fwrite(&v.heap, sizeof(HeapValue*), 1, f);
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
    // Load all data into heap so that things like e.g. record type field count can be referenced
    // before everything is fully loaded
    int b = ftell(f);
    fread(state.gc.active->data, hdr.heap_size, 1, f);
    fseek(f, b, SEEK_SET);

    char* sweep = state.gc.active->data;

    while(state.gc.block_cursor != hdr.heap_size) {
      int place = ftell(f);

      // allocating a HeapValue on the stack...how naughty
      // Read in the header which contains the object's size
      HeapValue v;
      fread(&v, sizeof(HeapValue), 1, f);
      fseek(f, place, SEEK_SET);

      switch(v.get_type()) {
        case FILE_PORT: {
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
  gc.collect();

  FILE *f = fopen(path.c_str(), "wb");
  assert(f);

  ImageHeader hdr;
  memset(&hdr, 0, sizeof(ImageHeader));
  strncpy((char*) &hdr.magic, (char*)MAGIC_STRING, sizeof(MAGIC_STRING));
  hdr.heap_size = gc.block_cursor;
  hdr.is_64_bit = 0;
  hdr.global_count = globals.size();
  hdr.gensym_counter = gensym_counter;

  fwrite(&hdr, sizeof(ImageHeader), 1, f);

  ImageWriter writer(*this, f);
  writer.updater.reading = false;
  writer.updater.begin = (size_t)gc.active->data;
  writer.updater.offset = ftell(f)  + (globals.size() * sizeof(size_t));
  writer.write_globals();
  AR_IMG_LOG("writing heap beginning at " << writer.updater.offset);
  writer.walk_heap();

  fclose(f);
}

const char* State::boot_from_image(const std::string& path) {
  AR_ASSERT(!booted);

  // TODO: Grow heap to fit boot image. Just delete the existing block and
  // allocate a new one.
  FILE* f = fopen(path.c_str(), "rb");

  AR_IMG_LOG("loading " << path);

  if(!f) return "failed to load image file";
  assert(f);

  ImageHeader hdr;
  fread(&hdr, sizeof(ImageHeader), 1, f);

  AR_IMG_LOG((hdr.is_64_bit ? "64-bit" : "32-bit") << " image with " << hdr.global_count
    << " globals");

  gensym_counter = hdr.gensym_counter;

  if(strncmp(hdr.magic, MAGIC_STRING, 13) != 0) {
    return "failed to read image header";
  }

  AR_IMG_LOG("heap size " << hdr.heap_size);

  ImageReader reader(*this, f);

  reader.hdr = hdr;
  reader.updater.reading = true;
  reader.updater.offset = (size_t)gc.active->data;
  reader.updater.begin = ftell(f) + (hdr.global_count * sizeof(size_t));
  AR_IMG_LOG("reading heap beginning at " << reader.updater.begin << " into " << (size_t)gc.active->data);
  reader.read_globals();
  reader.walk_heap();

  fclose(f);

  boot_common();

  return 0;
}

#else 

void State::save_image(const std::string& path) {
  std::cerr << "image loading disabled on incremental GC";
  exit(EXIT_FAILURE);
}

const char* State::boot_from_image(const std::string& path) {
  return "image loading disabled on incremental GC";
}

#endif

}
