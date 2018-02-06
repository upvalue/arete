// image.cpp - saving and loading of heap images

// TODO: Save sources

// TODO: Cross-platformity

// TODO: Support for non-semispace collectors. We need to be able to walk the entire heap, which
// is more complex for the incremental collector. Since we're dumping everything, one option
// might be to just create a GCSemispace and compact the entire heap once. However, this would
// still require the semispace collector to walk the incremental heap; it would also be ideal
// if we had a way to compact incremental and possibly inefficient allocations. However, we don't
// always know what the correct size for an object should be.

// TODO: It would be great to get rid of the function_id_to_ptr stuff. The problem is we don't know
// where all c function pointers will be stored in a way such that we can restore them easily.

// It should be possible to associate functions with names rather than pointers

// TODO: Why don't we just insist that each C function has a unique name?

#include <assert.h>
#include <fstream>

#include "arete.hpp"

#define AR_IMG_LOG(expr) AR_LOG(AR_LOG_TAG_IMAGE, "img", expr)

// Image structure:

// Magic string
// Heap image -- we iterate over the entire heap, replacing pointers with
//   offsets into the file

// Symbol table is reconstructed when the reader iterates over the heap

namespace arete {

static const char MAGIC_STRING[] = "#!arete-image\n";

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

  PointerUpdater(size_t begin_, size_t offset_, bool reading_): begin(begin_), offset(offset_),
    reading(reading_) {}
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
    SValue nv(update_heapvalue(v.heap));
    //AR_IMG_LOG("updated pointer from " << (size_t)v.heap << " to " << (size_t) nv.heap);
    return nv;
  }

  template <class T>
  T update_function_pointer(T addr) {
    // if(addr == 0) return (T) 0;
    if(reading) {
      size_t fid = (size_t) addr;
      AR_ASSERT((ptrdiff_t) function_id_to_ptr(fid) != 0);
      return (T) function_id_to_ptr((size_t) fid);
    } else {
     return (T) function_ptr_to_id((size_t) addr);
    }
  }
  
  void update_pointers(HeapValue* heap) {
      c_closure_t apply_vm_ = (c_closure_t)&arete::apply_vm;

    switch(heap->get_type()) {
      case FLONUM: case STRING: case CHARACTER: case BYTEVECTOR: 
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
          //AR_IMG_LOG(static_cast<RecordType*>(heap)->name);
        }
        static_cast<RecordType*>(heap)->apply = update_value(static_cast<RecordType*>(heap)->apply);
        static_cast<RecordType*>(heap)->print = update_value(static_cast<RecordType*>(heap)->print);
        static_cast<RecordType*>(heap)->name = update_value(static_cast<RecordType*>(heap)->name);
        static_cast<RecordType*>(heap)->parent = update_value(static_cast<RecordType*>(heap)->parent);
        static_cast<RecordType*>(heap)->field_names = update_value(static_cast<RecordType*>(heap)->field_names);
        if(static_cast<RecordType*>(heap)->finalizer != 0) {
          static_cast<RecordType*>(heap)->finalizer = update_function_pointer(static_cast<RecordType*>(heap)->finalizer);
        }
        break;
      }

      case FUNCTION:
        static_cast<VMFunction*>(heap)->procedure_addr = apply_interpreter;
        static_cast<Function*>(heap)->name = update_value(static_cast<Function*>(heap)->name);
        static_cast<Function*>(heap)->parent_env = update_value(static_cast<Function*>(heap)->parent_env);
        static_cast<Function*>(heap)->arguments = update_value(static_cast<Function*>(heap)->arguments);
        static_cast<Function*>(heap)->rest_arguments = update_value(static_cast<Function*>(heap)->rest_arguments);
        static_cast<Function*>(heap)->body = update_value(static_cast<Function*>(heap)->body);
        break;

      case VMFUNCTION: {
        static_cast<VMFunction*>(heap)->calls = 0;
        static_cast<VMFunction*>(heap)->procedure_addr = apply_vm_;
        static_cast<VMFunction*>(heap)->name = update_value(static_cast<VMFunction*>(heap)->name);
        static_cast<VMFunction*>(heap)->constants = (VectorStorage*)update_heapvalue(static_cast<VMFunction*>(heap)->constants);
        static_cast<VMFunction*>(heap)->macro_env = update_value(static_cast<VMFunction*>(heap)->macro_env);
        static_cast<VMFunction*>(heap)->sources = (Bytevector*)update_heapvalue(static_cast<VMFunction*>(heap)->sources);
        static_cast<VMFunction*>(heap)->free_variables = (Bytevector*)update_heapvalue(static_cast<VMFunction*>(heap)->free_variables);
        static_cast<VMFunction*>(heap)->code = (Bytevector*)update_heapvalue(static_cast<VMFunction*>(heap)->code);
        break;
      }

      case CLOSURE: {
        static_cast<VMFunction*>(heap)->procedure_addr = apply_vm_;
        static_cast<Closure*>(heap)->function = update_value(static_cast<Closure*>(heap)->function);
        static_cast<Closure*>(heap)->upvalues = (VectorStorage*)update_heapvalue(static_cast<Closure*>(heap)->upvalues);
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
        static_cast<CFunction*>(heap)->procedure_addr = update_function_pointer(static_cast<CFunction*>(heap)->procedure_addr);
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

      case UPVALUE:
        static_cast<Upvalue*>(heap)->U.converted = update_value(static_cast<Upvalue*>(heap)->U.converted);
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
  /** Offset of heap data in the file */
  size_t heap_offset;
  State& state;
  FILE* f;
  PointerUpdater updater;

  // Offset is past the header and globals
  ImageWriter(State& state_, FILE* f_): state(state_), f(f_),
    updater((size_t)state.gc.active->data, (ftell(f_) + (state.globals.size() * sizeof(size_t))),
    false) {

  }

  ~ImageWriter() {}

  void serialize_value(HeapValue* heap) {
    updater.update_pointers(heap);
    fwrite(heap, heap->size, 1, f);
  }

  void write_globals() {
    for(size_t i = 0; i != state.globals.size(); i++) {
      SValue v = updater.update_value(state.globals[i]);
      fwrite(&v.heap, sizeof(HeapValue*), 1, f);
      //fwrite(&globals[i], sizeof(ptrdiff_t), 1, f);
    }
    AR_IMG_LOG("wrote " << state.globals.size() << " globals");
  }

  void write_sources() {
    size_t source_names_count = state.source_names.size();
    fwrite(&source_names_count, sizeof(size_t), 1, f);
    for(size_t i = 0; i != state.source_names.size(); i++) {
      const std::string& source_name = state.source_names.at(i);
      size_t size = source_name.size();

      fwrite(&size, sizeof(size_t), 1, f);
      fwrite(source_name.c_str(), size, 1, f);
    }
    AR_IMG_LOG("wrote " << state.source_names.size() << " source names");
  }

  void walk_heap() {
    char* sweep = state.gc.active->data;
    while(sweep != (state.gc.active->data + state.gc.block_cursor)) {
      HeapValue* v = (HeapValue*) sweep;
      serialize_value(v);
      sweep += v->size;
    }
  }

};

struct ImageReader {
  State& state;
  FILE* f;
  const ImageHeader& hdr;
  PointerUpdater updater;

  ImageReader(State& state_, FILE* f_, ImageHeader& hdr_): state(state_), f(f_), hdr(hdr_),
    updater((ftell(f) + (hdr.global_count * sizeof(size_t))), (size_t)state.gc.active->data, true) {

    }

  ~ImageReader() {}


  void read_globals() {
    for(size_t i = 0; i != hdr.global_count; i++) {
      SValue ptr;
      fread(&ptr.heap, sizeof(HeapValue*), 1, f);
      ptr = updater.update_value(ptr);
      state.globals.push_back(ptr);
    }
  }

  void read_sources() {
    size_t sources;
    fread(&sources, sizeof(size_t), 1, f);
    AR_IMG_LOG("reading " << sources << " sources");
    for(size_t i = 0; i != sources; i++) {
      size_t string_size;
      fread(&string_size, sizeof(size_t), 1, f);
      char* cstr = (char*)calloc(1,string_size+1);

      fread(cstr, string_size, 1, f);
      std::string cxxstring(cstr);
      free(cstr);

      AR_IMG_LOG("source: " << cxxstring);
      state.source_names.push_back(cxxstring);
      state.source_contents.push_back("");

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
          heap->header = BYTEVECTOR;
          heap->size = v.size;

          sweep += v.size;
          state.gc.block_cursor += v.size;
          fseek(f, (long)v.size, SEEK_CUR);
          continue;
        }
      }

      HeapValue* heap = (HeapValue*) sweep;

      fread(heap, v.size, 1, f);

      updater.update_pointers(heap);

      SValue valu(heap);

      state.gc.block_cursor += v.size;
      sweep += v.size;
    }

    // Rebuild symbol table
    sweep = state.gc.active->data;
    size_t symbols_loaded = 0;
    while(sweep != (state.gc.active->data + state.gc.block_cursor)) {
      SValue valu((HeapValue*) sweep);
      if(valu.heap_type_equals(SYMBOL)) {
        std::string key(valu.symbol_name_data());
        state.symbol_table->insert(std::make_pair(key, valu.as<Symbol>()));
        symbols_loaded++;
      }
      sweep += valu.heap->size;
    }

    AR_IMG_LOG(symbols_loaded << " symbols loaded");
  }

};

void State::save_image(const std::string& path) {
  gc.collect();

  FILE *f = fopen(path.c_str(), "wb");
  assert(f);

  ImageHeader hdr;
  memset(&hdr, 0, sizeof(ImageHeader));
  strncpy((char*) &hdr.magic, (char*)MAGIC_STRING, sizeof(MAGIC_STRING));
  hdr.heap_size = gc.block_cursor;
  hdr.is_64_bit = AR_64_BIT;
  hdr.global_count = globals.size();
  hdr.gensym_counter = gensym_counter;

  fwrite(&hdr, sizeof(ImageHeader), 1, f);

  // AR_ASSERT(!gc.vm_stack_used && "image-save called during execution");

  ImageWriter writer(*this, f);
  writer.write_globals();
  AR_IMG_LOG("writing heap beginning at " << writer.updater.offset);
  writer.walk_heap();
  writer.write_sources();

  long bytes = ftell(f);

  std::cout << ";; wrote image: " << path << ' ' << (bytes / 1024) << "kb" << std::endl;

  fclose(f);
}

const char* State::boot_from_image(const std::string& path) {
  FILE* f = fopen(path.c_str(), "rb");

  AR_IMG_LOG("loading " << path);

  return boot_from_image(f);
}

const char* State::boot_from_memory_image(unsigned char* img, size_t size) {
#if AR_OS == AR_POSIX
  FILE* f = fmemopen((void*) img, size, "rb");
  return boot_from_image(f);
#else
  return "booting from memory image not supported on this platform";

#endif
}

const char* State::boot_from_image(FILE* f) {
  AR_ASSERT(!booted);

  // TODO: Grow heap to fit boot image. Just delete the existing block and
  // allocate a new one.

  if(!f) return "failed to load image file";
  assert(f);

  ImageHeader hdr;
  fread(&hdr, sizeof(ImageHeader), 1, f);

  if(strncmp(hdr.magic, MAGIC_STRING, 13) != 0) {
    return "failed to read image header";
  }

  if(hdr.is_64_bit != AR_64_BIT) {
    return "attempted to load image with wrong word size (64-bit on 32-bit or vice versa)";
  }

  AR_IMG_LOG((hdr.is_64_bit ? "64-bit" : "32-bit") << " image with " << hdr.global_count
    << " globals");

  gensym_counter = hdr.gensym_counter;

  AR_IMG_LOG("heap size " << hdr.heap_size);

  ImageReader reader(*this, f, hdr);

  AR_IMG_LOG("reading heap beginning at " << reader.updater.begin << " into " << (size_t)gc.active->data);
  reader.read_globals();
  reader.walk_heap();
  reader.read_sources();

  fclose(f);

  boot_common();

  return 0;
}

bool State::file_is_image(const std::string& path) {
  std::ifstream fs(path.c_str());
  char hdr[sizeof(MAGIC_STRING)];
  fs.read(hdr, sizeof(MAGIC_STRING));

  fs.close();

  return strncmp(hdr, MAGIC_STRING, sizeof(MAGIC_STRING)) == 0;
}

}
