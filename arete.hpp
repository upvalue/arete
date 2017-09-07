// arete.hpp - an embeddable scheme implementation

#ifndef ARETE_HPP
#define ARETE_HPP

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <list>
#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_map>

//= ## Configuration macros
//= As Arete is contained in a single header, it is configured in part through the use of #defines.
//= These should be defined before every inclusion of arete.hpp if changing them is desired.
//= In most cases, the defaults should be sane.

//= ### AR_ASSERT(expression)
//= This assertion is used throughout Arete to check internal invariants. Can be safely turned off.
#ifndef AR_ASSERT
# define AR_ASSERT assert
#endif

//= ### AR_TYPE_ASSERT(expression)
//= This assertion is used internally to type-check [#Value]() method calls. It should probably be
//= left on unless you're sure your code is type-safe.
#ifndef AR_TYPE_ASSERT
# define AR_TYPE_ASSERT assert
#endif 

#define AR_FRAME(state, ...)  \
  arete::FrameHack __arete_frame_ptrs[] = { __VA_ARGS__ };  \
  arete::Frame __arete_frame((state), sizeof(__arete_frame_ptrs) / sizeof(FrameHack), (HeapValue***) __arete_frame_ptrs); 

#define ARETE_BLOCK_SIZE 4096

//= ### ARETE_GC_LOAD_FACTOR = 80
//= If more than ARETE_GC_LOAD_FACTOR percent memory is used after a collection, Arete will allocate
//= more memory up to ARETE_GC_MAX_HEAP_SIZE
#ifndef ARETE_GC_LOAD_FACTOR
# define ARETE_GC_LOAD_FACTOR 80
#endif

#define ARETE_GC_SEMISPACE 0
#define ARETE_GC_INCREMENTAL 1

#ifndef ARETE_GC_STRATEGY
# define ARETE_GC_STRATEGY ARETE_GC_SEMISPACE
#endif 

#ifndef ARETE_LOG_TAGS
# define ARETE_LOG_TAGS 0
#endif 

#define ARETE_LOG_TAG_GC (1 << 0)

#define ARETE_COLOR_RED "\033[1;31m"
#define ARETE_COLOR_RESET "\033[0m"

#ifndef ARETE_LOG
# define ARETE_LOG(tag, prefix, msg) \
  if((ARETE_LOG_TAGS) & (tag)) { \
    std::cout << ARETE_COLOR_RED << "arete:" << prefix << ": " << ARETE_COLOR_RESET << msg << std::endl; \
  }
#endif 

#define ARETE_LOG_GC(msg) ARETE_LOG((ARETE_LOG_TAG_GC), "gc", msg)

namespace arete {

// Forward declarations
struct State;
struct Block;
struct Value;
struct SourceLocation;
struct Pair;

typedef Value (*c_function_t)(State&, size_t, Value*);

std::ostream& operator<<(std::ostream& os,  Value);

//= # Value representation

//= Scheme values can be either immediate (fixed-point integers and constants) or allocated on the heap.

//= Scheme values are generally referenced using an instance of the Value struct, which contains methods that
//= help interact with these values in a safe manner and can be tracked by the garbage collector.

//= Under the hood, a Value is a ptrdiff_t sized struct which can be either a pointer to a HeapValue
//= or an immediate value manipulated using ptrdiff_t Value::bits.

//= HeapValues only exist in garbage-collected memory and generally are only used internally.

//= The actual bits of a Value look like this:

//= - `...1` - Fixnum
//= - `..10` - Constant
//= - `..00` - Pointer to heap value

enum {
  // Should never be encountered
  RESERVED = 0,
  // Immediate values
  FIXNUM = 1,
  CONSTANT = 2,
  // Should never be encountered except in GC code
  BLOCK = 3,
  // No pointers
  FLONUM = 4,
  STRING = 5,
  CHARACTER = 6,
  // Pointers
  SYMBOL = 7,
  VECTOR = 9,
  VECTOR_STORAGE = 10,
  PAIR = 11, 
  EXCEPTION = 12,
  FUNCTION = 13,
  CFUNCTION = 14,
};


// Constants:

enum {
  C_TRUE = 2,             // 0000 0010 #t
  C_FALSE = 6,            // 0000 0110 #f
  C_NIL = 10,             // 0000 1010 ()
  C_UNSPECIFIED = 14 ,    // 0000 1110 #<unspecified> 
  C_EOF = 18,             // 0001 0010 #<eof>
  C_SYNTAX = 34,          // 0100 0010 #<syntax>
};

// A heap-allocated, garbage-collected value.
struct HeapValue {
  // Heap value headers are formatted like this:

  // ...m tttt tttt
  // m = mark bit
  // t = type
  // . = object-specific flags
  size_t header;

  // Size of the object
  size_t size;

  void initialize(unsigned type, unsigned mark_bit, size_t size_) {
    header = (type) + (mark_bit << 8);
    size = size_;
  }

  unsigned get_header() const { return header; }
  unsigned get_type() const { return header & 255; }
  unsigned char get_mark_bit() const { return (header >> 8) & 1; }
  unsigned get_header_bit(unsigned bit) const { return header & bit; }
  void set_header_bit(unsigned bit) { header += bit; }

  void flip_mark_bit() { header += get_mark_bit() ? -256 : 256; }
};

// Floating point number
struct Flonum : HeapValue {
  double number;  
};

struct String : HeapValue {
  size_t bytes;
  char data[1];

  static const unsigned CLASS_TYPE = STRING;
};

// TODO: These should not be heap-allocated if possible.
struct Char : HeapValue {
  char datum;
};

struct Value {
  union {
    HeapValue* heap;
    ptrdiff_t bits;
  };

  Value(): bits(0) {}
  Value(HeapValue* heap_): heap(heap_) {}
  Value(ptrdiff_t bits_): bits(bits_) {}

  /** Returns true if this value is immediate */
  bool immediatep() const { return (bits & 3) != 0 || bits == 0; }

  /** Safely retrieve the type of an object */
  unsigned int type() const {
    if(bits & 1) return FIXNUM;
    else if(bits & 2 || bits == 0) return CONSTANT;    
    else return heap->get_type();
  }

  // FIXNUMS

  /** Get the value of a fixnum */
  ptrdiff_t fixnum_value() const {
    AR_ASSERT(type() == FIXNUM);
    
    return bits >> 1;
  }

  /** Create a fixnum */
  static Value make_fixnum(ptrdiff_t fixnum) {
    return Value(((fixnum << 1) + 1));
  }

  // CONSTANTS
  unsigned constant_value() const {
    AR_ASSERT(type() == CONSTANT);
    return bits;
  }

  // FLONUMS
  double flonum_value() const {
    AR_TYPE_ASSERT(type() == FLONUM);
    return static_cast<Flonum*>(heap)->number;
  }

  // VECTORS
  Value vector_storage() const;
  Value vector_ref(size_t i) const;
  void vector_set(size_t i, Value);
  size_t vector_length() const;

  // STRINGS
  const char* string_data() const {
    AR_TYPE_ASSERT(type() == STRING);
    return static_cast<String*>(heap)->data;
  }

  size_t string_bytes() const {
    AR_TYPE_ASSERT(type() == STRING);
    return static_cast<String*>(heap)->bytes;
  }

  bool string_equals(const std::string& cmp) const {
    return cmp.compare(string_data()) == 0;
  }

  bool string_equals(const char* s) const {
    std::string cmp(s);
    return string_equals(cmp);
  }

  // CHARACTERS
  char character() const {
    AR_TYPE_ASSERT(type() == CHARACTER);
    return static_cast<Char*>(heap)->datum;
  }

  // SYMBOLS
  Value symbol_name() const;
  const char* symbol_name_bytes() const;
  Value symbol_value() const;

  /** Quickly compare symbol to string */
  bool symbol_equals(const char* s) const {
    std::string cmp(s);
    return cmp.compare(symbol_name_bytes()) == 0;
  }

  // PAIRS
  size_t list_length() const;
  Value list_ref(size_t) const;

  Value car() const;
  Value cadr() const;
  Value cddr() const;
  Value caddr() const;
  Value cadddr() const;
  Value cdr() const;
  void set_car(Value);
  void set_cdr(Value);

  static const unsigned PAIR_SOURCE_BIT = 1 << 9; 

  bool pair_has_source() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header_bit(PAIR_SOURCE_BIT);
  }

  SourceLocation* pair_src() const;
  void set_pair_src(SourceLocation&);

  // EXCEPTION
  static const unsigned EXCEPTION_ACTIVE_BIT = 1 << 9;
  bool is_active_exception() const;
  Value exception_tag() const;
  Value exception_message() const;
  Value exception_irritants() const;
  
  // FUNCTIONS
  static const unsigned FUNCTION_EVAL_ARGUMENTS_BIT = 1 << 9;

  Value function_name() const;
  Value function_arguments() const;
  Value function_rest_arguments() const;
  Value function_body() const;
  bool function_eval_args() const;

  // C FUNCTIONS
  c_function_t c_function_addr() const;

  // OPERATORS

  // Identity comparison
  inline bool operator==(const Value& other) const {
    return bits == other.bits;
  }

  inline bool operator!=(const Value& other) const { return bits != other.bits; }

  // CASTING
  template <class T> T* as() const {
    AR_TYPE_ASSERT(type() == T::CLASS_TYPE);
    return static_cast<T*>(heap);
  }

  template <class T> T* as() {
    AR_TYPE_ASSERT(type() == T::CLASS_TYPE);
    return static_cast<T*>(heap);
  }
};

// Below here: inline definitions of things that need Value to be declared
struct Symbol : HeapValue {
  Value name, value;

  static const unsigned CLASS_TYPE = SYMBOL;
};

inline Value Value::symbol_name() const {
  return as<Symbol>()->name;
}

inline const char* Value::symbol_name_bytes() const { return as<Symbol>()->name.as<String>()->data; }

inline Value Value::symbol_value() const {
  return as<Symbol>()->value;
}

/** 
 * Identifies a location in source code. File is an integer that corresponds to a string in
 * State::source_files
 */
struct SourceLocation {
  SourceLocation(): file(0), line(0) {}
  SourceLocation(size_t file_, size_t line_): file(file_), line(line_) {}

  size_t file, line;
};

struct Pair : HeapValue {
  Value data_car, data_cdr;
  SourceLocation src;

  static const unsigned char CLASS_TYPE = PAIR;
};

inline SourceLocation* Value::pair_src() const {
  AR_TYPE_ASSERT(type() == PAIR);
  AR_TYPE_ASSERT(pair_has_source());
  return &(static_cast<Pair*>(heap)->src);
}

inline void Value::set_pair_src(SourceLocation& loc) {
  AR_TYPE_ASSERT(type() == PAIR);
  AR_TYPE_ASSERT(pair_has_source());
  static_cast<Pair*>(heap)->src = loc;
}

//= ### Value::car() const
inline Value Value::car() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return static_cast<Pair*>(heap)->data_car;
}

inline size_t Value::list_length() const {
  Value check(bits);
  if(check == C_NIL) return 0;
  AR_TYPE_ASSERT(type() == PAIR);
  size_t len = 0;
  while(check.type() == PAIR) {
    ++len;
    check = check.cdr();
    if(check.type() != PAIR && check != C_NIL) {
      return 0;
    }
  }
  return len;
}

inline Value Value::list_ref(size_t n) const {
  Value check(bits);
  size_t i = 0;
  while(check.type() == PAIR && i++ != n) {
    check = check.cdr();
    if(check.type() != PAIR && check != C_NIL) {
      AR_TYPE_ASSERT(!"list-ref in non-list");
      return C_NIL;
    }
  }
  return check.car();
}

inline Value Value::cadr() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return cdr().car();
}

inline Value Value::cddr() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return cdr().cdr();
}

inline Value Value::caddr() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return cdr().cdr().car();
}

inline Value Value::cadddr() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return cdr().cdr().cdr().car();
}

inline Value Value::cdr() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return static_cast<Pair*>(heap)->data_cdr;
}

inline void Value::set_car(Value v) {
  AR_TYPE_ASSERT(type() == PAIR);
  static_cast<Pair*>(heap)->data_car = v;
}

inline void Value::set_cdr(Value v) {
  AR_TYPE_ASSERT(type() == PAIR);
  static_cast<Pair*>(heap)->data_cdr = v;
}

/// EXCEPTIONS

struct Exception : HeapValue {
  Value tag, message, irritants;
};

inline bool Value::is_active_exception() const {
  return type() == EXCEPTION;
}

inline Value Value::exception_tag() const {
  AR_TYPE_ASSERT(type() == EXCEPTION);
  return static_cast<Exception*>(heap)->tag;
}

inline Value Value::exception_message() const {
  AR_TYPE_ASSERT(type() == EXCEPTION);
  return static_cast<Exception*>(heap)->message;
}

inline Value Value::exception_irritants() const {
  AR_TYPE_ASSERT(type() == EXCEPTION);
  return static_cast<Exception*>(heap)->irritants;
}

/// FUNCTIONS

struct Function : HeapValue {
  Value name, parent_env, arguments, rest_arguments, body;

  static const unsigned CLASS_TYPE = FUNCTION;
};

inline Value Value::function_name() const {
  return as<Function>()->name;
}

inline Value Value::function_arguments() const {
  return as<Function>()->arguments;
}

inline Value Value::function_rest_arguments() const {
  return as<Function>()->rest_arguments;
}

inline Value Value::function_body() const {
  return as<Function>()->body;
}

inline bool Value::function_eval_args() const {
  AR_TYPE_ASSERT(type() == FUNCTION);
  return heap->get_header_bit(FUNCTION_EVAL_ARGUMENTS_BIT);
}


struct CFunction : HeapValue {
  c_function_t addr;
  size_t min_arity, max_arity;

  static const unsigned CLASS_TYPE = CFUNCTION;
};

inline c_function_t Value::c_function_addr() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return as<CFunction>()->addr;
}


/// VECTORS

struct VectorStorage : HeapValue {
  size_t length, capacity;
  Value data[1];

  static const unsigned CLASS_TYPE = VECTOR_STORAGE;
};

struct Vector : HeapValue {
  Value storage;

  static const unsigned CLASS_TYPE = VECTOR;
};

inline Value Value::vector_storage() const {
  AR_TYPE_ASSERT(type() == VECTOR);
  return static_cast<Vector*>(heap)->storage;
}

inline Value Value::vector_ref(size_t i) const {
  // TODO: Should this enforce bounds checking? Should it return an exception?
  AR_TYPE_ASSERT(type() == VECTOR);
  VectorStorage* store = static_cast<VectorStorage*>(static_cast<Vector*>(heap)->storage.heap);
  AR_ASSERT(i < store->length);
  return store->data[i];
}

inline void Value::vector_set(size_t i, Value val) {
  AR_TYPE_ASSERT(type() == VECTOR);
  VectorStorage* store = static_cast<VectorStorage*>(static_cast<Vector*>(heap)->storage.heap);
  AR_ASSERT(i < store->length);
  store->data[i] = val;
}

inline size_t Value::vector_length() const {
  AR_TYPE_ASSERT(type() == VECTOR);
  VectorStorage* store = static_cast<VectorStorage*>(static_cast<Vector*>(heap)->storage.heap);
  return store->length;
}

///// (GC) Garbage collector

struct GC;

/** 
 * FrameHack turns a Value& into a pointer to a stack-allocated Value. This is done to allow for the
 * possibility of a compaction step in the future; it's not necessary otherwise.
 */
struct FrameHack { FrameHack(Value& value_): value((HeapValue**) &value_.bits) {} ~FrameHack() {}

  HeapValue** value;
};

/** Frames are stack-allocated structures that save pointers to stack Values, allowing the garbage collector to move
 * objects and update pointers to them if necessary */
struct Frame {
  GC& gc;
  size_t size;
  HeapValue*** values;

  Frame(State& state, size_t size, HeapValue*** values);
  Frame(State* state, size_t size, HeapValue*** values);
  ~Frame();
};

/** An individual tracked pointer. Can be allocated on the heap. */
struct Handle {
  GC& gc;
  Value ref;
  std::list<Handle*>::iterator it;
  // Handle *next, *previous;

  Handle(State&);
  Handle(State&, Value);
  Handle(const Handle&);
  ~Handle();

  Value operator*() const { return ref; }
  Value operator->() const { return ref; }
  void operator=(Value ref_) { ref = ref_; }

  void initialize();
};

struct Block {
  char* data;
  size_t size;

  Block(size_t size_, unsigned char mark_bit): size(size_) {
    data = static_cast<char*>(calloc(1, size));
    ((HeapValue*) data)->initialize(BLOCK, !mark_bit, size_);
  }

  ~Block() { free(data); }

  /** Returns true if there is room in a block for a given allocation */
  bool has_room(size_t position, size_t room) const {
    return (data + (position + room)) <= (data + size);
  }
};

/** Garbage collector */
struct GC {
  State& state;
  std::vector<Frame*> frames;
  std::list<Handle*> handles;
  // Handle* handle_list;
  std::vector<Block*> blocks;
  // Position in the *blocks* vector
  size_t block_i;
  // Position in *blocks_i* block
  size_t block_cursor;
  // Number of total allocations
  size_t allocations;
  // Number of total collections
  size_t collections;
  size_t live_objects_after_collection, live_memory_after_collection, heap_size;
  unsigned char mark_bit;
  size_t block_size;

  GC(State& state_): state(state_), handles(0), block_i(0), block_cursor(0), allocations(0), collections(0), live_objects_after_collection(0),
      live_memory_after_collection(0), heap_size(ARETE_BLOCK_SIZE), mark_bit(1), block_size(ARETE_BLOCK_SIZE) {
    Block *b = new Block(ARETE_BLOCK_SIZE, mark_bit);
    blocks.push_back(b);

    // Blocks should be allocated dead
    AR_ASSERT(!marked((HeapValue*) b->data));
  }

  ~GC() {
    for(size_t i = 0; i != blocks.size(); i++) {
      delete blocks[i];
    }
  }

  bool marked(HeapValue* v) const {
    return v->get_mark_bit() == mark_bit;
  }

  void mark(HeapValue* v) {
    // We use a GOTO here to avoid creating unnecessary stack frames
  again: 
    // If there is no object or object has already been marked
    if(v == 0 || Value(v).immediatep() || marked(v))
      return;
    
    live_objects_after_collection++;
    // std::cout << "object of type " << v->get_type() << " is size " << v->size << std::endl;
    live_memory_after_collection += v->size;

    v->flip_mark_bit();

    AR_ASSERT(marked(v));

    switch(v->get_type()) {
      case FLONUM: case STRING: case CHARACTER:
      case CFUNCTION:
        return;
      case SYMBOL:
        mark(static_cast<Symbol*>(v)->name.heap);
        v = static_cast<Symbol*>(v)->value.heap;
        goto again;

      case FUNCTION:
        mark(static_cast<Function*>(v)->name.heap);
        mark(static_cast<Function*>(v)->parent_env.heap);
        mark(static_cast<Function*>(v)->arguments.heap);
        mark(static_cast<Function*>(v)->rest_arguments.heap);
        v = static_cast<Function*>(v)->body.heap;
      case PAIR:
        mark(static_cast<Pair*>(v)->data_car.heap);
        v = static_cast<Pair*>(v)->data_cdr.heap;
        goto again;
      case EXCEPTION:
        mark(static_cast<Exception*>(v)->message.heap);
        mark(static_cast<Exception*>(v)->tag.heap);
        v = static_cast<Exception*>(v)->irritants.heap;
        goto again;
      case VECTOR:
        v = static_cast<Vector*>(v)->storage.heap;
        goto again;
      case VECTOR_STORAGE: {
        size_t length = static_cast<VectorStorage*>(v)->length;
        if(length == 0) return;
        for(size_t i = 0; i != length - 1; i++) {
          mark(static_cast<VectorStorage*>(v)->data[i].heap);
        }
        v = static_cast<VectorStorage*>(v)->data[length - 1].heap;
        goto again;
      }
      default:
      case BLOCK: case FIXNUM: case CONSTANT:
        std::cout << v->get_type() << std::endl;
        AR_ASSERT(!"arete:gc: bad value on heap; probably a GC bug"); break;
    }
  }

  void mark_symbol_table();

  void collect() {
    // ARETE_LOG_GC("collecting");
    collections++;
    live_objects_after_collection = live_memory_after_collection = 0;
    block_i = block_cursor = 0;

    // Reverse meaning of mark bit
    mark_bit = !mark_bit;

    // Mark all live objects
    ARETE_LOG_GC("scanning " << frames.size() << " frames for roots");
    for(size_t i = 0; i != frames.size(); i++) {
      Frame* f = frames[i];
      for(size_t j = 0; j != f->size; j++) {
        if(f->values[j]) {
          mark(*(f->values[j]));
        }
      }
    }

    // TODO: Symbol table should be weak
    mark_symbol_table();

    ARETE_LOG_GC("found " << live_objects_after_collection << " live objects taking up " <<
      live_memory_after_collection << "b")

    // Allocate a new block if memory is getting a little overloaded
    double load_factor = (live_memory_after_collection * 100) / heap_size;

    ARETE_LOG_GC("load factor " << live_memory_after_collection << " " << live_objects_after_collection << " " << load_factor);
    AR_ASSERT(live_memory_after_collection <= heap_size);

    if(load_factor >= ARETE_GC_LOAD_FACTOR) {
      ARETE_LOG_GC(load_factor << "% of memory is still live after collection, adding a block");
      block_size *= 2;
      Block* b = new Block(block_size, mark_bit);
      AR_ASSERT(b->size == block_size);
      AR_ASSERT(!marked((HeapValue*)b->data));
      heap_size += block_size;
      blocks.push_back(b);
    }
  }

  HeapValue* allocate(unsigned type, size_t size) {
    size_t sz = align(8, size);
    bool collected = false;

    // Searches through live memory in a first-fit fashion for somewhere to allocate the value; if it fails,
    // a collection will be triggered.

    // TODO: Some of this code is a little awkward. A HeapIterator struct with 
    // -> and * overloaded might make it easier to read.
  retry:
    while(block_i != blocks.size()) {
      while(blocks[block_i]->has_room(block_cursor, sz)) {
        HeapValue* v = (HeapValue*)(blocks[block_i]->data + block_cursor);

        AR_ASSERT(v->size > 0); // assert that memory has been initialized with some kind of size

        if((!marked(v))) {
          // Combine dead objects
          while(true) {
            HeapValue* v2 = (HeapValue*)(blocks[block_i]->data + block_cursor + v->size);
            // If this is at the end of the heap, or this object is alive
            if(block_cursor + v->size >= blocks[block_i]->size || marked(v2)) {
              break;
            }
            // std::cout << "combining two dead objects " << std::endl;
            size_t dead_size = v2->size;
            v->size += dead_size;
            memset(v2, 0, dead_size);
          }
        }

        if((!marked(v) && v->size >= sz)) {
          size_t mem_size = v->size;

          // Success!
          char* memory = blocks[block_i]->data + block_cursor;

          // If there is enough room after this memory to handle another object, note down its
          // size and move on

          if(mem_size - sz >= sizeof(Flonum)) {
            // ARETE_LOG_GC("additional " << (mem_size - sz) << " bytes after object");
            HeapValue* next_object = ((HeapValue*) ((blocks[block_i]->data + block_cursor) + sz));
            next_object->initialize(BLOCK, !mark_bit, mem_size - sz);

            AR_ASSERT(!marked(next_object));
            AR_ASSERT(next_object->size >= sizeof(Flonum));

            block_cursor += sz;
          } else {
            // Otherwise, just allocate room for the object
            sz = mem_size;
            block_cursor += sz;
          }

          memset(memory, 0, sz);
          HeapValue* ret = (HeapValue *) memory;
          ret->initialize(type, mark_bit, sz);
          AR_ASSERT(marked(ret));

          AR_ASSERT(((ptrdiff_t) ret & 3) == 0);

          // Break up memory as necessary
          ret->size = sz;
          return ret;
        } else if(!marked(v)) {
          // Finally mark the object so that when all marks are reversed it will be dead
          v->flip_mark_bit();
        }
        // MARK OBJECT AS DEAD AND MOVE ON

        block_cursor += v->size;
      }

      ARETE_LOG_GC("block " << block_i << " (" << blocks[block_i]->size << "b) out of room, moving on");

      block_i++;
      block_cursor = 0;
    }

    ARETE_LOG_GC("reached end of " << blocks.size() << " blocks");

    if(!collected) {
      // No room, run a collection
      ARETE_LOG_GC("out of room, attempting collection for allocation of size " << size);
      collect();
      collected = true;
      goto retry;
    } else {
      // Collection has failed to create enough space. Give up.
      ARETE_LOG_GC("allocation of size " << size << " failed");
      AR_ASSERT(!"out of room; allocation failed");
      return 0;
    }
  }

  // Align a value along a boundary e.g. align(8, 7) == 8, align(8, 16) ==6
  static size_t align(size_t boundary, size_t value) {
    return (((((value) - 1) / (boundary)) + 1) * (boundary));
  }
};

/** A re-entrant instance of the Arete runtime */
struct State {
  typedef std::unordered_map<std::string, Symbol*> symbol_table_t;

  GC gc;
  symbol_table_t symbol_table;
  std::vector<std::string> source_names;

  State(): gc(*this) {}
  ~State() {
    // Free symbol names
    // for(symbol_table_t::const_iterator x = symbol_table.begin(); x != symbol_table.end(); ++x) {
    //  free((void*) x->second->name);
    // }
  }

  // Source code location tracking
  unsigned register_file(const std::string& path) {
    source_names.push_back(path);
    return source_names.size() - 1;
  }

  #define AR_SYMBOLS(_) \
    _(quote), _(quasiquote), _(unquote),  \
    _(define), _(lambda), _(if)

  #define AR_SYMBOLS_AUX(name) S_##name

  enum BuiltinSymbol { AR_SYMBOLS(AR_SYMBOLS_AUX),
    S_unquote_splicing,
    S_read_error, S_eval_error, S_type_error };
  
  std::vector<Handle> builtin_symbols;

  /** Performs various initializations required for an instance of Arete;
    * this is separate so the State itself can be used for lightweight testing */
  void boot() {
    source_names.push_back("unknown");
    source_names.push_back("c-string");

    // Initialize built-in symbols
    static const char* symbols[] = { 
      #define AR_SYMBOLS_AUX2(x) #x
      AR_SYMBOLS(AR_SYMBOLS_AUX2),
      "unquote-splicing",
      "read-error", "eval-error", "type-error"
    };

    for(size_t i = 0; i != sizeof(symbols) / sizeof(const char*); i++) {
      builtin_symbols.push_back(Handle(*this, get_symbol(symbols[i])));
    }

    get_symbol(S_define).as<Symbol>()->value = C_SYNTAX;
    get_symbol(S_lambda).as<Symbol>()->value = C_SYNTAX;
    get_symbol(S_if).as<Symbol>()->value = C_SYNTAX;
    get_symbol(S_quote).as<Symbol>()->value = C_SYNTAX;
    get_symbol(S_quasiquote).as<Symbol>()->value = C_SYNTAX;
    get_symbol(S_unquote).as<Symbol>()->value = C_SYNTAX;

    install_builtin_functions();
  }

#undef AR_SYMBOLS

  // Value creation; all of these will cause allocations
  Value make_flonum(double number) {
    Flonum* heap = (Flonum*) gc.allocate(FLONUM, sizeof(Flonum));
    heap->number = number;
    Value v(heap);
    return v;
  }

  Value get_symbol(BuiltinSymbol sym) {
    return *builtin_symbols.at((size_t) sym);
  }

  Value get_symbol(const std::string& name) {
    symbol_table_t::const_iterator x = symbol_table.find(name);
    if(x == symbol_table.end()) {
      Symbol* heap = static_cast<Symbol*>(gc.allocate(SYMBOL, sizeof(Symbol)));

      Value sym = heap, string;
      AR_FRAME(this, sym, string);

      string = make_string(name);

      heap->value = C_UNSPECIFIED;
      heap->name = string;

      symbol_table.insert(std::make_pair(name, heap));
      return heap;
    } else {
      return x->second;
    }
  }

  /** cons */
  Value make_pair(Value car = C_FALSE, Value cdr = C_FALSE,
      size_t size = sizeof(Pair) - sizeof(Pair::src)) {
    AR_FRAME(this, car, cdr);
    Pair* heap = (Pair*) gc.allocate(PAIR, size);

    heap->data_car = car;
    heap->data_cdr = cdr;
    return heap;
  }

  /** Generate a pair with source code information */
  Value make_src_pair(Value car, Value cdr, SourceLocation& loc) {
    Value pare;
    AR_FRAME(this, pare, car, cdr);
    pare = make_pair(car, cdr, sizeof(Pair));
    pare.heap->set_header_bit(Value::PAIR_SOURCE_BIT);
    pare.set_pair_src(loc);

    AR_ASSERT(pare.type() == PAIR);
    AR_ASSERT(pare.pair_has_source());

    return pare;
  }

  Value make_char(char c) {
    Char* heap = static_cast<Char*>(gc.allocate(CHARACTER, sizeof(Char)));
    heap->datum = c;
    return heap;
  }

  Value make_vector_storage(size_t capacity) {
    size_t size = (sizeof(VectorStorage) - sizeof(Value)) + (sizeof(Value) * capacity);
    VectorStorage* storage = static_cast<VectorStorage*>(gc.allocate(VECTOR_STORAGE, size));

    storage->capacity = capacity;
    storage->length = 0;

    return storage;
  }

  Value make_vector(size_t capacity = 2) {
    Value vec, storage;

    AR_FRAME(this, vec, storage);
    vec.heap = static_cast<Vector*>(gc.allocate(VECTOR, sizeof(Vector)));
    storage = make_vector_storage(capacity);
    static_cast<Vector*>(vec.heap)->storage = storage;

    return vec;
  }

  /** Mutating vector append */
  void vector_append(Value vector, Value value) {
    AR_TYPE_ASSERT(vector.type() == VECTOR);
    // std::cout << "vector_append" << std::endl;
    VectorStorage* store = static_cast<VectorStorage*>(vector.vector_storage().heap);

    store->data[store->length++] = value;
    
    AR_ASSERT(store->length <= store->capacity);
    if(store->length == store->capacity) {
      Value storage = store, new_storage;
      AR_FRAME(this, vector, value, storage);

      new_storage = make_vector_storage(store->capacity * 2);
      store = static_cast<VectorStorage*>(vector.vector_storage().heap);
      static_cast<VectorStorage*>(new_storage.heap)->length = store->length;
      
      memcpy(static_cast<VectorStorage*>(new_storage.heap)->data, store->data, sizeof(Value) * store->length);
      static_cast<Vector*>(vector.heap)->storage = new_storage;
    }
  }

  Value make_string(const std::string& body) {
    String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + body.size()));
    heap->bytes = body.size();
    strncpy(heap->data, body.c_str(), body.size());
    AR_ASSERT(heap->data[heap->bytes] == '\0');
    heap->data[heap->bytes] = '\0';

    return heap;
  }

  Value make_exception(Value tag, const std::string& cmessage, Value irritants = C_UNSPECIFIED) {
    Value message, exc;
    AR_FRAME(this, tag, message, irritants, exc);
    Exception* heap = static_cast<Exception*>(gc.allocate(EXCEPTION, sizeof(Exception)));
    message = make_string(cmessage);
    heap->tag = tag;
    heap->message = message;
    heap->irritants = irritants;
    return heap;
  }

  Value make_exception(const std::string& ctag, const std::string& cmessage, Value irritants = C_UNSPECIFIED) { 
    Value tag;
    AR_FRAME(this, tag, irritants);
    tag = get_symbol(ctag);
    return make_exception(tag, cmessage, irritants);
  }
  
  Value make_exception(BuiltinSymbol s, const std::string& cmessage, Value irritants = C_UNSPECIFIED) {
    return make_exception(get_symbol(s), cmessage, irritants);
  }

  /** Return a description of a source location */
  std::string source_info(const SourceLocation* loc) {
    std::ostringstream ss;
    ss << source_names[loc->file] << ':' << loc->line;
    return ss.str();
  }

  /** Return a description of the source location of a pair */
  std::string source_info(Value expr, bool& found) {
    found = false;
    if(expr.type() == PAIR && expr.pair_has_source()) {
      SourceLocation* loc = expr.pair_src();
      found = true;
      return source_info(loc);
    } else {
      return "unknown";
    }
  }

  ///// (EVAL) Interpreter

  // Functions for interacting with the Scheme environment

#define AR_FN_EXPECT_TYPE(state, argv, i, expect) \
  if((argv)[(i)].type() != (expect)) { \
    return (state).type_error("function "##fn_name ); \
  } 

  void install_builtin_functions();

  void defun(const std::string& name, c_function_t addr, size_t min_arity, size_t max_arity = 0) {
    Value sym;
    CFunction* cfn = 0;
    AR_FRAME(this, sym);
    sym = get_symbol(name);
    cfn = static_cast<CFunction*>(gc.allocate(CFUNCTION, sizeof(CFunction)));
    cfn->addr = addr;
    cfn->min_arity = min_arity;
    cfn->max_arity = max_arity;
    sym.as<Symbol>()->value = cfn;
  }

  std::ostream& warn() { return std::cerr << "arete: Warning: " ; }

  // Environments are just vectors containing the parent environment in the first position
  // and variable names/values in the even/odd positions afterwards e.g.

  // #(#f hello 12345)
  // for a environment one level below toplevel after a (define hello 12345)

  Value make_env(Value parent = C_FALSE) {
    Value vec;
    AR_FRAME(this, vec, parent);
    vec = make_vector(3);
    vector_append(vec, parent);
    return vec;
  }

  void env_set(Value env, Value name, Value val) {
    AR_FRAME(this, env, name, val);
    AR_TYPE_ASSERT(name.type() == SYMBOL);
    // Toplevel set
    if(env == C_FALSE) {
      // std::cout << "SET VALUE " << name << " " << val << std::endl;
      name.as<Symbol>()->value = val;
      return;
    }
    for(size_t i = 1; i != env.vector_length(); i += 2) {
      if(env.vector_ref(i) == name) {
        env.vector_set(i+1, val);
        return;
      }
    }
  }

  void env_set(Value env, const std::string& names, Value val) {
    Value name;
    AR_FRAME(this, env, name, val);
    name = get_symbol(names);
    // Toplevel set
    if(env == C_FALSE) {
      name.as<Symbol>()->value = val;
      return;
    }
    for(size_t i = 1; i != env.vector_length(); i += 2) {
      // TODO error handling
      if(env.vector_ref(i) == name) {
        env.vector_set(i+1, val);
      }
    }
  }

  Value env_lookup(Value env, Value name, bool one_level = false) {
    while(env != C_FALSE) {
      for(size_t i = 1; i != env.vector_length(); i += 2) {
        if(env.vector_ref(i) == name)
          return env.vector_ref(i+1);
      }
      env = env.vector_ref(0); // check parent environment
      if(one_level) break;
    }
    // reached toplevel, check symbol.
    return name.as<Symbol>()->value;
  }

  Value type_error(const std::string& msg) {
    return make_exception("type-error", msg);
  }

  /** Return an eval error with source code information */
  Value eval_error(const std::string& msg, Value exp) {
    std::ostringstream os;

    if(exp.type() == PAIR && exp.pair_has_source()) {
      os << source_info(exp.pair_src()) << ": ";
    }

    os << msg;
    return make_exception("eval-error", os.str());
  }
  
  Value eval_lambda(Value env, Value exp, bool eval_args = false) {
    Value fn_env, args, args_head, args_tail;
    AR_FRAME(*this, env, exp, fn_env, args);
    Function* fn = (Function*) gc.allocate(FUNCTION, sizeof(Function));
    args = exp.cadr();
    fn->name = C_FALSE;
    fn->parent_env = fn_env;
    if(args.type() == SYMBOL) {
      fn->arguments = C_NIL;
      fn->rest_arguments = args;
    } else {
      // First case: (lambda rest ...)
      if(args.type() == SYMBOL) {
        fn->rest_arguments = args;
        fn->arguments = C_NIL;
      } else if(args == C_NIL) {
        fn->arguments = C_NIL;
        fn->rest_arguments = C_FALSE;
      } else {
        // Second case
        fn->rest_arguments = C_FALSE;
        Value argi = args;
        while(argi.type() == PAIR) {
          if(argi.car().type() != SYMBOL) {
            return eval_error("lambda argument list must be all symbols", exp);
          }
          if(argi.cdr() == C_NIL) {
            break;
          } else if(argi.cdr().type() != PAIR) {
            if(argi.cdr().type() != SYMBOL) {
              return eval_error("lambda argument list must be all symbols", exp);
            }

            fn->rest_arguments = argi.cdr();
            argi.set_cdr(C_NIL);
          }
          argi = argi.cdr();
        }
        // TODO this should not modify the source list.
        fn->arguments = args;

        std::cout << fn->arguments << std::endl;
        std::cout << fn->rest_arguments << std::endl;
      }
    }
    fn->body = exp.cddr();
    return fn;
  }

  Value eval_define(Value env, Value exp) {
    Value name, body, tmp;
    AR_FRAME(this, name, body, tmp);

    name = exp.cadr();

    body = exp.caddr();

    if(name.type() != SYMBOL) {
      return eval_error("first argument to define must be a symbol", exp);
    }

    tmp = env_lookup(env, name, true);
    if(tmp != C_UNSPECIFIED) {
      warn() << source_info(exp.pair_src()) << " shadows existing definition of " << name;
    }

    tmp = eval(env, body);

    env_set(env, name, tmp);

    return C_UNSPECIFIED;
  }

  Value apply_c(Value env, Value fn, Value args, Value src_exp = C_FALSE) {
    Value fn_args, tmp;
    AR_FRAME(this, env, fn, args, src_exp, tmp);

    size_t given_args = args.list_length();
    size_t min_arity = fn.as<CFunction>()->min_arity;
    size_t max_arity = fn.as<CFunction>()->max_arity;

    if(given_args < min_arity) {
      std::ostringstream os;
      os << "function " << fn << " expected at least " << min_arity << " arguments but got " << given_args;
      return eval_error(os.str(), src_exp);
    }

    if(max_arity != 0 && given_args > max_arity) {
      std::ostringstream os;
      os << " function " << fn << " expected at most " << max_arity << " arguments but got " << given_args;
      return eval_error(os.str(), src_exp);
    }

    fn_args = make_vector();
    size_t argc = 0;
    while(args.type() == PAIR) {
      tmp = eval(env, args.car());
      vector_append(fn_args, tmp);
      argc++;
      args = args.cdr();
    }
    
    return fn.c_function_addr()(*this, argc, fn_args.as<Vector>()->storage.as<VectorStorage>()->data);
  }
  
  /** Apply a scheme function */
  Value apply_scheme(Value env, Value fn, Value args, Value src_exp = C_FALSE) {
    Value new_env, tmp, rest_args_name, fn_args, rest_args_head = C_NIL, rest_args_tail, body;
    AR_FRAME(this, env, fn, args, new_env, fn_args, tmp, src_exp, rest_args_name, rest_args_head,
      rest_args_tail, body);
    new_env = make_vector();
    vector_append(new_env, env);
    // bool eval_args = fn.function_eval_args();

    fn_args = fn.function_arguments();
    size_t arity = fn.function_arguments().list_length();
    size_t given_args = args.list_length();

    if(given_args < arity) {
      std::ostringstream os;
      os << "function " << fn << " expected at least " << arity << " arguments but got " << given_args;
      return eval_error(os.str(), src_exp);
    }

    rest_args_name = fn.function_rest_arguments();
    std::cout << "REST_ARGS_NAME " << rest_args_name << std::endl;

    if(rest_args_name == C_FALSE && given_args > arity) {
      std::ostringstream os;
      os << "function " << fn << " expected at most " << arity << " arguments but got " << given_args;
      return eval_error(os.str(), src_exp);
    }

    // Evaluate arguments left to right
    while(args.type() == PAIR && fn_args.type() == PAIR) {
      tmp = eval(env, args.car());
      vector_append(new_env, fn_args.car());
      fn_args = fn_args.cdr();
      vector_append(new_env, tmp);
      args = args.cdr();
    }

    // std::cout << "Args " << args << " " << args.type() << std::endl;
    if(fn.function_rest_arguments() != C_FALSE) {
      while(args.type() == PAIR) {
        tmp = eval(env, args.car());
        if(rest_args_head == C_NIL) {
          rest_args_head = rest_args_tail = make_pair(tmp, C_NIL);
        } else {
          tmp = make_pair(tmp, C_NIL);
          rest_args_tail.set_cdr(tmp);
          rest_args_tail = tmp;
        }
        args = args.cdr();
      }

      vector_append(new_env, fn.function_rest_arguments());
      vector_append(new_env, rest_args_head);
    }

    // Now eval body left to right
    body = fn.function_body();

    // std::cout << "eval body " << body << std::endl;
    while(body.type() == PAIR) {
      tmp = eval(new_env, body.car());
      if(body.cdr() == C_NIL) {
        return tmp;
      }
    }


    // std::cout << "created new environment " << new_env << std::endl;

    return C_UNSPECIFIED;

  }

  Value eval_if(Value env, Value exp, bool has_else) {
    Value cond = exp.list_ref(1);
    Value then_branch = exp.list_ref(2);
    Value else_branch = C_UNSPECIFIED;
    Value res = C_FALSE;

    AR_FRAME(this, cond, then_branch, else_branch, res);
    // TODO: protect

    if(has_else) {
      else_branch = exp.list_ref(3);
    }

    cond = eval(env, cond);

    if(cond.is_active_exception()) {
      return cond;
    } else if(cond != C_FALSE) {
      res = eval(env, then_branch);
    } else {
      res = eval(env, else_branch);
    }

    return res;
  }

  Value eval(Value env, Value exp) {
    Value res, car;

    AR_FRAME(this, env, exp, res, car);

    if(exp.immediatep())
      return exp;

    switch(exp.type()) {
      case VECTOR: case VECTOR_STORAGE: case FLONUM: case STRING:
        return exp;
      case PAIR: {
        size_t length = exp.list_length();
        if(length == 0) {
          return eval_error("non-list in source code", exp);
        }
        car = exp.car();
        // TODO should probably check if this is a list.

        if(car.type() == SYMBOL && car.symbol_value() == C_SYNTAX) {
          if(car == get_symbol(S_define)) {
            return eval_define(env, exp);
          } else if(car == get_symbol(S_lambda)) {
            return eval_lambda(env, exp);
          } else if(car == get_symbol(S_if)) {
            if(length != 3 && length != 4) {
              return eval_error("if requires 2-3 arguments", exp);
            }

            return eval_if(env, exp, length == 4);
          } else if(car == get_symbol(S_quote)) {
            if(length > 2) return eval_error("quote takes exactly 1 argument", exp);
            return exp.cadr();
          }
          // form, let, set!, if, quote
        } 

        car = eval(env, exp.car());

        if(car.is_active_exception()) return car;

        if(car.type() != FUNCTION && car.type() != CFUNCTION) {
          return eval_error("attempt to apply non-function", exp);
        } else {
          if(car.type() == FUNCTION) {
            return apply_scheme(env, car, exp.cdr(), exp);
          } else {
            return apply_c(env, car, exp.cdr(), exp);
          }
        }

        return exp;
      }
      case SYMBOL: {
        Value res = env_lookup(env, exp);
        if(res.bits == 0)
          return C_UNSPECIFIED;
        if(res == C_SYNTAX) {
          std::stringstream os;
          os << "attempt to use syntax " << res << " as value";
          return eval_error(os.str(), exp);
        }
        return res;
      }
    }

    return C_UNSPECIFIED;
  }

  Value eval_toplevel(Value exp) {
    return eval(C_FALSE, exp);
  }
};

///// (READ) Reader

/**
 * The reader is a dead simple recursive s-expression reader.
 * It does take pains to track source code information, when possible.
 */
struct Reader {
  State& state;
  std::istream is;
  size_t file, line, column;

  Reader(State& state_, std::istream& is_):
    state(state_), is(is_.rdbuf()), file(0), line(1), column(0) {
    // No skipping whitespace because we need to track lines
    is >> std::noskipws;
    }
  ~Reader() {}

  enum Token {
    TK_NONE, 
    TK_RPAREN,
    TK_DOT,
    TK_RBRACKET,
  };
  
  /** Returns true if a char is a separator and therefore
    * should stop reading of symbols, numbers etc */
  static bool is_separator(char c) {
    return c == '#' || c == '(' || c == ')' ||
      c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '[' 
      || c == ']' || c == '"';
  }

  /** Return current source code location so we'll know where e.g.
   * an erronous multi-line list began */
  SourceLocation save() {
    return SourceLocation(file, line);
  }

  /** cons quote/quasiquote and friends with an expression */
  Value read_aux(const std::string& name) {
    Value symbol, expr;
    AR_FRAME(state, symbol, expr);
    expr = read();
    if(expr.is_active_exception())
      return expr;
    symbol = state.get_symbol(name);
    expr = state.make_pair(expr, C_NIL);
    expr = state.make_pair(symbol, expr);
    return expr;
  }

  /** Make a pair annotated with current source location */
  Value make_pair(Value car, Value cdr = C_NIL) {
    Value pair;
    AR_FRAME(state, pair, car, cdr);
    SourceLocation loc(file, line);
    pair = state.make_src_pair(car, cdr, loc);
    AR_ASSERT(pair.type() == PAIR);
    AR_ASSERT(pair.pair_has_source());
    return pair;
  }

  /** Return an error */
  Value read_error(const std::string& description, size_t begin_line = 0) {
    std::ostringstream os;
    SourceLocation loc(file, begin_line || line);

    os << state.source_info(&loc) << ' ';
    if(begin_line != 0) {
      // If a beginning line has been provided
      os << "in list: ";
    } 
    os << description;

    return state.make_exception("read-error", os.str());
  }

  /** getc that tracks line/column information */
  bool getc(char& c) {
    is >> c;
    if(is.eof()) return false;
    if(c == '\n') {
      line++;
      column = 0;
    }
    column++;
    return !is.eof();
  }

  /** Read a symbol */
  Value read_symbol(char c) {
    std::string buffer;
    buffer += c;
    while(true) {
      c = is.peek();
      if(is.eof() || is_separator(c)) {
        break;
      }
      buffer += c;
      is >> c;
    }
    return state.get_symbol(buffer);
  }

  /** Consumes whitespace until the next token, if there is one */
  void peek_token(Token& return_token) {
    char c = is.peek();
    return_token = TK_NONE;
    while(!is.eof()) {
      switch(c) {
        case ' ': case '\r': case '\t': case '\n': getc(c); continue;
        case ')': getc(c); return_token = TK_RPAREN; return;
        case ']': getc(c); return_token = TK_RBRACKET; return;
        case '.': getc(c); return_token = TK_DOT; return;
        default: return;
      }
    }
  }

  Value read_number(ptrdiff_t start, bool negative) {
    // Fixnums
    ptrdiff_t number = start;
    ptrdiff_t length = 1;
    while(true) {
      char c = is.peek();
      if(c >= '0' && c <= '9') {
        is >> c;
        length++;
        number = (number * 10) + (c - '0');
      } else if(c == '.') {
        is >> c;
        c = is.peek();
        is.seekg(-(length + 1), is.cur);
        double d;
        // TODO This definitely probably isn't compliant with whatever standards
        is >> d;
        return state.make_flonum(d);
      } else {
        break;
      }
    }
    return Value::make_fixnum(negative ? -number : number);

  }

  /** Read a single expression */
  Value read_expr(Token& return_token) {
    return_token = TK_NONE;
    char c;
    while(getc(c)) {
      if(c >= '0' && c <= '9') {
        return read_number(c - '0', false);
      } else if(c == '#') {
        getc(c); // consume first char
        if(c == 't' || c == 'f') {
          // Constants (#t, #f)
          return c == 't' ? C_TRUE : C_FALSE;
        } else if(c == '(') {
          // Vectors 
          Value vec, x;
          AR_FRAME(state, vec, x);

          vec = state.make_vector();
          Token tk = TK_NONE;
          while(true) {
            x = read_expr(tk);
            if(x == C_EOF) {
              return read_error("unexpected EOF in vector");
            }
            if(tk == TK_RPAREN) break;
            else if(tk == TK_DOT || tk == TK_RBRACKET) {
              return read_error("unexpected token in vector");
            }
            state.vector_append(vec, x);
          }
          return vec;
        } else if(c == '\\') {
          // Character literals

          // Get character
          char c2;
          getc(c2);

          if(is.eof()) return read_error("unexpected EOF in character literal");

          if(c2 == ' ') return state.make_char(c2);

          // Get character afterwards; if it's a letter, this
          // may be #\space or #\newline etc
          char c3 = is.peek();
          if(!is.eof() && ((c3 >= 'A' && c3 <= 'Z') || (c3 >= 'a' && c <= 'z'))) {
            Value symbol = read_symbol(c2);
            // TODO: These should be saved off
            // and compared by identity
            if(symbol.symbol_equals("newline")) {
              c2 = '\n';
            } else if(symbol.symbol_equals("space")) {
              c2 = ' ';
            } else {
              std::ostringstream os;
              os << "unknown character constant #\\" << symbol;
              return read_error(os.str());
            }
          } 
          return state.make_char(c2);
        } else {
          return read_error("invalid sharp syntax");
        }
      } else if(c == ';') {
        // Comments
        while(true) {
          c = is.peek();
          // EOF in comment
          if(is.eof()) return C_EOF;
          else if(c == '\n') {
            break;
          }
          is >> c;
        }
      } else if(c == ' ' || c == '\r' || c == '\t' || c == '\n') {
        // Eat whitespace
      } else if(c == '(' || c == '[') {
        Token match = c == '(' ? TK_RPAREN : TK_RBRACKET;
        // Read a list
        Value head, tail, elt, swap;
        SourceLocation list_start = save();
        AR_FRAME(state, head, tail, elt, swap);
        bool dotted = false;

        // Attach source code information
        while(true) {
          Token tk;
          elt = read_expr(tk);
          if(dotted) {
            return read_error("expected one expression after dot in list but got multiple", list_start.line);
          }

          if(elt == C_EOF) {
            AR_ASSERT(is.eof());
            return read_error("unexpected EOF in list", list_start.line);
          } else if(elt.is_active_exception()) {
            return elt;
          }

          // ()
          if(tk == match && head.bits == 0) {
            return C_NIL;
          }

          // TODO: NEED A WAY TO CHECK ) HERE
          if(tk == TK_DOT) {
            dotted = true;
            elt = read_expr(tk);
            if(elt == C_EOF) {
              return read_error("unexpected EOF after dot", list_start.line);
            } else if(elt.is_active_exception()) {
              return elt;
            } else if(tk == match) {
              return read_error("expected expression after dot but got )", list_start.line);
            } 

            peek_token(tk);
            if(tk == match) {
              tail.set_cdr(elt);
              return head;
            } else {
              dotted = true;
            }
          } else if(tk == match) {
            break;
          } else if(tk == TK_RPAREN || tk == TK_RBRACKET) {
            // Little silly: this relies on the above else if failing to catch
            // which bracket shouldn't match

            // TODO: Do a better job of reporting mismatch
            return read_error("unexpected bracket in list",  list_start.line);
          }

          swap = make_pair(elt, C_NIL);

          if(head.bits == 0) {
            head = tail = swap;
          } else {
            tail.set_cdr(swap);
            tail = swap;
          }
        }

        // Read a list.
        return head;
      } else if(c == ')') {
        return_token = TK_RPAREN;
        return C_FALSE;
      } else if(c == ']') {
        return_token = TK_RBRACKET;
        return C_FALSE;
      } else if(c == '\'') {
        return read_aux("quote");
      } else if(c == '`') {
        return read_aux("quasiquote");
      } else if(c == '"') {
        // Strings
        std::string buffer;
        while(true) {
          getc(c);
          if(is.eof()) {
            return read_error("unexpected EOF in string");
          } else if(c == '"') {
            break;
          } else if(c == '\\') {
            getc(c); // get escape code
            if(c == 'n') {
              buffer += '\n';
            } else if(c == 't') {
              buffer += '\t';
            } else if(c == 'r') {
              buffer += '\r';
            } else if(c == '"' || c == '\\') {
              buffer += c;
            } else {
              std::ostringstream os;
              os << "unknown string escape \\" << c;
              return read_error(os.str());
            }
            continue;
          }
          
          buffer += c;
        }
        return state.make_string(buffer);
      } else if(c == ',') {
        // unquote and unquote-splicing
        c = is.peek();
        if(c == '@') {
          is >> c;
          return read_aux("unquote-splicing");
        } 
        return read_aux("unquote");
      } else {
        // Symbols
        // Special case: .. is also a valid symbol (really?)
        if(c == '.' && is.peek() != '.') {
          return_token = TK_DOT;
          return C_FALSE;
        }
        // Negative numvers
        if(c == '-'){
          c = is.peek();
          if(c >= '0' && c <= '9') {
            getc(c);
            return read_number(c - '0', true);
          }
        }
        return read_symbol(c);
      }
    }
    // Should not be reached unless there is an actual EOF
    AR_ASSERT(is.eof());
    return C_EOF;
  }

  Value read() {
    Token tk;
    Value exp = read_expr(tk);
    if(tk == TK_DOT) {
      return read_error("unexpected . at toplevel");
    } else if(tk == TK_RPAREN) {
      return read_error("unexpected ) at toplevel");
    } 

    return exp;
  }
};

/** Converts a C++ string to an std::istream for reading expressions */
struct StringReader {
  std::stringstream ss;
  Reader reader;

  StringReader(State& state, std::string str, const std::string& desc = "anonymous"): ss(str), reader(state, ss) {
    reader.file = desc.compare("anonymous") == 0 ? 1 : state.register_file(desc);
  }

  ~StringReader() {
    // delete reader;
  }

  Reader* operator->() {
    return &reader;
  }
};


/** Macro for giving descriptive source info for Scheme code in C strings e.g.
  * c-string@asdf.cpp:351 */
#define AR_STRINGIZE2(x) #x

#define AR_STRINGIZE(x) AR_STRINGIZE2(x)

#define AR_STRING_READER(name, state, string) \
  arete::StringReader name ((state), (string), ("c-string@" __FILE__ ":" AR_STRINGIZE(__LINE__)));

// Various inline garbage collector functions

inline void GC::mark_symbol_table() {
  ARETE_LOG_GC(state.symbol_table.size() << " live symbols");
  for(auto x = state.symbol_table.begin(); x != state.symbol_table.end(); x++) {
    mark(x->second);
  }
}

inline Frame::Frame(State& state, size_t size_, HeapValue*** ptrs): gc(state.gc), size(size_), values(ptrs) {
  gc.frames.push_back(this);
}

inline Frame::Frame(State* state, size_t size_, HeapValue*** ptrs): gc(state->gc), size(size_), values(ptrs) {
  gc.frames.push_back(this);
}

inline Frame::~Frame() {
  AR_ASSERT(gc.frames.back() == this);
  gc.frames.pop_back();
}

inline Handle::Handle(State& state): gc(state.gc), ref() { initialize(); }
inline Handle::Handle(State& state, Value ref_): gc(state.gc), ref(ref_) { initialize(); }
inline Handle::Handle(const Handle& cpy): gc(cpy.gc), ref(cpy.ref) {
  it = gc.handles.insert(gc.handles.end(), this);
}

inline void Handle::initialize() {
  it = gc.handles.insert(gc.handles.end(), this);
}

inline Handle::~Handle() {
  gc.handles.erase(it);
}

/** Output Arete values */
inline std::ostream& operator<<(std::ostream& os, Value v) {
  switch(v.type()) {
    case FIXNUM: return os << v.fixnum_value(); break;
    case CONSTANT: 
      switch(v.constant_value()) {
        case C_TRUE: return os << "#t";
        case C_NIL: return os << "()";
        case C_FALSE: return os << "#f";
        case C_EOF: return os << "#<eof>";
        case C_UNSPECIFIED: return os << "#<unspecified>";
        case C_SYNTAX: return os << "#<syntax>";
        default: return os << "<unknown constant>";
      }
    case FLONUM:
      return os << v.flonum_value(); 
    case SYMBOL:
      return os << v.symbol_name_bytes();
    case STRING: {
      const char* data = v.string_data();
      os << '"';
      for(size_t i = 0; i != v.string_bytes(); i++) {
        switch(data[i]) { 
          case '"': os << "\\\""; break;
          case '\\': os << "\\\\"; break;
          default: os << data[i]; break;
        }
      }
      return os << '"';;
    }
    case PAIR: {
      os << '(' << v.car();
      for(v = v.cdr(); v.type() == PAIR; v = v.cdr())  {
        os << ' ' << v.car();
      }
      if(v != C_NIL) {
        os << " . " << v;
      }
      return os << ')';
    }
    case CHARACTER: {
      os << "#\\";
      switch(v.character()) {
        case '\n': return os << "newline";
        case ' ': return os << "space";
      }
      return os << v.character();
    }
    case FUNCTION: {
      // TODO this should include source information
      os << "#<function ";
      Value name = v.function_name();
      if(name == C_FALSE) {
        os << (void*) v.bits;
      } else {
        os << name.string_data();
      }
      return os << '>';
    }
    case CFUNCTION: {
      os << "#<cfunction ";
      return os << (void*) v.c_function_addr() << '>';
    }
    case VECTOR:
      os << "#(";
      for(size_t i = 0; i != v.vector_length(); i++) {
        os << v.vector_ref(i);
        if(i != v.vector_length() - 1) os << ' ';
      }
      return os << ')';
    case EXCEPTION:
      os << "#<exception '" << v.exception_tag() << " " << v.exception_message();
      if(v.exception_irritants() != C_UNSPECIFIED) {
        os << ' ' << v.exception_irritants();
      }
      return os << '>';
    default: 
      return os << "<unknown>";
  }
  return os;
}

} // namespace arete

#endif // ARETE_HPP