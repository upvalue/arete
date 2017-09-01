// arete.hpp - an embeddable scheme implementation

/*
 * Search for these to navigate the source.
 * (TYPE) Representation of Scheme values.
 * (GC) Garbage collector
 * (RT) Runtime
 * (READ) S-expression reader
 */

#ifndef ARETE_HPP
#define ARETE_HPP

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <iostream>
#include <vector>
#include <unordered_map>

// Assertion macro
#ifndef AR_ASSERT
# define AR_ASSERT assert
#endif 

// Block size.
#ifndef AR_BLOCK_SIZE
# define AR_BLOCK_SIZE 4096
#endif

// If more than ARETE_GC_LOAD_FACTOR% is in use after a collection, the garbage collector will double in-use memory.
#ifndef ARETE_GC_LOAD_FACTOR
# define ARETE_GC_LOAD_FACTOR 80
#endif

#ifndef ARETE_LOG_TAGS
# define ARETE_LOG_TAGS 0
#endif 

#ifndef ARETE_BLOCK_SIZE
# define ARETE_BLOCK_SIZE 4096
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

///// # (TYPE) REPRESENTATION OF SCHEME VALUES

// Scheme values can be either immediate (fixed-point integers and constants) or allocated on the heap.

// Scheme values are generally referenced using an instance of the Value struct, which contains methods that
// help interact with these values in a safe manner and can be tracked by the garbage collector.

// Under the hood, a Value is a ptrdiff_t sized struct which can be either a pointer to a HeapValue
// or an immediate value.

// HeapValues only exist in garbage-collected memory and generally are only used internall.

// The actual bits of a Value look like this:

// - ...1 Fixnum
// - ..10 Constant
// - ..00 Pointer to heap value

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
  SYMBOL = 5,
  // Pointers
  PAIR = 6
};

// Constants:

// 0010 #t
// 0110 #f
// 1010 ()

enum { C_TRUE, C_FALSE, C_NIL };

// A heap-allocated, garbage-collected value.
struct HeapValue {
  // Heap value headers are formatted like this:

  // ...m tttt tttt
  // m = mark
  // t = value type
  size_t header;
  size_t size;

  void initialize(unsigned type, unsigned mark_bit, size_t size_) {
    header = (type) + (mark_bit << 8);
    size = size_;
  }

  unsigned get_type() const {
    return header & 255;
  }

  unsigned char get_mark_bit() const {
    return (header >> 8) & 1;
  }

  void flip_mark_bit() {
    if(get_mark_bit()) {
      header -= 256;
    } else {
      header += 256;
    }
  }
};

// Floating point number
struct Flonum : HeapValue {
  double number;  
};

struct Symbol : HeapValue {
  const char* name;
};

struct Value {
  union {
    HeapValue* heap;
    ptrdiff_t bits;
  };

  Value(): bits(0) {}
  Value(HeapValue* heap_): heap(heap_) {}
  Value(ptrdiff_t bits_): bits(bits_) {}

  // GENERIC METHODS

  bool immediatep() const { return (bits & 3) != 0 || bits == 0; }

  /** Safely retrieve the type of an object */
  unsigned int type() const {
    if(bits & 1) return FIXNUM;
    else if(bits & 2) return CONSTANT;    
    else return heap->get_type();
  }

  // FIXNUMS

  /** Get the value of a fixnum */
  ptrdiff_t fixnum_value() const {
    AR_ASSERT(type() == FIXNUM);
    
    return bits >> 1;
  }

  static Value make_fixnum(ptrdiff_t fixnum) {
    return Value(((fixnum << 1) + 1));
  }

  // CONSTANTS
  unsigned constant_value() const {
    AR_ASSERT(type() == CONSTANT);
    return bits == 2 ? C_TRUE : C_FALSE;
  }

  static Value t() { return Value(2); }
  static Value f() { return Value(6); }
  static Value nil() { return Value(10); }

  // FLONUMS
  double flonum_value() const {
    AR_ASSERT(type() == FLONUM);
    return static_cast<Flonum*>(heap)->number;
  }

  // SYMBOLS
  const char* symbol_name() const {
    AR_ASSERT(type() == SYMBOL);
    return static_cast<Symbol*>(heap)->name;
  }

  // PAIRS
  Value car() const;
  Value cdr() const;
};

struct Pair : HeapValue {
  Value data_car, data_cdr;
};

  
inline Value Value::car() const {
  AR_ASSERT(type() == PAIR);
  return static_cast<Pair*>(heap)->data_car;
}

inline Value Value::cdr() const {
  AR_ASSERT(type() == PAIR);
  return static_cast<Pair*>(heap)->data_cdr;
}

///// (GC) Garbage collector

// The Arete garbage collector is a simple semispace garbage collector.

// It uses some very hacky C++ code to save pointers to stack-values so that they can be replaced when values are
// moved.

struct GC;

/** FrameHack turns a Value& into a pointer to a stack-allocated Value */
struct FrameHack {
  FrameHack(Value& value_): value((HeapValue**) &value_.bits) {}
  ~FrameHack() {}

  HeapValue** value;
};

/** Frames are stack-allocated structures that save pointers to stack Values, allowing the garbage collector to move
 * objects and update pointers to them if necessary */
struct Frame {
  Frame(State& state, size_t size, HeapValue*** values);
  ~Frame();

  GC& gc;
  size_t size;
  HeapValue*** values;
};

#define AR_FRAME(state, ...) \
  FrameHack __arete_frame_ptrs[] = { __VA_ARGS__ };  \
  Frame __arete_frame((state), sizeof(__arete_frame_ptrs) / sizeof(FrameHack), (HeapValue***) __arete_frame_ptrs); 

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
  std::vector<Frame*> frames;
  std::vector<Block*> blocks;
  // Position in the *blocks* vector
  size_t block_i;
  // Position in *blocks_i* block
  size_t block_cursor;
  // Number of total allocations
  size_t allocations;
  // Number of total collections
  size_t collections;
  size_t live_objects_after_collection, live_memory_after_collection, allocated_memory;
  unsigned char mark_bit;
  size_t block_size;

  GC(): block_i(0), block_cursor(0), allocations(0), collections(0), live_objects_after_collection(0),
      live_memory_after_collection(0), allocated_memory(ARETE_BLOCK_SIZE), mark_bit(1), block_size(ARETE_BLOCK_SIZE) {
    Block *b = new Block(ARETE_BLOCK_SIZE, mark_bit);
    blocks.push_back(b);

    // Blocks should be allocated dead
    AR_ASSERT(!live((HeapValue*) b->data));
  }

  ~GC() {
    for(size_t i = 0; i != blocks.size(); i++) {
      delete blocks[i];
    }
  }

  bool live(HeapValue* v) const {
    return v->get_mark_bit() == mark_bit;
  }

  void mark(HeapValue* v) {
    // We use a GOTO here to avoid creating unnecessary stack frames
  again: 
    // If there is no object or object has already been marked
    if(v == 0 || Value(v).immediatep() || live(v))
      return;
    
    live_objects_after_collection++;
    live_memory_after_collection += v->size;

    v->flip_mark_bit();

    AR_ASSERT(live(v));

    switch(v->get_type()) {
      case BLOCK: case FIXNUM: case CONSTANT:
        AR_ASSERT(!"arete:gc: bad value on heap; probably a GC bug"); break;
      case FLONUM:
        return;
      case PAIR:
        mark(static_cast<Pair*>(v)->data_car.heap);
        v = static_cast<Pair*>(v)->data_cdr.heap;
        goto again;
    }
  }

  void collect() {
    ARETE_LOG_GC("collecting");
    collections++;
    live_objects_after_collection = 0;
    live_memory_after_collection = 0;

    block_i = 0;
    block_cursor = 0;

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

    ARETE_LOG_GC("found " << live_objects_after_collection << " live objects taking up " <<
      live_memory_after_collection << "b")

    // Allocate a new block if memory is getting a little overloaded
    double load_factor = (live_memory_after_collection * 100) / allocated_memory;
    if(load_factor >= ARETE_GC_LOAD_FACTOR) {
      ARETE_LOG_GC(load_factor << "% of memory is still live after collection, adding a block");
      block_size *= 2;
      Block* b = new Block(block_size, mark_bit);
      blocks.push_back(b);
    }
  }

  HeapValue* allocate(unsigned type, size_t size) {
    size_t sz = align(8, size);
    bool collected = false;
    // This is actually the meat of the garbage collection algorithm

    // It searches through live memory in a first-fit fashion for somewhere to allocate the value; if it fails,
    // a collection will be triggered.
  retry:
    while(block_i != blocks.size()) {
      while(blocks[block_i]->has_room(block_cursor, sz)) {
        HeapValue* v = (HeapValue*)(blocks[block_i]->data + block_cursor);

        AR_ASSERT(v->size > 0); // assert that memory has been initialized with some kind of size
        if((!live(v) && v->size >= sz)) {
          size_t mem_size = v->size;

          // Success!
          char* memory = blocks[block_i]->data + block_cursor;

          // If there is enough room after this memory to handle another object, note down its
          // size and move on

          // TODO: Coalesce memory
          if(mem_size - sz >= sizeof(Flonum)) {
            // ARETE_LOG_GC("additional " << (mem_size - sz) << " bytes after object");
            HeapValue* next_object = ((HeapValue*) ((blocks[block_i]->data + block_cursor) + sz));
            next_object->initialize(BLOCK, !mark_bit, mem_size - sz);

            AR_ASSERT(!live(next_object));
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
          AR_ASSERT(live(ret));

          // Break up memory as necessary
          ret->size = sz;
          return ret;
        }

        block_cursor += v->size;
      }

      ARETE_LOG_GC("block " << block_i << " ( " << blocks[block_i]->size << "b) out of room, moving on");

      block_i++;
      block_cursor = 0;
    }

    ARETE_LOG_GC("reached end of " << blocks.size() << " blocks");

    if(!collected) {
      // No room, run a collection
      collect();
      collected = true;
      goto retry;
    } else {
      // Collection has failed to create enough space. Give up.
      AR_ASSERT(!"out of room; allocation failed");
      return 0;
    }
  }

  static size_t align(size_t boundary, size_t value) {
    return (((((value) - 1) / (boundary)) + 1) * (boundary));
  }
};

/** A re-entrant instance of the Arete runtime */
struct State {
  typedef std::unordered_map<std::string, Symbol*> symbol_table_t;

  GC gc;
  symbol_table_t symbol_table;

  State(): gc() {}
  ~State() {
    // Free symbol names
    for(symbol_table_t::const_iterator x = symbol_table.begin(); x != symbol_table.end(); ++x) {
      free((void*) x->second->name);
    }
  }

  // Value creation; all of these will cause allocations
  Value make_flonum(double number) {
    Flonum* heap = (Flonum*) gc.allocate(FLONUM, sizeof(Flonum));
    heap->number = number;
    Value v(heap);
    return v;
  }

  Value get_symbol(const std::string& name) {
    symbol_table_t::const_iterator x = symbol_table.find(name);
    if(x == symbol_table.end()) {
      Symbol* heap = static_cast<Symbol*>(gc.allocate(SYMBOL, sizeof(Symbol)));
      const char* name_s = name.c_str();
      size_t size = name.size();
      const char* name_copy = strndup(name_s, size);

      heap->name = name_copy;

      symbol_table.insert(std::make_pair(name, heap));
      return heap;
    } else {
      return x->second;
    }
  }

  Value make_pair(Value car = Value::f(), Value cdr = Value::f()) {
    //HeapValue* pair = gc.allocate(PAIR, sizeof(Pair));
    //return pair;
    Pair* heap = (Pair*) gc.allocate(PAIR, sizeof(Pair));
    heap->data_car = car;
    heap->data_cdr = cdr;
    return heap;
  }
};

inline Frame::Frame(State& state, size_t size_, HeapValue*** ptrs): gc(state.gc), size(size_), values(ptrs) {
  gc.frames.push_back(this);
}

inline Frame::~Frame() {
  AR_ASSERT(gc.frames.back() == this);
  gc.frames.pop_back();
}


// Output Scheme values

inline std::ostream& operator<<(std::ostream& os, Value& v) {
  switch(v.type()) {
    case FIXNUM: return os << v.fixnum_value(); break;
    case CONSTANT: 
      switch(v.constant_value()) {
        case C_TRUE: return os << "#t";
        case C_FALSE: return os << "#f";
      }
    case FLONUM:
      return os << v.flonum_value(); 
    case SYMBOL:
      return os << v.symbol_name();
    case PAIR: {
      os << '('; 
      Value pare = v;
      while(true) {
        Value car = pare.car();
        os << car;
        Value cdr = pare.cdr();
        if(cdr.type() == PAIR) {
          pare = cdr;
        } else if(cdr.type() == CONSTANT && cdr.constant_value() == C_NIL) {
          os << ')';
          break;
        } else {
          os << " . ";
          os << cdr;
          break;
        }
      }
      return os << ')';
    }
    default: 
      return os << "<unknown>";
  }
  return os;
}

} // namespace arete


#endif // ARETE_HPP
