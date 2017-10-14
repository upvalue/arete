// arete.hpp - scheme implementation
#ifndef ARETE_HPP
#define ARETE_HPP

///// Table of contents 
// PRE! Preprocessor macros and compile-time configuration options 
// FWD! Forward declarations
// TYPE! Internal value representation and basic operations
// GC! Garbage collection
// RUN! Runtime 
// CLI! Command line interface
// READ! S-expression input and output
// MISC! Various inline functions 

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <list>
#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <chrono>


///// PRE! Preprocessor macros and compile-time configuration macros


// 0 = Do not evaluate assertions, internal assertions
// 1 = Print warnings, disable some internal assertions
// 2 = Use ARETE_ASSERTION_FAIL macro on failure (by default, exits program)

#ifdef ARETE_DEV
# define ARETE_GC_DEBUG 1
# define ARETE_ASSERTION_LEVEL 2
#endif

#ifndef ARETE_ASSERTION_LEVEL 
# define ARETE_ASSERTION_LEVEL 2
#endif

#ifndef AR_ASSERT
# define AR_ASSERT assert
#endif

#ifndef AR_TYPE_ASSERT
# define AR_TYPE_ASSERT assert
#endif 

#define AR_FRAME(state, ...)  \
  arete::FrameHack __arete_frame_ptrs[] = { __VA_ARGS__ };  \
  arete::Frame __arete_frame((state), sizeof(__arete_frame_ptrs) / sizeof(FrameHack), (HeapValue***) __arete_frame_ptrs); 

#define AR_FRAME_ARRAY(state, size, array, var) \
  HeapValue** var = new HeapValue*[(size)]; \
  HeapValue*** __ar_roots = new HeapValue**[(size)]; \
  for(size_t i = 0; i != (size); i++) { var[i] = (HeapValue*) array[i].heap; __ar_roots[i] = & var [i]; } \
  arete::Frame __arete_array_frame((state), (size), (HeapValue***) __ar_roots );

#ifndef ARETE_BLOCK_SIZE
# define ARETE_BLOCK_SIZE 4096
#endif

#ifndef ARETE_HEAP_SIZE 
# define ARETE_HEAP_SIZE (512000)
#endif 

#ifndef ARETE_GC_LOAD_FACTOR
# define ARETE_GC_LOAD_FACTOR 80
#endif

#define ARETE_GC_SEMISPACE 0
#define ARETE_GC_INCREMENTAL 1

#ifndef ARETE_GC_STRATEGY
# define ARETE_GC_STRATEGY ARETE_GC_INCREMENTAL
#endif 

#ifndef ARETE_GC_DEBUG
# define ARETE_GC_DEBUG 0
#endif

#if ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE && ARETE_GC_DEBUG == 1
# ifndef ARETE_ASSERT_LIVE
   // AR_ASSERT("debug failure" && (arete::current_state != 0));
#  define ARETE_ASSERT_LIVE(obj) \
   AR_ASSERT("attempt to invoke method on non-live object" && (arete::current_state->gc.live((obj)) == true));
# endif
#endif

#ifndef ARETE_ASSERT_LIVE
# define ARETE_ASSERT_LIVE(obj) 
#endif

#ifndef ARETE_LOG_TAGS
# define ARETE_LOG_TAGS 0
#endif 

#define ARETE_LOG_TAG_GC (1 << 0)
#define ARETE_LOG_TAG_READER (1 << 1)

#define ARETE_COLOR_RED "\033[1;31m"
#define ARETE_COLOR_RESET "\033[0m"

#ifndef ARETE_LOG
# define ARETE_LOG(tag, prefix, msg) \
  if((ARETE_LOG_TAGS) & (tag)) { \
    std::cout << ARETE_COLOR_RED << "arete:" << prefix << ": " << ARETE_COLOR_RESET << msg << std::endl; \
  }
#endif 

#define ARETE_LOG_GC(msg) ARETE_LOG((ARETE_LOG_TAG_GC), "gc", msg)
#define ARETE_LOG_READ(msg) ARETE_LOG((ARETE_LOG_TAG_READER), "read", msg)

namespace arete {

// FWD! Forward declarations

struct State;
struct Block;
struct Value;
struct SourceLocation;
struct Pair;

extern size_t gc_collect_timer;

// For debugging purposes only: a global instance of the current state
extern State* current_state;

typedef Value (*c_function_t)(State&, size_t, Value*);

std::ostream& operator<<(std::ostream& os,  Value);

///// TYPE! Internal value representation and basic operations

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

enum Type {
  // Should never be encountered
  RESERVED = 0,
  // Immediate values
  FIXNUM = 1,
  CONSTANT = 2,
  // Should never be encountered except in GC code
  BLOCK = 3,
  // Have no pointers
  FLONUM = 4,
  STRING = 5,
  CHARACTER = 6,
  // Have pointers
  SYMBOL = 7,
  VECTOR = 9,
  VECTOR_STORAGE = 10,
  PAIR = 11, 
  EXCEPTION = 12,
  FUNCTION = 13,
  CFUNCTION = 14,
  TABLE = 15,
  RENAME = 16,
};

inline std::ostream& operator<<(std::ostream& os, Type type) {
  switch(type) {
    case FIXNUM: return os << "fixnum";
    case CONSTANT: return os << "constant";
    case BLOCK: return os << "block";
    case FLONUM: return os << "flonum";
    case STRING: return os << "string";
    case CHARACTER: return os << "character";
    case SYMBOL: return os << "symbol";
    case RENAME: return os << "rename";
    case VECTOR: return os << "vector";
    case PAIR: return os << "pair";
    case EXCEPTION: return os << "exception";
    case FUNCTION: return os << "function";
    case CFUNCTION: return os << "cfunction";
    case TABLE: return os << "table";
    default: return os << "unknown";
  }
}

// Constants:

enum {
  C_TRUE = 2,             // 0000 0010 #t
  C_FALSE = 6,            // 0000 0110 #f
  C_NIL = 10,             // 0000 1010 ()
  C_UNSPECIFIED = 14 ,    // 0000 1110 #<unspecified> 
  C_EOF = 18,             // 0001 0010 #<eof>
  C_SYNTAX = 34,          // 0100 0010 #<syntax>
  C_UNDEFINED = 66,
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

  static const Type CLASS_TYPE = STRING;
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
  static bool immediatep(Value v) { return (v.bits & 3) != 0 || v.bits == 0; }

  bool procedurep() const { return type() == FUNCTION || type() == CFUNCTION; }
  bool identifierp() const { return type() == RENAME || type() == SYMBOL; }

  bool numeric() const {
    Type tipe = type();
    return tipe == FLONUM || tipe == FIXNUM;
  }
  inline bool eqv(const Value &rhs) const {
    if (bits == rhs.bits)
      return true;
    if (type() == FLONUM && rhs.type() == FLONUM) {
      return flonum_value() == rhs.flonum_value();
    }
    return false;
  }

  bool hashable() const {
    Type tipe = type();
    return tipe == STRING || tipe == SYMBOL || tipe == FIXNUM || tipe == CONSTANT;
  }

  /** Safely retrieve the type of an object */
  Type type() const;
  Type type_unsafe() const {
    if(bits & 1) return FIXNUM;
    else if(bits & 2 || bits == 0) return CONSTANT;    
    else return (Type) heap->get_type();
  }

  // FIXNUMS

  ptrdiff_t fixnum_value() const {
    AR_ASSERT(type_unsafe() == FIXNUM);
    
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

  static Value make_boolean(ptrdiff_t cmp) {
    return cmp == 0 ? C_FALSE : C_TRUE;
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
  void vector_clear();
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
  const char* symbol_name_data() const;
  Value symbol_value() const;
  void set_symbol_value(Value);

  /** Quickly compare symbol to string */
  bool symbol_equals(const char* s) const {
    std::string cmp(s);
    return cmp.compare(symbol_name_data()) == 0;
  }

  // Syntactic closures
  Value rename_expr() const;
  Value rename_env() const;
  Value rename_gensym() const;

  // PAIRS
  size_t list_length() const;
  Value list_ref(size_t) const;

  Value car() const;
  Value cadr() const;
  Value caar() const;
  Value cdar() const;
  Value cddr() const;
  Value caddr() const;
  Value cadddr() const;
  Value cdr() const;
  void set_car(Value);
  void set_cdr(Value);

  // True if a pair has been allocated with source code information
  static const unsigned PAIR_SOURCE_BIT = 1 << 9; 
  // True if a pair has been generated as a result of the expansion pass
  static const unsigned PAIR_GENERATED_BIT = 1 << 10;

  bool pair_has_source() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header_bit(PAIR_SOURCE_BIT);
  }

  bool pair_generated() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header_bit(PAIR_GENERATED_BIT);
  }

  /** Return a pointer to a pair's source location. Note that this may be moved by the GC, so should
  be copied. */
  SourceLocation pair_src() const;
  void set_pair_src(const SourceLocation&);

  // EXCEPTION
  static const unsigned EXCEPTION_ACTIVE_BIT = 1 << 9;
  bool is_active_exception() const;
  Value exception_tag() const;
  Value exception_message() const;
  Value exception_irritants() const;
  
  // FUNCTIONS
  static const unsigned FUNCTION_MACRO_BIT = 1 << 10;

  Value function_name() const;
  Value function_arguments() const;
  Value function_parent_env() const;
  Value function_rest_arguments() const;
  Value function_body() const;
  bool function_is_macro() const;

  // C FUNCTIONS
  static const unsigned CFUNCTION_VARIABLE_ARITY_BIT = 1 << 9;

  c_function_t c_function_addr() const;
  Value c_function_name() const;
  bool c_function_variable_arity() const;

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

inline const char* Value::symbol_name_data() const {
  return as<Symbol>()->name.as<String>()->data;
}

inline Value Value::symbol_value() const {
  return as<Symbol>()->value;
}

inline void Value::set_symbol_value(Value v) {
  as<Symbol>()->value = v;
}

struct Rename : HeapValue {
  Value expr, gensym, env;

  static const unsigned CLASS_TYPE = RENAME;
};

inline Value Value::rename_expr() const { return as<Rename>()->expr; }
inline Value Value::rename_env() const { return as<Rename>()->env; }
inline Value Value::rename_gensym() const { return as<Rename>()->gensym; }

/** 
 * Identifies a location in source code.
 */
struct SourceLocation {
  SourceLocation(): source(0), line(0), begin(0), length(0) {}
  SourceLocation(unsigned source_, unsigned line_): source(source_), line(line_) {}

  /** An integer that corresponds to an entry in State::source_names */
  unsigned source;
  unsigned line, begin, length;
};

inline std::ostream& operator<<(std::ostream& os, const SourceLocation& src) {
  os << "SRC " << src.source << " line: " << src.line << " begin: " << src.begin <<
    " length: " << src.length << " end: ";
  if(src.length == 0) 
    return os << "(end of line)";
  else
    return os << (src.begin + src.length);
    //return os << (src.length - src.begin);
}

struct Pair : HeapValue {
  Value data_car, data_cdr;
  SourceLocation src;

  static const unsigned char CLASS_TYPE = PAIR;
};

inline SourceLocation Value::pair_src() const {
  AR_TYPE_ASSERT(type() == PAIR);
  AR_TYPE_ASSERT(pair_has_source());
  return (static_cast<Pair*>(heap)->src);
}

inline void Value::set_pair_src(const SourceLocation& loc) {
  AR_TYPE_ASSERT(type() == PAIR);
  AR_TYPE_ASSERT(pair_has_source());
  static_cast<Pair*>(heap)->src = loc;
}

inline Value Value::car() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return static_cast<Pair*>(heap)->data_car;
}

inline size_t Value::list_length() const {
  Value check(bits);
  if(check == C_NIL) return 0;
  //AR_TYPE_ASSERT(type() == PAIR);
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

inline Value Value::caar() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return car().car();
}

inline Value Value::cdar() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return car().cdr();
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
  Value tag, irritants, message;
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

inline Value Value::function_parent_env() const {
  return as<Function>()->parent_env;
}

inline Value Value::function_rest_arguments() const {
  return as<Function>()->rest_arguments;
}

inline Value Value::function_body() const {
  return as<Function>()->body;
}

inline bool Value::function_is_macro() const {
  AR_TYPE_ASSERT(type() == FUNCTION);
  return heap->get_header_bit(FUNCTION_MACRO_BIT);
}

struct CFunction : HeapValue {
  Value name;
  c_function_t addr;
  size_t min_arity, max_arity;

  static const unsigned CLASS_TYPE = CFUNCTION;
};

inline Value Value::c_function_name() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return as<CFunction>()->name;
}

inline c_function_t Value::c_function_addr() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return as<CFunction>()->addr;
}

inline bool Value::c_function_variable_arity() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return heap->get_header_bit(CFUNCTION_VARIABLE_ARITY_BIT);
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
  AR_ASSERT(i < store->length && "vector out of bounds error");
  return store->data[i];
}

inline void Value::vector_set(size_t i, Value val) {
  AR_TYPE_ASSERT(type() == VECTOR);
  VectorStorage* store = static_cast<VectorStorage*>(static_cast<Vector*>(heap)->storage.heap);
  AR_ASSERT(i < store->length && "vector out of bounds error");
  store->data[i] = val;
}

inline void Value::vector_clear() {
  AR_TYPE_ASSERT(type() == VECTOR);
  as<Vector>()->storage.as<VectorStorage>()->length = 0;
}

inline size_t Value::vector_length() const {
  AR_TYPE_ASSERT(type() == VECTOR);
  VectorStorage* store = static_cast<VectorStorage*>(static_cast<Vector*>(heap)->storage.heap);
  return store->length;
}

// HASH TABLES

struct Table : HeapValue  {
  static const size_t LOAD_FACTOR = 90;

  VectorStorage* chains;
  unsigned char size_log2;
  size_t entries, max_entries;

  static const Type CLASS_TYPE = TABLE;
};

inline ptrdiff_t wang_integer_hash(ptrdiff_t key) {
  key = (~key) + (key << 21);
  key = key ^ (key >> 24);
  key = (key + (key << 3)) + (key << 8); 
  key = key ^ (key >> 14);
  key = (key + (key << 2)) + (key << 4); 
  key = key ^ (key >> 28);
  key = key + (key << 31);
  return key;
}

inline ptrdiff_t x31_string_hash(const char* s) {
  ptrdiff_t h = *s;
  if(h) for(++s; *s; ++s) h = (h << 5) - h + *s;
  return h;
}

inline ptrdiff_t hash_value(Value x, bool& unhashable) {
  unhashable = false;
  switch(x.type()) {
    case STRING:
      return x31_string_hash(x.string_data());
    case SYMBOL:
#if ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE
      // If symbols can be moved (semispace), we need to hash the string, otherwise we can just
      // hash the pointer, so this will fall-through to the below integer hash in the incremental
      // collector
      return x31_string_hash(x.symbol_name_data());
#endif
    case FIXNUM:
    case CONSTANT:
      return wang_integer_hash(x.bits);
    default:
      unhashable = true;
      return 0;
  }
}

///// GC! Garbage collection

struct GC;

/** 
 * FrameHack turns a Value& into a pointer to a stack-allocated Value. 
 */
struct FrameHack {
  FrameHack(Value& value_): value((HeapValue**) &value_.bits) {}
  ~FrameHack() {}
  HeapValue** value;
};

/** 
 * Frames are stack-allocated structures that save pointers to stack Values, allowing the garbage
 * collector to move objects and update pointers to them if necessary 
 */
struct Frame {
  State& state;
  size_t size;
  HeapValue*** values;

  Frame(State& state, size_t size, HeapValue*** values);
  Frame(State* state, size_t size, HeapValue*** values);
  ~Frame();
};

/** An individual tracked pointer. Can be allocated on the heap. */
struct Handle {
  State& state;
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

/** Common GC variables */
struct GCCommon {
  State& state;
  std::vector<Frame*> frames;
  std::list<Handle*> handles;
  // Number of total allocations
  bool collect_before_every_allocation;
  size_t allocations;
  size_t collections, live_objects_after_collection, live_memory_after_collection, heap_size;
  size_t block_size;

  GCCommon(State& state_, size_t heap_size_ = ARETE_BLOCK_SIZE): state(state_), 
    collect_before_every_allocation(false),
    allocations(0),
    collections(0), live_objects_after_collection(0), live_memory_after_collection(0),
    heap_size(heap_size_),
    block_size(heap_size_) {

  }

  ~GCCommon() {}

  // Align a value along a boundary e.g. align(8, 7) == 8, align(8, 16) == 16
  static size_t align(size_t boundary, size_t value) {
    return (((((value) - 1) / (boundary)) + 1) * (boundary));
  }
};

/** Semispace garbage collector */
struct GCSemispace : GCCommon {
  Block *active, *other;
  size_t block_cursor;
  char* other_cursor;
  bool collect_before_every_allocation;

  GCSemispace(State& state_): GCCommon(state_), active(0), other(0), block_cursor(0),
    collect_before_every_allocation(false) {
    active = new Block(heap_size, 0);
  }

  ~GCSemispace() {
    delete active;
  }

  void copy(HeapValue** ref) {
    // If this is a null ptr or immediate value, nothing is necessary
    if(ref == 0 || Value::immediatep(*ref))
      return;
    
    HeapValue* obj = *ref;

    // This object has already been copied
    if(obj->header == RESERVED) {
      (*ref) = (HeapValue*) obj->size;
      return;
    }

    size_t size = obj->size;
    HeapValue* cpy = (HeapValue*) other_cursor;
    other_cursor += size;

    memcpy(cpy, obj, size);
    
    // We use the object's size field to store the forward pointer
    obj->size = (size_t) cpy;
    obj->header = RESERVED;

    (*ref) = cpy;
  }

  void copy_roots();

  void collect(size_t request = 0, bool force = false) {
    collections++;

#ifdef ARETE_BENCH_GC
    auto t1 = std::chrono::high_resolution_clock::now();
#endif

    size_t new_heap_size = heap_size;
    size_t pressure = (live_memory_after_collection * 100) / heap_size;

    ARETE_LOG_GC("gc pressure " << pressure);
    // If we need to grow
    if((pressure >= ARETE_GC_LOAD_FACTOR) || force) {
      new_heap_size *= 2;
      if(new_heap_size <= request) {
        new_heap_size = (heap_size * 2) + request;
      }
    }

    heap_size = new_heap_size = align(ARETE_BLOCK_SIZE, new_heap_size);
    ARETE_LOG_GC("allocating new space of " << new_heap_size << "b");

    other = new Block(new_heap_size, 0);
    other_cursor = other->data;

    copy_roots();


    char* sweep = other->data;
    while(sweep != other_cursor) {
      HeapValue* obj = (HeapValue*) sweep;
      size_t size = obj->size;

      switch(obj->get_type()) {
#define AR_COPY(type, field) copy((HeapValue**) &(((type*)obj)->field))
        // No pointers
        case FLONUM: case CHARACTER: case STRING: break;
        // One pointer
        case VECTOR:
        case CFUNCTION:
        case TABLE:
          AR_COPY(Vector, storage);
          break;
        // Two pointers 
        case SYMBOL:
        case PAIR:
          AR_COPY(Symbol, name);
          AR_COPY(Symbol, value);
          break;
        // Three pointers
        case RENAME:
        case EXCEPTION:
          AR_COPY(Exception, message);
          AR_COPY(Exception, tag);
          AR_COPY(Exception, irritants);
          break;
        // Five pointers
        case FUNCTION:
          AR_COPY(Function, name);
          AR_COPY(Function, parent_env);
          AR_COPY(Function, arguments);
          AR_COPY(Function, rest_arguments);
          AR_COPY(Function, body);
          break;
        // Variable ptrs
        case VECTOR_STORAGE: {
          size_t length = static_cast<VectorStorage*>(obj)->length;
          for(size_t i = 0; i != length; i++) {
            copy((HeapValue**)&static_cast<VectorStorage*>(obj)->data[i].bits);
          }
          break;
        }
        // Should never be encountered on heap
        case BLOCK: case CONSTANT: case FIXNUM: default:
          AR_ASSERT(!"arete:gc: encountered bad value on heap; probably a GC bug");
          break;
#undef AR_COPY
      }

      sweep += size;
    }

    // All done
    block_cursor = other_cursor - other->data;
    live_memory_after_collection = block_cursor;

    // std::cout <<  "live memory after collection " << block_cursor << ' ' << live_memory_after_collection <<  " out of " << heap_size << std::endl;

    delete active;
    active = other;

#ifdef ARETE_BENCH_GC
    auto t2 = std::chrono::high_resolution_clock::now();
    gc_collect_timer += std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
#endif 
  }

  // live is a special debug method available with the Semispace collector.
  // Since checking whether a variable is live is simple pointer arithmetic,
  // we can then insert liveness checks throughout the code to see if we've
  // forgotten to track any particular variables.
  bool live(const Value v) const {
    if(v.immediatep()) return true;
    char* addr = (char*) v.heap;
    return (addr >= active->data && (addr < (active->data + active->size)));
  }

  bool live(HeapValue* v) const {
    char* addr = (char*) v;
    return (addr >= active->data && (addr < (active->data + active->size)));
  }

  bool has_room(size_t size) const {
    return active->has_room(block_cursor, size);
  }

  HeapValue* allocate(Type type, size_t size) {
    size = align(8, size);

    // Bump allocation possible
    if(!has_room(size) || collect_before_every_allocation) {
      collect(size);
      if(!has_room(size)) {
        collect(size, true);
        if(!has_room(size)) {
          AR_ASSERT(!"arete:gc: semispace allocation failed");
          // TODO there should be some kind of error available here
        }
      }
    }

    allocations++;
    HeapValue* v = (HeapValue*) (active->data + block_cursor);
    v->initialize(type, 0, size);
    block_cursor += size;
    AR_ASSERT(v->size == size);
    return v;
  }
};

/** Incremental garbage collector */
struct GCIncremental : GCCommon {
  unsigned char mark_bit;
  std::vector<Block*> blocks;
  size_t block_i, block_cursor;

  GCIncremental(State& state_): GCCommon(state_), mark_bit(1), block_i(0), block_cursor(0) {
    Block *b = new Block(ARETE_BLOCK_SIZE, mark_bit);
    blocks.push_back(b);

    // Blocks should be allocated dead
    AR_ASSERT(!marked((HeapValue*) b->data));
  }

  ~GCIncremental() {
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
    if(v == 0 || Value::immediatep(v) || marked(v))
      return;
    
    live_objects_after_collection++;
    // std::cout << "object of type " << v->get_type() << " is size " << v->size << std::endl;
    live_memory_after_collection += v->size;

    v->flip_mark_bit();

    AR_ASSERT(marked(v));

    switch(v->get_type()) {
      case FLONUM: case STRING: case CHARACTER:
        break;
      // One pointer
      case VECTOR:
      case CFUNCTION:
      case TABLE:
        v = static_cast<CFunction*>(v)->name.heap;
        goto again;
      // Two pointers
      case SYMBOL:
      case PAIR:
        mark(static_cast<Symbol*>(v)->name.heap);
        v = static_cast<Symbol*>(v)->value.heap;
        goto again;
      // Three pointers
      case RENAME:
      case EXCEPTION:
        mark(static_cast<Exception*>(v)->message.heap);
        mark(static_cast<Exception*>(v)->tag.heap);
        v = static_cast<Exception*>(v)->irritants.heap;
        goto again;
      // Five pointers
      case FUNCTION:
        mark(static_cast<Function*>(v)->name.heap);
        mark(static_cast<Function*>(v)->parent_env.heap);
        mark(static_cast<Function*>(v)->arguments.heap);
        mark(static_cast<Function*>(v)->rest_arguments.heap);
        v = static_cast<Function*>(v)->body.heap;
        goto again;
      // Variable pointers
      case VECTOR_STORAGE: {
        size_t length = static_cast<VectorStorage*>(v)->length;
        if(length == 0) return;
        for(size_t i = 0; i != length - 1; i++) {
          mark(static_cast<VectorStorage*>(v)->data[i].heap);
        }
        v = static_cast<VectorStorage*>(v)->data[length - 1].heap;
        goto again;
      }
      case BLOCK: case FIXNUM: case CONSTANT:
        std::cout << v->get_type() << std::endl;
        AR_ASSERT(!"arete:gc: bad value on heap; probably a GC bug"); break;
    }
  }

  void mark_symbol_table();

  void collect() {
#ifdef ARETE_BENCH_GC
    auto t1 = std::chrono::high_resolution_clock::now();
#endif 
    // ARETE_LOG_GC("collecting");
    collections++;
    live_objects_after_collection = live_memory_after_collection = 0;
    block_i = block_cursor = 0;

    // TODO if called early this should go through marking everything

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

    for(std::list<Handle*>::iterator i = handles.begin(); i != handles.end(); i++) {
      mark((*i)->ref.heap);
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
#ifdef ARETE_BENCH_GC
    auto t2 = std::chrono::high_resolution_clock::now();
    gc_collect_timer += std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
#endif 
  }

  HeapValue* allocate(Type type, size_t size) {
    size_t sz = align(8, size);
    bool collected = false;
    ++allocations;

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
};

// RUN! The Arete Runtime

/** A re-entrant instance of the Arete runtime */
struct State {

#if ARETE_GC_STRATEGY == ARETE_GC_INCREMENTAL
  GCIncremental gc;
#elif ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE
  GCSemispace gc;
#endif 

  typedef std::unordered_map<std::string, Symbol*> symbol_table_t;
  size_t gensym_counter;
  // Fascinating: removing this causes some kind of error with symbol_table_t.
  // Some kind of C++ initialization issue, but I'm not sure what kind.
  bool print_expansions;
  symbol_table_t symbol_table;

  State():  gc(*this), gensym_counter(0) {
    current_state = this;
  }
  ~State() {
    current_state = 0;
  }

  std::vector<std::string> source_names;
  std::vector<std::string> source_contents;

  // Source code location tracking
  unsigned register_source(const std::string& path, std::istream& is) {
    ARETE_LOG_READ("registering source " << path);
    std::ostringstream contents;
    contents << is.rdbuf();
    is.seekg(0, std::ios::beg);

    source_names.push_back(path);
    source_contents.push_back(contents.str());

    return source_names.size() - 1;
  }

  #define AR_SYMBOLS_AUX(name) S_##name

  std::vector<Value> globals;

  // Global variables
  
  // Note that order is important here. Everything from S_QUOTE to S_LETREC_SYNTAX
  // will be set to C_SYNTAX meaning that its values can't be handled directly
  // by Scheme code. 

  // All the symbols here will be allocated on boot so that using them doesn't cause an allocation
  // eg in the reader. And are defined in the State::boot procedure.
  enum Global {
    // C_SYNTAX values
    S_QUOTE,
    S_BEGIN,
    S_DEFINE,
    S_LAMBDA,
    S_IF,
    S_COND,
    S_AND,
    S_OR,
    S_SET,
    // Module forms
    S_DEFINE_LIBRARY,
    S_IMPORT,
    S_DEFINE_SYNTAX,
    S_LET_SYNTAX,
    S_LETREC_SYNTAX,
    // Everything above this line will be set to C_SYNTAX
    // Used by interpreter
    S_ELSE,
    // Used by reader
    S_QUASIQUOTE,
    S_UNQUOTE,
    S_UNQUOTE_SPLICING,
    S_RENAME,
    // Used by module system (rename is also used in module declarations)
    S_UNQUALIFIED,
    S_ONLY,
    S_EXCEPT,
    S_PREFIX,
    // Errors that may be thrown by the runtime
    S_FILE_ERROR,
    S_READ_ERROR,
    S_EVAL_ERROR,
    S_TYPE_ERROR,
    S_EXPAND_ERROR,
    S_SYNTAX_ERROR,
    // Global variables
    G_EXPANDER_PRINT,
    G_EXPANDER,
    G_END
  };

  /** Performs various initializations required for an instance of Arete;
    * this is separate so the uninitialized State can be unit tests in various ways */
  void boot() {
    source_names.push_back("unknown");
    source_names.push_back("anonymous");
    source_contents.push_back("");
    source_contents.push_back("");

    static const char* _symbols[] = {
      // C_SYNTAX values
      "quote", "begin", "define", "lambda", "if", "cond", "and", "or", "set!",
      "define-syntax", "let-syntax", "letrec-syntax", "define-library", "import",
      // Used by interpreter
      "else",
      // Used by reader      
      "quasiquote", "unquote", "unquote-splicing", "rename",
      // Modules
      "unqualified", "only", "except", "prefix",
      // Tags for errors that may be thrown by the runtime
      "file", "read", "eval", "type", "expand", "syntax",
      // Various variables
      "*expander-print*",
      "expander"
    };

    AR_ASSERT((sizeof(_symbols) / sizeof(const char*)) == G_END &&
      "did you forget to change _symbols to match enum Global?");

     // TODO: This is suitably confusing and there should probably be multiple vectors:
     // for symbols used directly, for symbols used to store values, and for other values (strings)
    Value s;
    for(size_t i = 0; i != G_END; i++) {
      if(i >= G_END) {
        s = make_string(_symbols[i]);
      } else {
        s = get_symbol(_symbols[i]);

        if(i <= S_LETREC_SYNTAX) {
          s.as<Symbol>()->value = C_SYNTAX;
        } else {
          s.as<Symbol>()->value = C_UNDEFINED;
        }
      }
      globals.push_back(s);
    }

    install_core_functions();
    // gc.allocations = 0;
  }

#undef AR_SYMBOLS

  // Value creation; all of these will cause allocations
  Value make_flonum(double number) {
    Flonum* heap = (Flonum*) gc.allocate(FLONUM, sizeof(Flonum));
    heap->number = number;
    Value v(heap);
    return v;
  }

  Value get_symbol(Global sym) {
    Value sym2 =  (globals.at((size_t) sym));
    // std::cout << sym2 << std::endl;
    AR_ASSERT(sym2.type() == SYMBOL);
    return sym2;
  }

  Value get_global_value(Global sym) {
    Value s = globals.at((size_t) sym);
    return s.as<Symbol>()->value;
  }

  void set_global_value(Global sym, Value v) {
    globals.at((size_t) sym).as<Symbol>()->value = v;
  }

  Value get_symbol(Value name) {
    AR_TYPE_ASSERT(name.type() == STRING);
    std::string cname(name.string_data());
    return get_symbol(cname);
  }

  Value get_symbol(const std::string& name) {
    symbol_table_t::const_iterator x = symbol_table.find(name);
    if(x == symbol_table.end()) {
      Symbol* heap = static_cast<Symbol*>(gc.allocate(SYMBOL, sizeof(Symbol)));
      AR_ASSERT(heap->get_type() == SYMBOL);

      Value sym = heap, string;
      AR_FRAME(this, sym, string);

      string = make_string(name);

      sym.as<Symbol>()->value = C_UNDEFINED;
      sym.as<Symbol>()->name = string;

      symbol_table.insert(std::make_pair(name, (Symbol*) sym.heap));

      return sym;
    } else {
      AR_ASSERT(symbol_table.size() > 0);
      return x->second;
    }
  }

  Value gensym(Value sym) {
    std::ostringstream os;
    os << "#:" << sym.symbol_name_data() << gensym_counter;
    gensym_counter++;
    return get_symbol(os.str());
  }

  Value make_rename(Value expr, Value env) {
    AR_FRAME(this, expr, env);
    Rename* heap = static_cast<Rename*>(gc.allocate(RENAME, sizeof(Rename)));
    heap->expr = expr;
    heap->env = env;
    heap->gensym = C_FALSE;
    return heap;
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
    Value pare = C_FALSE;
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

  void table_setup(Value table, size_t size_log2) {
    VectorStorage* chains = 0;

    AR_FRAME(this, table);

    size_t size = 1 << size_log2;
    chains = static_cast<VectorStorage*>(make_vector_storage(size).heap);
    // In case of GC move
    Table *heap = table.as<Table>();
    heap->entries = 0;
    heap->size_log2 = size_log2;
    // Fill entries with #f
    chains->length = size;
    for(size_t i = 0; i != chains->length; i++) {
      chains->data[i] = C_FALSE;
    } 
    heap->chains = chains;
    // Pre-calculate max entries
    heap->max_entries = (Table::LOAD_FACTOR * size) / 100;
  }

  ptrdiff_t hash_index(Value table, Value key, bool& unhashable) {
    ptrdiff_t hash = hash_value(key, unhashable);
    return hash & table.as<Table>()->chains->length - 1;
  }

  void table_grow(Value table) {
    Value old_chains_ref, chain;
    AR_FRAME(this, table, old_chains_ref, chain);
    old_chains_ref = table.as<Table>()->chains;
    table_setup(table, table.as<Table>()->size_log2 + 1);
    // Insert all old values
    VectorStorage* old_chains = old_chains_ref.as<VectorStorage>();
    for(size_t i = 0; i != old_chains->length; i++) {
      chain = old_chains->data[i];
      while(chain.type() == PAIR) {
        table_insert(table, chain.caar(), chain.cdar());
        old_chains = old_chains_ref.as<VectorStorage>();
        chain = chain.cdr();
      }
    }
  }

  Value unhashable_error(Value irritant) {
    std::ostringstream os;
    os << " value " << irritant << " is unhashable";
    return type_error(os.str());
  }

  bool equals(Value a, Value b) const {
    if(a.bits == b.bits) return true;

    if(a.type() == VECTOR && b.type() == VECTOR) {
      if(a.vector_length() != b.vector_length()) return false;
      for(size_t i = 0; i < a.vector_length(); i++) {
        if(!equals(a.vector_ref(i), b.vector_ref(i))) {
          return false;
        }
      }
      return true;
    } else if(a.type() == PAIR && b.type() == PAIR) {
      while(a.type() == PAIR && b.type() == PAIR) {
        if(!equals(a.car(), b.car())) {
          return false;
        }
        a = a.cdr();
        b = b.cdr();
      }

      if(a != C_NIL || b != C_NIL) {
        return equals(a, b);
      }

      return C_TRUE;
    } else if(a.type() == STRING && b.type() == STRING) {
      if(a.string_bytes() != b.string_bytes()) return false;
      return strncmp(a.string_data(), b.string_data(), a.string_bytes()) == 0;
    }

    return a.bits == b.bits;
  }

  Value table_get_cell(Value table, Value key) {
    bool unhashable;
    ptrdiff_t index = hash_index(table, key, unhashable);
    if(unhashable) return unhashable_error(key);
    Value chain = C_FALSE;
    chain = table.as<Table>()->chains->data[index];
    while(chain.type() == PAIR) {
      if(equals(chain.caar(), key)) {
        return chain.car();
      }
      chain = chain.cdr();
    }
    return C_FALSE;
  }

  Value table_set(Value table, Value key, Value value) {
    Value cell = table_get_cell(table, key);
    if(cell.is_active_exception()) return cell;
    if(cell != C_FALSE) {
      cell.set_cdr(value);
      return C_TRUE;
    } else {
      return table_insert(table, key, value);
    }
    return C_FALSE;
  }

  Value table_get(Value table, Value key, bool& found) {
    Value cell = table_get_cell(table, key);
    if(cell != C_FALSE) {
      found = true;
      return cell.cdr();
    } else {
      found = false;
      return C_FALSE;
    }
  }

  Value table_insert(Value table, Value key, Value value) {
    AR_TYPE_ASSERT(table.type() == TABLE);
    Value chain;
    AR_FRAME(this, table, key, value, chain);

    Table* htable = table.as<Table>();

    if(htable->entries >= htable->max_entries) {
      table_grow(table);
    }

    bool unhashable;
    ptrdiff_t index = hash_index(table, key, unhashable);
    if(unhashable) return unhashable_error(value);

    // Build chain
    chain = make_pair(key, value);

    if(key.type() == STRING) {
      key = string_copy(key);
    }

    htable = table.as<Table>();

    // Handle collision
    if(htable->chains->data[index] != C_FALSE) {
      chain = make_pair(chain, htable->chains->data[index]);
    } else {
      chain = make_pair(chain, C_NIL);
    }

    // Insert chain
    htable = table.as<Table>();
    htable->chains->data[index] = chain;
    htable->entries++;

    return C_UNSPECIFIED;
  }

  Value make_string(const std::string& body) {
    String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + body.size()));
    heap->bytes = body.size();
    strncpy(heap->data, body.c_str(), body.size());
    AR_ASSERT(heap->data[heap->bytes] == '\0');
    // heap->data[heap->bytes] = '\0';

    return heap;
  }

  Value string_copy(Value x) {
    AR_FRAME(this, x);
    String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + x.string_bytes()));
    strncpy(heap->data, x.string_data(), x.string_bytes());
    heap->bytes = x.string_bytes();
    heap->data[x.string_bytes()] = '\0';
    return heap;
  }

  Value make_exception(Value tag, Value message, Value irritants = C_UNSPECIFIED) {
    Value exc;
    AR_FRAME(this, tag, message, irritants, exc);
    Exception* heap = static_cast<Exception*>(gc.allocate(EXCEPTION, sizeof(Exception)));
    exc.heap = heap;
    heap->tag = tag;
    heap->message = message;
    heap->irritants = irritants;
    return heap;
  }

  Value make_exception(Value tag, const std::string& cmessage, Value irritants = C_UNSPECIFIED) {
    Value message;
    AR_FRAME(this, tag, message, irritants);
    message = make_string(cmessage);
    return make_exception(tag, message, irritants);
  }

  Value make_exception(const std::string& ctag, const std::string& cmessage, Value irritants = C_UNSPECIFIED) { 
    Value tag;
    AR_FRAME(this, tag, irritants);
    tag = get_symbol(ctag);
    return make_exception(tag, cmessage, irritants);
  }
  
  /** Make an exception with a builtin tag */
  Value make_exception(Global s, const std::string& cmessage, Value irritants = C_UNSPECIFIED) {
    return make_exception(get_symbol(s), cmessage, irritants);
  }

  Value make_table(size_t size_log2 = 4) {
    Value table = static_cast<Table*>(gc.allocate(TABLE, sizeof(Table)));
    AR_FRAME(this, table);

    table_setup(table, size_log2);

    return table;
  }
  
  Value make_c_function(Value name, c_function_t addr, size_t min_arity, size_t max_arity, bool variable_arity) {
    if(max_arity == 0)
      max_arity = min_arity;
    AR_FRAME(this, name);
    CFunction *cfn = static_cast<CFunction*>(gc.allocate(CFUNCTION, sizeof(CFunction)));
    cfn->name = name;
    cfn->addr = addr;
    cfn->min_arity = min_arity;
    cfn->max_arity = max_arity;
    if(variable_arity)
      cfn->set_header_bit(Value::CFUNCTION_VARIABLE_ARITY_BIT);
    return cfn;
  }

  // Print out a table's internal structure for debugging purposes
  void print_table_verbose(Value tbl) {
    Table* table = tbl.as<Table>();
    std::cout << "#<table size_log2: " << (size_t) table->size_log2 << " entries: " << table->entries << 
      " max_entries: " << table->max_entries << std::endl;

    for(size_t i = 0; i != table->chains->length; i++) {
      Value chain = table->chains->data[i];
      if(chain != C_FALSE) {
        std::cout << "  chain " << i << ": ";
        std::cout << chain << std::endl;
      }
    }

    std::cout << '>' << std::endl;
  }

  /**
   * Print information about an erroneous pair
   */
  void print_src_pair(std::ostream& os, Value pair) {
    if(pair.type() == PAIR && pair.pair_has_source()) {
      SourceLocation src(pair.pair_src());
      print_src_line(os, src);
    }
  }

  /**
   * Print an erroneous line of source code with offending information highlighted
   */
  void print_src_line(std::ostream& os, const SourceLocation& src) {
    unsigned seek_line = 1;
    std::string source_line;
    char c;
    
    if(src.source > 0 && source_contents[src.source].size() > 0) {
      std::stringstream ss;

      ss >> std::noskipws;
      ss << source_contents[src.source];
      os << "At " << source_name(src.source) << " line " << src.line << std::endl;

      while(!ss.eof()) {
        if(seek_line == src.line) {
          unsigned line_position = ss.tellg();
          std::getline(ss, source_line);
          os << ARETE_COLOR_RED << source_line << ARETE_COLOR_RESET << std::endl;

          size_t i, j;
          // Print whitespace for each char in source line until the beginning of this element
          for(i = line_position, j = 0; i < src.begin; i++, j++) {
            os << ' ';
          }

          unsigned limit = j + src.length;
          // If end_position is 0, we'll highlight the whole line this error occurred on.
          // Otherwise, try to highlight specific errors eg.
          // (hello #bad)
          //         ^
          for(; j != source_line.size() && ((src.length == 0) || j != limit); j++) {
            os << '^';
          }
          break;
        }

        ss >> c;
        if(c == '\n') {
          seek_line++;
        }
      }
    }
    os << std::endl;
  }

  /**
   * Special-cased exception printing: this pretty-prints an exception, but also handles 
   * printing erroneous source code for certain builtin exceptions which can communicate this
   * information (eg errors generated by the macro expander)
   */
  void print_exception(std::ostream& os, Value exc) {
    AR_TYPE_ASSERT(exc.type() == EXCEPTION);

    print_stack_trace(os);

    if(exc.exception_tag() == globals[State::S_EVAL_ERROR]) {
      os << "Evaluation error: " << exc.exception_message().string_data() << std::endl;
      print_src_pair(os, exc.exception_irritants());
    } else if(exc.exception_tag() == globals[State::S_EXPAND_ERROR]
      || exc.exception_tag() == globals[State::S_SYNTAX_ERROR]) {

      Value irritants = exc.exception_irritants();

      if(exc.exception_tag() == globals[State::S_EXPAND_ERROR]) {
        os << "Error during expansion: ";
      } else if(exc.exception_tag() == globals[State::S_SYNTAX_ERROR]) {
        os << "Error in macro synatx: ";
      }

      os << exc.exception_message().string_data() << std::endl;

      if(irritants.list_length() == 1) {
        print_src_pair(os, irritants.list_ref(0));
      } else if(irritants.list_length() == 2) {
        print_src_pair(os, irritants.list_ref(1));

      }
    } else {
      os << exc << std::endl;
    }
  }

  void print_gc_stats(std::ostream& os) {
    os << (gc.heap_size / 1024) << "kb in use after " << gc.collections << " collections and "
      << gc.allocations << " allocations " << std::endl;

#ifdef ARETE_BENCH_GC
    std::cout << (gc_collect_timer / 1000) << "ms in collection" << std::endl;
#endif
  }

  std::string source_name(unsigned source) {
    return source_names.at(source);
  }

  void print_stack_trace(std::ostream& os = std::cerr, bool clear = true) {
    if(stack_trace.size() > 0)
      os << "Stack trace: " << std::endl;
    for(size_t i = 0; i != stack_trace.size(); i++)
      os << stack_trace.at(i) << std::endl;


    if(clear)
      stack_trace.clear();
  }

  /** Return a description of a source location */
  std::string source_info(const SourceLocation loc, Value fn_name = C_FALSE) {
    std::ostringstream ss;
    ss << source_names[loc.source] << ':' << loc.line;
    if(fn_name == C_TRUE) 
      ss << " in toplevel";
    else if(fn_name != C_FALSE)
      ss << " in " << fn_name;
    return ss.str();
  }

  /** Return a description of the source location of a pair */
  std::string source_info(Value expr, bool& found) {
    found = false;
    if(expr.type() == PAIR && expr.pair_has_source()) {
      SourceLocation loc = expr.pair_src();
      found = true;
      return source_info(loc);
    } else {
      return "unknown";
    }
  }

  ///// (EVAL) Interpreter

  // Functions for interacting with the Scheme environment

#define AR_FN_EXPECT_POSITIVE(state, argv, i) \
  if((argv[(i)]).fixnum_value() < 0) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected argument " << (i) << " to be a positive fixnum but got " << (argv)[(i)];  \
    return (state).eval_error(__os.str()); \
  }

#define AR_FN_EXPECT_TYPE(state, argv, i, expect) \
  if((argv)[(i)].type() != (expect)) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected argument " << (i) << " to be of type " << (Type)(expect) << " but got " << (Type)(argv[i].type()); \
    return (state).type_error(__os.str()); \
  } 

#define AR_FN_ASSERT_ARG(state, i, msg, expr) \
  if(!(expr)) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected argument " << (i) << ' ' << msg ; \
    return (state).type_error(__os.str()); \
  }

  std::vector<std::string> stack_trace;

  void install_core_functions();

  /** Defines a function both in the core module and as a top-level value; used during booting */
  void defun_core(const std::string& cname, c_function_t addr, size_t min_arity, size_t max_arity = 0, bool variable_arity = false) {
    Value cfn, sym, name;

    AR_FRAME(this, cfn, sym, name);
    name = make_string(cname);
    cfn = make_c_function(name, addr, min_arity, max_arity, variable_arity);

    sym = get_symbol(name);
    sym.set_symbol_value(cfn);

  }
 
  std::ostream& warn() { return std::cerr << "arete: Warning: " ; }

  // Environments are just vectors containing the parent environment in the first position
  // and variable names/values in the even/odd positions afterwards e.g.

  // #(#f hello 12345)
  // for a environment one level below toplevel after a (define hello 12345)

  static const size_t VECTOR_ENV_FIELDS = 2;

  Value make_env(Value parent = C_FALSE) {
    Value vec;
    AR_FRAME(this, vec, parent);
    vec = make_vector(3);
    vector_append(vec, parent);
    vector_append(vec, C_FALSE);
    return vec;
  }

  void env_set(Value env, Value name, Value val) {
    AR_FRAME(this, env, name, val);
    AR_TYPE_ASSERT(name.identifierp());
    // AR_TYPE_ASSERT(name.type() == SYMBOL);
    // Toplevel set
    while(env != C_FALSE) {
      for(size_t i = env.vector_length() - 1; i >= VECTOR_ENV_FIELDS; i -= 2) {
        if(identifier_equal(env.vector_ref(i-1), name)) {
          env.vector_set(i, val);
          return;
        }
      }
      env = env.vector_ref(0);
    }
    if(name.type() == RENAME) {
      name = name.rename_expr();
    }
    name.as<Symbol>()->value = val;
  }

  bool identifier_equal(Value id1, Value id2) {
    if(id1 == id2) return true;

    if(id1.type() == RENAME && id2.type() == RENAME) {
      return (id1.rename_env() == id1.rename_env()) && (id1.rename_expr() == id2.rename_expr());
    }

    return false;
  }

  bool env_defined(Value env, Value name) {
    if(name.type() != SYMBOL) return false;

    if(env.type() == VECTOR) {
      for(size_t i = env.vector_length() - 1; i >= VECTOR_ENV_FIELDS; i -= 2) {
        if(identifier_equal(env.vector_ref(i-1), name)) {
          return true;
        }
      }
    } else if(env == C_FALSE) {
      return name.symbol_value() != C_UNDEFINED;
    }
    return false;
  }


  /**
   * This is the env_lookup backend. It is necessarily complex because it handles lookups for the
   * interpreter and for the hygienic macro expander. See fn_env_compare and fn_env_resolve in
   * arete.cpp.
   */
  Value env_lookup_impl(Value& env, Value name, Value& rename_key, bool& reached_toplevel) {
    rename_key = C_FALSE;
    reached_toplevel = false;

    // First we search through vectors with identifier_equal, which only
    // returns true if symbols are the same or if renames have the same env and expr
    while(env.type() == VECTOR) {
      for(size_t i = env.vector_length() - 1; i >= VECTOR_ENV_FIELDS; i -= 2) {
        // Here we check for function-level renames in the environment.
        // This is necessary because the expansion pass will replace them
        // with gensyms before returning the full expression
        if(identifier_equal(env.vector_ref(i-1), name)) {
          rename_key = env.vector_ref(i-1);
          return env.vector_ref(i);
        }
      }
      env = env.vector_ref(0); // check parent environment
    }

    // If we've reached here, this is a module or toplevel rename and thus we can resolve it
    // the same as a symbol
    if(name.type() == RENAME) {
      env = name.rename_env();
      name = name.rename_expr();
    }

    reached_toplevel = true;

    AR_ASSERT(name.type() == SYMBOL);

    return name.as<Symbol>()->value;
  }

  Value env_lookup(Value env, Value name) {
    Value rename_key;
    bool found;
    return env_lookup_impl(env, name, rename_key, found);
  }

  Value type_error(const std::string& msg) {
    return make_exception("type-error", msg);
  }

  Value eval_error(const std::string& msg, Value exp = C_FALSE);
  Value eval_body(Value env, Value fn_name, Value calling_fn_name, Value src_exp, Value body);
  Value eval_cond(Value env, Value exp, Value fn_name);
  Value eval_boolean_op(Value env, Value exp, Value fn_name, bool is_or);
  Value eval_begin(Value env, Value exp, Value fn_name);
  Value eval_lambda(Value env, Value exp);
  Value eval_define(Value env, Value exp, Value fn_name);
  Value eval_set(Value env, Value exp, Value fn_name);
  Value eval_if(Value env, Value exp, bool has_else, Value fn_name);
  Value eval(Value env, Value exp, Value fn_name = C_FALSE);

  /** Apply a scheme function */
  Value apply_scheme(Value env,  Value fn, Value args, Value src_exp, Value calling_fn_name, bool eval_args = true);
  Value apply_c(Value env, Value fn, Value args, Value src_exp, Value fn_name, bool eval_args = true);
  
  /** Apply a C or a Scheme function */
  Value apply_generic(Value fn, Value args, bool eval_args) {
    AR_ASSERT(fn.procedurep());
    if(fn.type() == FUNCTION) {
      return apply_scheme(C_FALSE, fn, args, C_FALSE, C_FALSE, eval_args);
    } else {
      return apply_c(C_FALSE, fn, args, C_FALSE, C_FALSE, false);
    }
  }

  Value expand_expr(Value exp) {
    Value expand = get_global_value(G_EXPANDER);

    // Comment out to disable macroexpansion
    if(expand != C_UNDEFINED) {
      Value args, sym, mod;
      AR_FRAME(this, expand, exp, args, sym);
      args = make_pair(C_FALSE, C_NIL);
      args = make_pair(exp, args);

      exp = apply_scheme(C_FALSE, expand, args, exp, C_FALSE, false);
      if(exp.is_active_exception()) {
        stack_trace.insert(stack_trace.begin(), "Error during expansion");
        return exp;
      }

      if(get_global_value(G_EXPANDER_PRINT) == C_TRUE) {
        std::cout << "Expanded: " << exp << std::endl;
      }
    } 

    return exp;
  }

  Value eval_toplevel(Value exp) {
    exp = expand_expr(exp);

    return eval(C_FALSE, exp);
  }

  ///// MODULES

  std::vector<std::string> load_paths;

  Value load_stream(std::istream&, size_t source = 0);
  Value load_file(const std::string&);
  Value load_module(const std::string&);

  // Command line interface

  int enter_cli(int, char*[]);
};

///// READ! S-Expression reader

struct XReader {
  /** Instance of the Arete runtime */
  State& state;
  /** Open stream */
  std::istream& is;
  /** Current position in the stream */
  unsigned source, position, line, column;
  /** Either C_FALSE or an exception that has been encountered during reading; necessary because 
   * the tokenizer does not return Values. read should never be called after this has been set */
  Value active_error;

  /** A temporary buffer that the tokenizer will fill with atomic data (strings, numbers, etc) */
  std::string buffer;
  /** Position where BUFFER has started */
  unsigned token_start_position, token_start_line;

  enum TokenType {
    TK_ERROR,
    TK_READ_NEXT,
    TK_LPAREN,
    TK_RPAREN,
    TK_VECTOR_OPEN,
    TK_DOT,
    TK_SYMBOL,
    TK_STRING,
    TK_QUOTE,
    TK_UNQUOTE,
    TK_UNQUOTE_SPLICING,
    TK_QUASIQUOTE,
    TK_RENAME,
    TK_FLONUM,
    TK_FIXNUM,
    TK_CHARACTER,
    TK_TRUE,
    TK_FALSE,
    TK_EXPRESSION_COMMENT,
    TK_EOF
  };

  XReader(State& state_, std::istream& is_, const std::string& desc = "anonymous"): state(state_), is(is_), source(0), position(0),
      line(1), column(1), active_error(C_FALSE) {
    
    is >> std::noskipws;

    if(desc.compare("anonymous") == 0) {
      source = 0;
    } else {
      source = state.register_source(desc, is);
    }
  }

  ~XReader() {}

  /**
   * Read a character while tracking source code location
   * @param c reference to a character to be read.
   * @return Whether the stream has hit EOF
   */
  bool getc(char& c) {
    is >> c;
    if(is.eof()) return false;
    column++;
    position++;
    if(c == '\n') {
      line++; column = 0;
    }
    return !is.eof();
  }

  /**
   * Peek at the next character
   * @return Whether the stream has hit EOF
   */
  bool peekc(char& c) {
    c = is.peek();
    return !is.eof();
  }

  /**
   * Consume a character while tracking source code location
   */
  void eatc() {
    char c;
    getc(c);
  }

  /** Makes a pair with source code information. See SourceLocation for details on arguments */
  Value make_src_pair(Value, Value, unsigned, unsigned, unsigned);

  /**
   * Set active_error to an exception highlighting an erronenous expression
   * @param message Exception text
   * @param start_line The line the irritant occurred on
   * @param start_position The position in the stream where the irritant occurred
   * @param end_position The position in the stream to stop highlighting at; will continue to the end of the line if 0.
   */
  Value read_error(const std::string& message, unsigned start_line, unsigned start_position, unsigned end_position);

  /** Set active_error to a message describing an unexpected end-of-file. Same params as read_error */
  Value unexpected_eof(const std::string& message, unsigned start_line, unsigned start_position, unsigned end_position);

  TokenType tokenize_number(bool negative = false);
  void tokenize_symbol();
  void tokenize_string();

  TokenType next_token();
  Value read_aux(const std::string&, unsigned, Value);
  // #'asdf => (rename (quote asdf))
  Value read_aux2(const std::string&, unsigned, Value, Value);
  Value read_expr(TokenType);
  /** The entry point for reading an expression */
  Value read();
};

/** Output Arete values */
inline std::ostream& operator<<(std::ostream& os, Value v) {
  switch(v.type()) {
    case FIXNUM:  {
      return os << v.fixnum_value();
    }
    case CONSTANT: 
      switch(v.constant_value()) {
        case C_TRUE: return os << "#t";
        case C_NIL: return os << "()";
        case C_FALSE: return os << "#f";
        case C_EOF: return os << "#<eof>";
        case C_UNSPECIFIED: return os << "#<unspecified>";
        case C_SYNTAX: return os << "#<syntax>";
        case C_UNDEFINED: return os << "#<undefined>";
        default: return os << "<unknown constant " << ((ptrdiff_t) v.bits) << '>';
      }
    case FLONUM:
      return os << v.flonum_value(); 
    case SYMBOL:
      return os << v.symbol_name_data();
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
      // if(v.pair_has_source()) os << '&';
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
      os << "#<" << (v.function_is_macro() ? "macro" : "function") << ' ';
      Value name = v.function_name();
      if(name == C_FALSE) {
        os << (void*) v.bits;
      } else {
        os << name;
      }
      return os << '>';
    }
    case CFUNCTION: {
      os << "#<cfunction ";
      return os << v.c_function_name().string_data() << '>';
    }
    case VECTOR:
      os << "#(";
      for(size_t i = 0; i != v.vector_length(); i++) {
        os << v.vector_ref(i);
        if(i != v.vector_length() - 1) os << ' ';
      }
      return os << ')';
    case RENAME: {
      Value env = v.rename_env(),
        sym = v.rename_gensym() == C_FALSE ? v.rename_expr() : v.rename_gensym();

      if(env == C_FALSE) {
        return os << "#R:" << sym;
      } else if(env.type() == TABLE) {
        return os << "#R:module:" << sym;
      } else {
        return os << "#R:local:" << sym;
      }
    }
    case TABLE:
      return os << "#<table entries: " << v.as<Table>()->entries << '>';
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

// MISC! Various inline functions 

inline void GCIncremental::mark_symbol_table() {
  ARETE_LOG_GC(state.symbol_table.size() << " live symbols");
  for(auto x = state.symbol_table.begin(); x != state.symbol_table.end(); x++) {
    mark(x->second);
  }
}

inline void GCSemispace::copy_roots() {
  // std::cout << state.symbol_table.size() << " live symbols" << std::endl;
  for(size_t i = 0; i != frames.size(); i++) {
    Frame* f = frames[i];
    for(size_t j = 0; j != f->size; j++) {
      copy(f->values[j]);
    }
  }

  for(size_t i = 0; i != state.globals.size(); i++) {
    copy(&state.globals[i].heap);
  }

  for(std::list<Handle*>::iterator i = handles.begin(); i != handles.end(); i++) {
    copy(&((*i)->ref.heap));
  }

  ARETE_LOG_GC(state.symbol_table.size() << " live symbols");
  // TODO: To make this a weak table, simply check for RESERVED in this. If forwarded, set it up
  // otherwise delete reference
  for(auto x = state.symbol_table.begin(); x != state.symbol_table.end(); x++) {
    HeapValue* v = x->second;
    copy(&v);
    state.symbol_table[x->first] = (Symbol*) v;
  }
}

inline Type Value::type() const {
  if(!immediatep()) {
    ARETE_ASSERT_LIVE(heap);
  }
  return type_unsafe();
}

inline Frame::Frame(State& state_, size_t size_, HeapValue*** ptrs): state(state_), size(size_), values(ptrs) {
  state.gc.frames.push_back(this);
}

inline Frame::Frame(State* state_, size_t size_, HeapValue*** ptrs): state(*state_), size(size_), values(ptrs) {
  state.gc.frames.push_back(this);
}

inline Frame::~Frame() {
  AR_ASSERT(state.gc.frames.back() == this);
  state.gc.frames.pop_back();
}

inline Handle::Handle(State& state_): state(state_), ref(C_FALSE) { initialize(); }
inline Handle::Handle(State& state_, Value ref_): state(state_), ref(ref_) { initialize(); }
inline Handle::Handle(const Handle& cpy): state(cpy.state), ref(cpy.ref) {
  it = state.gc.handles.insert(state.gc.handles.end(), this);
}

inline void Handle::initialize() {
  it = state.gc.handles.insert(state.gc.handles.end(), this);
}

inline Handle::~Handle() {
  state.gc.handles.erase(it);
}

///// CLI! 
// Command line interface

/**
 * Hand over execution to the Arete CLI under the given State
 * @returns an exit code 
 */
int enter_cli(State&, int, char*[]);

///// TEST! 
// Functionality related to builtin unit tests

} // namespace arete

#endif // ARETE_HPP
