// arete.hpp - Arete Scheme
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
#include <stdio.h>
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
# define ARETE_BENCH_GC 1
#endif

#ifndef ARETE_ASSERTION_LEVEL
# define ARETE_ASSERTION_LEVEL 0
#endif

#if ARETE_ASSERTION_LEVEL == 2
# define AR_ASSERT assert
# define AR_TYPE_ASSERT assert
#elif ARETE_ASSERTION_LEVEL == 1
# define AR_TYPE_ASSERT(x) if(!(x)) { std::cerr << "arete:assert: " << #x << " at " << __FILE__ << ':' << __LINE__ << " failed" << std::endl; }
# define AR_ASSERT(x) 
#else
# define AR_ASSERT(x)
# define AR_TYPE_ASSERT(x)
#endif

#ifndef AR_LINENOISE
# define AR_LINENOISE 1
#endif

#define AR_FRAME(state, ...)  \
  arete::FrameHack __arete_frame_ptrs[] = { __VA_ARGS__ };  \
  arete::Frame __arete_frame((state), sizeof(__arete_frame_ptrs) / sizeof(FrameHack), (HeapValue***) __arete_frame_ptrs); 

#ifndef ARETE_BLOCK_SIZE
# define ARETE_BLOCK_SIZE 4096
#endif

#ifndef ARETE_HEAP_SIZE 
# define ARETE_HEAP_SIZE (1024 * 1024)
#endif 

#ifndef ARETE_GC_LOAD_FACTOR
# define ARETE_GC_LOAD_FACTOR 80
#endif

#define ARETE_GC_SEMISPACE 0
#define ARETE_GC_INCREMENTAL 1

#ifndef ARETE_GC_STRATEGY
# define ARETE_GC_STRATEGY ARETE_GC_SEMISPACE
#endif 

#ifndef ARETE_GC_DEBUG
# define ARETE_GC_DEBUG 0
#endif

// TODO: It's possible, though expensive and more complex, to do this with the incremental
// collector. However, it shouldn't be necessary.
#if ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE && ARETE_GC_DEBUG == 1
# ifndef ARETE_ASSERT_LIVE
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
#define ARETE_LOG_TAG_VM (1 << 2)

#define ARETE_COLOR_BLUE "\033[1;34m"
#define ARETE_COLOR_YELLOW "\33[1;33m"
#define ARETE_COLOR_GREEN "\033[1;32m"
#define ARETE_COLOR_RED "\033[1;31m"
#define ARETE_COLOR_RESET "\033[0m"

#ifndef ARETE_LOG
# define ARETE_LOG(tag, prefix, msg) \
  if((ARETE_LOG_TAGS) & (tag)) { \
    std::cerr << ARETE_COLOR_RED << "arete:" << prefix << ": " << ARETE_COLOR_RESET << msg << std::endl; \
  }
#endif 

namespace arete {

// FWD! Forward declarations

struct State;
struct Block;
struct Value;
struct SourceLocation;
struct Pair;
struct VectorStorage;
struct VMFunction;

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

// TODO: Rearrange according to pointer count 

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
  BLOB = 7,
  // Have pointers
  SYMBOL = 8,
  VECTOR = 9,
  VECTOR_STORAGE = 10,
  PAIR = 11, 
  EXCEPTION = 12,
  FUNCTION = 13,
  CFUNCTION = 14,
  TABLE = 15,
  RENAME = 16,
  RECORD = 17,
  RECORD_TYPE = 18,
  VMFUNCTION = 19,
  CLOSURE = 20,
  UPVALUE = 21,
  FILE_PORT = 23,
};

std::ostream& operator<<(std::ostream& os, Type type);

// Constants:

enum {
  C_FALSE = 0,            // 0000 0000 #f
  C_TRUE = 2,             // 0000 0010 #t
  C_NIL = 10,             // 0000 1010 ()
  C_UNSPECIFIED = 14 ,    // 0000 1110 #<unspecified> 
  C_EOF = 18,             // 0001 0010 #<eof>
  C_SYNTAX = 34,          // 0100 0010 #<syntax>
  C_UNDEFINED = 66,       // 1000 0010 #<undefined>
};

/** A heap-allocated, garbage collected value. */
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
  void unset_header_bit(unsigned bit) {
    header -= bit;
  }

  unsigned get_shared_count() const {
    return header >> 32;
  }

  void set_shared_count(unsigned count) {
    // Extract header only
    header = header & ((size_t)((size_t)1 << 32) - 1);
    header += ((size_t)count << 32);
    // header = header & 255;
  }

  void flip_mark_bit() { header += get_mark_bit() ? -256 : 256; }
};

/**
 * A floating point number
 */
struct Flonum : HeapValue {
  double number;  
};

/**
 * A string
 */
struct String : HeapValue {
  size_t bytes;
  char data[1];

  static const Type CLASS_TYPE = STRING;
};

/**
 * A character.
 */
struct Char : HeapValue {
  char datum;
};

/**
 * A Blob is a heap-allocated value with no pointers,
 * intended to store binary data.
 */
struct Blob : HeapValue {
  size_t length;

  char data[1];

  template <class T> T blob_ref(size_t idx) const {
    return ((T*) data)[idx];
  }

  static const unsigned CLASS_TYPE = BLOB;
};

struct FilePort {
  FILE* handle;

  static const unsigned CLASS_TYPE = FILE_PORT;
};

/**
 * Value is the primary data structure used to represent, interact with and manipulate Scheme
 * values from C++ code. 
 * 
 * Internally, Scheme values are represented either as immediate values: constants and fixnums,
 * which end either in 10, 01, or are #f (which is NULL). Pointers to heap-allocated values always
 * end in 00.
 * 
 * The methods on the Value class generally make assertions about the type of an object before any
 * sort of potentially unsafe heap accesses are made.
 * 
 * Note that most mutation methods (e.g. set-car!) are part of the State object. This is because
 * the generational garbage collector needs to keep track of pointer stores.
 */
struct Value {
  union {
    HeapValue* heap;
    ptrdiff_t bits;
  };

  /** Default constructor: set Value to C_FALSE */
  Value(): bits(0) {}
  Value(HeapValue* heap_): heap(heap_) {}
  Value(ptrdiff_t bits_): bits(bits_) {}

  /** Returns true if this value is immediate */
  bool immediatep() const { return (bits & 3) != 0 || bits == 0; }

  /** Static immediate checker, used by garbage collector */
  static bool immediatep(Value v) { return (v.bits & 3) != 0 || v.bits == 0; }

  /** Returns true if the value contains no pointers. */
  bool atomic() const {
    switch(type()) {
      case FIXNUM: case FLONUM: case CONSTANT: case STRING: case BLOB: return true;
      default: return false;
    }
  }

  /** Returns true if object's visible representation may have shared structure */
  bool print_recursive() const {
    switch(type()) {
      case PAIR:
      case RECORD:
      case VECTOR:
        return true;
      default:
        return false;
    }
  }

  /** Returns true if the object is a procedure? in Scheme terms. */
  bool procedurep() const {
    switch(type()) {
      case FUNCTION: case CFUNCTION: case VMFUNCTION: case CLOSURE: return true;
      default: return false;
    }
  }
    
  /** Returns true if the object is applicable */
  bool applicable() const;

  /** Returns true if the object is an identifier (a rename or a symbol) */
  bool identifierp() const { return type() == RENAME || type() == SYMBOL; }

  /** Returns true if the object is a number */
  bool numeric() const {
    switch(type()) {
      case FLONUM: case FIXNUM: return true;
      default: return false;
    }
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
  Type type() const 
#ifdef __GNUC__
  __attribute__((always_inline))
#endif
  
  ;
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

  // VECTOR STORAGE
  Value* vector_storage_data() const;

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

  // BLOBS
  template <class T> void blob_set(size_t idx, const T val) {
    ((T*)(as<Blob>()->data))[idx] = val;
  }

  template <class T> T blob_ref(size_t idx) const {
    return ((T*) as<Blob>()->data)[idx];
  }

  size_t blob_length() const { return as<Blob>()->length; }

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

  bool symbol_gensym() const {
    AR_TYPE_ASSERT(type() == SYMBOL);
    return heap->get_header_bit(SYMBOL_GENSYM_BIT);
  }

  bool symbol_immutable() const {
    AR_TYPE_ASSERT(type() == SYMBOL);
    return heap->get_header_bit(SYMBOL_IMMUTABLE_BIT);
  }

  /** Quickly compare symbol to string */
  bool symbol_equals(const char* s) const {
    std::string cmp(s);
    return cmp.compare(symbol_name_data()) == 0;
  }

  static const unsigned SYMBOL_IMMUTABLE_BIT = 1 << 9;
  static const unsigned SYMBOL_GENSYM_BIT = 1 << 10;

  // Syntactic closures
  Value rename_expr() const;
  Value rename_env() const;
  Value rename_gensym() const;

  // PAIRS
  size_t list_length() const;
  bool listp() const;
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

  /* Check whether a pair has source code information attached to it */
  bool pair_has_source() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header_bit(PAIR_SOURCE_BIT);
  }

  bool pair_generated() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header_bit(PAIR_GENERATED_BIT);
  }

  /** Return a pair's source code information. This is an error if the pair does not have
   * source code information */
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

  // VM FUNCTIONS
  unsigned vm_function_constant_count() const;
  unsigned vm_function_min_arity() const;
  unsigned vm_function_max_arity() const;
  bool vm_function_variable_arity() const {
    return heap->get_header_bit(VMFUNCTION_VARIABLE_ARITY_BIT);
  }

  Value vm_function_name() const;
  size_t* vm_function_code() const;
  VectorStorage* vm_function_constants() const;

  static const unsigned VMFUNCTION_VARIABLE_ARITY_BIT = 1 << 9;
  static const unsigned VMFUNCTION_LOG_BIT = 1 << 10;
  static const unsigned VMFUNCTION_MACRO_BIT = 1 << 11;

  // UPVALUES
  
  /** Determine whether an upvalue has been closed-over or not */
  bool upvalue_closed() const;
  /** Dereference an upvalue */
  Value upvalue();

  /** Set an upvalue */
  void upvalue_set(const Value);

  void upvalue_close();

  // CLOSURES

  /** Retrieve a Closure's function */
  VMFunction* closure_function() const;

  /** If a type is a closure, return its function */
  Value closure_unbox() const;

  static const unsigned UPVALUE_CLOSED_BIT = 1 << 9;

  // RECORD
  Value record_type() const;
  Value record_ref(unsigned) const;
  void record_set(unsigned, Value);
  unsigned record_field_count() const;

  bool record_applicable() const;
  bool record_isa(Value) const;

  // RECORD TYPES

  Value record_type_name() const;
  Value record_type_apply() const;
  Value record_type_print() const;
  Value record_type_parent() const;
  unsigned record_type_field_count() const;
  Value record_type_field_names() const;

  // PORTS

  FILE* file_port_handle() const;

  static const unsigned FILE_PORT_INPUT_BIT = 1 << 9;
  static const unsigned FILE_PORT_OUTPUT_BIT = 1 << 10;

  // OPERATORS

  /** Identity comparison */
  inline bool operator==(const Value& other) const {
    return bits == other.bits;
  }

  inline bool operator!=(const Value& other) const { return bits != other.bits; }

  // CASTING
  template <class T> T* as() const {
    AR_TYPE_ASSERT(type() == T::CLASS_TYPE);
    return static_cast<T*>(heap);
  }

  template <class T> T* as_unsafe() const {
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

inline bool Value::listp() const {
  return bits == C_NIL || list_length() > 0;
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

/** An interpreted Scheme function */
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

/**
 * A pointer to a C++ function, callable from
 * Scheme. 
 */
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

struct VMFunction : HeapValue {
  Value name;
  VectorStorage* constants;
  Blob* free_variables;
  Blob* sources;

  unsigned constant_count, min_arity, max_arity, stack_max, local_count;
  size_t bytecode_size;

  static const unsigned CLASS_TYPE = VMFUNCTION;

  size_t* code_pointer() {
    return (size_t*)(((char*)((size_t) this)) + sizeof(VMFunction));
  }

};

inline unsigned Value::vm_function_constant_count() const {
  return as<VMFunction>()->constant_count;
}

inline unsigned Value::vm_function_min_arity() const {
  return as<VMFunction>()->min_arity;
}

inline unsigned Value::vm_function_max_arity() const {
  return as<VMFunction>()->max_arity;
}

inline Value Value::vm_function_name() const {
  return as<VMFunction>()->name;
}

inline size_t* Value::vm_function_code() const {
  // No type check because this will be invoked by the GC on a not-yet-live object.
  return (size_t*)(((char*) heap) + sizeof(VMFunction));
}

inline VectorStorage* Value::vm_function_constants() const {
  return as<VMFunction>()->constants;
}

struct Upvalue : HeapValue {
  union {
    Value* local;
    Value converted;
  };

  static const unsigned CLASS_TYPE = UPVALUE;
};

inline bool Value::upvalue_closed() const {
  AR_TYPE_ASSERT(type() == UPVALUE);
  return heap->get_header_bit(UPVALUE_CLOSED_BIT);
}

inline void Value::upvalue_set(const Value rhs) {
  AR_TYPE_ASSERT(type() == UPVALUE);
  if(heap->get_header_bit(UPVALUE_CLOSED_BIT)) {
    static_cast<Upvalue*>(heap)->converted = rhs;
  } else {
    (*static_cast<Upvalue*>(heap)->local) = rhs;
    AR_ASSERT(rhs.type() != UPVALUE);
  }
}

inline Value Value::upvalue() {
  AR_TYPE_ASSERT(type() == UPVALUE);
  if(heap->get_header_bit(UPVALUE_CLOSED_BIT)) {
    //return static_cast<Upvalue*>(heap)->converted;
    return as<Upvalue>()->converted;
  } else {
    // return *(static_cast<Upvalue*>(heap)->local);
    return *(as<Upvalue>()->local);
  }
}

inline void Value::upvalue_close() {
  AR_TYPE_ASSERT(type() == UPVALUE);
  AR_TYPE_ASSERT(!upvalue_closed());
  heap->set_header_bit(UPVALUE_CLOSED_BIT);
  as<Upvalue>()->converted = (*as<Upvalue>()->local);
  AR_ASSERT(upvalue_closed());
}

struct Closure : HeapValue {
  Value function;
  VectorStorage* upvalues;

  static const unsigned CLASS_TYPE = CLOSURE;
};

inline Value Value::closure_unbox() const {
  if(type() == CLOSURE) {
    as<Closure>()->function;
  }
  return heap;
}

inline VMFunction* Value::closure_function() const {
  return as<Closure>()->function.as<VMFunction>();
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

inline Value* Value::vector_storage_data() const {
  return (Value*)(&(as<VectorStorage>()->data[0].heap));
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

struct RecordType : HeapValue {
  /** Allow record to handle application */
  Value apply;
  
  /** Allow record to custom print */
  Value print;

  /** String name describing the record-type */
  Value name;

  /** Optional: record type to inherit from */
  Value parent;

  /** Field names */
  Value field_names;

  /** Count of garbage-collected data stored in the record */
  unsigned field_count;
  /** Size of uncollected data at the end of the record */
  unsigned data_size;

  static const unsigned CLASS_TYPE = RECORD_TYPE;
};

inline Value Value::record_type_name() const {
  return as<RecordType>()->name;
}

inline Value Value::record_type_print() const {
  return as<RecordType>()->print;
}

inline Value Value::record_type_parent() const {
  return as<RecordType>()->parent;
}

inline unsigned Value::record_type_field_count() const {
  return as<RecordType>()->field_count;
}

inline Value Value::record_type_field_names() const {
  return as<RecordType>()->field_names;

}

inline Value Value::record_type_apply() const {
  return as<RecordType>()->apply;
}

/** 
 * A record is a fixed size array with some type information included; it is used internally 
 * to implement records, multiple return values, and user-extended data types. The underlying
 */
struct Record : HeapValue {
  RecordType* type;
  Value fields[1];

  static const unsigned CLASS_TYPE = RECORD;
};

inline Value Value::record_type() const {
  return as<Record>()->type;
}

inline Value Value::record_ref(unsigned i) const {
  AR_ASSERT(record_type().as<RecordType>()->field_count > i && "record out of bounds error");
  return as<Record>()->fields[i];
}

inline unsigned Value::record_field_count() const {
  return record_type().as<RecordType>()->field_count;
}

inline void Value::record_set(unsigned i, Value v) {
  AR_ASSERT(record_type().as<RecordType>()->field_count > i && "record out of bounds error");
  as<Record>()->fields[i] = v;
}

inline bool Value::record_applicable() const {
  return record_type().record_type_apply().applicable();
}

inline bool Value::record_isa(Value rhs) const {
  if(rhs.type() != RECORD_TYPE) return false;

  Value lhs = record_type();


  while(lhs != C_FALSE) {
    if(lhs == rhs) {
      return true;
    }
    lhs = lhs.record_type_parent();
  }

  return false;
}

inline bool Value::applicable() const {
  if(type() == FUNCTION || type() == CFUNCTION || type() == VMFUNCTION || type() == CLOSURE) {
    return true;
  }
  return false;
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
 * collector to move objects and update pointers to them if necessary.
 */
struct Frame {
  State& state;
  size_t size;
  HeapValue*** values;

  Frame(State& state, size_t size, HeapValue*** values);
  Frame(State* state, size_t size, HeapValue*** values);
  ~Frame();
};

/**
 * Like frames, but specifically for the virtual machine which does complex stack allocation
 */
struct VMFrame {
  State& state;

  VMFrame* previous;
  VMFunction* fn;
  Closure* closure;
  Value* stack;
  Value* locals;
  Value* upvalues;
  /** Pointer to wordcode, updated by the GC */
  size_t *code;
  Value exception;
  size_t stack_i;
  size_t depth;

  VMFrame(State& state);
  ~VMFrame();

  void destroy();
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
  VMFrame* vm_frames;
  // Number of total allocations
  bool collect_before_every_allocation;
  size_t allocations;
  size_t collections, live_objects_after_collection, live_memory_after_collection, heap_size;
  size_t block_size;

  std::vector<std::string> load_paths;

  GCCommon(State& state_, size_t heap_size_ = ARETE_HEAP_SIZE): state(state_), 
    vm_frames(0),
    collect_before_every_allocation(false),
    allocations(0),
    collections(0), live_objects_after_collection(0), live_memory_after_collection(0),
    heap_size(heap_size_),
    block_size(heap_size_) {

  }

  ~GCCommon() {}

  // Align a value along a boundary e.g. align(8, 7) == 8, align(8, 16) == 16,
  // and align(8, 247) == 248 
  static size_t align(size_t boundary, size_t value) {
    return (value + (boundary - 1)) & -boundary;
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

  void copy(HeapValue** ref);

  void copy_roots();
  void collect(size_t request = 0, bool force = false);

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

    // TODO: The collect_before_every_allocation stuff should be disabled in production builds;
    // care should be taken to make sure this function is very small and inlinable

    // Bump allocation possible
    if(!has_room(size) || collect_before_every_allocation) {
      collect(size);
      if(!has_room(size)) {
        collect(size, true);
        if(!has_room(size)) {
          std::cerr << "arete:gc: semispace allocation of size " << size << " failed with heap of size " << heap_size << std::endl;
          AR_ASSERT(!"arete:gc: semispace allocation failed");
        }
      }
    }

    allocations++;
    HeapValue* v = (HeapValue*) (active->data + block_cursor);
    v->initialize(type, 0, size);
    block_cursor += size;
    // Assert that pointer is aligned properly.
    AR_ASSERT(!Value(v).immediatep());
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

  /** This doesn't do anything, but is here so GCSemispace::live calls can be used in normal
   * source code */
  bool live(const Value v) { return true; }

  bool marked(HeapValue* v) const {
    return v->get_mark_bit() == mark_bit;
  }

  void mark(HeapValue* v);
  void mark_symbol_table();

  void collect();

  HeapValue* allocate(Type type, size_t size);
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
  typedef std::pair<unsigned, bool> print_info_t;
  typedef std::unordered_map<unsigned, print_info_t> print_table_t;

  /** Counts how many times gensym has been called; appended to the end of gensyms to ensure
   * their uniqueness */ 
  size_t gensym_counter;

  /** The symbol table */
  symbol_table_t* symbol_table;

  std::vector<RecordType> what;

  /** A list of the names of various sources; mostly filenames but these can also be descriptors
   * of C++ strings and REPL lines */
  std::vector<std::string> source_names;

  /** Contents of all sources, for verbose error messages. */
  std::vector<std::string> source_contents;

  /** An array of permanently live values */
  std::vector<Value> globals;

  /** A GC-tracked array of temporary values. May be cleared by function calls. */
  std::vector<Value> temps;

  /** Used to mark objects when printing shared structure */
  unsigned shared_objects_begin;
  unsigned shared_objects_i;

  /** A stack trace. */
  std::vector<std::string> stack_trace;

  State():
      gc(*this),
      gensym_counter(0),
      symbol_table(),
      source_names(),
      source_contents(),
      globals(),
      temps(),

      shared_objects_begin(0),
      shared_objects_i(0) {
    symbol_table = new symbol_table_t();
    current_state = this;
  }

  ~State() {
    delete symbol_table;
    current_state = 0;
  }
  
  // Note that order is important here. Everything from S_QUOTE to S_LETREC_SYNTAX
  // will be set to C_SYNTAX meaning that its values can't be handled directly
  // by Scheme code. 

  /** Builtin global variables */
  enum Global {
    // C_SYNTAX values
    S_QUOTE,
    S_BEGIN,
    S_DEFINE,
    S_LAMBDA,
    S_IF,
    S_COND,
    S_COND_EXPAND,
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
    * this is separate so the uninitialized State can be unit tested in various ways */
  void boot();

  // Value creation; all of these will cause allocations

  /** Create a Flonum */
  Value make_flonum(double number);

    /** Get the value of a Global symbol */
  Value get_global_value(Global sym);
  
  /** Set the value of a Global symbol */
  void set_global_value(Global sym, Value v);


  /** Get a Global symbol */
  Value get_symbol(Global sym);

  /**
   * Return a symbol for a given String
   */
  Value get_symbol(Value name);

  /** 
   * Given a C++ string, retrieve a symbol. May cause allocation if the symbol has not already been
   * interned
   */
  Value get_symbol(const std::string& name);

  /** Generate a unique symbol. */
  Value gensym(Value sym);

  Value make_rename(Value expr, Value env);

  Value make_pair(Value car = C_FALSE, Value cdr = C_FALSE,
    size_t size = sizeof(Pair) - sizeof(SourceLocation));
  Value make_src_pair(Value car, Value cdr, SourceLocation& loc);
  Value make_char(char c);

  /** Create vector backing storage */
  Value make_vector_storage(size_t capacity);

  /** Create a vector
   * @param capacity The initial capacity of the vector. Can save reallocs if this is known
   * in advance
   */
  Value make_vector(size_t capacity = 2);
  /**
   * In-place vector append.
   */ 
  void vector_append(Value vector, Value value);

  /**
   * In-place vector storage append. Error if it goes over capacity!
   */
  void vector_storage_append(Value sstore, Value value);

  ///// TABLES

  void table_setup(Value table, size_t size_log2);
  ptrdiff_t hash_index(Value table, Value key, bool& unhashable);
  void table_grow(Value table);
  Value unhashable_error(Value irritant);

  /** Set a hash table value */
  Value table_set(Value table, Value key, Value value);
  
  /** Get a hash table value
   * @param found false if table_get failed
   * @return The value, C_FALSE, or an exception if value was unhashable
   */
  Value table_get(Value table, Value key, bool& found);

  /** Get the storage cell of a hash table key
   * @return C_FALSE or a (key . value) pair
   */
  Value table_get_cell(Value table, Value key);

  /**
   * Insert a new value into a hash table
   * @param key A key
   * @returns An exception if the value was unhashable, or unspecified
   */
  Value table_insert(Value table, Value key, Value value);

  /**
   * Allocate a new hash table
   * @param size_log2 log2(size_log2) = memory usage of the hash table
   */
  Value make_table(size_t size_log2 = 4);

  /**
   * Deep equality comparison. Will not terminate on shared structure.
   */
  bool equals(Value a, Value b);

  // Strings
  
  /**
   * Create a copy of a string
   * @param x A STRING Value
   */
  Value string_copy(Value x);

  // Records

  /**
   * Register a value as a permanent global
   * @return Its index in the State::globals array
   */
  size_t register_global(Value glob) {
    globals.push_back(glob);
    return globals.size() - 1;
  }

  /** Register a new type of record */
  size_t register_record_type(const std::string& cname, unsigned field_count, unsigned data_size,
      Value field_names = C_FALSE, Value parent = C_FALSE);

  void record_set(Value rec_, unsigned field, Value value);

  Value make_string(const std::string& body);

  template <class T>
  Value make_blob(size_t count) {
    Blob* heap = static_cast<Blob*>(gc.allocate(BLOB, sizeof(Blob) + (count * sizeof(T))));
    heap->length = count;
    return heap;
  }

  /** Make an exception from Scheme values */
  Value make_exception(Value tag, Value message, Value irritants = C_UNSPECIFIED);
  /** Make an exception with a C++ std::string message */
  Value make_exception(Value tag, const std::string& cmessage, Value irritants = C_UNSPECIFIED);
  /** Make an exception with a C++ std::string message and tag */
  Value make_exception(const std::string& ctag, const std::string& cmessage,
    Value irritants = C_UNSPECIFIED);
  /** Make an exception with a builtin tag */
  Value make_exception(Global s, const std::string& cmessage, Value irritants = C_UNSPECIFIED);
  Value make_record(Value tipe);

  Value make_record(size_t tag);

  Value make_c_function(Value name, c_function_t addr, size_t min_arity, size_t max_arity,
    bool variable_arity);

  Value make_file_port(const std::string& path);

  // WRITE! Output

  // Print out a table's internal structure for debugging purposes
  void print_table_verbose(Value tbl);

  Value pretty_print(std::ostream& os, Value v);
  bool pretty_print_shared_obj(std::ostream& os, Value v, print_table_t* printed);
  Value pretty_print_mark(Value v, unsigned&, print_table_t* printed);
  Value pretty_print_sub(std::ostream& os, Value v, print_table_t*);

  // Source code location tracking
  unsigned register_source(const std::string& path, std::istream& is);

  /**
   * Print information about an erroneous pair
   * @return true if argument's source was successfully printed
   */
  bool print_src_pair(std::ostream& os, Value pair);

   /**
   * Print an erroneous line of source code with offending information highlighted
   */
  void print_src_line(std::ostream& os, const SourceLocation& src);
  
  /**
   * Special-cased exception printing: this pretty-prints an exception, but also handles 
   * printing erroneous source code for certain builtin exceptions which can communicate this
   * information (eg errors generated by the macro expander)
   */
  void print_exception(std::ostream& os, Value exc);

  /** Print GC stats, including time spent if compile flag is enabled */
  void print_gc_stats(std::ostream& os);

  /** Print a stack trace. */
  void print_stack_trace(std::ostream& os = std::cerr, bool clear = true);

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

  /** Return the name of a source */
  std::string source_name(unsigned source) {
    return source_names.at(source);
  }

  ///// EVAL! Interpreter

  void load_builtin_functions();
  void load_numeric_functions();

  /** Defines a built-in function */
  void defun_core(const std::string& cname, c_function_t addr, size_t min_arity, size_t max_arity = 0, bool variable_arity = false);
 
  std::ostream& warn() { return std::cerr << "arete: Warning: " ; }

  // Environments are just vectors containing the parent environment in the first position
  // and variable names/values in the even/odd positions afterwards e.g.

  // #(#f hello 12345)
  // for a environment one level below toplevel after a (define hello 12345)

  static const size_t VECTOR_ENV_FIELDS = 2;

  Value make_env(Value parent = C_FALSE);

  void env_set(Value env, Value name, Value val);

  bool identifier_equal(Value id1, Value id2) {
    if(id1 == id2) return true;

    if(id1.type() == RENAME && id2.type() == RENAME) {
      return (id1.rename_env() == id1.rename_env()) && (id1.rename_expr() == id2.rename_expr());
    }

    return false;
  }

  bool env_defined(Value env, Value name);

  /**
   * This is the env_lookup backend. It is necessarily complex because it handles lookups for the
   * interpreter and for the hygienic macro expander. See fn_env_compare and fn_env_resolve in
   * arete.cpp.
   */
  Value env_lookup_impl(Value& env, Value name, Value& rename_key, bool& reached_toplevel);

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

  // The internal application machinery is unfortunately somewhat complex due to the need for
  // interpreted, c++ and virtual machine functions to all be able to call eachother.

  // Moreover, C++ and virtual machine functions use a different calling convention: argc/argv
  // style
  // whereas the interpreter takes a list and converts it into an environment for evaluation.

  // The functions below should only be called by the interpreter and can optionally evaluate
  // their arguments 

  /** Apply a scheme function */
  Value eval_apply_scheme(Value env,  Value fn, Value args, Value src_exp, Value calling_fn_name, bool eval_args = true);
  Value eval_apply_vm(Value env,  Value fn, Value args, Value src_exp, Value calling_fn_name, bool eval_args = true);
  Value eval_apply_c(Value env, Value fn, Value args, Value src_exp, Value fn_name, bool eval_args = true);
  Value apply_record(Value env, Value fn, Value args, Value src_exp, Value fn_name);
  
  /** Apply a Scheme function against a list of already-evaluated arguments */
  Value eval_apply_function(Value fn, Value args);

  Value expand_expr(Value exp);
  Value eval_toplevel(Value exp);

  /**
   * This is the primary application function, and the only one that should be called by C++
   * functions. Argv can be evaluated on the stack or a pointer to the temps vector.
   *
   * NOTE: This should not be called against State::temps, as it is used to construct a list when
   * interpreter functions are called.
   */
  Value apply(Value fn, size_t argc, Value* argv);

  /**
   * Shorthand for calling apply against a vector. Necessary when building up garbage-collected
   * lists of arguments.
   */
  Value apply_vector_storage(Value fn, Value vec);


  ///// MODULES

  Value load_stream(std::istream&, size_t source = 0);
  Value load_file(const std::string&);
  Value load_module(const std::string&);

  ///// VIRUTAL MACHINE

  Value apply_vm(Value fn, size_t argc, Value* argv);

  /** Shortcut: apply a VM function to something in the temps array */
  Value apply_vm_temps(Value fn) {
    return apply_vm(fn, temps.size(), &temps[0]);
  }

  /** Print a readable version of a function's bytecode. */
  void disassemble(std::ostream&, Value);

  // Command line interface

  /**
   * Allow Arete's command line interface to run.
   * @return A normal exit code
   */
  int enter_cli(int argc, char* argv[]);
};

///// READ! S-Expression reader

/** 
 * S-expression reader.
 */
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

  XReader(State& state_, std::istream& is_, const std::string& desc = "anonymous"):
      state(state_), is(is_), source(0), position(0),
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

  /** Reads a number into the buffer */
  TokenType tokenize_number(bool negative = false);

  /** Reads a symbol into the buffer */
  void tokenize_symbol();

  /** Reads a string into the buffer */
  void tokenize_string();

  /** Look at the next token */
  TokenType next_token();

  /** Read auxiliary syntax (e.g. quote, quasiquote, etc) */
  Value read_aux(const std::string&, unsigned, Value);

  /** Read rename auxiliary syntax */
  Value read_aux2(const std::string&, unsigned, Value, Value);

  /** Read the next expression */
  Value read_expr(TokenType);

  /** The entry point for reading an expression */
  Value read();
};

std::ostream& operator<<(std::ostream& os, Value v);

// MISC! Various inline functions 

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

} // namespace arete

// Handy macros for writing CFunctions

#define AR_FN_STACK_TRACE(state) \
  std::ostringstream __stackinfo; \
  __stackinfo << __FILE__ << ":" << __LINE__; \
  (state).stack_trace.push_back(__stackinfo.str());

#define AR_FN_EXPECT_POSITIVE(state, argv, i) \
  if((argv[(i)]).fixnum_value() < 0) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected argument " << (i) << " to be a positive fixnum but got " << (argv)[(i)];  \
    AR_FN_STACK_TRACE(state); \
    return (state).eval_error(__os.str()); \
  }

#define AR_FN_EXPECT_TYPE(state, argv, i, expect) \
  if((argv)[(i)].type() != (expect)) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected argument " << (i) << " to be of type " << (Type)(expect) << " but got " << (Type)(argv[i].type()); \
    AR_FN_STACK_TRACE(state); \
    return (state).type_error(__os.str()); \
  } 

#define AR_FN_ASSERT_ARG(state, i, msg, expr) \
  if(!(expr)) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected argument " << (i) << ' ' << msg ; \
    AR_FN_STACK_TRACE(state); \
    return (state).type_error(__os.str()); \
  }

#define AR_FN_EXPECT_APPLICABLE(state, argv, arg) \
  if(!((argv)[(arg)].applicable())) { \
    std::ostringstream os; \
    os << fn_name << " expected argument " << (arg) << " to be applicable but got a non-applicable " << (argv)[(arg)].type(); \
    AR_FN_STACK_TRACE(state); \
    return state.type_error(os.str()); \
  }

#endif // ARETE_HPP
