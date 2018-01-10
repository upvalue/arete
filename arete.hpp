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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <list>
#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_map>

///// PRE! Preprocessor macros and compile-time configuration macros

#ifdef ARETE_DEV
# define ARETE_GC_DEBUG 1
# ifndef ARETE_ASSERTION_LEVEL
#  define ARETE_ASSERTION_LEVEL 2
# endif
# define ARETE_BENCH_GC 1
#endif

#if UINTPTR_MAX == 0xffffffff
# define ARETE_64_BIT 0
# define AR_ALIGN // __attribute__((aligned(8)))
#elif UINTPTR_MAX == 0xffffffffffffffff
# define ARETE_64_BIT 1
# define AR_ALIGN 
#endif

// 0 = Do not evaluate assertions, internal assertions
// 1 = Print warnings when assertions fail, disable some internal assertions
// 2 = Exit program when assertions fail, enable all assertions
#ifndef ARETE_ASSERTION_LEVEL
# define ARETE_ASSERTION_LEVEL 0
#endif

#if ARETE_ASSERTION_LEVEL == 2
# define AR_ASSERT assert
# define AR_TYPE_ASSERT assert
#elif ARETE_ASSERTION_LEVEL == 1
# define AR_TYPE_ASSERT(x) if(!(x)) { std::cerr << "arete:assert: " << #x << " at " << __FILE__ << ':' << __LINE__ << " failed" << std::endl; stri}
# define AR_ASSERT(x) ((void) 0)
#else
# define AR_ASSERT(x) ((void) 0)
# define AR_TYPE_ASSERT(x) ((void) 0)
#endif

#ifndef AR_LINENOISE
# ifdef __EMSCRIPTEN__
#  define AR_LINENOISE 0
# else
#  define AR_LINENOISE 1
# endif
#endif

#ifdef __GNUC__
# define AR_FORCE_INLINE __attribute__((always_inline))
# define AR_LIKELY(x) (__builtin_expect((x), 1))
# define AR_UNLIKELY(x) (__builtin_expect((x), 0))
#else
# define AR_LIKELY(x) (x)
# define AR_UNLIKELY(x) (x)
# define AR_FORCE_INLINE
#endif

#define _AR_FRAME2_(state, counter, ...)  \
  arete::FrameHack __arete_frame_ptrs##counter[] = { __VA_ARGS__ };  \
  arete::Frame __arete_frame##counter((state), sizeof(__arete_frame_ptrs##counter) / sizeof(FrameHack), (HeapValue***) __arete_frame_ptrs##counter); 
#define _AR_FRAME(state, counter, ...) _AR_FRAME2_(state, counter, __VA_ARGS__)

#define AR_FRAME(state, ...) _AR_FRAME(state, __COUNTER__, __VA_ARGS__)

#ifndef ARETE_BLOCK_SIZE
# define ARETE_BLOCK_SIZE 4096
#endif

#ifndef ARETE_HEAP_SIZE 
# define ARETE_HEAP_SIZE (1024 * 1024 * 8)
#endif 

#ifndef ARETE_GC_LOAD_FACTOR
# define ARETE_GC_LOAD_FACTOR 80
#endif

#define ARETE_GC_SEMISPACE 0
#define ARETE_GC_INCREMENTAL 1

#ifndef ARETE_GC_DEBUG
# define ARETE_GC_DEBUG 0
#endif

#if ARETE_GC_DEBUG == 1
# define ARETE_ASSERT_LIVE(obj) \
   AR_ASSERT("attempt to invoke method on non-live object" && (arete::current_state->gc.live((obj)) == true));
#endif

#ifndef ARETE_ASSERT_LIVE
# define ARETE_ASSERT_LIVE(obj) 
#endif

#ifndef ARETE_LOG_TAGS
# define ARETE_LOG_TAGS 0
#endif 

// Various internal log tags for debugging

#define ARETE_LOG_TAG_GC (1 << 0)
#define ARETE_LOG_TAG_READER (1 << 1)
#define ARETE_LOG_TAG_VM (1 << 2)
#define ARETE_LOG_TAG_IMAGE (1 << 3)
#define ARETE_LOG_TAG_DEFUN (1 << 4)
#define ARETE_LOG_TAG_JIT (1 << 5)

#define AR_POSIX 0
#define AR_WINDOWS 1

#ifdef _MSC_VER
# define AR_OS AR_WINDOWS
# define ARETE_COLOR 0
#else
# define AR_OS AR_POSIX
#endif

#ifdef __EMSCRIPTEN__
# define ARETE_COLOR 0
#endif

#if ARETE_COLOR
# define ARETE_COLOR_BLUE "\033[1;34m"
# define ARETE_COLOR_YELLOW "\33[1;33m"
# define ARETE_COLOR_GREEN "\033[1;32m"
# define ARETE_COLOR_RED "\033[1;31m"
# define ARETE_COLOR_RESET "\033[0m"
#else
# define ARETE_COLOR_BLUE ""
# define ARETE_COLOR_YELLOW ""
# define ARETE_COLOR_GREEN ""
# define ARETE_COLOR_RED ""
# define ARETE_COLOR_RESET ""
#endif

#ifndef ARETE_LOG
# define ARETE_LOG(tag, prefix, msg) \
  if((ARETE_LOG_TAGS) & (tag)) { \
    std::cerr << ARETE_COLOR_RED << "arete:" << prefix << ": " << ARETE_COLOR_RESET << msg << std::endl; \
  }
#endif 

// Included libraries
#ifndef AR_LIB_SDL
# define AR_LIB_SDL 1
#endif

// Macros for generating unique names for variables
#define _AR_UNIQUE2_(name, count) name##count
#define _AR_UNIQUE2(name, count) _AR_UNIQUE2_(name, count)
#define _AR_UNIQUE(name) _AR_UNIQUE2(name, __COUNTER__)

namespace arete {

// FWD! Forward declarations

struct State;
struct Block;
struct Value;
struct SourceLocation;
struct Pair;
struct VectorStorage;
struct VMFunction;
struct XReader;

extern size_t gc_collect_timer;

// For debugging purposes only: a global instance of the current state
extern State* current_state;

// We have to use (void*) here to make Emscripten happy.
typedef Value (*c_closure_t)(State&, size_t, Value*, void*);

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
  BYTEVECTOR = 7,
  // Have pointers
  SYMBOL = 8,
  VECTOR = 9,
  VECTOR_STORAGE = 10,
  PAIR = 11, 
  EXCEPTION = 12,
  FUNCTION = 13,
  CFUNCTION = 14,
  NATIVE_FUNCTION = 22,
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
  C_REST_OBJECT = 50,     // 0011 0010 #!rest
  C_UNDEFINED = 66,       // 1000 0010 #<undefined>
  C_OPTIONAL_OBJECT = 82, // 0101 0010 #!optional
  C_KEYS_OBJECT = 86,     // 0101 0110 #!keys
  C_KEY_OBJECT = 114,     // 0111 0010 #!key
};

/** A heap-allocated, garbage collected value. */
struct HeapValue {
  // Heap value headers are formatted like this:

  // .... .... .... ....
  // ffff ffpm tttt tttt
  // . = an integer (32-bit on 64-bit systems, 16-bit on 32-bit systems) used for
  //     writing objects with shared references. could also be used for caching various computations.
  // f = object-specific flags
  // p = 1 if this is a struct derived from Procedure and thus has a valid function address attached
  // to it
  // m = mark bit for incremental GC
  // t = type
  size_t header;

  /** Size of the object. In the moving collector, this field is also used to store a pointer to
   * copied objects */
  size_t size;

  void initialize(unsigned type, unsigned mark_bit, size_t size_) {
    header = (type) + (mark_bit << 8);
    size = size_;
  }

  unsigned get_header() const { return (unsigned) header; }
  unsigned get_type() const { return header & 255; }
  unsigned char get_mark_bit() const { return (header >> 8) & 1; }
  unsigned get_header_bit(unsigned bit) const { return (header & bit) == bit; }
  void set_header_bit(unsigned bit) {
    AR_ASSERT(!get_header_bit(bit));
    header += bit;
  }

  void unset_header_bit(unsigned bit) {
    header -= bit;
  }

  unsigned get_header_int() const {
    return header >> HEADER_INT_SHIFT;
  }

  void set_header_int(unsigned count) {
    // Extract header only
    header = header & ((size_t)((size_t)1 << HEADER_INT_SHIFT) - 1);
    header += ((size_t)count << HEADER_INT_SHIFT);
  }

  void flip_mark_bit() { header += get_mark_bit() ? -256 : 256; }
#if ARETE_64_BIT
  static const size_t HEADER_INT_SHIFT = 32;
#else
  static const size_t HEADER_INT_SHIFT = 16;
#endif

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
 * A bytevector is a value for storing binary data.
 */
struct Bytevector : HeapValue {
  size_t length;

  char data[1];

  template <class T> T bv_ref(size_t idx) const {
    return ((T*) data)[idx];
  }

  static const unsigned CLASS_TYPE = BYTEVECTOR;
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
  static const unsigned VALUE_PROCEDURE_BIT = 1 << 9;

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
      case FIXNUM: case FLONUM: case CONSTANT: case STRING: case BYTEVECTOR: return true;
      default: return false;
    }
  }

  /** Returns true if object's visible representation may have shared structure */
  bool print_recursive() const {
    switch(type()) {
      case PAIR: case RECORD: case VECTOR: case TABLE:
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

  void procedure_install(c_closure_t addr);
    
  /** Returns true if the object is applicable */
  bool applicable() const;
  /** Check if a procedure's arity is exactly equal to argc */
  bool procedure_arity_equals(size_t argc) const;

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
    Type lhs_type = type(), rhs_type = rhs.type();
    if(lhs_type != rhs_type) return false;
    switch(lhs_type) {
      case FLONUM: return flonum_value() == rhs.flonum_value();
      case CHARACTER: return character() == rhs.character();
      default: return (*this) == rhs;
    }
  }

  bool hashable() const {
    Type tipe = type();
    return tipe == STRING || tipe == SYMBOL || tipe == FIXNUM || tipe == CONSTANT;
  }

  /** Safely retrieve the type of an object */
  Type type() const AR_FORCE_INLINE;
  Type type_unsafe() const {
    if((bits & 3) == 0) {
      if(bits == 0) return CONSTANT;
      return (Type)heap->get_type();
    } else {
      return (bits & 1) ? FIXNUM : CONSTANT;
    }
    // So this is a pretty interesting bit of optimization... the above code
    // shaves a solid 10ms off bootstrap time compared to the code below.
    /*
    if(bits & 1) return FIXNUM;
    else if(bits & 2 || bits == 0) return CONSTANT;    
    else return (Type) heap->get_type();
    */
  }

  std::string type_desc() const;

  Type heap_type() const {
    return (((bits & 3) == 0) && (bits != 0)) ? (Type)heap->get_type() : FIXNUM;
  }

  bool heap_type_equals(Type tipe) const {
    return (((bits & 3) == 0) && (bits != 0) && heap->get_type() == tipe);
  }


  // FIXNUMS

  /** Quick fixnum check. */
  bool fixnump() const {
    return bits & 1;
  }

  ptrdiff_t fixnum_value() const {
    AR_TYPE_ASSERT(fixnump());
    
    return bits >> 1;
  }

  /** Returns 0 if value is not a fixnum, value if it is. */
  ptrdiff_t fixnum_value_or_zero() const {
    return fixnump() ? fixnum_value() : 0;
  }

  /** Create a fixnum */
  static Value make_fixnum(ptrdiff_t fixnum) {
    return Value(((fixnum << 1) + 1));
  }

  // CONSTANTS
  unsigned constant_value() const {
    AR_ASSERT(type() == CONSTANT);
    return (unsigned) bits;
  }

  static Value make_boolean(ptrdiff_t cmp) {
    return cmp == 0 ? C_FALSE : C_TRUE;
  }

  bool boolean_value() const {
    return bits != C_FALSE;
  }

  // FLONUMS
  double flonum_value() const {
    AR_TYPE_ASSERT(type() == FLONUM);
    return static_cast<Flonum*>(heap)->number;
  }

  double inexact_number() const {
    if(fixnump()) return (double) fixnum_value();
    else return flonum_value();
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

  char* string_data_mod() const {
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

  // BYTEVECTORS
  template <class T> void bv_set(size_t idx, const T val) {
    ((T*)(as<Bytevector>()->data))[idx] = val;
  }

  template <class T> T bv_ref(size_t idx) const {
    return ((T*) as<Bytevector>()->data)[idx];
  }

  template <class T = unsigned char> T* bv_data() const {
    return (T*)as<Bytevector>()->data;
  }

  size_t bv_length() const { return as<Bytevector>()->length; }

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

  /**
   * @return true if a symbol has been seen by the reader
   */
  bool symbol_was_read() const {
    AR_TYPE_ASSERT(type() == SYMBOL);
    return heap->get_header_bit(SYMBOL_READ_BIT);
  }

  /** Quickly compare symbol to string */
  bool symbol_equals(const char* s) const {
    std::string cmp(s);
    return cmp.compare(symbol_name_data()) == 0;
  }

  bool symbol_qualified() const {
    AR_TYPE_ASSERT(type() == SYMBOL);
    return heap->get_header_bit(SYMBOL_QUALIFIED_BIT);
  }

  bool symbol_keyword() const {
    AR_TYPE_ASSERT(heap_type_equals(SYMBOL));
    return heap->get_header_bit(SYMBOL_KEYWORD_BIT);
  }

  static const unsigned SYMBOL_GENSYM_BIT = 1 << 11;
  static const unsigned SYMBOL_READ_BIT = 1 << 12;
  static const unsigned SYMBOL_QUALIFIED_BIT = 1 << 13;
  // True if symbol is a GC root 
  static const unsigned SYMBOL_ROOT_BIT = 1 << 14;
  // True if symbol is a keyword, e.g. keyword: 
  static const unsigned SYMBOL_KEYWORD_BIT = 1 << 15;

  // RENAMES

  Value rename_expr() const;
  Value rename_env() const;
  Value rename_gensym() const;

  // TABLES
  size_t table_entries() const;

  // PAIRS
  size_t list_length();
  bool listp();
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
  static const unsigned PAIR_SOURCE_BIT = 1 << 10; 
  // True if a pair is immutable
  static const unsigned PAIR_IMMUTABLE_BIT = 1 << 11;

  /* Check whether a pair has source code information attached to it */
  bool pair_has_source() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header_bit(PAIR_SOURCE_BIT);
  }

  bool pair_immutable() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header_bit(PAIR_IMMUTABLE_BIT);
  }

  /** Return a pair's source code information. This is an error if the pair does not have
   * source code information */
  SourceLocation pair_src() const;
  void set_pair_src(const SourceLocation&);

  // EXCEPTION
  static const unsigned EXCEPTION_ACTIVE_BIT = 1 << 10;
  // If not set, no stack trace will be generated. Used to implement 
  // one-shot continuations
  static const unsigned EXCEPTION_TRACE_BIT = 1 << 11;

  void exception_deactivate();
  void exception_activate();
  bool is_active_exception() const;
  bool exception_trace() const;
  Value exception_tag() const;
  Value exception_message() const;
  Value exception_irritants() const;
  
  // FUNCTIONS
  static const unsigned FUNCTION_MACRO_BIT = 1 << 10;
  static const unsigned FUNCTION_IDENTIFIER_MACRO_BIT = 1 << 11;

  Value function_name() const;
  Value function_arguments() const;
  Value function_parent_env() const;
  Value function_rest_arguments() const;
  Value function_body() const;
  bool function_is_macro() const;

  // C FUNCTIONS
  static const unsigned CFUNCTION_VARIABLE_ARITY_BIT = 1 << 10;
  static const unsigned CFUNCTION_CLOSURE_BIT = 1 << 11;

  bool c_function_is_closure() const { 
    AR_TYPE_ASSERT(heap_type_equals(CFUNCTION));
    return heap->get_header_bit(CFUNCTION_CLOSURE_BIT);
  }

  c_closure_t c_function_closure_addr() const;
  Value c_function_closure_data() const;
  Value c_function_name() const;
  size_t c_function_min_arity() const;
  size_t c_function_max_arity() const;
  bool c_function_variable_arity() const;

  // VM FUNCTIONS
  static const unsigned VMFUNCTION_VARIABLE_ARITY_BIT = 1 << 10;
  static const unsigned VMFUNCTION_LOG_BIT = 1 << 11;
  static const unsigned VMFUNCTION_MACRO_BIT = 1 << 12;
  static const unsigned VMFUNCTION_IDENTIFIER_MACRO_BIT = 1 << 13;
  // True if this has been compiled to native code
  static const unsigned VMFUNCTION_NATIVE_BIT = 1 << 14;

  unsigned vm_function_min_arity() const;
  unsigned vm_function_max_arity() const;

  bool vm_function_is_macro() const {
    return heap->get_header_bit(VMFUNCTION_MACRO_BIT);
  }

  bool vm_function_variable_arity() const {
    return heap->get_header_bit(VMFUNCTION_VARIABLE_ARITY_BIT);
  }

  Value vm_function_name() const;
  Value vm_function_macro_env() const;
  Value vm_function_code() const;
  VectorStorage* vm_function_constants() const;

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

  static const unsigned UPVALUE_CLOSED_BIT = 1 << 10;

  // RECORD
  Value record_type() const;
  Value record_ref(unsigned) const;
  void record_set(unsigned, Value);
  unsigned record_field_count() const;

  bool record_applicable() const;
  bool record_isa(Value) const;

  bool record_finalized() const { return heap->get_header_bit(RECORD_FINALIZED_BIT); }

  void record_set_finalized() {
    AR_ASSERT(type() == RECORD) ;
    if(!record_finalized()) heap->set_header_bit(RECORD_FINALIZED_BIT);
  }

  template <class T> T* record_data();

  static const unsigned RECORD_FINALIZED_BIT = 1 << 10;

  // RECORD TYPES

  Value record_type_name() const;
  Value record_type_apply() const;
  Value record_type_print() const;
  Value record_type_parent() const;
  unsigned record_type_field_count() const;
  unsigned record_type_data_size() const;
  Value record_type_field_names() const;

  // PORTS

  std::istream* file_port_input_handle() const;
  std::ostream* file_port_output_handle() const;
  XReader* file_port_reader() const;
  Value file_port_path() const;

  bool file_port_readable() const {
    return heap->get_header_bit(FILE_PORT_INPUT_BIT);
  }

  bool file_port_writable() const {
    return heap->get_header_bit(FILE_PORT_OUTPUT_BIT);
  }

  static const unsigned FILE_PORT_INPUT_BIT = 1 << 10;
  static const unsigned FILE_PORT_OUTPUT_BIT = 1 << 11;
  static const unsigned FILE_PORT_NEVER_CLOSE_BIT = 1 << 12;
  static const unsigned FILE_PORT_STRING_BIT = 1 << 13;

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
  if(!heap->get_header_bit(SYMBOL_ROOT_BIT))
    heap->set_header_bit(SYMBOL_ROOT_BIT);
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
  AR_TYPE_ASSERT(!pair_immutable());
  static_cast<Pair*>(heap)->data_car = v;
}

inline void Value::set_cdr(Value v) {
  AR_TYPE_ASSERT(type() == PAIR);
  AR_TYPE_ASSERT(!pair_immutable());
  static_cast<Pair*>(heap)->data_cdr = v;
}

/// EXCEPTIONS

struct Exception : HeapValue {
  Value tag, irritants, message;
};

inline void Value::exception_activate() {
  AR_TYPE_ASSERT(heap_type_equals(EXCEPTION));
  if(!heap->get_header_bit(Value::EXCEPTION_ACTIVE_BIT)) {
    heap->set_header_bit(Value::EXCEPTION_ACTIVE_BIT);
  }
}

inline void Value::exception_deactivate() {
  AR_TYPE_ASSERT(heap_type_equals(EXCEPTION));
  if(heap->get_header_bit(Value::EXCEPTION_ACTIVE_BIT)) {
    heap->unset_header_bit(Value::EXCEPTION_ACTIVE_BIT);
  }
}

inline bool Value::is_active_exception() const {
  return AR_UNLIKELY(heap_type_equals(EXCEPTION) && heap->get_header_bit(Value::EXCEPTION_ACTIVE_BIT));
}

inline bool Value::exception_trace() const {
  AR_TYPE_ASSERT(heap_type_equals(EXCEPTION));
  return heap->get_header_bit(Value::EXCEPTION_TRACE_BIT);
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

/**
 * For ease of use and performance, all Procedures have a pointer to a native function at their
 * beginning. For VM functions and closures, this always points to apply_vm, for interpreted
 * functions, to apply_interpreter.
 */

struct Procedure : HeapValue {
  c_closure_t procedure_addr;
};

inline void Value::procedure_install(c_closure_t addr) {
  AR_TYPE_ASSERT(!immediatep());
  heap->set_header_bit(Value::VALUE_PROCEDURE_BIT);
  as_unsafe<Procedure>()->procedure_addr = addr;
  AR_ASSERT(procedurep());
  AR_ASSERT(as_unsafe<Procedure>()->procedure_addr);
}

/** An interpreted Scheme function */
struct Function : Procedure {
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

/** A Scheme function, compiled to native code */
struct NativeFunction : HeapValue {
  Bytevector* code;
  Value constants;

  size_t min_arity, max_arity;
};

/**
 * A pointer to a C++ function, callable from
 * Scheme. 
 */
struct CFunction : Procedure {
  Value name, closure;

  size_t min_arity, max_arity;

  static const unsigned CLASS_TYPE = CFUNCTION;
};

inline Value Value::c_function_name() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return as<CFunction>()->name;
}

inline Value Value::c_function_closure_data() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return as<CFunction>()->closure;
}

inline size_t Value::c_function_min_arity() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return as<CFunction>()->min_arity;
}

inline size_t Value::c_function_max_arity() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return as<CFunction>()->max_arity;
}

inline bool Value::c_function_variable_arity() const {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  return heap->get_header_bit(CFUNCTION_VARIABLE_ARITY_BIT);
}

struct VMFunction : Procedure {
  Value name;
  VectorStorage* constants;
  Bytevector* free_variables;
  Bytevector* sources;
  Value macro_env;
  /**
   * A pointer to a VMFunction's code. Can be either VM wordcode or a natively-compiled function.
   */
  Bytevector* code;
  Bytevector* native_code;

  unsigned min_arity, max_arity, stack_max, local_count, upvalue_count;

  static const unsigned CLASS_TYPE = VMFUNCTION;

  size_t* code_pointer() {
    return (size_t*)&code->data;
    //return (size_t*)(((char*)((size_t) this)) + sizeof(VMFunction));
  }
};

inline Value Value::vm_function_macro_env() const {
  return as<VMFunction>()->macro_env;
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

inline Value Value::vm_function_code() const {
  return as<VMFunction>()->code;
}

inline VectorStorage* Value::vm_function_constants() const {
  return as<VMFunction>()->constants;
}

/**
 * An free variable. When the function it is in is still alive on the stack, the upvalue contains
 * an index into the stack noting where that variable can be found.
 * After that function exits, it is converted into a freestanding garbage-collected variable
 */
struct Upvalue : HeapValue {
  union U {
    U(): converted(C_FALSE) {}

    Value* local;
    size_t vm_local_idx;
    Value converted;
  } U;

  static const unsigned CLASS_TYPE = UPVALUE;
};

inline bool Value::upvalue_closed() const {
  AR_TYPE_ASSERT(type() == UPVALUE);
  return heap->get_header_bit(UPVALUE_CLOSED_BIT);
}

inline void Value::upvalue_set(const Value rhs) {
  AR_TYPE_ASSERT(type() == UPVALUE);
  if(heap->get_header_bit(UPVALUE_CLOSED_BIT)) {
    static_cast<Upvalue*>(heap)->U.converted = rhs;
  } else {
    (*static_cast<Upvalue*>(heap)->U.local) = rhs;
    AR_ASSERT(rhs.type() != UPVALUE);
  }
}

inline Value Value::upvalue() {
  AR_TYPE_ASSERT(type() == UPVALUE);
  if(heap->get_header_bit(UPVALUE_CLOSED_BIT)) {
    return as<Upvalue>()->U.converted;
  } else {
    return *(as<Upvalue>()->U.local);
  }
}

inline void Value::upvalue_close() {
  AR_TYPE_ASSERT(type() == UPVALUE);
  AR_TYPE_ASSERT(!upvalue_closed());
  heap->set_header_bit(UPVALUE_CLOSED_BIT);
  as<Upvalue>()->U.converted = (*as<Upvalue>()->U.local);
  AR_ASSERT(upvalue_closed());
}

struct Closure : Procedure {
  Value function;
  VectorStorage* upvalues;

  static const unsigned CLASS_TYPE = CLOSURE;
};

inline Value Value::closure_unbox() const {
  if(heap_type_equals(CLOSURE)) {
    return as_unsafe<Closure>()->function;
  }
  return heap;
}

inline VMFunction* Value::closure_function() const {
  return as<Closure>()->function.as<VMFunction>();
}


/// VECTORS

/** Backing storage for vectors. These are also used internally when the size
 * of a vector-like object is known in advance */
struct VectorStorage : HeapValue {
  size_t length;
  Value data[1];

  static const unsigned CLASS_TYPE = VECTOR_STORAGE;
};

struct Vector : HeapValue {
  Value storage;
  size_t capacity;

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
  VectorStorage* chains;
  unsigned char size_log2;
  size_t entries, max_entries;

  static const Type CLASS_TYPE = TABLE;
};

inline size_t Value::table_entries() const {
  AR_TYPE_ASSERT(heap_type_equals(TABLE));
  return as<Table>()->entries;
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

  c_closure_t finalizer;

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

inline unsigned Value::record_type_data_size() const {
  return as<RecordType>()->data_size;
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

template <class T>
inline T* Value::record_data() {
  Value rtd = record_type();
  AR_ASSERT(rtd.type() == RECORD_TYPE);
  AR_ASSERT(record_isa(rtd));
  size_t data_offset = sizeof(Record) + (rtd.record_type_field_count() * sizeof(Value)) - sizeof(Value);
  return ((T*) (((char*) heap) + data_offset));
 }

/** 
 * Dummy class to be extended by C++ code
 */
struct CRecord : HeapValue {
  RecordType* type;

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
  switch(type()) {
    case FUNCTION: case CFUNCTION: case VMFUNCTION: case CLOSURE:
      return true;
    default: return false;
  }
}

struct FilePort : HeapValue {
  Value path;
  union {
    std::istream* input_handle;
    std::ostream* output_handle;
  };

  /** S-expression reader. Created lazily */
  XReader* reader;

  static const unsigned CLASS_TYPE = FILE_PORT;
};

inline std::istream* Value::file_port_input_handle() const {
  return as<FilePort>()->input_handle;
}

inline std::ostream* Value::file_port_output_handle() const {
  return as<FilePort>()->output_handle;
}

inline Value Value::file_port_path() const {
  return as<FilePort>()->path;
}

///// GC! Garbage collection

struct GC;

/** 
 * FrameHack turns a Value& into a pointer to a stack-allocated Value. 
 */
struct FrameHack {
  // TODO: If the collector is incremental, there's no need to save
  // pointer-to-pointers
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
  Frame *previous;

  Frame(State& state, size_t size, HeapValue*** values);
  Frame(State* state, size_t size, HeapValue*** values);
  ~Frame();
};

/**
 * Like frames, but for natively-compiled code.
 */
struct NativeFrame {
  NativeFrame* previous;
  size_t value_count;
  Value values[1];
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

  Block(size_t size_, unsigned char mark_bit);
  ~Block();

  /** Returns true if there is room in a block for a given allocation */
  bool has_room(size_t position, size_t room) const {
    return (data + (position + room)) <= (data + size);
  }
};

/** Common GC variables */
struct GCCommon {
  State& state;
  //std::vector<Frame*> frames;
  Frame* frames;
  std::list<Handle*> handles;

  /** Vector of objects that need to be finalized */
  std::vector<Value> finalizers;
  std::vector<Value> finalizers2;
  NativeFrame* native_frames;
  Value* vm_stack;
  size_t vm_stack_size;
  size_t vm_stack_used;

  // For virtual machine: protect a single array during function initialization
  size_t protect_argc;
  Value* protect_argv;

  /** If true, collect before every allocation. Flushes out GC bugs, incredibly expensive */
  bool collect_before_every_allocation;
  /** Number of total allocations */
  size_t allocations;
  /** Number of total collections */
  size_t collections;
  size_t live_objects_after_collection, live_memory_after_collection, heap_size;
  size_t block_size;

  GCCommon(State& state_, size_t heap_size_);
  ~GCCommon();

  template <class T> void visit_roots(T& visitor);

  bool stack_needs_realloc(size_t size) const {
    return (vm_stack_used + size) > vm_stack_size;
  }

  /** Check whether the VM stack needs to be grown and do it, if so
   * @returns true if VM stack was grown
   */
  bool grow_stack(size_t size) {
    vm_stack_used += size;
    if(vm_stack_used > vm_stack_size) {
      vm_stack_size *= 2;
      vm_stack = static_cast<Value*>(realloc(vm_stack, vm_stack_size * sizeof(Value)));
      return true;
    }
    return false;
  }

  void shrink_stack(size_t size) {
    vm_stack_used -= size;
  }

  // Align a value along a boundary e.g. align(8, 7) == 8, align(8, 16) == 16,
  // and align(8, 247) == 248 
  static size_t align(int boundary, size_t value) {
    return (size_t)((value + (boundary - 1)) & -boundary);
  }
};

/** Semispace garbage collector */
struct GCSemispace : GCCommon {
  Block *active, *other;
  size_t block_cursor;
  char* other_cursor;
  bool collect_before_every_allocation;

  GCSemispace(State&, size_t);
  ~GCSemispace();

  void copy(HeapValue** ref);

  void copy_roots();
  void collect(size_t request = 0, bool force = false);
  void run_finalizers(bool finalize_all);

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

  void allocation_failed(size_t size);

  HeapValue* allocate(Type type, size_t size) {
    size = align(8, size);

    // TODO: The collect_before_every_allocation stuff should be disabled in production builds;
    // care should be taken to make sure this function is very small and inlinable

    // Bump allocation possible
    if(!has_room(size) || collect_before_every_allocation) {
      allocation_failed(size);
    }

    allocations++;
    HeapValue* v = (HeapValue*) (active->data + block_cursor);
    memset((((char*)v) + sizeof(HeapValue)), 0, size - sizeof(HeapValue));
    v->initialize(type, 0, size);
    block_cursor += size;
    // Assert that pointer is aligned properly.
    // AR_ASSERT(!Value(v).immediatep());
    // AR_ASSERT(v->size == size);
    return v;
  }
};

// RUN! The Arete Runtime

/** A re-entrant instance of the Arete runtime */
struct State {
  struct EvalFrame;
  typedef std::unordered_map<std::string, Symbol*> symbol_table_t;

  GCSemispace gc;

  /** Counts how many times gensym has been called; appended to the end of gensyms to ensure
   * their uniqueness */ 
  size_t gensym_counter;

  bool tco_enabled;

  bool booted;

  /** The symbol table */
  symbol_table_t* symbol_table;

  /** A list of the names of various sources; mostly filenames but these can also be descriptors
   * of C++ strings and REPL lines */
  std::vector<std::string> source_names;

  /** Contents of all sources, for verbose error messages. */
  std::vector<std::string> source_contents;

  /** An array of permanently live values */
  std::vector<Value> globals;

  /** A GC-tracked array of temporary values. May be cleared by function calls. */
  std::vector<Value> temps;

  struct StackTrace {
    StackTrace(const std::string& text_): text(text_), seen(0) {}
    ~StackTrace() {}

    std::string text;
    size_t seen;
  };

  /** A stack trace. */
  std::vector<StackTrace> stack_trace;
  
  State();
  ~State();

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
    S_AND,
    S_OR,
    S_SET,
    S_LET,
    // Module forms
    S_MODULE,
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
    S_CONTINUATION,
    S_FILE_ERROR,
    S_READ_ERROR,
    S_EVAL_ERROR,
    S_TYPE_ERROR,
    S_EXPAND_ERROR,
    S_SYNTAX_ERROR,
    // Global variables
    G_FEATURES,
    G_COMMAND_LINE,
    G_EXPANDER,
    G_COMPILER,
    // Flags that can be changed by users
    G_RECURSION_LIMIT,
    G_TCO_ENABLED,
    G_VM_LOG_REPL,
    G_EXPANDER_PRINT,
    G_FORBID_INTERPRETER,
    // Print parameters
    G_PRINT_READABLY,
    G_PRINT_TABLE_MAX,
    // stdin/stdout
    G_CURRENT_INPUT_PORT,
    G_CURRENT_OUTPUT_PORT,
    // Modules
    G_MODULE_TABLE,
    G_CURRENT_MODULE,
    G_PUSH_MODULE,
    G_CORE_MODULE,
    // Strings
    G_STR_MODULE_NAME,
    G_STR_MODULE_EXPORTS,
    G_END
  };

  /** Performs various initializations required for an instance of Arete;
    * this is separate so the uninitialized State can be unit tested in various ways */
  void boot();

  void boot_common();

  // Value operations

  /**
   * Deep equality comparison. Will not terminate on shared structure.
   */
  bool equals(Value a, Value b);

  /**
   * Register a value as a permanent global
   * @return Its index in the State::globals array
   */
  size_t register_global(Value glob) {
    globals.push_back(glob);
    return globals.size() - 1;
  }

  /** Register a feature for the global *features* list, 
   * which is used in cond-expand 
   */
  void register_feature(const std::string& name);

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

  /**
   * Dequalify a symbol e.g. ##user#x becomes just x. For reporting error messages.
   */
  Value symbol_dequalify(Value sym);

  /** Generate a unique symbol. */
  Value gensym(Value sym);

  Value make_rename(Value expr, Value env);

  Value make_pair(Value car = C_FALSE, Value cdr = C_FALSE,
    size_t size = sizeof(Pair) - sizeof(SourceLocation));
  Value make_src_pair(Value car, Value cdr, SourceLocation& loc);
  Value make_src_pair(Value car, Value cdr, Value src);
  Value make_char(int c);

  /** Create vector backing storage */
  Value make_vector_storage(size_t capacity);

  /** Create a vector
   * @param capacity The initial capacity of the vector. Can save reallocs if this is known
   * in advance
   */
  Value make_vector(size_t capacity = 2);

  Value make_vector(Value vector_storage);
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

  Value make_module(const std::string& name);
  void module_define(Value module, const std::string& key, Value value);

  // Strings
  
  /**
   * Create a copy of a string
   * @param x A STRING Value
   */
  Value string_copy(Value x);

  // Records

  /** Register a new type of record. Returns an index into the globals array. */
  size_t register_record_type(const std::string& cname, unsigned field_count, unsigned data_size,
      Value field_names = C_FALSE, Value parent = C_FALSE);

  void record_type_set_finalizer(size_t global_id, c_closure_t finalizer);

  void record_set(Value rec_, unsigned field, Value value);

  template <class T>
  T* record_data(size_t tag, Value record) {
    Value rtd = globals.at(tag);
    AR_ASSERT(rtd.type() == RECORD_TYPE);
    AR_ASSERT(record.record_isa(rtd));
    size_t data_offset = sizeof(Record) + (rtd.record_type_field_count() * sizeof(Value)) - sizeof(Value);
    return ((T*) (((char*) record.heap) + data_offset));
  }

  Value make_string(const std::string& body);
  Value make_string(size_t length);

  template <class T>
  Value make_bytevector(size_t count) {
    Bytevector* heap = static_cast<Bytevector*>(gc.allocate(BYTEVECTOR,
      sizeof(Bytevector) + (count * sizeof(T))));
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

  Value make_c_function(Value name, Value closure,
    c_closure_t addr, size_t min_arity, size_t max_arity, bool variable_arity);

  Value make_input_file_port(Value path);
  Value make_input_file_port(const char* cpath, std::istream* is);
  Value make_output_file_port(Value path);
  Value make_output_file_port(const char* cpath, std::ostream* os);

  /*
   * Finalize an object that relies on external resources. Arete will emit a warning if 
   * the GC has to finalize anything as programs should use methods like call-with-input-file
   * to ensure that ports have limited extent.
   * @param type Object type, must be passed manually as object may be dead when this is called
   * @param object The object to finalize
   * @param called_by_gc True if called by GC; 
   */
  void finalize(Type object_type, Value object, bool called_by_gc);

  // WRITE! Output

  // Print out a table's internal structure for debugging purposes
  void print_table_verbose(Value tbl);

  /** Pretty print an object. Expensive */
  Value pretty_print(std::ostream& os, Value v);

  // Source code location tracking
  unsigned register_source(const std::string& path, std::istream& is);

  /**
   * Print information about an erroneous pair
   * @return true if argument's source was successfully printed
   */
  bool print_src_pair(std::ostream& os, Value pair, const char* color = ARETE_COLOR_RED);

  /**
   * Attempt to lazily load source code information
   */
  void lazy_load_source(size_t source);

   /**
   * Print an line of source code with a specific source object highlighted
   */
  void print_src_line(std::ostream& os, const SourceLocation& src,
    const char* color = ARETE_COLOR_RED);
  
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

  /**
   * Return a description of a source location
   * @param from_eval True if this was called from interpreter; string will be prepended with eval:
   * in that case.
   */
  std::string source_info(const SourceLocation loc, Value fn_name = C_FALSE,
    bool from_eval = false);

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
  void load_file_functions();

  /** Defines a built-in function */
  void defun_core(const std::string& cname, c_closure_t addr, size_t min_arity, size_t max_arity = 0, bool variable_arity = false);
  void defun_core_closure(const std::string& cname, Value closure, c_closure_t addr, size_t min_arity, size_t max_arity = 0, bool variable_arity = false);

  std::ostream& warn(Value src = C_FALSE);
 
  // Environments are just vectors containing the parent environment in the first position
  // and variable names/values in the even/odd positions afterwards e.g.

  // #(#f hello 12345)
  // for a environment one level below toplevel after a (define hello 12345)

  static const size_t VECTOR_ENV_FIELDS = 1;

  Value make_env(Value parent = C_FALSE, size_t size = 0);

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
  Value env_lookup_impl(Value& env, Value name, bool& reached_toplevel);

  Value env_lookup(Value env, Value name);

  Value file_error(const std::string& msg) {
		std::string file("file");
    return make_exception(file, msg);
  }

  Value type_error(const std::string& msg) {
		std::string type("type");
    return make_exception(type, msg);
  }

  Value eval_error(const std::string& msg, Value exp = C_FALSE);
  Value eval_form(EvalFrame frame, Value exp, unsigned);
  Value eval_body(EvalFrame frame, Value exp, bool single = false);
  Value eval_exp(Value exp);
  /** The primary application function */
  Value eval_list(Value lst, bool expand = true, Value env = C_FALSE);

  // Build a list of out of temps[limit:]
  Value temps_to_list(size_t limit = 0);

  Value expand_expr(Value exp);

  /**
   * This is the primary application function, and the only one that should be called by C++
   * functions. Argv can be evaluated on the stack or a pointer to the temps vector.
   *
   * NOTE: This should never be called against State::temps, as it is used to construct a list when
   * interpreted functions are called.
   */
  Value apply(Value fn, size_t argc, Value* argv);

  /**
   * Shorthand for calling apply against a vector. Necessary when building up garbage-collected
   * lists of arguments.
   */
  Value apply_vector_storage(Value fn, Value vec);

  ///// MODULES

  Value slurp_file(const std::string& path);
  Value load_file(const std::string&);

  ///// Virtual machine
  void trace_function(Value fn, size_t frames_lost, size_t code_offset);

  // Command line interface

  /**
   * Allow Arete's command line interface to run.
   * @return EXIT_SUCCESS or EXIT_FAILURE
   */
  int enter_cli(int argc, char* argv[]);

  /**
   * Allow Arete's REPL to run.
   * @param read_only If true, do not evaluate expressions, just read and print them back.
   * @param history_file path to a history file (relative to home directory)
   * @return false if errors occurred, true otherwise
   */
  bool enter_repl(bool read_only, const char* history_file);

  bool enter_repl() { return enter_repl(false, 0); }

  // Image saving and loading

  /** Save an image. Must exit after calling. */
  void save_image(const std::string& path);

  /** Load an image. Cannot be called after boot(). */
  const char* boot_from_image(const std::string& path);
};

// Functions for Procedure::procedure_addr. Must be freestanding because taking
// a pointer to a State member function does not work well on Windows or Emscripten.
Value apply_interpreter(State& state, size_t argc, Value* argv, void* fnp);
Value apply_vm(State& state, size_t argc, Value* argv, void* fnp);

///// READ! S-Expression reader

/** Reads a number from a string. This has to be separated out so it can be called independently
 * from Scheme in the form of number->string */
struct NumberReader {
  State& state;
  std::string string;
  int radix;
  bool radix_set;
  bool exact;
  bool exact_set;
  bool flonum_allowed;
  /** A description of an error which has occurred. */
  std::string error_desc;

  NumberReader(State& state_, const std::string& string_);
  ~NumberReader() {}

  bool set_radix_param(int);
  bool set_radix(int);
  bool set_exact(bool);
  bool consume_numeric_directive(size_t&);
  bool check_radix_gte(int, char);
  Value read();
};

/** 
 * S-expression reader.
 */
struct XReader {
  /** A table mapping numbers to shared objects; for reading objects with shared structure.
   * Created lazily.
   */
  Value shared_object_table;
  /** Instance of the Arete runtime */
  State& state;
  /** Open stream */
  std::istream& is;
  /** Current position in the stream */
  unsigned source, position, line, column;
  /** Either C_FALSE or an exception that has been encountered during reading; necessary because 
   * the tokenizer does not return Values. read should never be called after this has been set */
  Value active_error;
  Value return_constant;

  /** A temporary buffer that the tokenizer will fill with atomic data (strings, numbers, etc) */
  std::string buffer;
  /** Position where BUFFER has started */
  unsigned token_start_position, token_start_line;
  /** True when within #` syntax that causes all symbols NOT within unquotes to be renamed. */
  bool quasiquote_renaming;

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
    TK_UNQUOTE_INJECT,
    TK_UNQUOTE,
    TK_UNQUOTE_SPLICING,
    TK_QUASIQUOTE,
    TK_QUASIQUOTE_RENAMING,
    TK_RENAME,
    TK_FLONUM,
    TK_FIXNUM,
    TK_NUMBER,
    TK_LBRACKET,
    TK_RBRACKET,
    TK_CHARACTER,
    TK_EXPRESSION_COMMENT,
    TK_CONSTANT,
    TK_EOF
  };

  XReader(State& state_, std::istream& is_, bool slurp_source, const std::string& desc = 
    "anonymous");
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

  TokenType tokenize_number(bool sharp, char start = '\0');

  /** Reads a symbol into the buffer */
  void tokenize_symbol(bool tokenize_sharp = false);

  /** Reads a string into the buffer */
  void tokenize_string();

  /** Look at the next token */
  TokenType next_token();

  /** Read auxiliary syntax (e.g. quote, quasiquote, etc) */
  Value read_aux(const std::string&, unsigned, Value, bool renaming = false);

  /** Read rename auxiliary syntax */
  Value read_aux2(const std::string&, unsigned, Value, Value);

  /** Read the next expression */
  Value read_expr(TokenType);

  /** The entry point for reading an expression */
  Value read();
};

std::ostream& operator<<(std::ostream& os, Value v);

// MISC! Various inline functions 

///// Virtual machine instructions

// Note: The value and order of these instructions is important. It must be matched in the native
// and bytecode compiler, and in the computed goto array and actual code of the virtual machine

enum {
  // These instructions are the core of the virtual machine; they are arranged roughly
  // by their purpose
  OP_BAD = 0,
  // Simple stack operations
  OP_PUSH_CONSTANT = 1,
  OP_PUSH_IMMEDIATE = 2,
  OP_POP = 3,
  // Getters and setters
  OP_GLOBAL_GET = 4,
  OP_GLOBAL_SET = 5,
  OP_LOCAL_GET = 6,
  OP_LOCAL_SET = 7,
  OP_UPVALUE_GET = 8,
  OP_UPVALUE_SET = 9,
  OP_CLOSE_OVER = 10,
  // Application
  OP_APPLY = 11,
  OP_APPLY_TAIL = 12,
  // Flow control
  OP_RETURN = 13,
  OP_JUMP = 14,
  OP_JUMP_WHEN = 15,
  OP_JUMP_WHEN_POP = 16,
  OP_JUMP_UNLESS = 17,
  // Prologue instructions
  OP_ARGC_EQ = 18,
  OP_ARGC_GTE = 19,
  OP_ARG_OPTIONAL = 20,
  OP_ARGV_REST = 21,
  // Instructions below this point are primitive versions of the builtin C++ routines for speed;
  // they are not necessary for code to execute correctly.
  OP_ADD = 22,
  OP_SUB = 23,
  OP_LT = 24,
  OP_CAR = 25,
  OP_CDR = 26,
  OP_LIST_REF = 27,
  OP_NOT = 28,
  OP_EQ = 29,
  OP_FX_LT = 30,
  OP_FX_ADD = 31,
  OP_FX_SUB = 32,
};

inline Type Value::type() const {
#if ARETE_DEV
  if(!immediatep()) {
    ARETE_ASSERT_LIVE(heap);
  }
#endif
  return type_unsafe();
}

inline Frame::Frame(State& state_, size_t size_, HeapValue*** ptrs): state(state_), size(size_),
  values(ptrs), previous(state_.gc.frames) {
  state.gc.frames = this;
}

inline Frame::Frame(State* state_, size_t size_, HeapValue*** ptrs): state(*state_), size(size_),
  values(ptrs), previous(state_->gc.frames) {
  state.gc.frames = this;
}

inline Frame::~Frame() {
  AR_ASSERT(state.gc.frames == this);
  state.gc.frames = previous;
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

// Auto-registration of functions, and assigning of unique IDs to function pointers
// Necessary for the saving and loading of images.

struct DefunGroup;
struct Defun;

extern DefunGroup* defun_group;
c_closure_t function_id_to_ptr(size_t id);
c_closure_t function_ptr_to_id(ptrdiff_t addr);

/** Free some dynamic memory allocations that may be done on program startup. States
 * cannot be created after this is called.
 */
void arete_free_function_tables();

struct DefunGroup {
  DefunGroup(const char* name_): name(name_) {
    defun_group = this;
  }

  ~DefunGroup() {}

  const char* name;
  std::vector<Defun*> data;

  void install(State& state);
  void install_closure(State& state, Value closure);
  void install_module(State& state, const std::string& name, Value closure);
};

struct Defun {
  Defun(const char*, c_closure_t, size_t, size_t = 0, bool = false);
  Defun(void*);
  ~Defun() {}

  const char* fn_name;
  c_closure_t fn;
  size_t min_arity, max_arity;
  bool var_arity;
  bool install;
};

#define AR_DEFUN(name, addr, ...) \
  static Defun _AR_UNIQUE(defun) ((name), addr, __VA_ARGS__);

// Various convenience objects. Note that their fields must be registered in an AR_FRAME
// manually.

/** Builds lists efficiently by tracking tail */
struct ListAppender {
  ListAppender(): head(C_NIL) {}

  Value head, tail;

  void append_cell(Value v) {
    if(head == C_NIL) {
      head = tail = v;
    } else {
      tail.set_cdr(v);
      tail = v;
    }
  }

  void append(State& state, Value v) {
    append_cell(state.make_pair(v, C_NIL));
  }
};

/** 
 * An object for easily iterating over the keys/values of a table. Assumes no insertion 
 * takes place
 */
struct TableIterator {
  TableIterator(Value table_): i(0), table(table_), chain(C_NIL), cell(C_FALSE) {}

  ~TableIterator() {}

  bool operator++();

  Value operator*() const {
    return cell;
  }

  Value key() const { AR_ASSERT(cell != C_FALSE); return cell.car(); }
  Value value() const { AR_ASSERT(cell != C_FALSE); return cell.cdr(); }

  size_t i;
  Value table, chain, cell;
};

} // namespace arete

// Handy macros for writing CFunctions

#define AR_FN_CLOSURE(state, fn_pointer, kast, name) \
  kast name = (kast) Value((ptrdiff_t)(fn_pointer)).c_function_closure_data().bits;

#define AR_FN_ARGC_EQ(state, argc, expect) \
  if((argc) != (expect)) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected exactly " << (expect) << " arguments but got " << (argc); \
    AR_FN_STACK_TRACE(state); \
    return (state).eval_error(__os.str()); \
  }

#define AR_FN_ARGC_GTE(state, argc, expect) \
  if((argc) < (expect)) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected at least " << (expect) << " arguments but got " << (argc); \
    AR_FN_STACK_TRACE(state); \
    return (state).eval_error(__os.str()); \
  }

#define AR_FN_ARGC_LTE(state, argc, expect) \
  if((argc) > (expect)) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected at least " << (expect) << " arguments but got " << (argc); \
    AR_FN_STACK_TRACE(state); \
    return (state).eval_error(__os.str()); \
  }

#define AR_FN_ARGC_BETWEEN(state, argc, e1, e2) \
  if((argc) < e1) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected at least " << (e1) << " arguments but got " << (argc); \
    AR_FN_STACK_TRACE(state); \
    return (state).eval_error(__os.str()); \
  } else if ((argc) > e2) { \
    std::ostringstream __os; \
    __os << "function " << (fn_name) << " expected at most " << (e2) << " arguments but got " << (argc); \
    AR_FN_STACK_TRACE(state); \
    return (state).eval_error(__os.str()); \
  }

#define AR_FN_STACK_TRACE(state) \
  std::ostringstream __stackinfo; \
  __stackinfo << __FILE__ << ":" << __LINE__; \
  (state).stack_trace.push_back(__stackinfo.str());

#define AR_FN_CHECK(state, exc) \
  if((exc).is_active_exception()) { \
    AR_FN_STACK_TRACE(state); \
    return state; \
  }

#define AR_FN_STATE_STACK_TRACE() \
  std::ostringstream __stackinfo; \
  __stackinfo << __FILE__ << ":" << __LINE__; \
  stack_trace.push_back(__stackinfo.str());

#define AR_FN_STATE_CHECK(exc) \
  if((exc).is_active_exception()) { \
    AR_FN_STATE_STACK_TRACE(); \
    return (exc); \
  }

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
    __os << "function " << (fn_name) << " expected argument " << (i) << " to be of type " << (Type)(expect) << " but got " << (argv[i].type_desc()); \
    AR_FN_STACK_TRACE(state); \
    return (state).type_error(__os.str()); \
  } 

#define AR_FN_EXPECT_HEAP_TYPE(state, argv, i, expect) \
  if(!((argv)[(i)].heap_type_equals(expect))) { \
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

#define AR_FN_EXPECT_APPLICABLE_ARITY(state, argv, arg, arity) \
  if(!((argv)[(arg)].applicable())) { \
    std::ostringstream os; \
    os << fn_name << " expected argument " << (arg) << " to be applicable but got a non-applicable " << (argv)[(arg)].type(); \
    AR_FN_STACK_TRACE(state); \
    return state.type_error(os.str()); \
  } else if(!(argv)[(arg)].procedure_arity_equals((arity))) { \
    std::ostringstream os; \
    os << fn_name << " expected argument " << (arg) << " to be an applicable value that takes " << (arity) << " arguments"; \
    AR_FN_STACK_TRACE(state); \
    return state.type_error(os.str()); \
  }

#define AR_FN_EXPECT_APPLICABLE(state, argv, arg) \
  if(!((argv)[(arg)].applicable())) { \
    std::ostringstream os; \
    os << fn_name << " expected argument " << (arg) << " to be applicable but got a non-applicable " << (argv)[(arg)].type(); \
    AR_FN_STACK_TRACE(state); \
    return state.type_error(os.str()); \
  }

#define AR_FN_EXPECT_NUMBER(state, argv, arg) AR_FN_ASSERT_ARG(state, arg, "to be a number", (argv[(arg)].numeric()))

#define AR_FN_EXPECT_RECORD_ISA(state, argv, arg, tag_index) \
  if((argv)[(arg)].type() != RECORD) { \
    std::ostringstream __os; \
    __os << fn_name << " expected argument " << (arg) << " to be a record but got " << ((argv)[(arg)].type()); \
    return state.type_error(__os.str()); \
  } else if((argv)[(arg)].record_type() != (state).globals.at((tag_index))) { \
    std::ostringstream __os; \
    __os << fn_name << " expected argument " << (arg) << " to be a record of type " << (state).globals.at((tag_index)).record_type_name() << " but got " << ((argv)[(arg)].record_type().record_type_name()); \
    return state.type_error(__os.str()); \
  }

#define AR_FN_CHECK_BOUNDS(state, tipe, len, idx) \
  if(len <= (size_t) idx) { \
    std::ostringstream os; \
    os << fn_name << " attempted to get index " << (idx) << " in a " << tipe << " of length " << (len); \
    return state.type_error(os.str()); \
  }

#define AR_FN_CHECK_FX_RANGE(state, arg, val, min, max) \
  if((val).fixnum_value() < (min) || (val).fixnum_value() > (max)) { \
    std::ostringstream os; \
    os << fn_name << " needed argument " << (arg) << " to be " << min << " < " << (val).fixnum_value() << " max"; \
    return state.type_error(os.str()); \
  }

#endif // ARETE_HPP
