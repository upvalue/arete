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
#include <sstream>
#include <vector>
#include <unordered_map>

/** General assertions. */
#ifndef AR_ASSERT
# define AR_ASSERT assert
#endif

/** Type assertions when dealing with dynamic types; should probably be left on */
#ifndef AR_TYPE_ASSERT
# define AR_TYPE_ASSERT assert
#endif 

#define AR_FRAME(state, ...)  \
  FrameHack __arete_frame_ptrs[] = { __VA_ARGS__ };  \
  Frame __arete_frame((state), sizeof(__arete_frame_ptrs) / sizeof(FrameHack), (HeapValue***) __arete_frame_ptrs); 

// Block size.
#ifndef AR_BLOCK_SIZE
# define AR_BLOCK_SIZE 4096
#endif

// If more than ARETE_GC_LOAD_FACTOR% is in use after a collection, the garbage collector will
// double in-use memory.
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
struct SourceLocation;
struct Pair;

std::ostream& operator<<(std::ostream& os,  Value);

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
  STRING = 6,
  // Pointers
  VECTOR = 8,
  VECTOR_DATA = 9,
  PAIR = 10, 
  EXCEPTION = 11,
};

// Constants:

enum {
  C_TRUE = 2,             // 0000 0010 #t
  C_FALSE = 6,            // 0000 0110 #f
  C_NIL = 10,             // 0000 1010 ()
  C_UNSPECIFIED = 14 ,    // 0000 1110 #<unspecified> 
  C_EOF = 18,             // 0001 0010 #<eof>
};

// A heap-allocated, garbage-collected value.
struct HeapValue {
  // Heap value headers are formatted like this:

  // ...m tttt tttt
  // m = mark
  // t = value type
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

  void flip_mark_bit() { header += get_mark_bit() ? -256 : 256; }
};

// Floating point number
struct Flonum : HeapValue {
  double number;  
};

struct Symbol : HeapValue {
  const char* name;
};

struct String : HeapValue {
  size_t bytes;
  char data[1];
};

struct VectorData;

struct Vector : HeapValue {
  VectorData* data;
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
    return bits;
  }

  static Value t() { return C_TRUE; }
  static Value f() { return C_FALSE; }
  static Value nil() { return C_NIL; }
  static Value unspecified() { return C_UNSPECIFIED; }
  static Value eof() { return C_EOF; }

  // FLONUMS
  double flonum_value() const {
    AR_TYPE_ASSERT(type() == FLONUM);
    return static_cast<Flonum*>(heap)->number;
  }

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

  // SYMBOLS
  const char* symbol_name() const {
    AR_TYPE_ASSERT(type() == SYMBOL);
    return static_cast<Symbol*>(heap)->name;
  }

  // PAIRS
  Value car() const;
  Value cdr() const;
  void set_car(Value);
  void set_cdr(Value);

  bool pair_has_source() const {
    AR_TYPE_ASSERT(type() == PAIR);
    return heap->get_header() & 512;
  }

  SourceLocation* pair_src() const;
  void set_pair_src(SourceLocation&);

  // EXCEPTION
  bool is_active_exception() const;
  Value exception_tag() const;
  Value exception_message() const;
  Value exception_irritants() const;

  // OPERATORS

  // Identity comparison
  inline bool operator==(const Value& other) const {
    return bits == other.bits;
  }

  inline bool operator!=(const Value& other) const { return bits != other.bits; }
};

/** Identifies a particular piece of code */
struct SourceLocation {
  SourceLocation(): file(0), line(0) {}
  SourceLocation(size_t file_, size_t line_): file(file_), line(line_) {}

  size_t file, line;
};

struct Pair : HeapValue {
  Value data_car, data_cdr;
  SourceLocation src;
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

inline Value Value::car() const {
  AR_TYPE_ASSERT(type() == PAIR);
  return static_cast<Pair*>(heap)->data_car;
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

struct VectorData: HeapValue {
  size_t length, capacity;
  Value data[1];
};

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
  Frame(State* state, size_t size, HeapValue*** values);
  ~Frame();

  GC& gc;
  size_t size;
  HeapValue*** values;
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

  GC(State& state_): state(state_), block_i(0), block_cursor(0), allocations(0), collections(0), live_objects_after_collection(0),
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
      case FLONUM:
      case STRING:
      case SYMBOL:
        return;
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
        v = static_cast<Vector*>(v)->data;
        goto again;
      default:
      case BLOCK: case FIXNUM: case CONSTANT:
        std::cout << v->get_type() << std::endl;
        AR_ASSERT(!"arete:gc: bad value on heap; probably a GC bug"); break;
    }
  }

  void mark_symbol_table();

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

    mark_symbol_table();

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

          AR_ASSERT(((ptrdiff_t) ret & 3) == 0);

          // Break up memory as necessary
          ret->size = sz;
          return ret;
        }

        block_cursor += v->size;
      }

      ARETE_LOG_GC("block " << block_i << " (" << blocks[block_i]->size << "b) out of room, moving on");

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
    for(symbol_table_t::const_iterator x = symbol_table.begin(); x != symbol_table.end(); ++x) {
      free((void*) x->second->name);
    }
  }

  // Source code location tracking
  unsigned register_file(const std::string& path) {
    source_names.push_back(path);
    return source_names.size() - 1;
  }

  /** Performs various initializations required for an instance of Arete;
    * this is separate so the State itself can be used for lightweight testing */
  void boot() {
    source_names.push_back("unknown");
    source_names.push_back("c-string");
    // We do these side-effecting calls here to ensure that no allocations are required
    // when these symbols are used.
    get_symbol("quote"); get_symbol("quasiquote"); get_symbol("unquote");
    // get_symbol("read-error");
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

  /** cons */
  Value make_pair(Value car = Value::f(), Value cdr = Value::f(),
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
    pare.heap->header += 512;
    pare.set_pair_src(loc);

    AR_ASSERT(pare.type() == PAIR);
    AR_ASSERT(pare.pair_has_source());

    return pare;
  }

  Value make_string(const std::string& body) {
    String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + body.size()));
    heap->bytes = body.size();
    strncpy(heap->data, body.c_str(), body.size());
    AR_ASSERT(heap->data[heap->bytes] == '\0');
    heap->data[heap->bytes] = '\0';

    return heap;
  }

  Value make_exception(const std::string& ctag, const std::string& cmessage, Value irritants = C_UNSPECIFIED) {
    Value tag, message, exc;
    AR_FRAME(this, tag, message, irritants, exc);
    Exception* heap = static_cast<Exception*>(gc.allocate(EXCEPTION, sizeof(Exception)));
    tag = get_symbol(ctag);
    message = make_string(cmessage);
    heap->tag = tag;
    heap->message = message;
    heap->irritants = irritants;
    return heap;
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
};

inline void GC::mark_symbol_table() {
  ARETE_LOG_GC(state.symbol_table.size() << " live symbols");
  for(auto x = state.symbol_table.begin(); x != state.symbol_table.end(); x++) {
    mark(x->second);
  }
}

///// (READ) Reader

// Reader.
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
  
  struct EofHandler {
    SourceLocation loc;
  } eof;

  /** Returns true if a char is a separator and therefore
    * should stop reading of symbols, numbers etc */
  static bool is_separator(char c) {
    return c == '#' || c == '(' || c == ')' ||
      c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '[' 
      || c == ']' || c == '"';
  }

  // Save source location
  SourceLocation save() {
    return SourceLocation(file, line);
  }

  /** Read quasiquote and friends */
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
  Value read_error(const std::string& description, bool begin = false, size_t begin_line = 0) {
    std::ostringstream os;
    SourceLocation loc(file, begin ? begin_line : line);

    os << state.source_info(&loc) << ' ';
    if(begin) {
      os << "in list: ";
    } 
    os << description;

    return state.make_exception("read-error", os.str());
  }

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

  /** Read a single expression */
  Value read_expr(Token& return_token) {
    return_token = TK_NONE;
    char c;
    while(getc(c)) {
      if(c >= '0' && c <= '9') {
        // Fixnums
        ptrdiff_t number = c - '0';
        while(true) {
          c = is.peek();
          if(c >= '0' && c <= '9') {
            is >> c;
            number = (number * 10) + (c - '0');
          } else {
            break;
          }
        }
        return Value::make_fixnum(number);
      } else if(c == '#') {
        // Constants (#t, #f)
        c = is.peek();
        if(c == 't' || c == 'f') {
          is >> c;
          return c == 't' ? C_TRUE : C_FALSE;
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

        // Attach source code information
        while(true) {
          Token tk;
          elt = read_expr(tk);

          if(elt == C_EOF) {
            AR_ASSERT(is.eof());
            return read_error("unexpected EOF in list", true, list_start.line);
          } else if(elt.is_active_exception()) {
            return elt;
          }

          // ()
          if(tk == match && head.bits == 0) {
            return C_NIL;
          }

          if(tk == TK_DOT) {
            elt = read_expr(tk);
            if(elt == C_EOF) {
              return read_error("unexpected EOF after dot", true, list_start.line);
            } else if(elt.is_active_exception()) {
              return elt;
            } else if(tk == match) {
              return read_error("unexpected ) after dot", true, list_start.line);
            } 
            tail.set_cdr(elt);
          } else if(tk == match) {
            break;
          } else if(tk == TK_RPAREN || tk == TK_RBRACKET) {
            // Little silly: this relies on the above else if failing to catch
            // which bracket shouldn't match
            return read_error("unexpected bracket in list", true, list_start.line);
          }

          swap = make_pair(elt, Value::nil());

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
          c = is.peek();
          if(is.eof()) {
            return read_error("unexpected EOF in string");
          } else if(c == '"') {
            is >> c;
            break;
          }
          
          buffer += c;
          is >> c;
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

struct StringReader {
  Reader* reader;
  std::stringstream ss;

  StringReader(State& state, std::string str, const std::string& desc = "anonymous") {
    ss << str;
    reader = new Reader(state, ss);
    reader->file = desc.compare("anoymous") == 0 ? 1 : state.register_file(desc);
  }

  ~StringReader() {
    delete reader;
  }

  Reader* operator->() {
    return reader;
  }
};


/** Macro for giving descriptive source info for Scheme code in C strings e.g.
  * c-string@asdf.cpp:351 */
#define AR_STRINGIZE2(x) #x

#define AR_STRINGIZE(x) AR_STRINGIZE2(x)

#define AR_STRING_READER(name, state, string) \
  StringReader name ((state), (string), ("c-string@" __FILE__ ":" AR_STRINGIZE(__LINE__)));

// Various inline functions

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

// Output Scheme values

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
        default: os << "<unknown constant>";
      }
    case FLONUM:
      return os << v.flonum_value(); 
    case SYMBOL:
      return os << v.symbol_name();
    case STRING:
      return os << '"' << v.string_data() << '"';
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
