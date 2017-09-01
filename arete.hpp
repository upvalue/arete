// arete.hpp - an embeddable scheme implementation
#ifndef ARETE_HPP
#define ARETE_HPP

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <iostream>
#include <vector>

#define ARETE_ASSERT assert

#ifndef ARETE_LOG_TAGS
# define ARETE_LOG_TAGS 0
#endif 

#ifndef ARETE_BLOCK_SIZE
# define ARETE_BLOCK_SIZE 4096
#endif

#ifndef ARETE_GC_LOAD_FACTOR
# define ARETE_GC_LOAD_FACTOR 77
# endif

#define ARETE_LOG_TAG_GC (1 << 0)

#ifndef ARETE_COLOR_RED
# define ARETE_COLOR_RED "\033[1;31m"
#endif 

#ifndef ARETE_COLOR_RESET
# define ARETE_COLOR_RESET "\033[0m"
#endif

#define ARETE_LOG(tag, prefix, msg) \
  if((ARETE_LOG_TAGS) & (tag)) { \
    std::cout << ARETE_COLOR_RED << "arete:" << prefix << ": " << ARETE_COLOR_RESET << msg << std::endl; \
  }

#define ARETE_LOG_GC(msg) ARETE_LOG((ARETE_LOG_TAG_GC), "gc", msg)

namespace arete {

// Forward declarations
struct State;
struct Block;

//
///// REPRESENTATION OF SCHEME VALUES
//

/** A Scheme value */
struct Value {
  enum Type {
    /** A fixed-point integer */
    FIXNUM,
    /** A unique symbol */
    SYMBOL,
    STRING,
    PAIR,
    /** A constant */
    CONSTANT
  };

  /** Size of the object */
  size_t size;
  /** 
    * Whether or not an object is marked alive. Note that the meaning of this may change depending
    * on GC state. See GC docs for more information.
    */
  unsigned char mark_bit;

  Value() {}
  virtual ~Value() {}

  virtual void print_to_ostream(std::ostream& os) {}
  virtual Type type() const { return FIXNUM; }
  /** Returns true if this is an immediate value e.g. a fixed-point integer */
  virtual bool immediatep() const { return false; }

  void* operator new(size_t size, char* memory) {
    return memory;
  }
};

struct Constant : Value {
  enum {
    T, F, NIL
  };

  unsigned char data;

  virtual Type type() const { return CONSTANT; }
  virtual bool immediatep() const { return true; }
};

struct Fixnum : Value {
  ptrdiff_t data;

  virtual Value::Type type() const {
    return Value::FIXNUM;
  }

  virtual void print_to_ostream(std::ostream& os) {
    os << data;
  }

  virtual bool immediatep() const { return true; }
};

struct Pair : Value {
  Value *data_car, *data_cdr;

  virtual Type type() const {
    return PAIR;
  }

  virtual void print_to_ostream(std::ostream& os) {
    os << '(';
    Value* pare = this;
    while(true) {
      static_cast<Pair*>(pare)->data_car->print_to_ostream(os);
      if(static_cast<Pair*>(pare)->data_cdr->type() == PAIR) {
        os << ' ';
        pare = static_cast<Pair*>(pare)->data_cdr;
      } else if(static_cast<Pair*>(pare)->data_cdr->type() == CONSTANT &&
        static_cast<Constant*>(pare)->data == Constant::NIL) {
        os << ')';
        break;
      } else {
        os << " . ";
        static_cast<Pair*>(pare)->data_cdr->print_to_ostream(os);
        os << ')';
        break;
      }
    }

  }
};

struct Symbol : Value {
  const char data[1];

  virtual Type type() const {
    return SYMBOL;
  }
};

//
///// GARBAGE COLLECTOR
//

struct Frame {
  Frame(State&, size_t, Value*** roots);
  ~Frame();

  State& state;
  size_t size;
  Value*** roots;
};

struct FrameHack {
  FrameHack(Fixnum*& fx): v((Value**) &fx) {}
  FrameHack(Pair*& fx): v((Value**) &fx) {}
  ~FrameHack() {}

  Value** v;
};

#define ARETE_FRAME(state, ...) \
  FrameHack __arete_frame_ptrs[] = { __VA_ARGS__ }; \
  Frame __arete_frame(state, sizeof(__arete_frame_ptrs) / sizeof(FrameHack), (Value***) __arete_frame_ptrs);

/** A block of allocated memory */
struct Block {
  char* data;
  size_t size;

  Block(size_t size_, unsigned char mark_bit): size(size_) {
    data = static_cast<char*>(calloc(1, size));
    Value* header = (Value*) data;
    header->size = size_;
    header->mark_bit = !mark_bit;
  }

  ~Block() {}

  /** Returns true if there is room in a block for a given allocation */
  bool has_room(size_t position, size_t room) const {
    return (data + (position + room)) <= (data + size);
  }
};

/** Garbage collector */
struct GC {
  std::vector<Block*> blocks;
  size_t block_i;
  size_t block_cursor;
  size_t allocations;
  size_t collections;
  size_t live_objects_after_collection;
  size_t live_memory_after_collection;
  size_t allocated_memory;
  std::vector<Frame*> frames;
  unsigned char mark_bit;
  size_t block_size;

  GC(): block_i(0), block_cursor(0), allocations(0), collections(0), live_objects_after_collection(0),
      live_memory_after_collection(0), allocated_memory(ARETE_BLOCK_SIZE), mark_bit(1), block_size(ARETE_BLOCK_SIZE) {
    Block *b = new Block(ARETE_BLOCK_SIZE, mark_bit);

    ARETE_ASSERT(!live((Value*) b->data));
    blocks.push_back(b);
  }

  ~GC() {
    for(size_t i = 0; i != blocks.size(); i++) {
      Block* b = blocks[i];
      free(b->data);
      delete b;
    }
  }

  void mark(Value* v) {
    // If there is no object or object has already been marked
    if(v == 0 || v->mark_bit == mark_bit) {
      return;
    }

    live_objects_after_collection++;

    v->mark_bit = mark_bit;

    if(v->immediatep()) {
      live_memory_after_collection += sizeof(Fixnum);
      return;
    }

    switch(v->type()) { 
      case Value::PAIR:
        live_memory_after_collection += sizeof(Pair);
        mark(static_cast<Pair*>(v)->data_car);
        mark(static_cast<Pair*>(v)->data_cdr);
        break;
      case Value::CONSTANT:
      case Value::FIXNUM:
      default:
        ARETE_ASSERT(!"could not mark object");
        break;
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
        if(f->roots[j]) {
          mark(*(f->roots[j]));
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

  bool live(Value* v) const {
    return v->mark_bit == mark_bit;
  }

  template <class T>
  T* allocate(size_t additional = 0) {
    allocations++;

    // TODO align to 8 bytes
    size_t sz = sizeof(T) + additional;

    // TODO: Allocate for overly large objects
    bool collected = false;

  retry:
    // Mark and don't sweep garbage collection

    // If there is room in the current block, simply use it
    while(block_i != blocks.size()) {
      while(blocks[block_i]->has_room(block_cursor, sz)) {
        Value* v = (Value*)(blocks[block_i]->data + block_cursor);

        ARETE_ASSERT(v->size > 0); // assert that memory has been initialized with some kind of size
        if((!live(v) && v->size >= sz)) {
          size_t mem_size = v->size;

          // Success!
          char* memory = blocks[block_i]->data + block_cursor;

          // If there is enough room after this memory to handle another object, note down its
          // size and move on

          // TODO: Coalesce memory
          if(mem_size - sz >= sizeof(Constant)) {
            // ARETE_LOG_GC("additional " << (mem_size - sz) << " bytes after object");
            Value* next_object = ((Value*) ((blocks[block_i]->data + block_cursor) + sz));
            next_object->mark_bit = !mark_bit;
            next_object->size = mem_size - sz;

            ARETE_ASSERT(!live(next_object));
            ARETE_ASSERT(next_object->size >= sizeof(Constant));

            block_cursor += sz;
          } else {
            sz = mem_size;
            block_cursor += sz;
          }

          memset(memory, 0, sz);
          T* ret = new (memory) T();
          //T* ret = reinterpret_cast<T*>(memory);
          ret->mark_bit = mark_bit;
          ARETE_ASSERT(live(ret));

          // Break up memory as necessary
          ret->size = sz;
          return ret;
        }

        block_cursor += v->size;

      }

      ARETE_LOG_GC("block " << block_i << " out of room, moving on");

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
      // ARETE_ASSERT(!"out of room; allocation failed");
      return 0;
    }
  }
};

/** Reader; reads S-expressions */
struct Reader {
  Reader(State& state_, std::istream& stream_): state(state_), stream(stream_) {

  }
  ~Reader() {}

  State& state;
  std::istream& stream;

  Value* read();
};

/** Compiles s-expressions to Functions */
struct Compiler {
  Compiler() {}
  ~Compiler() {}
  
  void compile();
};

/** Toplevel state object. Contains common functionality and runtime state. */
struct State {
  State() {}
  ~State() {}

  GC gc;

  Pair* make_pair(Value *car = 0, Value* cdr = 0) {
    Pair* p = gc.allocate<Pair>();
    p->data_car = car;
    p->data_cdr = cdr;
    return p;
  }
};

Frame::Frame(State& state_, size_t size_, Value*** roots_): state(state_), size(size_), roots(roots_) {
  state.gc.frames.push_back(this);
  ARETE_ASSERT(state.gc.frames.back() == this);
}

Frame::~Frame() {
  ARETE_ASSERT(state.gc.frames.back() == this);
  state.gc.frames.pop_back();
}

// Print objects to stdout
inline std::ostream& operator<<(std::ostream& os, arete::Value* value) {
  value->print_to_ostream(os);
  return os;
}

} // namespace arete

#endif // ARETE_HPP
