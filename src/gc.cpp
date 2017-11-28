// gc.cpp - Garbage collection

// TODO: Reduction of code duplication between collectors. It should be possible to share
// the code that visits the pointers. The marking collector is recursive and uses a GOTO to save
// stack space. The copying collector is not.

// TODO: Finalizers should probably simply be disabled in production builds; this should be used
// for warnings only.

// They'll all be searched after every collection. Depending on how many persistent finalizable
// objects are in a program, this could be a source of slowdowns.

// TODO: It seems like duplicating AR_FRAME values in the same function call causes issues e.g.
// AR_FRAME(this, something)
//   AR_FRAME(this, something)

// Leads to errors. Issues with the reference hack used?

// TODO: Lazy sweep improvements

// One idea would be to do a little bit of work for each allocation - sweep over X bytes of memory,
// and do things like find holes that are sized well for common objects like pairs. This would
// reduce fragmentation as well as the worst case scenario of e.g. having to sweep over a bunch
// of large objects to find a small one. The sweep would still eventually have to go over all
// objects (to mark them), but this might improve things. It would, however, increase complexity
// quite a bit.


#include <chrono>

#include "arete.hpp"

#define ARETE_LOG_GC(msg) ARETE_LOG((ARETE_LOG_TAG_GC), "gc", msg)

namespace arete {

void State::finalize(Type object_type, Value object, bool called_by_gc) {
  bool needed_finalization = false;
  std::string needed_finalization_desc;

  switch(object_type) {
    case FILE_PORT: {
      needed_finalization_desc = "files";
      FilePort* fp = object.as_unsafe<FilePort>();
      if(fp->reader) {
        delete fp->reader;
        fp->reader = 0;
        // We won't yell at the user if they've inadvertently created a heap-allocated XReader
        // for stdin
        if(!fp->get_header_bit(Value::FILE_PORT_NEVER_CLOSE_BIT))
          needed_finalization = true;
      } 

      if(fp->get_header_bit(Value::FILE_PORT_NEVER_CLOSE_BIT))
        break;

      if(object.file_port_readable() && fp->input_handle) {
        delete fp->input_handle;
        fp->input_handle = 0;
        needed_finalization = true;
      } else if(object.file_port_writable() && fp->output_handle) {
        delete fp->output_handle;
        needed_finalization = true;
        fp->output_handle = 0;
      }

      break;
    }
    case RECORD: {
      if(!object.record_finalized()) {
        needed_finalization = true;
        RecordType* rtd = object.record_type().as<RecordType>();
        if(rtd->finalizer) {
          (rtd->finalizer)(*this, object);
        }
        needed_finalization_desc = rtd->name.string_data();
        object.record_set_finalized();
      }
      break;
    }
    case BLOB: {
      break;
    }
    default: 
      warn() << "don't know how to finalize object of type " << object_type << std::endl;
      break;
  }

  if(called_by_gc && needed_finalization) {
    warn() << "finalizer called by GC. " << needed_finalization_desc << " should always be closed in program code." << std::endl;;
  }
}

size_t gc_collect_timer = 0;
size_t gc_alloc_timer = 0;
size_t gc_overall_timer = 0;
size_t gc_longest_pause = 0;

#ifdef ARETE_BENCH_GC
struct GCTimer {
  GCTimer(size_t& timer_): timer(timer_) {
    t1 = std::chrono::high_resolution_clock::now();
  }

  ~GCTimer() {
    size_t elapsed = 
      (std::chrono::duration_cast<std::chrono::microseconds>((
        std::chrono::high_resolution_clock::now() - t1))).count();
    gc_longest_pause = std::max(elapsed, gc_longest_pause);
    timer += elapsed;
  }

  void end() {
    size_t elapsed = 
      (std::chrono::duration_cast<std::chrono::microseconds>((
        std::chrono::high_resolution_clock::now() - t1))).count();
    timer += elapsed;
  }

  size_t& timer;
  std::chrono::time_point<std::chrono::high_resolution_clock> t1, t2;
};
#else
struct GCTimer {
  GCTimer(size_t) {}
  ~GCTimer() {}
};
#endif

GCTimer gc_overall_timer_i(gc_overall_timer);

Block::Block(size_t size_, unsigned char mark_bit): size(size_) {
  data = static_cast<char*>(malloc(size));
  ((HeapValue*) data)->initialize(BLOCK, !mark_bit, size_);
}

Block::~Block() {
  free(data);
}

void State::print_gc_stats(std::ostream& os) {
  os << (gc.heap_size / 1024) << "kb in use after " << gc.collections << " collections and "
     << gc.allocations << " allocations " << std::endl;

#ifdef ARETE_BENCH_GC
  std::cout << (gc_collect_timer / 1000) << "ms in collection" << std::endl;
# if ARETE_GC_STRATEGY == ARETE_GC_INCREMENTAL
  std::cout << (gc_alloc_timer / 1000) << "ms in allocation" << std::endl;
  std::cout << ((gc_collect_timer + gc_alloc_timer) / 1000) << " ms total" << std::endl;
# else
  std::cout << ((size_t)((((char*)gc.active->data + gc.block_cursor) - (char*)gc.active->data)) / 1024) << "kb allocated" << std::endl;
# endif

  // TODO: This cannot be called twice and remain accurate
  gc_overall_timer_i.end();
  std::cout << "approximately " << ((double)(gc_collect_timer + gc_alloc_timer) * 100) / (double)(gc_overall_timer) << "% of program time spent in GC" << std::endl;
  std::cout << "longest pause: " << (gc_longest_pause / 1000) << "ms" << std::endl;

#endif
}

GCCommon::GCCommon(State& state_, size_t heap_size_):
  state(state_), 
  frames(0),
  vm_frames(0),
  collect_before_every_allocation(false),
  allocations(0),
  collections(0), live_objects_after_collection(0), live_memory_after_collection(0),
  heap_size(heap_size_),
  block_size(heap_size_) {

}

// This applies a GC-specific function to all the root variables of a program
template <class T>
void GCCommon::visit_roots(T& walker) {
  for(Frame* f = frames; f != 0; f = f->previous) 
    for(size_t j = 0; j != f->size; j++) 
      walker.touch(f->values[j]);

  for(size_t i = 0; i != state.globals.size(); i++) 
    walker.touch(state.globals[i].heap);

  for(size_t i = 0; i != state.temps.size(); i++)
    walker.touch(state.temps[i].heap);

  for(std::list<Handle*>::iterator i = handles.begin(); i != handles.end(); i++)
    walker.touch(((*i)->ref.heap));

  for(auto x = state.symbol_table->begin(); x != state.symbol_table->end(); x++) {
    Symbol* v = x->second;
    walker.touch((HeapValue**) &v);
    state.symbol_table->at(x->first) = v;
  }

  VMFrame* link = vm_frames;
  while(link != 0) {
    VMFunction* fn = link->fn;

    size_t stack_i = link->stack_i;
    unsigned local_count = fn->local_count;
    size_t free_vars = fn->free_variables ? fn->free_variables->length : 0;

    walker.touch((HeapValue**) &link->fn);

    walker.touch((HeapValue**) &link->exception);

    for(size_t i = 0; i != free_vars; i++)
      walker.touch((HeapValue**) &link->upvalues[i].heap);

    if(link->closure != 0)
      walker.touch((HeapValue**) &link->closure);

    if(link->stack != 0)
      for(size_t i = 0; i != stack_i; i++)
        walker.touch((HeapValue**) &link->stack[i]);

    if(link->locals != 0)
      for(unsigned i = 0; i != local_count; i++)
        walker.touch((HeapValue**) &link->locals[i]);

    // Update code pointer.
    //link->code = link->fn->code_pointer();

    // I lost like two hours to a missing paren here
    // Like this: link->code = (size_t*)(char*) (link->fn) + sizeof(VMFunction);
    // Fun stuff.
    link = link->previous;
  }
}

//
///// SEMISPACE COLLECTOR
//

GCSemispace::GCSemispace(State& state_, size_t heap_size):
    GCCommon(state_, heap_size), 
    active(0), other(0),
    block_cursor(0),
    collect_before_every_allocation(false) {

  active = new Block(heap_size, 0);
}

GCSemispace::~GCSemispace() {
  delete active;
  if(other != 0) delete other;

}

void GCSemispace::copy(HeapValue** ref) {
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
  
  AR_ASSERT(obj->size == size);
  // We use the object's size field to store the forward pointer
  obj->size = (size_t) cpy;
  obj->header = RESERVED;

  (*ref) = cpy;
}

void GCSemispace::allocation_failed(size_t size) {
  collect(size);
  if(!has_room(size)) {
    collect(size, true);
    if(!has_room(size)) {
      std::cerr << "arete:gc: semispace allocation of size " << size << " failed with heap of size " << heap_size << std::endl;
      AR_ASSERT(!"arete:gc: semispace allocation failed");
    }
  }
}

extern bool thing;

void GCSemispace::run_finalizers(bool finalize_all) {
  // Finalize objects
  std::vector<Value> finalizers2;
  ARETE_LOG_GC("checking " << finalizers.size() << " finalizable objects");
  for(size_t i = 0; i != finalizers.size(); i++) {
    Value f = finalizers[i];
    // This object is dead, finalize it
    // The pointer itself in finalizers is not updated, but if the object is dead, its
    // heap-allocated type should now be reserved
    if(f.heap->get_type() == RESERVED) {
      f.heap = reinterpret_cast<HeapValue*>(f.heap->size);
      if(!finalize_all) {
        finalizers2.push_back(f);
        continue;
      }
    }
    // This object is dead, finalize it
    state.finalize((Type)f.heap->get_type(), f.heap, true);
  }
  ARETE_LOG_GC(finalizers2.size() << " finalizable objects survived collection");

  finalizers = finalizers2;
}

// Semispace collector
void GCSemispace::collect(size_t request, bool force) {
  collections++;

  GCTimer timer(gc_collect_timer);

  size_t new_heap_size = heap_size;
  size_t pressure = (live_memory_after_collection * 100) / heap_size;
  bool gc_grew = false;

  ARETE_LOG_GC("gc pressure " << pressure);
  // If we need to grow
  if((pressure >= ARETE_GC_LOAD_FACTOR) || force) {
    gc_grew = true;
    new_heap_size *= 2;
    if(new_heap_size <= request) {
      new_heap_size = (heap_size * 2) + request;
    }
  }

  heap_size = new_heap_size = align(ARETE_BLOCK_SIZE, new_heap_size);

  if(other == 0 || gc_grew) {
    // If we need to grow the heap, delete the existing semispace.
    if(other != 0) {
      ARETE_LOG_GC("deleting existing space");
      delete other;
      other = 0;
    }
    ARETE_LOG_GC("allocating new space of " << new_heap_size << "b");
    other = new Block(new_heap_size, 0);
  }

  other_cursor = other->data;

  copy_roots();

  char* sweep = other->data;

  // AR_ASSERT(sweep != other_cursor);
  while(sweep != other_cursor) {
    HeapValue* obj = (HeapValue*) sweep;
    size_t size = obj->size;

    switch(obj->get_type()) {
#define AR_COPY(type, field) copy((HeapValue**) &(((type*)obj)->field))
      // No pointers
      case FLONUM: case CHARACTER: case STRING: case BLOB: break;
      // One pointer
      case UPVALUE:
        if(!obj->get_header_bit(Value::UPVALUE_CLOSED_BIT)) {
          // There is no need to do anything here as the local will be updated by copy_roots
          break;
        } else {
          AR_COPY(Upvalue, U.converted);
          break;
        }
      case VECTOR:
      case TABLE:
      case FILE_PORT:
        AR_COPY(Vector, storage);
        break;
      // Two pointers 
      case CFUNCTION:
      case CLOSURE:
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
      // Four pointers
      case VMFUNCTION:
        AR_COPY(VMFunction, name);
        AR_COPY(VMFunction, constants);
        AR_COPY(VMFunction, free_variables);
        AR_COPY(VMFunction, sources);
        AR_COPY(VMFunction, macro_env);
        break;
      case RECORD_TYPE:
        AR_COPY(RecordType, apply);
        AR_COPY(RecordType, print);
        AR_COPY(RecordType, name);
        AR_COPY(RecordType, parent);
        AR_COPY(RecordType, field_names);
        break;
      // Five pointers
      case FUNCTION:
        AR_COPY(Function, name);
        AR_COPY(Function, parent_env);
        AR_COPY(Function, arguments);
        AR_COPY(Function, rest_arguments);
        AR_COPY(Function, body);
        break;
      // Variable ptrs / more complex collection required
      case RECORD: {
        RecordType rt(*static_cast<Record*>(obj)->type);

        AR_COPY(Record, type);
        size_t fc = rt.field_count;

        for(size_t i = 0; i != fc; i++) {
          AR_COPY(Record, fields[i]);
        }
        break;
      }
      case VECTOR_STORAGE: {
        size_t length = static_cast<VectorStorage*>(obj)->length;
        for(size_t i = 0; i != length; i++) {
          copy((HeapValue**)&static_cast<VectorStorage*>(obj)->data[i].bits);
        }
        break;
      }
      // Should never be encountered on heap
      case BLOCK: case CONSTANT: case FIXNUM: default:
        std::cerr << "mystery object type " << obj->get_type() << std::endl;
        std::cerr << "mystery object size: " << obj->size << std::endl; 
        std::cerr << obj << std::endl; 
        AR_ASSERT(!"arete:gc: encountered bad value on heap; probably a GC bug");
        break;
#undef AR_COPY
    }

    sweep += size;
  }

  // All done
  block_cursor = other_cursor - other->data;
  live_memory_after_collection = (block_cursor);

  run_finalizers(false);

  Block* swap = active;
  active = other;
  other = swap;

  // TODO: Currently, Arete holds onto both semispaces during program execution; it's not clear
  // whether this is the best course of action in terms of performance and memory usage.

  // It means Arete is always holding 2x working memory, but ensures we aren't allocating and
  // receiving different x megabyte address spaces from the system allocator for every collection

  // if(true) here can force deletion after every collect.
  // This could become an option in the future.

  if(gc_grew) {
    delete other; 
    other = 0;
  }
}

struct SemispaceRootVisitor {
  GCSemispace& gc;

  SemispaceRootVisitor(GCSemispace& gc_): gc(gc_) {}

  void touch(HeapValue*& value) {
    gc.copy(&value);
  }

  void touch(HeapValue** value) {
    gc.copy(value);
  }
};

void GCSemispace::copy_roots() {
  ARETE_LOG_GC(state.symbol_table->size() << " live symbols");
  SemispaceRootVisitor visitor(*this);

  visit_roots(visitor);
}

//
///// INCREMENTAL COLLECTOR
//

GCIncremental::GCIncremental(State& state_, size_t heap_size):
    GCCommon(state_, heap_size),
    mark_bit(1), block_i(0), block_cursor(0) {
  Block *b = new Block(heap_size, mark_bit);
  blocks.push_back(b);

  // Blocks should be allocated dead
  AR_ASSERT(!marked((HeapValue*) b->data));
}

GCIncremental::~GCIncremental() {
  for(size_t i = 0; i != blocks.size(); i++) {
    delete blocks[i];
  }
}

void GCIncremental::mark(HeapValue* v) {
  // We use a GOTO here to avoid creating unnecessary stack frames
  again: 
  // If there is no object or object has already been marked
  if(v == 0 || Value::immediatep(v) || marked(v))
    return;
  
  live_objects_after_collection++;
  live_memory_after_collection += v->size;

  v->flip_mark_bit();

  AR_ASSERT(marked(v));

  switch(v->get_type()) {
    case FLONUM: case STRING: case CHARACTER: case BLOB:
      break;
    // One pointer
    case UPVALUE:
      if(!v->get_header_bit(Value::UPVALUE_CLOSED_BIT)) {
        // There is no need to do anything here as the local will be updated by copy_roots
        break;
      } else {
        v = static_cast<Upvalue*>(v)->U.converted.heap;
        goto again;
      }
    // One pointer
    case VECTOR:
    case TABLE:
      v = static_cast<CFunction*>(v)->name.heap;
      goto again;
    // Two pointers
    case CFUNCTION:
    case SYMBOL:
    case CLOSURE:
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
    // Four pointers
    case VMFUNCTION:
    case RECORD_TYPE:
      mark(static_cast<RecordType*>(v)->apply.heap);
      mark(static_cast<RecordType*>(v)->print.heap);
      mark(static_cast<RecordType*>(v)->name.heap);
      v = static_cast<RecordType*>(v)->parent.heap;
      break;
    // Five pointers
    case FUNCTION:
      mark(static_cast<Function*>(v)->name.heap);
      mark(static_cast<Function*>(v)->parent_env.heap);
      mark(static_cast<Function*>(v)->arguments.heap);
      mark(static_cast<Function*>(v)->rest_arguments.heap);
      v = static_cast<Function*>(v)->body.heap;
      goto again;
    // Variable pointers
    case RECORD: {
      RecordType* rt = static_cast<RecordType*>(static_cast<Record*>(v)->type);
      for(unsigned i = 0; i != rt->field_count; i++) {
        mark(static_cast<Record*>(v)->fields[i].heap);
      }
      v = rt;
      goto again;
    }
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

struct IncrementalRootVisitor {
  GCIncremental& gc;

  IncrementalRootVisitor(GCIncremental& gc_): gc(gc_) {}

  void touch(HeapValue* value) {
    gc.mark(value);
  }

  void touch(HeapValue** value) {
    gc.mark(*value);
  }
};

void GCIncremental::collect() {
  GCTimer timer(gc_collect_timer);
  // ARETE_LOG_GC("collecting");
  collections++;
  live_objects_after_collection = live_memory_after_collection = 0;
  block_i = block_cursor = 0;

  // TODO if called early this should go through marking everything

  // Reverse meaning of mark bit
  mark_bit = !mark_bit;

  // Mark all live objects
  IncrementalRootVisitor visitor(*this);
  visit_roots(visitor);

  ARETE_LOG_GC("found " << live_objects_after_collection << " live objects taking up " <<
    live_memory_after_collection << "b")

  // Allocate a new block if memory is getting a little overloaded
  double load_factor = (live_memory_after_collection * 100) / heap_size;

  ARETE_LOG_GC("load factor " << live_memory_after_collection << " " << live_objects_after_collection << " " << load_factor);
  AR_ASSERT(live_memory_after_collection <= heap_size);

  if(load_factor >= ARETE_GC_LOAD_FACTOR) {
    ARETE_LOG_GC(load_factor << "% of memory is still live after collection, adding a block of size " << block_size);
    grow_heap();
  }
}

void GCIncremental::grow_heap() {
  block_size *= 2;
  Block* b = new Block(block_size, mark_bit);
  AR_ASSERT(b->size == block_size);
  AR_ASSERT(!marked((HeapValue*)b->data));
  heap_size += block_size;
  blocks.push_back(b);
}

HeapValue* GCIncremental::allocate(Type type, size_t size) {
  GCTimer timer(gc_alloc_timer);
  size_t sz = align(8, size);
  unsigned collected = 0;
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

      // When we locate a dead object, coalesce it along with all other dead objects into a bigger chunk.
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
          //memset(v2, 0, dead_size);
        }
      }

      // This dead object has enough room to fit this allocation
      if((!marked(v) && v->size >= sz)) {
        size_t mem_size = v->size;

        // Success!
        char* memory = blocks[block_i]->data + block_cursor;

        // If there is enough room after this memory to handle another object, note down its
        // size and move on

        // Flonum is used here as the smallest useful object
        if(mem_size - sz >= sizeof(Flonum)) {
          // ARETE_LOG_GC("additional " << (mem_size - sz) << " bytes after object");
          HeapValue* next_object = ((HeapValue*) ((blocks[block_i]->data + block_cursor) + sz));
          next_object->initialize(BLOCK, !mark_bit, mem_size - sz);

          AR_ASSERT(!marked(next_object));
          AR_ASSERT(next_object->size >= sizeof(Flonum));

          block_cursor += sz;
        } else {
          // Otherwise, just use the entire thing
          sz = mem_size;
          block_cursor += sz;
        }

        // Zero out memory after the header, which will always be initialized by the
        // HeapValue::initialize call
        memset((char*)memory + (sizeof(HeapValue)), 0, sz - sizeof(HeapValue));
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

  if(collected == 0) {
    // No room, run a collection
    ARETE_LOG_GC("out of room, attempting collection for allocation of size " << size);
    collect();
    collected = true;
    goto retry;
  } else if(collected == 1) {
    ARETE_LOG_GC("collection failed to free up enough space for allocation of size " << size << " adding another block");
    grow_heap();
    collected++;
    goto retry;
  } else {
    // Collection has failed to create enough space. Give up.
    ARETE_LOG_GC("allocation of size " << size << " failed");
    AR_ASSERT(!"out of room; allocation failed");
    return 0;
  }
}

}
