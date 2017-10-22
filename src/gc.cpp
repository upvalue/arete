// gc.cpp - Garbage collection

#include "arete.hpp"

#define ARETE_LOG_GC(msg) ARETE_LOG((ARETE_LOG_TAG_GC), "gc", msg)

namespace arete {

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
  
  // We use the object's size field to store the forward pointer
  obj->size = (size_t) cpy;
  obj->header = RESERVED;

  (*ref) = cpy;
}

// Semispace collector
void GCSemispace::collect(size_t request, bool force) {
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
      case UPVALUE:
        AR_COPY(Vector, storage);
        break;
      // Two pointers 
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
      case VMFUNCTION: {
        AR_COPY(VMFunction, name);
        AR_COPY(VMFunction, constants);
        break;
      }
      case RECORD: {
        RecordType rt(*static_cast<Record*>(obj)->type);

        AR_COPY(Record, type);

        for(size_t i = 0; i != rt.field_count; i++) {
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
        std::cout << obj << std::endl; 
        std::cout << "mystery object size: " << obj->size << std::endl; 
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

void GCSemispace::copy_roots() {
  // std::cout << state.symbol_table.size() << " live symbols" << std::endl;
  for(size_t i = 0; i != frames.size(); i++) {
    Frame* f = frames[i];
    for(size_t j = 0; j != f->size; j++) {
      copy(f->values[j]);
    }
  }

  VMFrame* link = vm_frames;
  while(link != 0) {
    VMFunction* fn = link->fn;

    size_t stack_i = link->stack_i;
    unsigned local_count = fn->local_count;

    copy((HeapValue**) &link->fn);

    for(size_t i = 0; i != stack_i; i++) {
      copy((HeapValue**) &link->stack[i]);
    }

    for(unsigned i = 0 ; i != local_count; i++) {
      copy((HeapValue**) &link->locals[i]);
    }
    link = link->previous;
  }

  for(size_t i = 0; i != state.globals.size(); i++) {
    copy(&state.globals[i].heap);
  }

  for(std::list<Handle*>::iterator i = handles.begin(); i != handles.end(); i++) {
    copy(&((*i)->ref.heap));
  }

  ARETE_LOG_GC(state.symbol_table->size() << " live symbols");
  // TODO: To make this a weak table, simply check for RESERVED in this. If forwarded, set it up
  // otherwise delete reference
  for(auto x = state.symbol_table->begin(); x != state.symbol_table->end(); x++) {
    HeapValue* v = x->second;
    copy(&v);
    state.symbol_table->at(x->first) = (Symbol*) v;
  }
}

///// INCREMENTAL
void GCIncremental::mark(HeapValue* v) {
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
    // Four pointers
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

void GCIncremental::mark_symbol_table() {
  ARETE_LOG_GC(state.symbol_table->size() << " live symbols");
  for(auto x = state.symbol_table->begin(); x != state.symbol_table->end(); x++) {
    mark(x->second);
  }
}

void GCIncremental::collect() {
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

HeapValue* GCIncremental::allocate(Type type, size_t size) {
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

}