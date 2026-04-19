

// TODO: Reduction of code duplication between collectors. It should be possible to share
// the code that visits the pointers. The marking collector is recursive and uses a GOTO to save
// stack space. The copying collector is not and does not.

// TODO: Finalizers should probably simply be disabled in production builds; this should be used
// for warnings only.

// They'll all be searched after every collection. Depending on how many persistent finalizable
// objects are in a program, this could be a source of slowdowns.

// TODO: Lazy sweep improvements

// One idea would be to do a little bit of work for each allocation - sweep over X bytes of memory,
// and do things like find holes that are sized well for common objects like pairs. This would
// reduce fragmentation as well as the worst case scenario of e.g. having to sweep over a bunch
// of large objects to find a small one. The sweep would still eventually have to go over all
// objects (to mark them), but this might improve things. It would, however, increase complexity
// quite a bit.

#define NOMINMAX // MSVC helpfully defines min and max as macros

#include <algorithm>
#include <chrono>
#include <fstream>

#include "arete.hpp"

#if AR_OS == AR_POSIX
# include <sys/mman.h>
#else
# include <Windows.h>
#endif

#define AR_LOG_GC(msg) AR_LOG((AR_LOG_TAG_GC), "gc", msg)

namespace arete {

#if AR_OS == AR_POSIX

static void* gc_allocate(size_t size, bool executable) {
  int prot = PROT_READ | PROT_WRITE;
  if(executable) prot |= PROT_EXEC;
  return mmap(NULL, size, prot, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

static void gc_free(void* ptr, size_t length) {
  munmap(ptr, length);
}

#else 

static void* gc_allocate(size_t size, bool executable) {
	void* mem = VirtualAlloc(0, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
	DWORD w;
  int prot = executable ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE;
	VirtualProtect(mem, size, PAGE_EXECUTE_READWRITE, &w);
	// TODO: FlushInstructionCache (?)
	return mem;
}

static void gc_free(void* ptr, size_t size) {
	(void) size;
	VirtualFree(ptr, size, MEM_DECOMMIT);
}

#endif

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
          (rtd->finalizer)(*this, 0, 0, (void*) object.bits);
        }
        needed_finalization_desc = rtd->name.string_data();
        object.record_set_finalized();
      }
      break;
    }
    case BYTEVECTOR: {
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

struct GCPauseTimer {
  GCPauseTimer(size_t& timer_, size_t& longest_):
    timer(timer_), longest(longest_),
    t1(std::chrono::high_resolution_clock::now()) {}

  ~GCPauseTimer() {
    size_t elapsed =
      (std::chrono::duration_cast<std::chrono::microseconds>((
        std::chrono::high_resolution_clock::now() - t1))).count();
    longest = std::max(elapsed, longest);
    timer += elapsed;
  }

  size_t& timer;
  size_t& longest;
  std::chrono::time_point<std::chrono::high_resolution_clock> t1;
};

PerfStats::PerfStats():
  apply_calls(0),
  interpreter_calls(0),
  vm_calls(0),
  current(PERF_IDLE),
  last_switch(std::chrono::steady_clock::now()) {
  for(int i = 0; i < PERF_N; i++) time_us[i] = 0;
}

void PerfStats::switch_to(PerfComponent c) {
  auto now = std::chrono::steady_clock::now();
  time_us[current] += (size_t)
    std::chrono::duration_cast<std::chrono::microseconds>(now - last_switch).count();
  last_switch = now;
  current = c;
}

void PerfStats::flush() {
  auto now = std::chrono::steady_clock::now();
  time_us[current] += (size_t)
    std::chrono::duration_cast<std::chrono::microseconds>(now - last_switch).count();
  last_switch = now;
}

Block::Block(size_t size_, bool executable): size(size_) {
  data = static_cast<char*>(gc_allocate(size, executable));
  ((HeapValue*) data)->initialize(BLOCK, size_);
}

Block::~Block() {
  gc_free(data, size);
}

static size_t wall_time_us(const std::chrono::steady_clock::time_point& start) {
  return (std::chrono::duration_cast<std::chrono::microseconds>(
    std::chrono::steady_clock::now() - start)).count();
}

static size_t native_code_bytes(const std::vector<Block*>& native_code) {
  size_t b = 0;
  for(size_t i = 0; i != native_code.size(); i++) b += native_code[i]->size;
  return b;
}

void State::print_gc_stats(std::ostream& os) {
  size_t wall_us = wall_time_us(gc.start_time);
  size_t in_use = gc.block_cursor;

  os << (gc.heap_size / 1024) << "kb heap size after " << gc.collections << " collections and "
     << gc.allocations << " allocations " << std::endl;

  os << (gc.collect_time_us / 1000) << "ms in collection" << std::endl;
  os << (in_use / 1024) << "kb in use" << std::endl;

  if(wall_us > 0) {
    os << "approximately " << ((double)(gc.collect_time_us) * 100) / (double)(wall_us)
       << "% of program time spent in GC" << std::endl;
  }
  os << "longest pause: " << (gc.longest_pause_us / 1000) << "ms" << std::endl;

  os << "stack: " << gc.vm_stack_used << "/" << gc.vm_stack_size << " slots" << std::endl;

  os << (native_code_bytes(native_code) / 1024) << "kb allocated for native code" << std::endl;
}

// Escape a string for inclusion in JSON output.
static void json_escape(std::ostream& os, const std::string& s) {
  os << '"';
  for(size_t i = 0; i != s.size(); i++) {
    unsigned char c = (unsigned char) s[i];
    switch(c) {
      case '"':  os << "\\\""; break;
      case '\\': os << "\\\\"; break;
      case '\b': os << "\\b"; break;
      case '\f': os << "\\f"; break;
      case '\n': os << "\\n"; break;
      case '\r': os << "\\r"; break;
      case '\t': os << "\\t"; break;
      default:
        if(c < 0x20) {
          char buf[8];
          snprintf(buf, sizeof(buf), "\\u%04x", c);
          os << buf;
        } else {
          os << (char) c;
        }
    }
  }
  os << '"';
}

void State::write_perf_report(std::ostream& os) {
  perf.flush();

  size_t wall_us = wall_time_us(gc.start_time);
  size_t in_use = gc.block_cursor;
  size_t native_bytes = native_code_bytes(native_code);
  double gc_pct = wall_us > 0
    ? ((double)(gc.collect_time_us) * 100) / (double)(wall_us)
    : 0.0;
  double interp_pct = wall_us > 0
    ? ((double)(perf.time_us[PERF_INTERP]) * 100) / (double)(wall_us)
    : 0.0;
  double vm_pct = wall_us > 0
    ? ((double)(perf.time_us[PERF_VM]) * 100) / (double)(wall_us)
    : 0.0;

  os << "{\n";
  os << "  \"arete_version\": ";
  json_escape(os, "0.0.1");
  os << ",\n";
  os << "  \"wall_time_us\": " << wall_us << ",\n";
  os << "  \"gc\": {\n";
  os << "    \"collections\": " << gc.collections << ",\n";
  os << "    \"allocations\": " << gc.allocations << ",\n";
  os << "    \"collect_time_us\": " << gc.collect_time_us << ",\n";
  os << "    \"longest_pause_us\": " << gc.longest_pause_us << ",\n";
  os << "    \"time_in_gc_pct\": " << gc_pct << ",\n";
  os << "    \"heap_size_bytes\": " << gc.heap_size << ",\n";
  os << "    \"in_use_bytes\": " << in_use << ",\n";
  os << "    \"live_bytes_after_last_collection\": " << gc.live_memory_after_collection << "\n";
  os << "  },\n";
  os << "  \"interpreter\": {\n";
  os << "    \"calls\": " << perf.interpreter_calls << ",\n";
  os << "    \"exclusive_time_us\": " << perf.time_us[PERF_INTERP] << ",\n";
  os << "    \"exclusive_time_pct\": " << interp_pct << "\n";
  os << "  },\n";
  os << "  \"vm\": {\n";
  os << "    \"calls\": " << perf.vm_calls << ",\n";
  os << "    \"exclusive_time_us\": " << perf.time_us[PERF_VM] << ",\n";
  os << "    \"exclusive_time_pct\": " << vm_pct << ",\n";
  os << "    \"stack_slots_used\": " << gc.vm_stack_used << ",\n";
  os << "    \"stack_slots_capacity\": " << gc.vm_stack_size << "\n";
  os << "  },\n";
  os << "  \"apply_calls\": " << perf.apply_calls << ",\n";
  os << "  \"idle_time_us\": " << perf.time_us[PERF_IDLE] << ",\n";
  os << "  \"native_code_bytes\": " << native_bytes << "\n";
  os << "}\n";
}

void State::write_perf_report() {
  if(perf_report_path.empty() || perf_report_path == "-") {
    write_perf_report(std::cerr);
    return;
  }

  std::ofstream out(perf_report_path.c_str());
  if(!out) {
    std::cerr << "arete: could not open performance report file " << perf_report_path
              << "; writing to stderr instead" << std::endl;
    write_perf_report(std::cerr);
    return;
  }
  write_perf_report(out);
}

GCCommon::GCCommon(State& state_, size_t heap_size_):
  state(state_),
  frames(0),
  native_frames(0),
  vm_stack(static_cast<Value*>(calloc(2048, sizeof(Value)))),
  vm_stack_size(2048), vm_stack_used(0),
  protect_argc(0), protect_argv(0),
  collect_before_every_allocation(false),
  allocations(0),
  collections(0), live_objects_after_collection(0), live_memory_after_collection(0),
  heap_size(heap_size_),
  block_size(heap_size_),
  collect_time_us(0),
  longest_pause_us(0),
  start_time(std::chrono::steady_clock::now()) {

}

GCCommon::~GCCommon() {
  free(vm_stack);
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

  // variables are program roots

  for(auto x = state.symbol_table->begin(); x != state.symbol_table->end(); x++) {
    Symbol* v = x->second;
    if(v->get_header_bit(Value::SYMBOL_ROOT_BIT)) {
      walker.touch((HeapValue**) &v);
    }
    state.symbol_table->at(x->first) = v;
  }

  NativeFrame* native = native_frames;
  while(native != nullptr) {
    #if 0

    if(AR_LOG_TAGS & AR_LOG_TAG_JIT) {
      std::cout << "native frame " << (ptrdiff_t) native << std::endl;
      std::cout << "native frame previous " << (ptrdiff_t) native->previous << " count: " << (ptrdiff_t)native->value_count << std::endl;
    }
    #endif
    for(size_t i = 0; i != native->value_count; i++) {
      size_t saved = native->values[i].bits;
      walker.touch((HeapValue**) &native->values[i].bits);
      #if 0
      if(AR_LOG_TAGS & AR_LOG_TAG_JIT) {
        std::cout << "native frame ptr ";
        if(native->values[i].immediatep()) {
          std::cout << "immediate";
        } else {
          std::cout << (Type)native->values[i].type_unsafe();
        }
        std::cout << ' ' << i << ": " << saved << " => " << (ptrdiff_t)native->values[i].bits << " stack address: " << (ptrdiff_t)(void*)&native->values[i] << std::endl;
      }
      #endif
    }
    native = native->previous;
  }

  for(size_t i = 0; i != protect_argc; i++) {
    walker.touch((HeapValue**) &protect_argv[i]);
  }

  for(size_t i = 0; i != vm_stack_used; i++) {
    walker.touch((HeapValue**) &vm_stack[i]);
  }
}

//
///// SEMISPACE COLLECTOR
//

// TODO: Remove GCCommon and stuff intended to make the GC generic.

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

  // This reference has already been updated
  if((char*)obj >= other->data && (char*)obj < (other->data + other->size)) {
    return;
  }

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

  // TODO: For some reason, allocating a vector on the stack here does not play well with
  // natively-compiled Scheme code. STL must do something weird to registers (?)

  finalizers2.clear();

  AR_LOG_GC("checking " << finalizers.size() << " finalizable objects");
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
  AR_LOG_GC(finalizers2.size() << " finalizable objects survived collection");

  finalizers = finalizers2;
}

// Semispace collector
void GCSemispace::collect(size_t request, bool force) {
  collections++;

  GCPauseTimer timer(collect_time_us, longest_pause_us);

  size_t new_heap_size = heap_size;
  size_t pressure = (live_memory_after_collection * 100) / heap_size;
  bool gc_grew = false;

  AR_LOG_GC("gc pressure " << pressure);
  // If we need to grow
  if((pressure >= AR_GC_LOAD_FACTOR) || force) {
    gc_grew = true;
    new_heap_size *= 2;
    if(new_heap_size <= request) {
      new_heap_size = (heap_size * 2) + request;
    }
  }

  heap_size = new_heap_size = align(AR_BLOCK_SIZE, new_heap_size);

  if(other == 0 || gc_grew) {
    // If we need to grow the heap, delete the existing semispace.
    if(other != 0) {
      AR_LOG_GC("deleting existing space");
      delete other;
      other = 0;
    }
    AR_LOG_GC("allocating new space of " << new_heap_size << "b");
    other = new Block(new_heap_size, 0);
  }

  other_cursor = other->data;

  copy_roots();

  char* sweep = other->data;

  while(sweep != other_cursor) {
    HeapValue* obj = (HeapValue*) sweep;
    size_t size = obj->size;

    switch(obj->get_type()) {
#define AR_COPY(type, field) copy((HeapValue**) &(((type*)obj)->field))
      // No pointers
      case FLONUM: case CHARACTER: case STRING: case BYTEVECTOR: break;
      // One pointer
      case BOX:
        if(!obj->get_header_bit(Value::BOX_CLOSED_BIT)) {
          // There is no need to do anything here as the local will be updated by copy_roots
          break;
        } else {
          AR_COPY(Box, U.converted);
          break;
        }
      case VECTOR:
      case TABLE:
      case FILE_PORT:
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
      // Four pointers
      case RECORD_TYPE:
        AR_COPY(RecordType, apply);
        AR_COPY(RecordType, print);
        AR_COPY(RecordType, name);
        AR_COPY(RecordType, parent);
        AR_COPY(RecordType, field_names);
        break;
      
      // In these values, pointers are offset by the procedure address
      case CFUNCTION:
        AR_COPY(CFunction, name);
        AR_COPY(CFunction, closure);
        break;
      case CLOSURE:
        AR_COPY(Closure, function);
        AR_COPY(Closure, captures);
        break;
      case VMFUNCTION:
        AR_COPY(VMFunction, name);
        AR_COPY(VMFunction, constants);
        AR_COPY(VMFunction, free_variables);
        AR_COPY(VMFunction, sources);
        AR_COPY(VMFunction, macro_env);
        AR_COPY(VMFunction, code);
        // AR_COPY(VMFunction, native_code);
        // Update pointer into offset of GC space
        if(obj->get_header_bit(Value::VMFUNCTION_NATIVE_BIT)) {
          //((Procedure*)obj)->procedure_addr = (c_closure_t)((VMFunction* )obj)->native_code->data;
        }
        break;
      case FUNCTION:
        AR_COPY(Function, name);
        AR_COPY(Function, parent_env);
        AR_COPY(Function, arguments);
        AR_COPY(Function, rest_arguments);
        AR_COPY(Function, body);
        AR_COPY(Function, macro_env);
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

  // Update symbol table after collection
  size_t symbols_collected = 0;

  for(auto x = state.symbol_table->begin(); x != state.symbol_table->end();) {
    Symbol* v = x->second;
    // Value was collected, thus this needs to be updated
    if(v->header == RESERVED) {
      x->second = (Symbol*) v->size;
      //state.symbol_table->at(x->first) = (Symbol*) v->size;
      ++x;
    } else if(v->get_header_bit(Value::SYMBOL_ROOT_BIT)) {
      ++x;
    } else {
      //std::cout << "collting symbol " << Value(x->second).as_unsafe<Symbol>()->name.as_unsafe<String>()->data << std::endl;
      //++symbols_collected;
      state.symbol_table->erase(x++);
    }
  }

  AR_LOG_GC("collects " << symbols_collected << " symbols");

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
  AR_LOG_GC(state.symbol_table->size() << " live symbols");
  SemispaceRootVisitor visitor(*this);

  visit_roots(visitor);
}

}
