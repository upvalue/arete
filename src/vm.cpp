// vm.cpp - Virtual machine

// TODO Disassembler

// TODO: Style. It's somewhat awkward and difficult to manipulate the stack by arithmetic
// e.g. after a function application there should probably be a simple way to save a position
// on the stack and refer to it later.

// TODO: Memory usage could be reduced by actual bytecode, but this would increase complexity

#include "arete.hpp"

// TODO: Non C-stack improvement. There's no need to malloc every function call. Rather we should
// use more intelligent allocation. The problem is that realloc'ing on the fly is rather complex as
// the existing pointers need to be updated; I tried this approach and couldn't get it to work.

// TODO: Are there more optimizations that can be done? With a little trouble, for example, we could
// use variables outside the VMFrame struct. Does putting everything in VMFrame prevent the compiler
// from storing them in registers?

// Another approach might be to just chain together malloc'd blocks

#ifdef _MSC_VER
# define AR_USE_C_STACK 0
# define AR_COMPUTED_GOTO 0
#endif

// If true, will allocate Scheme stack values, local variables, and upvalues on the C stack.
// Improves performance, but relies on non-standard C++ dynamically sized stack arrays.
// May also cause issues with multithreaded programs.

#ifndef AR_USE_C_STACK
# define AR_USE_C_STACK 1
#endif

// If true, will use "computed goto" instead of a normal switch statement
#ifndef AR_COMPUTED_GOTO
# define AR_COMPUTED_GOTO 1
#endif

#if AR_COMPUTED_GOTO
# define VM_CASE(label) LABEL_##label 
# define VM_DISPATCH() goto *dispatch_table[f.code[code_offset++]]
# define VM_SWITCH()
#else 
# define VM_CASE(label) case label 
# define VM_DISPATCH() break ;
# define VM_SWITCH() switch(f.code[code_offset++])
#endif

#define VM_EXCEPTION(type, msg) \
  { std::ostringstream __os; __os << msg ; f.exception = make_exception(type, __os.str()); \
    goto exception;}

//#define VM_CODE() (assert(gc.live((HeapValue*)f.code)), f.code)
#define VM_CODE() (f.code)

#define AR_VM_LOG_ALWAYS true

#define AR_LOG_VM(msg) \
  if(((ARETE_LOG_TAGS & ARETE_LOG_TAG_VM) && f.fn->get_header_bit(Value::VMFUNCTION_LOG_BIT))) { \
    ARETE_LOG((ARETE_LOG_TAG_VM), "vm", depth_to_string(f) << msg); \
  }
//#define AR_LOG_VM(msg)

#if 0
#define AR_PRINT_STACK() \
  std::cerr << "stack " << (f.stack_i) << " out of " << f.fn->stack_max << " allocated " << std::endl; \
  for(size_t i = 0; i != f.stack_i; i++) std::cerr << i << ": " << f.stack[i] << std::endl;
#endif
#define AR_PRINT_STACK()


namespace arete {

/**
 * Like SourceLocation, but includes a code offset
 */
struct VMSourceLocation {
  size_t code, source, line, begin, length;
};

std::ostream& operator<<(std::ostream& os, const VMSourceLocation& loc) {
  os << "#<VMSourceLocation code-position: " << loc.code << ' ' <<
    "source: " << loc.source << " line: " << loc.line << " begin: " << loc.begin <<
    " length: " << loc.length << '>';
  return os;
}

static std::string depth_to_string(const VMFrame& f) {
  std::string str;
  size_t d = f.depth;
  while(d--) {
    str += '>';
  }
  return str;
}

VMFrame::VMFrame(State& state_): state(state_), closure(0), 
    stack(0), locals(0), upvalues(0), exception(C_FALSE),stack_i(0),
    depth(0), destroyed(false) {
  previous = state.gc.vm_frames;
  if(previous) {
    depth = previous->depth+1;
  } else {
    state.tco_enabled = state.get_global_value(State::G_TCO_ENABLED).boolean_value();
  }
  state.gc.vm_frames = this;
  AR_ASSERT(state.gc.vm_frames == this);
}

VMFrame::~VMFrame() {
  //std::cerr << "~VMFrame " << depth << std::endl;
  AR_ASSERT(destroyed);
  if(!destroyed) close_over();
  // Pop frame
  AR_ASSERT(state.gc.vm_frames == this);
  state.gc.vm_frames = previous;
}

void VMFrame::setup(Value to_apply) {
  destroyed = false;

  if(to_apply.type_unsafe() == CLOSURE) {
    closure = to_apply.as_unsafe<Closure>();
    fn = closure->function.as_unsafe<VMFunction>();
  } else {
    closure = 0;
    fn = to_apply.as_unsafe<VMFunction>();
  }

#if !AR_USE_C_STACK
  size_t upvalue_count = fn->free_variables ? fn->free_variables->length : 0;
  size_t alloc_size = (fn->stack_max + fn->local_count + upvalue_count);
  stack = (Value*) malloc(alloc_size * sizeof(void*));
  locals = (Value*)  (((char*) stack) + (fn->stack_max * sizeof(void*)));
  if(upvalue_count)
    upvalues = (Value*) (((char*) locals) + (fn->local_count * sizeof(void*)));
#if 0
  std::cerr << "allocated " << (alloc_size * sizeof(Value)) << "b stack space" <<std::endl;

  std::cout << "stack " << fn->stack_max << " @ " << (size_t) stack << std::endl;

  std::cout << "locals " << fn->local_count << " @ stack + " << (fn->stack_max * sizeof(void*)) << ' ' << (size_t) locals << std::endl;
  if(upvalue_count) {
    std::cout << "upvals " << upvalue_count << " @ stack + " << ((fn->stack_max + fn->local_count) * sizeof(void*)) << ' ' << (size_t) upvalues << std::endl;
  }
#endif
#endif
}

// Due to something related to the way clang destroys stack-allocated variably sized
// arrays (which we use to store the stack, locals, and free variables), we have to
// manually destroy VMFrames ahead of their actual ~VMFrame call rather than
// relying on RAII.

// This isn't necessary with GNU c.

void VMFrame::close_over() {
  AR_ASSERT(!destroyed);
  // Close over upvalues
  if(fn->free_variables) {
    // AR_LOG_VM("closing over " << fn->free_variables->length << " free variables");
    AR_ASSERT(upvalues);
    for(size_t i = 0; i != fn->free_variables->length; i++) {
      //std::cout << "Closing over free variable " << fn->free_variables
      //std::cout << "Closing over free variable " << i << " value of " << upvalues[i].upvalue() << std::endl;
      Value saved_local = upvalues[i].upvalue();
      (void) saved_local;
      AR_ASSERT(state.gc.live(saved_local));
      upvalues[i].upvalue_close();
      AR_ASSERT(upvalues[i].upvalue_closed());
      AR_ASSERT(upvalues[i].upvalue() == saved_local);
    }
  }

#if !AR_USE_C_STACK
  free(stack);
  //state.vm_stack_i = vm_stack_begin;
  // std::cerr << "restoring state.vm_stack_i to " << vm_stack_begin << std::endl;
#endif
  fn = 0;
  closure = 0;
  upvalues = 0;
  stack = 0;
  locals = 0;
  stack_i = 0;
  destroyed = true;
}

// Note: The value of these instructions must be reflected in the computed-goto array below
// and in the compiler
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
  OP_JUMP_IF_FALSE = 15,
  OP_JUMP_IF_TRUE = 16,

  // Instructions below this point are primitive versions of the builtin C++ routines for speed;
  // they are not necessary for code to execute correctly.
  OP_ADD = 17,
  OP_SUB = 18,
  OP_LT = 19,
  OP_CAR = 20,
  OP_LIST_REF = 21,
  OP_EQ = 22,
};

Value State::apply_vm(Value fn, size_t argc, Value* argv) {
#if AR_COMPUTED_GOTO
  static void* dispatch_table[] = {
    &&LABEL_OP_BAD, &&LABEL_OP_PUSH_CONSTANT, &&LABEL_OP_PUSH_IMMEDIATE, &&LABEL_OP_POP,
    
    &&LABEL_OP_GLOBAL_GET, 
    &&LABEL_OP_GLOBAL_SET, &&LABEL_OP_LOCAL_GET, &&LABEL_OP_LOCAL_SET, &&LABEL_OP_UPVALUE_GET,
    &&LABEL_OP_UPVALUE_SET,
    
    &&LABEL_OP_CLOSE_OVER, &&LABEL_OP_APPLY, &&LABEL_OP_APPLY_TAIL,

    &&LABEL_OP_RETURN, &&LABEL_OP_JUMP, &&LABEL_OP_JUMP_IF_FALSE, &&LABEL_OP_JUMP_IF_TRUE,

    // Primitives implemented directly in the VM
    &&LABEL_OP_ADD, &&LABEL_OP_SUB,
    &&LABEL_OP_LT,
    &&LABEL_OP_CAR,
    &&LABEL_OP_LIST_REF,
    &&LABEL_OP_EQ,
  };
#endif
  // Frames lost due to tail call optimization
  size_t frames_lost = 0;
  VMFrame f(*this);
  f.setup(fn);
 tail:

  AR_ASSERT(gc.vm_frames == &f);

  AR_LOG_VM("ENTERING FUNCTION " << f.fn->name << " closure: " << (f.closure == 0 ? "#f" : "#t")
     << " free_variables: " << f.fn->free_variables);

  // Allocate function's required storage
#if AR_USE_C_STACK
  AR_ASSERT(gc.live(f.fn->name));
  AR_ASSERT(gc.live(f.fn->constants));
  AR_ASSERT(gc.live(f.fn->macro_env));
  if(f.fn->sources) {
    AR_ASSERT(gc.live(f.fn->sources));
  }
  if(f.fn->free_variables) {
    AR_ASSERT(gc.live(f.fn->free_variables));
  }
  size_t upvalue_count = f.fn->free_variables ? f.fn->free_variables->length : 0;     

  // Add storage to VM frame
  void* stack[f.fn->stack_max];
  void* locals[f.fn->local_count];
  void* upvalues[upvalue_count];

  f.stack = (Value*) stack;
  f.locals = (Value*) locals;
  f.upvalues = upvalue_count ? (Value*) upvalues : 0;
#else
#if 0
  size_t alloc_size = f.fn->stack_max + f.fn->local_count + upvalue_count;
  AR_LOG_VM("VM: growing heap-allocated stack at " << vm_stack_i << " by " << alloc_size);

  // This is quite tricky, 

  // we can't realloc easily in the middle of VM execution because pointers have already been saved
  // on the stack at each function call. We have to update them all in place. But even that isn't
  // enough, because we copy locals from the existing stack, so we have to leave an unused margin at
  // the end of the stack, allocate and do that first, then realloc.

  f.vm_stack_begin = vm_stack_i;
  vm_stack_i += alloc_size;

  void* stack = ((char*) vm_stack) + (f.vm_stack_begin * sizeof(void*));
  void* locals = (((char*) vm_stack) + ((f.vm_stack_begin + f.fn->stack_max) * sizeof(void*)));
  void* upvalues = (((char*) vm_stack) +
    + ((f.vm_stack_begin + f.fn->stack_max + f.fn->local_count) * sizeof(void*)));

  // Initialize local variables
  memcpy(locals, argv, argc * sizeof(Value));

  for(unsigned i = argc; i < f.fn->local_count; i++) { 
    ((Value*)locals)[i] = C_UNSPECIFIED;
  }

  if(vm_stack_i + alloc_size > (vm_stack_size - 256)) {
    AR_ASSERT(vm_stack_i < vm_stack_size);
    AR_LOG_VM("VM: reallocating heap-allocated stack to " << vm_stack_size * 2);
    std::cerr << "REALLOCATING HEAP-ALLOCATED STACK TO " << vm_stack_size * 2 << std::endl;
    vm_stack_size *= 2;
    AR_ASSERT(vm_stack_i + alloc_size < vm_stack_size);
    vm_stack = (void**)realloc(vm_stack, vm_stack_size * sizeof(void*));

    for(VMFrame* fi = &f; fi != 0; fi = fi->previous) {
      AR_ASSERT(!fi->destroyed);
      // Update all pointers
      fi->stack = (Value*)(((char*) vm_stack) + (fi->vm_stack_begin * sizeof(void*)));
      fi->locals = (Value*)(((char*) vm_stack) + ((fi->vm_stack_begin + fi->fn->stack_max) * sizeof(void*)));
      if(fi->upvalues) {
        fi->upvalues = (Value*)(((char*) vm_stack) +
          ((fi->vm_stack_begin + fi->fn->stack_max + fi->fn->local_count) * sizeof(void*)));
      }
    }
  } else {
    f.stack = (Value*) stack;
    f.locals = (Value*) locals;
    f.upvalues = upvalue_count ? (Value*) upvalues : 0;
  }

    /*
  std::cout << "stack " << f.fn->stack_max << " @ " << (size_t) stack << std::endl;

  std::cout << "locals " << f.fn->local_count << " @ stack + " << (f.fn->stack_max * sizeof(void*)) << ' ' << (size_t) locals << std::endl;
  if(upvalue_count) {
    std::cout << "upvals " << upvalue_count << " @ stack + " << ((f.fn->stack_max + f.fn->local_count) * sizeof(void*)) << ' ' << (size_t) upvalues;
  }
  */
#endif 
/*
  size_t alloc_size = (f.fn->stack_max + f.fn->local_count + upvalue_count);
  void* stack_storage = malloc(alloc_size * sizeof(void*));

  f.stack = (Value*) stack_storage;
  f.locals = (Value*)  ((char*) stack_storage) + (f.fn->stack_max * sizeof(void*));
  f.upvalues = (Value*) ((char*) f.locals) + (f.fn->local_count * sizeof(void*));
  */
#endif
  // Initialize local variables
  memcpy(f.locals, argv, argc * sizeof(Value));

  for(unsigned i = argc; i < f.fn->local_count; i++) { 
    f.locals[i] = C_UNSPECIFIED;
  }

  if(f.fn->free_variables != 0) {
    // Allocate upvalues as needed
    // TODO: Is GC allocating here dangerous? We can copy the blob locally as well.
    // If necessary
    AR_LOG_VM("Allocating space for " << f.fn->free_variables->length << " upvalues");

    memset(f.upvalues, 0, f.fn->free_variables->length * sizeof(Value));

    for(size_t i = 0; i != f.fn->free_variables->length; i++) {
      f.upvalues[i] = gc.allocate(UPVALUE, sizeof(Upvalue));
      size_t idx = ((size_t*) f.fn->free_variables->data)[i];
      AR_LOG_VM("tying free variable " << i << " to local idx " << idx);
      f.upvalues[i].as<Upvalue>()->U.local = &f.locals[idx];
    }
  }

  size_t code_offset = 0;

	f.code = (size_t*) f.fn->code_pointer();

  AR_ASSERT(gc.live((HeapValue*) f.code));

  while(true) {
#if AR_COMPUTED_GOTO
    VM_DISPATCH();
#endif
    
    VM_SWITCH()  {
      VM_CASE(OP_PUSH_CONSTANT): {
        size_t idx = VM_CODE()[code_offset++];
        f.stack[f.stack_i++] = f.fn->constants->data[idx];
        AR_PRINT_STACK();
        AR_LOG_VM("push-constant idx: " << idx << "; " << f.fn->constants->data[idx]);
        VM_DISPATCH();
      }

      VM_CASE(OP_POP): {
        AR_LOG_VM("pop");
        f.stack_i--;
        VM_DISPATCH();
      }

      VM_CASE(OP_PUSH_IMMEDIATE): {
        AR_LOG_VM("push-immediate " << Value(VM_CODE()[code_offset]));
        f.stack[f.stack_i++] = VM_CODE()[code_offset++];
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_GET): {
        size_t idx = VM_CODE()[code_offset++];
        Value sym = f.fn->constants->data[idx];
        AR_LOG_VM("global-get idx: " << idx << " ;; " << f.fn->constants->data[idx]);
        // AR_ASSERT(sym.type() == SYMBOL && "global-get called against non-symbol");
        f.stack[f.stack_i++] = sym.symbol_value();
        if(f.stack[f.stack_i - 1] == C_UNDEFINED) {
          std::ostringstream os;
          os << "reference to undefined variable " << symbol_dequalify(sym);
          f.exception = eval_error(os.str());
          goto exception;
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_SET): {
        size_t err_on_undefined = VM_CODE()[code_offset++];
        size_t constant_id = VM_CODE()[code_offset++];
        AR_ASSERT(f.stack_i >= 1);

        //Value val = f.stack[f.stack_i - 2], key = f.stack[f.stack_i - 1];
        Value val = f.stack[f.stack_i - 1];
        Value key = f.fn->constants->data[constant_id];
        AR_ASSERT(key.type() == SYMBOL);
        AR_LOG_VM("global-set (" << (err_on_undefined ? "set!" : "define") << ") " << key << " = " << val);
        if(err_on_undefined) {
          if(key.symbol_value() == C_UNDEFINED) {
            std::ostringstream os;
            os << "attempt to set! undefined variable " << key;
            f.exception = eval_error(os.str());
            goto exception;
          }
        }
        if(key.symbol_immutable()) {
          std::ostringstream os;
          os << "attempt to set! immutable symbol " << key;
          f.exception = eval_error(os.str());
          goto exception;
        }
        f.stack_i -= 1;
        key.set_symbol_value(val);
        VM_DISPATCH();
      }

      VM_CASE(OP_LOCAL_GET): {
        size_t idx = VM_CODE()[code_offset++];
        AR_LOG_VM("local-get idx: " << idx << " = " << f.locals[idx]);
        f.stack[f.stack_i++] = f.locals[idx];
        VM_DISPATCH();
      }

      VM_CASE(OP_LOCAL_SET): {
        size_t idx = VM_CODE()[code_offset++];
        AR_ASSERT("stack underflow" && f.stack_i >= 1);
        Value val = f.stack[--f.stack_i];
        AR_LOG_VM("local-set idx: " << idx << " = " << val);
        f.locals[idx] = val;
        VM_DISPATCH();
      }

      VM_CASE(OP_UPVALUE_GET): {
        AR_ASSERT(f.closure);

        size_t idx = VM_CODE()[code_offset++];
        AR_LOG_VM("upvalue-get " << idx);
        AR_ASSERT(f.closure->upvalues->data[idx].type() == UPVALUE);
        AR_ASSERT(gc.live(f.closure->upvalues->data[idx]));
        AR_ASSERT(gc.live(f.closure->upvalues->data[idx].upvalue()));
        f.stack[f.stack_i++] = f.closure->upvalues->data[idx].upvalue();
        VM_DISPATCH();
      }

      VM_CASE(OP_UPVALUE_SET): {
        AR_ASSERT(f.closure);
        size_t idx = VM_CODE()[code_offset++];
        AR_ASSERT(f.stack_i >= 1);
        Value val = f.stack[--f.stack_i];
        Value upval = f.closure->upvalues->data[idx];
        upval.upvalue_set(val);
        AR_LOG_VM("upvalue-set " << idx << " = " << val);
        VM_DISPATCH();
      }
      // TODO: Tail calls of C code. Possible/necessary?
      VM_CASE(OP_APPLY):
      VM_CASE(OP_APPLY_TAIL): {
        size_t insn = VM_CODE()[code_offset-1];
        size_t fargc = VM_CODE()[code_offset++];
        Value afn = f.stack[f.stack_i - fargc - 1];
        AR_LOG_VM((insn == OP_APPLY ? "apply" : "apply-tail") << " " << f.stack_i);
        AR_ASSERT(gc.live(afn));
        AR_LOG_VM((insn == OP_APPLY ? "apply" : "apply-tail") << " fargc: " << fargc << " fn: " << afn);

        switch(afn.type()) {
          case CFUNCTION: {
            size_t min_arity = afn.as<CFunction>()->min_arity;
            size_t max_arity = afn.as<CFunction>()->max_arity;
            bool var_arity = afn.c_function_variable_arity();

            if(fargc < min_arity) {
              std::ostringstream os;
              os << "function " << afn << " expected at least " << min_arity << " arguments " <<
                "but only got " << fargc;
              f.exception = eval_error(os.str());
              goto exception;
            } else if(fargc > max_arity && !var_arity) {
              std::ostringstream os;
              os << "function " << afn << " expected at most " << max_arity << " arguments " <<
                "but got " << fargc;
              f.exception = eval_error(os.str());
              goto exception;
            } 

            // AR_PRINT_STACK();

            // Replace function on stack with its result
            f.stack[f.stack_i - fargc - 1] =
              f.stack[f.stack_i - fargc - 1].c_function_apply(*this, fargc, &f.stack[f.stack_i - fargc]);

            f.stack_i -= (fargc);
            
            if(f.stack[f.stack_i - 1].is_active_exception()) {
              f.exception = f.stack[f.stack_i - 1];
              goto exception;
            }

            break;
          }
          case VMFUNCTION:
          case CLOSURE: {
            // Check for a closure and extract function from it if necessary,
            // so we can inspect the actual function
            Value to_apply = afn;

            if(afn.type() == CLOSURE) {
              afn = afn.closure_function();
            }

            // Check argument arity
            size_t min_arity = afn.vm_function_min_arity(), max_arity = afn.vm_function_max_arity();
            bool var_arity = afn.vm_function_variable_arity();

            if(fargc < min_arity) {
              std::ostringstream os;
              os << "function " << afn << " expected at least " << min_arity << " arguments " <<
                "but only got " << fargc;

              f.exception = eval_error(os.str());
              goto exception;
            } else if(fargc > max_arity && !var_arity) {
              std::ostringstream os;
              os << "function " << afn << " expected at most " << max_arity << " arguments " <<
                "but got " << fargc;
              f.exception = eval_error(os.str());
              goto exception;
            } else if(var_arity) {
              Value rest = C_NIL;

              AR_FRAME(this, to_apply, rest);

              for(size_t i = 0; i != fargc - min_arity; i++) {
                rest = make_pair(f.stack[f.stack_i - i - 1], rest);
              }

              // Replace beginning of varargs with list
              f.stack[(f.stack_i - fargc - 1) + min_arity + 1] = rest;

              // Pop additional arguments off of stack, but leave mandatory arguments plus
              // new rest list on stack
              f.stack_i = (f.stack_i - fargc - 1) + min_arity + 2;

              fargc = min_arity + 1;
            }

            if(tco_enabled && insn == OP_APPLY_TAIL) {
              frames_lost++;

              temps.clear();
              temps.insert(temps.end(), &f.stack[f.stack_i - fargc], &f.stack[f.stack_i]);

              argv = &temps[0];

              argc = fargc;
              fn = to_apply;
              
              f.close_over();
              AR_ASSERT(f.destroyed);
              f.setup(fn);
              AR_ASSERT(!f.destroyed);

              goto tail;
            } else {
              // Replace function on stack with result of function
              AR_ASSERT(((ptrdiff_t) (f.stack_i - fargc - 1)) >= 0);
              f.stack[f.stack_i - fargc - 1] =
                apply_vm(to_apply, fargc, &f.stack[f.stack_i - fargc]);

              // Pop arguments
              f.stack_i -= (fargc);
              AR_ASSERT(f.stack_i > 0);

              // Finally, check for an exception
              if(f.stack[f.stack_i - 1].is_active_exception()) {
                f.exception = f.stack[f.stack_i - 1];
                goto exception;
              }
            }
            break;
          }
          case FUNCTION: {
            /*
            temps.clear();
            temps.push_back(C_NIL);

            for(size_t i = 0; i != fargc; i++) {
              temps[0] = make_pair(f.stack[f.stack_i - i - 1], temps[0]);
            }

            f.stack[f.stack_i - fargc - 1] =
              eval_apply_function(f.stack[f.stack_i - fargc - 1], temps[0]);
            */

            f.stack[f.stack_i - fargc - 1] =
              eval_apply_function(f.stack[f.stack_i - fargc - 1], fargc, &f.stack[f.stack_i - fargc]);

            f.stack_i -= (fargc);

            AR_ASSERT(f.stack_i > 0);

            if(f.stack[f.stack_i - 1].is_active_exception()) {
              f.exception = f.stack[f.stack_i - 1];
              goto exception;
            }

            break;

          }
          default:
            std::ostringstream os;
            AR_PRINT_STACK();
            os << "vm: attempt to apply non-applicable value " << afn;
            f.exception = eval_error(os.str());
            goto exception;
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_CLOSE_OVER): {
        size_t upvalues = VM_CODE()[code_offset++];
        AR_LOG_VM("close-over " << upvalues);

        AR_ASSERT(upvalues > 0);

        temps.clear();
        // NOTE: Using AR_FRAME here with COMPUTED_GOTO causes issues. Seems the frame is not
        // destroyed correctly on goto *

        // More investigation required.

        temps.push_back(make_vector_storage(upvalues));

        for(size_t i = 0; i != upvalues; i++) {
          size_t is_enclosed = VM_CODE()[code_offset++];
          size_t idx = VM_CODE()[code_offset++];

          if(is_enclosed) {
            AR_LOG_VM("enclosing free variable " << i << " from closure idx " << idx);
            AR_ASSERT(f.closure->upvalues->data[idx].type() == UPVALUE);
            vector_storage_append(temps[0], f.closure->upvalues->data[idx]);
          } else {
            //AR_LOG_VM("enclosing local variable " << f.fn->free_variables->blob_ref<size_t>(i)  << " as upvalue " << i << " from f.upvalues idx " << idx);
            //AR_ASSERT("local variable is live." && gc.live(f.locals[f.fn->free_variables->blob_ref<size_t>(i)]));

            /*
            AR_ASSERT(!f.fn->free_variables || idx < f.fn->free_variables->length);
            AR_ASSERT(gc.live(f.upvalues[idx]));
            AR_ASSERT(f.upvalues[idx].type() == UPVALUE);
            AR_ASSERT(!f.upvalues[idx].upvalue_closed());
            AR_ASSERT(f.upvalues[idx].upvalue().type() != UPVALUE);
            */
            // AR_ASSERT(f.upvalues[idx].as<Upvalue>()->local == &f.locals[f.fn->free_variables->blob_ref<size_t>(i)]);
            vector_storage_append(temps[0], f.upvalues[idx]);
          }

          AR_LOG_VM("upvalue " << i << " = " << temps[0].as<VectorStorage>()->data[i] << " " << temps[0].as<VectorStorage>()->data[i].upvalue())
        }
        temps.push_back(gc.allocate(CLOSURE, sizeof(Closure)));
        AR_ASSERT(gc.live(temps[0]));
        AR_ASSERT(gc.live(temps[1]));
        temps[1].as<Closure>()->upvalues = temps[0].as<VectorStorage>();
        temps[1].as<Closure>()->function = f.stack[f.stack_i-1];
        AR_ASSERT(gc.live(f.stack[f.stack_i - 1]));

        for(size_t i = 0; i != upvalues; i++) {
          AR_ASSERT(gc.live(temps[1].as<Closure>()->upvalues->data[i]));
        }


        f.stack[f.stack_i-1] = temps[1];
        VM_DISPATCH();
      }

      ///// FLOW CONTROL

      VM_CASE(OP_RETURN): {
        AR_LOG_VM("return");
        AR_PRINT_STACK();
        Value ret = f.stack_i == 0 ? C_UNSPECIFIED : f.stack[f.stack_i - 1];
        f.close_over();
        AR_ASSERT(f.destroyed);
        return ret;
        //return f.stack[f.stack_i - 1];
      }
      // Application logic.


      VM_CASE(OP_JUMP): {
        // Drop condition from stack
        code_offset = VM_CODE()[code_offset];
        AR_LOG_VM("jump " << code_offset);
        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_IF_FALSE): {
        size_t jmp_offset = VM_CODE()[code_offset++];
        Value val = f.stack[f.stack_i-1];
        size_t pop = VM_CODE()[code_offset++];

        if(val == C_FALSE) {
          AR_LOG_VM("jump-if-false jumping " << jmp_offset);
          code_offset = jmp_offset;
        } else {
          AR_LOG_VM("jump-if-false not jumping" << jmp_offset);
        }

        f.stack_i -= pop;

        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_IF_TRUE): {
        size_t jmp_offset = VM_CODE()[code_offset++];
        Value val = f.stack[f.stack_i-1];
        size_t pop = VM_CODE()[code_offset++];

        if(val == C_FALSE) {
          AR_LOG_VM("jump-if-true not jumping " << jmp_offset);
        } else {
          AR_LOG_VM("jump-if-true jumping " << jmp_offset);
          code_offset = jmp_offset;
        }

        if(pop) {
          f.stack_i--;
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_ADD): {
        size_t argc = VM_CODE()[code_offset++];
        AR_LOG_VM("add " << argc);
        ptrdiff_t fxresult = 0;
        Value num;
        size_t j = 0;
        // Fast path for fixnums
        for(j = 0; j != argc; j++) {
          num = f.stack[f.stack_i - argc + j];
          if(num.fixnump())  {
            fxresult += num.fixnum_value();
          } else {
            break;
          }
        }

        if(j == argc) {
          f.stack_i -= argc;
          f.stack_i += 1;
          f.stack[f.stack_i - 1] = Value::make_fixnum(fxresult);
          VM_DISPATCH();
        }

        double flresult = (double) fxresult;

        // Check flonum, check error
        for(; j != argc; j++) {
          num = f.stack[f.stack_i - argc + j];
          switch(num.type()) {
            case FLONUM:
              flresult += num.flonum_value(); continue;
            case FIXNUM:
              flresult += (double) num.fixnum_value(); continue;
            default:
              VM_EXCEPTION("type", "primitive + expected fixnums or flonums as arguments but got " << num.type());
              break;
          }
        }

        f.stack_i -= argc;
        f.stack_i += 1;
        f.stack[f.stack_i - 1] = make_flonum(flresult);
        VM_DISPATCH();
      }

      VM_CASE(OP_SUB): {
        size_t argc = VM_CODE()[code_offset++];
        AR_LOG_VM("sub " << argc);

        ptrdiff_t fxresult = 0;
        bool flonum = false;

        double flresult = 0.0;

        // Get initial argument
        switch(f.stack[f.stack_i - argc].type()) {
          case FIXNUM:
            if(argc == 1) {
              f.stack[f.stack_i - 1] = Value::make_fixnum(-f.stack[f.stack_i - 1].fixnum_value());
              f.stack_i -= argc - 1;
              VM_DISPATCH();
            }
            fxresult = f.stack[f.stack_i - argc].fixnum_value();
            flresult = (double) fxresult;
            break;
          case FLONUM:
            if(argc == 1) {
              f.stack[f.stack_i - 1] = make_flonum(-f.stack[f.stack_i - 1].flonum_value());
              f.stack_i -= argc - 1;
              VM_DISPATCH();
            }
            flresult = f.stack[f.stack_i - argc].flonum_value();
            flonum = true;
            break;
          default:
            VM_EXCEPTION("type", "primitive - expected fixnum or flonum as argument but got " << f.stack[f.stack_i-argc+1].type());
            break;
        }

        size_t i = 1;
        if(!flonum) {
          for(; i != argc; i++) {
            if(f.stack[f.stack_i - argc + i].fixnump()) {
              fxresult -= f.stack[f.stack_i - argc + i].fixnum_value();
            } else {
              break;
            }
          }
        }

        if(i == argc) {
          f.stack_i -= argc - 1;
          f.stack[f.stack_i - 1] = Value::make_fixnum(fxresult);
          VM_DISPATCH();
        }

        for(; i != argc; i++) {
          Value num = f.stack[f.stack_i - argc + i];
          switch(num.type()) {
            case FIXNUM:
              flresult -= (double) f.stack[f.stack_i - argc + i].fixnum_value();
              break;
            case FLONUM:
              flresult -= f.stack[f.stack_i - argc + i].flonum_value();
              break;
            default:
              VM_EXCEPTION("type", "primitive - expected fixnum or flonum as argument but got " << f.stack[f.stack_i-argc+i]);
          }
        }

        f.stack_i -= argc - 1;
        f.stack[f.stack_i - 1] = make_flonum(flresult);
        VM_DISPATCH();
      }

      VM_CASE(OP_LT): {
        size_t argc = VM_CODE()[code_offset++];
        AR_LOG_VM("< " << argc);
        Value last = f.stack[f.stack_i - 2];
        bool result = false;
        for(size_t j = 0; j != argc - 1; j++) {
          Value num = f.stack[f.stack_i - argc + j];
          AR_ASSERT(num.type() == FIXNUM);
          if(num.bits < last.bits)
            result = true;
        }
        f.stack_i -= argc;
        f.stack_i += 1;
        f.stack[f.stack_i - 1] = Value::make_boolean(result);
        VM_DISPATCH();
      }

      VM_CASE(OP_CAR): {
        if(!(f.stack[f.stack_i-1].heap_type_equals(PAIR))) {
          VM_EXCEPTION("type", "vm primitive car expected a pair as its argument but got " << f.stack[f.stack_i - 1].type());
        }
        f.stack[f.stack_i-1] = f.stack[f.stack_i-1].as_unsafe<Pair>()->data_car;
        VM_DISPATCH();
      }

      VM_CASE(OP_LIST_REF): {
        Value lst = f.stack[f.stack_i - 2], idx = f.stack[f.stack_i - 1];
        // std::cout << lst << std::endl;
        // std::cout << idx << std::endl;

        if(!idx.fixnump()) {
          VM_EXCEPTION("type", "vm primitive list-ref expected a fixnum as its second argument but got " << idx.type());
        }

        if(lst.type() != PAIR) {
          VM_EXCEPTION("type", "vm primitive list-ref expected a list as its first argument but got " << lst.type());
        }

        ptrdiff_t index = idx.fixnum_value(), i = 0;

        f.stack_i -= 1;

        if(index == 0) {
          f.stack[f.stack_i-1] = lst.car();
        } else {
          while(lst.heap_type_equals(PAIR)) {
            if(i == index) {
              f.stack[f.stack_i - 1] = lst.car();
              break;
            }

            lst = lst.cdr();

            i++;

            if(lst.type() != PAIR) {
              if(lst == C_NIL) {
                if(i == index) {
                  VM_EXCEPTION("type", "vm primitive list-ref asked to get element " << index << " in a list of length " << i);
                }
                break;
              }
              VM_EXCEPTION("type", "vm primitive list-ref ran into a dotted list");
            }
          }
        }
        
        if(i != index) {
          VM_EXCEPTION("type", "vm primitive list-ref asked to get element " << index << " in a list of length " << i);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_EQ): {
        f.stack[f.stack_i - 2] = Value::make_boolean(f.stack[f.stack_i - 2].bits == f.stack[f.stack_i - 1].bits);
        f.stack_i--;
        VM_DISPATCH();
      }

      VM_CASE(OP_BAD): {
#ifndef AR_COMPUTED_GOTO
      default:
#endif
        warn() << "encountered bad opcode: " << VM_CODE()[code_offset-1] << std::endl;
        AR_ASSERT(!"bad opcode");
        break;
      }
    }
  }

  std::cerr << "This should never be reached" << std::endl; 

  return f.stack[f.stack_i-1];

  exception:
    // Create VM tracebacks

    // We take an array of VMSourceLocations and cycle through them until we find the closest
    // one to the code position at which the exception occurred.

    Value sources(f.fn->sources);
    if(sources.type() == BLOB && sources.blob_length() > 0) {
      // Source code information is available
      size_t i = 0;
      VMSourceLocation vmloc = sources.blob_ref<VMSourceLocation>(0);
      for(i = 1; i < sources.blob_length() / 5; i++) {
        VMSourceLocation vmloc2 = sources.blob_ref<VMSourceLocation>(i);
        if(vmloc2.code >= code_offset) {
          break;
        }
        vmloc = vmloc2;
      }

      SourceLocation loc;
      loc.source = vmloc.source;
      loc.line = vmloc.line;
      loc.begin = vmloc.begin;
      loc.length = vmloc.length;

      std::ostringstream os;
      os << source_info(loc, f.fn->name);
      if(frames_lost > 0) {
        os << std::endl << "-- " << frames_lost << " frames lost due to tail call optimization";
      }
      stack_trace.push_back(os.str());
    }


    AR_ASSERT(!f.destroyed);
    Value exc = f.exception;
    f.close_over();
    AR_ASSERT(exc.is_active_exception());
    return exc;
}

void State::disassemble(std::ostream& os, Value fn) {
  VMFunction *vmf;
  if(fn.type() == CLOSURE) {
    vmf = fn.closure_function();
  } else {
    vmf = fn.as<VMFunction>();
  }

  os << "vmfunction " << vmf->name << std::endl;

  // size_t i = 0;

  while(true) {

  }
}

}
