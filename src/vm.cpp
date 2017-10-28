// vm.cpp - Virtual machine

// TODO Disassembler

// TODO Portable alternative to stack-allocated arrays.

// TODO: Style. It's somewhat awkward and difficult to manipulate the stack by arithmetic
// e.g. after a function application there should probably be a simple way to save a position
// on the stack and refer to it later.

#include "arete.hpp"

#define AR_COMPUTED_GOTO

#ifdef AR_COMPUTED_GOTO
# define VM_CASE(label) LABEL_##label 
# define VM_DISPATCH() goto *dispatch_table[f.code[code_offset++]]
# define VM_SWITCH()
#else 
# define VM_CASE(label) case label 
# define VM_DISPATCH() break ;
# define VM_SWITCH() switch(f.code[code_offset++])
#endif

//#define VM_CODE() (assert(gc.live((HeapValue*)f.code)), f.code)
#define VM_CODE() (f.code)

#define AR_LOG_VM(msg) \
  if(((ARETE_LOG_TAGS & ARETE_LOG_TAG_VM) && f.fn->get_header_bit(Value::VMFUNCTION_LOG_BIT))) { \
    ARETE_LOG((ARETE_LOG_TAG_VM), "vm", depth_to_string(f) << msg); \
  }
// #define AR_LOG_VM(msg)


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

VMFrame::VMFrame(State& state_): state(state_), closure(0), exception(C_FALSE), stack_i(0), depth(0) {
  previous = state.gc.vm_frames;
  if(previous) {
    depth = previous->depth+1;
  }
  state.gc.vm_frames = this;
  AR_ASSERT(state.gc.vm_frames == this);
}

VMFrame::~VMFrame() {
  destroy();
}

void VMFrame::destroy() {
  // Close over upvalues
  if(fn->free_variables) {
    // AR_LOG_VM("closing over " << fn->free_variables->length << " free variables");
    for(size_t i = 0; i != fn->free_variables->length; i++) {
      //std::cout << "Closing over free variable " << fn->free_variables
      //std::cout << "Closing over free variable " << i << " value of " << upvalues[i].upvalue() << std::endl;
      Value saved_local = upvalues[i].upvalue();
      upvalues[i].upvalue_close();
      AR_ASSERT(upvalues[i].upvalue_closed());
      AR_ASSERT(upvalues[i].upvalue() == saved_local);
    }
  }

  // Pop frame
  AR_ASSERT(state.gc.vm_frames == this);
  state.gc.vm_frames = previous;
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

  // Instructions below this point are "open-coded" versions of the builtin C++ routines for speed;
  // they are not necessary for the VM to function.
};

#define AR_PRINT_STACK() \
  AR_LOG_VM("stack " << (f.stack_i) << " out of " << f.fn->stack_max << " allocated "); \
  for(size_t i = 0; i != f.stack_i; i++) AR_LOG_VM(i << ": " << f.stack[i]);

Value State::apply_vm(Value fn, size_t argc, Value* argv) {
#ifdef AR_COMPUTED_GOTO
  static void* dispatch_table[] = {
    &&LABEL_OP_BAD, &&LABEL_OP_PUSH_CONSTANT, &&LABEL_OP_PUSH_IMMEDIATE, &&LABEL_OP_POP,
    
    &&LABEL_OP_GLOBAL_GET, 
    &&LABEL_OP_GLOBAL_SET, &&LABEL_OP_LOCAL_GET, &&LABEL_OP_LOCAL_SET, &&LABEL_OP_UPVALUE_GET,
    &&LABEL_OP_UPVALUE_SET,
    
    &&LABEL_OP_CLOSE_OVER, &&LABEL_OP_APPLY, &&LABEL_OP_APPLY_TAIL,

    &&LABEL_OP_RETURN, &&LABEL_OP_JUMP, &&LABEL_OP_JUMP_IF_FALSE, &&LABEL_OP_JUMP_IF_TRUE
    /*
    &&LABEL_OP_BAD, &&LABEL_OP_PUSH_CONSTANT, &&LABEL_OP_GLOBAL_GET, &&LABEL_OP_GLOBAL_SET
    , &&LABEL_OP_RETURN, &&LABEL_OP_APPLY, &&LABEL_OP_APPLY_TAIL, &&LABEL_OP_LOCAL_GET,
    &&LABEL_OP_LOCAL_SET, &&LABEL_OP_UPVALUE_GET, &&LABEL_OP_UPVALUE_SET, &&LABEL_OP_CLOSE_OVER,
   &&LABEL_OP_JUMP, &&LABEL_OP_JUMP_IF_FALSE, &&LABEL_OP_POP, &&LABEL_OP_PUSH_IMMEDIATE,
   &&LABEL_OP_JUMP_IF_TRUE,
   */
  };
#endif
  // Frames lost due to tail call optimization
  size_t frames_lost = 0;
 tail:

  VMFrame f(*this);

  AR_ASSERT(gc.vm_frames == &f);

  if(fn.type() == CLOSURE) {
    f.closure = fn.as<Closure>();
    f.fn = fn.closure_function();
  } else {
    f.fn = fn.as<VMFunction>();
  }

  AR_LOG_VM("ENTERING FUNCTION " << f.fn->name << " closure: " << (f.closure == 0 ? "#f" : "#t")
     << " free_variables: " << f.fn->free_variables);

  size_t upvalue_count = f.fn->free_variables ? f.fn->free_variables->length : 0;     

  void* stack[f.fn->stack_max];
  void* locals[f.fn->local_count];
  void* upvalues[upvalue_count];

  // Allocate storage
  f.stack = (Value*) stack;
  f.locals = (Value*) locals;

  // Initialize local variables
  memcpy(f.locals, argv, argc * sizeof(Value));

  for(unsigned i = argc; i < f.fn->local_count; i++) { 
    AR_LOG_VM("LOCAL " << i << " = unspecified");
    f.locals[i] = C_UNSPECIFIED;
  }

  f.upvalues = 0;

  if(f.fn->free_variables != 0) {
    // Allocate upvalues as needed
    // TODO: Is GC allocating here dangerous? We can copy the blob locally as well.
    // If necessary
    AR_LOG_VM("Allocating space for " << f.fn->free_variables->length << " upvalues");
    f.upvalues = (Value*) upvalues;
    // f.upvalues = (Value*) malloc(f.fn->free_variables->length * sizeof(Value));

    //memset(f.upvalues, 0, f.fn->free_variables->length * sizeof(Value));
    for(size_t i = 0; i != f.fn->free_variables->length; i++) {
      f.upvalues[i].bits = 0;
    }

    for(size_t i = 0; i != f.fn->free_variables->length; i++) {
      f.upvalues[i] = gc.allocate(UPVALUE, sizeof(Upvalue));
      size_t idx = ((size_t*) f.fn->free_variables->data)[i];
      AR_LOG_VM("tying free variable " << i << " to local idx " << idx);
      f.upvalues[i].as<Upvalue>()->local = &f.locals[idx];
    }
  }

  // TODO: This doesn't necessarily need to be initialized; we could
  // place the stack pointer in the VMFrame structure and only
  // collect what has been set

  // memset(f.locals, 0, f.fn->local_count * sizeof(Value));

  size_t code_offset = 0;

  f.code = (size_t*) ((char*) (f.fn) + sizeof(VMFunction));
  // gc.collect();
  AR_ASSERT(gc.live((HeapValue*) f.code));

  while(true) {
    //size_t* code = (size_t*) ((char*) (f.fn) + sizeof(VMFunction));
    //f.code = code;
    //size_t insn = f.code[code_offset++];
#ifdef AR_COMPUTED_GOTO
    VM_DISPATCH();

    // goto *dispatch_table[insn];
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
          os << "reference to undefined global " << sym;
          f.exception = eval_error(os.str());
          goto exception;
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_SET): {
        size_t err_on_undefined = VM_CODE()[code_offset++];
        AR_ASSERT(f.stack_i >= 2);

        Value val = f.stack[f.stack_i - 2], key = f.stack[f.stack_i - 1];
        AR_ASSERT(key.type() == SYMBOL);
        AR_LOG_VM("global-set (" << (err_on_undefined ? "set!" : "define") << ") " << key << " = " << val);
        if(err_on_undefined) {
          if(key.symbol_value() == C_UNDEFINED) {
            std::ostringstream os;
            os << "attempt to set! undefined variable " << key;
            return eval_error(os.str());
          }
        }
        if(key.symbol_immutable()) {
          std::ostringstream os;
          os << "attempt to set! immutable symbol " << key;
          return eval_error(os.str());
        }
        f.stack_i -= 2;
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
        AR_ASSERT(f.stack_i - argc - 1 >= 0);
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
                "but  " << fargc;
              f.exception = eval_error(os.str());
              goto exception;
            } 

            // AR_PRINT_STACK();

            // Replace function on stack with its result
            f.stack[f.stack_i - fargc - 1] =
              f.stack[f.stack_i - fargc - 1].c_function_addr()(*this, fargc, &f.stack[f.stack_i - fargc]);

            // AR_PRINT_STACK();

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
              temps.clear();
              // We have to save to_apply on temps because it might be collected during make_pair
              temps.push_back(to_apply);
              temps.push_back(C_NIL);

              for(size_t i = 0; i != fargc - min_arity; i++) {
                // Say we have something like
                // (define (a b . c) #t)
                // min_arity is 1
                // fargc is 2
                // so we want to grab the values at f._stack
                temps[1] = make_pair(f.stack[f.stack_i - i - 1], temps[1]);
                // std::cout << temps[1] << std::endl;
              }

              to_apply = f.stack[f.stack_i - fargc - 1];

              // Replace beginning of varargs with 
              f.stack[(f.stack_i - fargc - 1) + min_arity + 1] = temps[1];

              // Pop additional arguments off of stack, but leave mandatory arguments plus
              // new rest list on stack
              f.stack_i = (f.stack_i - fargc - 1) + min_arity + 2;

              fargc = min_arity + 1;
            }

            if(insn == OP_APPLY_TAIL) {
              frames_lost++;

              temps.clear();
              temps.insert(temps.end(), &f.stack[f.stack_i - fargc], &f.stack[f.stack_i]);

              argc = fargc;
              argv = &temps[0];
              fn = to_apply;

              goto tail;
            } else {
              // Replace function on stack with result of function
              AR_ASSERT(((ptrdiff_t) f.stack_i - fargc - 1) >= 0);
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
            temps.clear();
            temps.push_back(C_NIL);

            for(size_t i = 0; i != fargc; i++) {
              temps[0] = make_pair(f.stack[f.stack_i - i - 1], temps[0]);
            }

            // std::cout << "built args: " << temps[0] << std::endl;

            f.stack[f.stack_i - fargc - 1] =
              eval_apply_function(f.stack[f.stack_i - fargc - 1], temps[0]);
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
            os << "attempt to apply non-applicable value " << afn;
            return eval_error(os.str());
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

        temps.push_back(make_vector(upvalues));

        for(size_t i = 0; i != upvalues; i++) {
          size_t is_enclosed = VM_CODE()[code_offset++];
          size_t idx = VM_CODE()[code_offset++];

          if(is_enclosed) {
            AR_LOG_VM("enclosing free variable " << i << " from closure idx " << idx);
            AR_ASSERT(f.closure->upvalues->data[idx].type() == UPVALUE);
            vector_append(temps[0], f.closure->upvalues->data[idx]);
          } else {
            //AR_LOG_VM("enclosing local variable " << f.fn->free_variables->blob_ref<size_t>(i)  << " as upvalue " << i << " from f.upvalues idx " << idx);

            AR_ASSERT(!f.fn->free_variables || idx < f.fn->free_variables->length);
            AR_ASSERT(gc.live(f.upvalues[idx]));
            AR_ASSERT(f.upvalues[idx].type() == UPVALUE);
            AR_ASSERT(!f.upvalues[idx].upvalue_closed());
            AR_ASSERT(f.upvalues[idx].upvalue().type() != UPVALUE);
            // AR_ASSERT(f.upvalues[idx].as<Upvalue>()->local == &f.locals[f.fn->free_variables->blob_ref<size_t>(i)]);
            vector_append(temps[0], f.upvalues[idx]);
          }

          AR_LOG_VM("upvalue " << i << " = " << temps[0].vector_ref(i) << " " << temps[0].vector_ref(i).upvalue());
          AR_ASSERT(temps[0].vector_ref(i).type() == UPVALUE);
          AR_ASSERT(temps[0].vector_ref(i).upvalue().type() != UPVALUE);
        }
        temps.push_back(gc.allocate(CLOSURE, sizeof(Closure)));
        temps[1].as<Closure>()->upvalues = temps[0].vector_storage().as<VectorStorage>();
        temps[1].as<Closure>()->function = f.stack[f.stack_i-1];
        f.stack[f.stack_i-1] = temps[1];
        VM_DISPATCH();
      }

      ///// FLOW CONTROL

      VM_CASE(OP_RETURN): {
        AR_LOG_VM("return");
        AR_PRINT_STACK();
        if(f.stack_i == 0) return C_UNSPECIFIED;
        return f.stack[f.stack_i - 1];
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

        if(pop) {
          f.stack_i--;
        }

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

  return f.stack[f.stack_i];

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
        if(vmloc2.code > code_offset) {
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
        os << std::endl << "<" << frames_lost << " frames lost due to tail call optimization>";
      }
      stack_trace.push_back(os.str());
    }

    return f.exception;
}

}
