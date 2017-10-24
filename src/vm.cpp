// vm.cpp - Virtual machine

// TODO Computed goto

#include <alloca.h>

#include "arete.hpp"

#define AR_LOG_VM(msg) ARETE_LOG((ARETE_LOG_TAG_VM), "vm", depth_to_string(f) << msg)

// #define AR_LOG_VM_INSN(msg) ARETE_LOG(())

// A simple portable alternative to alloca would be just using a giant malloc'd or
// stack-allocated array.

// It could be free'd by VMFrame, or something

namespace arete {

static std::string depth_to_string(const VMFrame& f) {
  std::string str;
  size_t d = f.depth;
  while(d--) {
    str += '>';
  }
  return str;
}

VMFrame::VMFrame(State& state_): state(state_), stack_i(0), depth(0) {
  previous = state.gc.vm_frames;
  if(previous) {
    depth = previous->depth+1;
  }
  state.gc.vm_frames = this;
  AR_ASSERT(state.gc.vm_frames == this);
}

VMFrame::~VMFrame() {
  // Close over upvalues
  if(fn->free_variables) {
    // AR_LOG_VM("closing over " << fn->free_variables->length << " free variables");
    for(size_t i = 0; i != fn->free_variables->length; i++) {
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

enum {
  OP_BAD = 0,
  OP_PUSH_CONSTANT = 1,
  OP_GLOBAL_GET = 2,
  OP_GLOBAL_SET = 3,
  OP_RETURN = 4,
  OP_APPLY = 5,
  OP_APPLY_TAIL = 6,
  OP_LOCAL_GET = 7,
  OP_LOCAL_SET = 8,
  OP_UPVALUE_GET = 9,
  OP_UPVALUE_SET = 10,
  OP_CLOSE_OVER = 11,
  OP_JUMP = 12,
  OP_JUMP_IF_FALSE = 13,
  OP_POP = 14,
};

#define AR_PRINT_STACK() \
  AR_LOG_VM("stack " << (f.stack_i-1) << " out of " << f.fn->stack_max << " allocated "); \
  for(size_t i = 0; i != f.stack_i; i++) AR_LOG_VM(i << ": " << f.stack[i]);


Value State::apply_vm(Value fn, size_t argc, Value* argv) {
// tail:

  VMFrame f(*this);

  AR_ASSERT(gc.vm_frames == &f);

  if(fn.type() == CLOSURE) {
    f.closure = fn.as<Closure>();
    f.fn = fn.closure_function();
  } else {
    f.closure = 0;
    f.fn = fn.as<VMFunction>();
  }

  AR_LOG_VM("ENTERING FUNCTION " << f.fn->name << " closure: " << (f.closure == 0 ? "#f" : "#t")
     << " free_variables: " << f.fn->free_variables);
  
  // Allocate storage
  f.stack = (Value*) alloca(f.fn->stack_max * sizeof(Value));
  f.locals = (Value*) alloca(f.fn->local_count * sizeof(Value));

  // Initialize local variables
  for(unsigned i = 0; i != argc; i++) {
    AR_LOG_VM("LOCAL " << i << ' ' << argv[i]);
    f.locals[i] = argv[i];
  }

  for(unsigned i = argc; i != f.fn->local_count; i++) { 
    f.locals[i] = C_UNSPECIFIED;
  }

  if(f.fn->free_variables != 0) {
    // Allocate upvalues as needed
    // TODO: Is GC allocating here dangerous? We can copy the blob locally as well.
    // If necessary
    AR_LOG_VM("Allocating space for " << f.fn->free_variables->length << " upvalues");
    f.upvalues = (Value*) alloca(f.fn->free_variables->length);

    for(size_t i = 0; i != f.fn->free_variables->length; i++) {
      f.upvalues[i].bits = 0;
    }

    for(size_t i = 0; i != f.fn->free_variables->length; i++) {
      f.upvalues[i] = gc.allocate(UPVALUE, sizeof(Upvalue));
      size_t idx = ((size_t*) f.fn->free_variables->data)[i];
      f.upvalues[i].as<Upvalue>()->local = &f.locals[idx];
      AR_ASSERT(f.upvalues[i].type() == UPVALUE);
      AR_ASSERT(f.upvalues[i].upvalue() == f.locals[idx]);
    }
  }

  // TODO: This doesn't necessarily need to be initialized; we could
  // place the stack pointer in the VMFrame structure and only
  // collect what has been set

  // memset(f.locals, 0, f.fn->local_count * sizeof(Value));

  size_t code_offset = 0;
   
  gc.collect();

  // VM main loop
  while(true) {
    gc.collect();
    // We have to account for things moving.

    size_t* code = (size_t*) ((char*) (f.fn) + sizeof(VMFunction));
    // size_t* code = (fn2.vm_function_bytecode());
    size_t insn = code[code_offset++];


    switch(insn) {
      case OP_PUSH_CONSTANT: {
        size_t idx = code[code_offset++];
        f.stack[f.stack_i++] = f.fn->constants->data[idx];
        AR_PRINT_STACK();
        AR_LOG_VM("push-constant idx: " << idx << "; " << f.fn->constants->data[idx]);
        continue;
      }

      case OP_GLOBAL_GET: {
        size_t idx = code[code_offset++];
        Value sym = f.fn->constants->data[idx];
        AR_LOG_VM("global-get idx: " << idx << " ;; " << f.fn->constants->data[idx]);
        AR_ASSERT(sym.type() == SYMBOL && "global-get called against non-symbol");
        f.stack[f.stack_i++] = sym.symbol_value();
        continue;
      }

      case OP_GLOBAL_SET: {
        size_t err_on_undefined = code[code_offset++];
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
        f.stack_i -= 2;
        key.set_symbol_value(val);
        continue;
      }

      case OP_LOCAL_GET: {
        size_t idx = code[code_offset++];
        AR_LOG_VM("local-get idx: " << idx << " = " << f.locals[idx]);
        f.stack[f.stack_i++] = f.locals[idx];
        continue;
      }

      case OP_LOCAL_SET: {
        size_t idx = code[code_offset++];
        AR_ASSERT("stack underflow" && f.stack_i >= 1);
        Value val = f.stack[f.stack_i - 1];
        AR_LOG_VM("local-set idx: " << idx << " = " << val);
        f.locals[idx] = val;
        continue;
      }

      case OP_UPVALUE_GET: {
        AR_ASSERT(f.closure);

        size_t idx = code[code_offset++];
        AR_LOG_VM("upvalue-get " << idx);
        AR_ASSERT(f.closure->upvalues->data[idx].type() == UPVALUE);
        AR_ASSERT(gc.live(f.closure->upvalues->data[idx].upvalue()));
        f.stack[f.stack_i++] = f.closure->upvalues->data[idx].upvalue();
        continue;
      }

      case OP_UPVALUE_SET: {
        AR_ASSERT(f.closure);
        size_t idx = code[code_offset++];
        AR_ASSERT(f.stack_i >= 1);
        Value val = f.stack[--f.stack_i];
        Value upval = f.closure->upvalues->data[idx];
        upval.upvalue_set(val);
        AR_LOG_VM("upvalue-set " << idx << " = " << val);
      }

      case OP_RETURN: {
        AR_LOG_VM("return");
        AR_PRINT_STACK();
        if(f.stack_i == 0) return C_UNSPECIFIED;
        return f.stack[f.stack_i - 1];
      }

      // Application logic.

      // If C function, we pass it part of the stack (how to deal with tail calls? possible? 
      // necessary?)

      // If interpreted function, we build an arg-list out of the stack and apply it
      // This may not actually be necessary if bootstrapping only goes one direction
      // Should a VM function ever call an interpreted function?
      // Well, don't worry about that for now.
      
      // If VM function, we call apply_vm with necessary args, copy args to locals at the beginning
      // so they are collected properly, after which we don't access argv

      case OP_APPLY:
      case OP_APPLY_TAIL: {
        size_t argc = code[code_offset++];
        Value fn = f.stack[f.stack_i - argc - 1];
        AR_LOG_VM((insn == OP_APPLY ? "apply" : "apply-tail") << " argc: " << argc << " fn: " << fn);

        if(fn.type() == CFUNCTION) {
          size_t min_arity = fn.as<CFunction>()->min_arity;
          size_t max_arity = fn.as<CFunction>()->max_arity;
          bool var_arity = fn.c_function_variable_arity();

          if(argc < min_arity) {
            std::ostringstream os;
            os << "function " << fn << " expected at least " << min_arity << " arguments " <<
              "but only got " << argc;
            return eval_error(os.str());
          } else if(argc > max_arity && !var_arity) {
            std::ostringstream os;
            os << "function " << fn << " expected at most " << max_arity << " arguments " <<
              "but only got " << argc;
            return eval_error(os.str());
          }

          AR_PRINT_STACK();

          // Replace function on stack with its result
          f.stack[f.stack_i - argc - 1] =
            fn.c_function_addr()(*this, argc, &f.stack[f.stack_i - argc]);

          AR_PRINT_STACK();


          f.stack_i -= (argc);
        } else if(fn.type() == FUNCTION) {
          std::cerr << "cannot call interpreted function from VM code" << std::endl;
          AR_ASSERT(!"cannot call interpreted function from VM code");
        } else if(fn.type() == VMFUNCTION || fn.type() == CLOSURE) {

          // Check for a closure and extract function from it if necessary,
          // so we can inspect the actual function
          Value to_apply = fn;

          if(fn.type() == CLOSURE) {
            fn = fn.closure_function();
          }

          // Check argument arity
          size_t min_arity = fn.vm_function_min_arity(), max_arity = fn.vm_function_max_arity();
          bool var_arity = fn.vm_function_variable_arity();

          if(argc < min_arity) {
            std::ostringstream os;
            os << "function " << fn << " expected at least " << min_arity << " arguments " <<
              "but only got " << argc;
            return eval_error(os.str());
          } else if(argc > max_arity && !var_arity) {
            std::ostringstream os;
            os << "function " << fn << " expected at most " << max_arity << " arguments " <<
              "but only got " << argc;
            return eval_error(os.str());
          }

          // Replace function on stack with result of function
          f.stack[f.stack_i - argc - 1] = apply_vm(to_apply, argc, &f.stack[f.stack_i - argc]);

          // Pop arguments
          f.stack_i -= (argc);

          // Finally, check for an exception
          if(f.stack[f.stack_i - 1].is_active_exception()) 
            return f.stack[f.stack_i - 1];
        }
        continue;
      }

      case OP_CLOSE_OVER: {
        size_t upvalues = code[code_offset++];
        AR_LOG_VM("close-over " << upvalues);

        AR_ASSERT(upvalues > 0);

        Value klosure, vec, tmp;
        AR_FRAME(*this, klosure, vec, tmp);

        vec = make_vector(upvalues);

        for(size_t i = 0; i != upvalues; i++) {
          size_t is_enclosed = code[code_offset++];
          size_t idx = code[code_offset++];

          if(is_enclosed) {
            AR_LOG_VM("enclosing free variable " << i << " from closure idx " << idx);
            AR_ASSERT(f.closure->upvalues->data[idx].type() == UPVALUE);
            vector_append(vec, f.closure->upvalues->data[idx]);
          } else {
            AR_LOG_VM("enclosing local variable " << i << " from f.upvalues idx " << idx);
            // Problem: Upvalue array does not necessarily correlate 1:1 with locals.

            // In other words, something like (lambda (c d) (lambda () d))
            // Will cause this to fail.
            
            // So we need to track a variable's location in the upvalue array,
            // as well as its location in the locals index
            // How to do this?

            // close-over 0 0 1 0 1

            // emit close-over 0 0 1
            // 0 = this is a local
            // 0 = location in upvalues array, for initialization
            // 1 = location in locals

            //std::cout << idx << std::endl;
            //std::cout << f.fn->free_variables->length << std::endl;
            AR_ASSERT(!f.fn->free_variables || idx < f.fn->free_variables->length);
            // std::cout << f.upvalues[idx].type() << std::endl;
            // std::cout << (ptrdiff_t) f.upvalues[idx].bits << std::endl;
            AR_ASSERT(gc.live(f.upvalues[idx]));
            AR_ASSERT(f.upvalues[idx].type() == UPVALUE);
            AR_ASSERT(!f.upvalues[idx].upvalue_closed());
            vector_append(vec, f.upvalues[idx]);
          }

          //std::cout << vec.vector_ref(i) << std::endl;
          // AR_ASSERT("AUGH" && gc.live(vec.vector_ref(i)));
          //std::cout << vec.vector_ref(i).upvalue_closed() << std::endl;
          AR_LOG_VM("ENCLOSING VALUE " << i << " = " << vec.vector_ref(i) << " " << vec.vector_ref(i).upvalue());
        }
        klosure = gc.allocate(CLOSURE, sizeof(Closure));
        klosure.as<Closure>()->upvalues = vec.vector_storage().as<VectorStorage>();
        klosure.as<Closure>()->function = f.stack[f.stack_i-1];
        f.stack[f.stack_i-1] = klosure;
        continue;
      }

      case OP_JUMP: {
        // Drop condition from stack
        // std::cout << "DROPPING CONDITION " << f.stack[f.stack_i - 2] << std::endl;
        // std::cout << "REPLACED WITH " << f.stack[f.stack_i - 1] << std::endl;
        //f.stack[f.stack_i - 1] = f.stack[f.stack_i - 2];
        //f.stack_i--;

        //std::cout << f.stack[f.stack_i] << std::endl;
        //std::cout << f.stack[f.stack_i - 1] << std::endl;

        code_offset = code[code_offset];
        AR_LOG_VM("jump " << code_offset);
        continue;
      }


      case OP_JUMP_IF_FALSE: {
        size_t jmp_offset = code[code_offset++];
        Value val = f.stack[f.stack_i-1];
        size_t pop = code[code_offset++];

        AR_LOG_VM("jump-if-false " << jmp_offset);
        if(val == C_FALSE) {
          AR_LOG_VM("jump-if-false jumping");
          code_offset = jmp_offset;
        } else {
          AR_LOG_VM("jump-if-false not jumping");
        }

        if(pop) {
          f.stack_i--;
        }
        continue;
      }

      case OP_POP: {
        AR_LOG_VM("pop");
        f.stack_i--;
        continue;
      }

      case OP_BAD: {
      default:
        warn() << "encountered bad opcode: " << insn << std::endl;
        AR_ASSERT(!"bad opcode");
        break;
      }
    }
  }

  // Evaluate arguments and use them to initialize locals array
  // Figure out how to allocate locals array
  // Figure out how to allocate stack
  return f.stack[f.stack_i];
}

}
