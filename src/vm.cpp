// vm.cpp - Virtual machine

// TODO Computed goto

#include <alloca.h>

#include "arete.hpp"

#define AR_LOG_VM(msg) ARETE_LOG((ARETE_LOG_TAG_VM), "vm", msg)

// #define AR_LOG_VM_INSN(msg) ARETE_LOG(())

// A simple portable alternative to alloca would be just using a giant malloc'd or stack-allocated array.

namespace arete {

VMFrame::VMFrame(State& state_): state(state_), stack_i(0) {
  previous = state.gc.vm_frames;
  state.gc.vm_frames = this;
}

VMFrame::~VMFrame() {
  std::cout << state.gc.vm_frames << std::endl;
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
};

#define AR_PRINT_STACK() \
  std::cout << "stack " << (f.stack_i-1) << " out of " << f.fn->stack_max << " allocated " << std::endl; \
  for(size_t i = 0; i != f.stack_i; i++) std::cout << i << ": " << f.stack[i] << std::endl;

Value State::apply_vm(Value fn, size_t argc, Value* argv) {
  
tail:

  VMFrame f(*this);

  if(fn.type() == CLOSURE) {
    f.closure = fn.as<Closure>();
    f.fn = fn.closure_function();
  } else {
    f.closure = 0;
    f.fn = fn.as<VMFunction>();
  }

  
  // Allocate storage
  f.stack = (Value*) alloca(f.fn->stack_max * sizeof(Value));
  f.locals = (Value*) alloca(f.fn->local_count * sizeof(Value));

  // Initialize local variables

  for(unsigned i = 0; i != argc; i++) {
    AR_LOG_VM("LOCAL " << i << ' ' << argv[i]);
    f.locals[i] = argv[i];
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

      case OP_LOCAL_GET: {
        size_t idx = code[code_offset++];
        AR_LOG_VM("local-get idx: " << idx << " = " << f.locals[idx]);
        f.stack[f.stack_i++] = f.locals[idx];
        continue;
      }

      case OP_RETURN: {
        AR_LOG_VM("return");
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
          bool varargs = fn.c_function_variable_arity();

          AR_PRINT_STACK();

          // Replace function on stack with its result
          f.stack[f.stack_i - argc - 1] =
            fn.c_function_addr()(*this, argc, &f.stack[f.stack_i - argc]);

          AR_PRINT_STACK();


          f.stack_i -= (argc);
        } else if(fn.type() == FUNCTION) {
          std::cerr << "cannot call interpreted function from VM code" << std::endl;
          AR_ASSERT(!"cannot call interpreted function from VM code");
        } else if(fn.type() == VMFUNCTION) {
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

          Value result = apply_vm(fn, argc, &f.stack[f.stack_i - argc]);

          AR_PRINT_STACK();

          // Pop arguments, replace function with results
          f.stack_i -= (argc);
          f.stack[f.stack_i - 1] = result;

          std::cout << "RESULT OF APPLYING YE FUNCTION " << result << std::endl;

          AR_PRINT_STACK();
        }
        // TODO check arity.
        // AR_PRINT_STACK();
        // AR_PRINT_STACK();
        continue;
      }

      case OP_UPVALUE_GET: {
        AR_ASSERT(f.closure);

        size_t idx = code[code_offset++];
        f.stack[f.stack_i++] = f.closure->upvalues->data->upvalue();
        break;
      }

      case OP_CLOSE_OVER: {
        size_t upvalues = code[code_offset++];

        AR_ASSERT(upvalues > 0);

        Value closure, vec;
        AR_FRAME(*this, closure, vec);

        for(size_t i = 0; i != upvalues; i++) {
          size_t is_upvalue = code[code_offset++];
          size_t idx = code[code_offset++];

          //if(is_upvalue) {
            // vector_append()
          //} else {
          //}

        }
        closure = gc.allocate(CLOSURE, sizeof(Closure));
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
