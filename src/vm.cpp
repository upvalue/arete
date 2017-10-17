// vm.cpp - Virtual machine

#include <alloca.h>

#include "arete.hpp"

namespace arete {

VMFrame::VMFrame(State& state_): state(state_) {
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
};

Value State::apply_vm(Value fn, size_t argc, Value* argv) {
  tail:

  VMFrame f(*this);
  f.fn = fn.as<VMFunction>();
  f.stack = (Value*) alloca(f.fn->stack_size * sizeof(Value));

  // TODO: This doesn't necessarily need to be initialized; we could
  // place the stack pointer in the VMFrame structure and only
  // collect what has been set

  memset(f.stack, 0, f.fn->stack_size * sizeof(Value));

  size_t code_offset = 0;
  size_t stack_i = 0;

  while(true) {
    // TODO: This is a pretty awkward way to program.
    Value fn2(f.fn);
    size_t* code = (fn2.vm_function_bytecode());
    size_t insn = code[code_offset++];

    switch(insn) {
      case OP_PUSH_CONSTANT: {
        size_t idx = code[code_offset++];
        f.stack[stack_i++] = fn2.vm_function_constants()->data[idx];
        AR_LOG_VM("push-constant idx: " << idx << "; " << fn2.vm_function_constants()->data[idx]);
        continue;
      }

      case OP_GLOBAL_GET: {
        size_t idx = code[code_offset++];
        Value sym = fn2.vm_function_constants()->data[idx];
        AR_LOG_VM("global-get idx: " << idx << " ;; " << fn2.vm_function_constants()->data[idx]);
        AR_ASSERT(sym.type() == SYMBOL && "global-get called against non-symbol");
        f.stack[stack_i++] = sym.symbol_value();
        continue;
      }

      case OP_RETURN: {
        return f.stack[stack_i - 1];
      }

      case OP_APPLY: {
        size_t argc = code[code_offset++];
        Value fn = f.stack[stack_i - argc - 1];
        AR_LOG_VM("apply argc: " << argc << " fn: " << fn);
        AR_ASSERT(fn.type() == CFUNCTION);
        Value v = C_FALSE;
        fn.c_function_addr()(*this, argc, &v);
        return C_TRUE;
      }

      default:
      case OP_BAD: {
        warn() << "encountered bad opcode: " << insn << std::endl;
        AR_ASSERT(!"bad opcode");
      }
    }
  }

  // Evaluate arguments and use them to initialize locals array
  // Figure out how to allocate locals array
  // Figure out how to allocate stack
  return Value::make_fixnum(5678);
}

}