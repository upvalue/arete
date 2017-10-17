// vm.cpp - Virtual machine

#include <alloca.h>

#include "arete.hpp"

namespace arete {

VMFrame::VMFrame(State& state_): state(state_) {
  previous = state.gc.vm_frames;
  state.gc.vm_frames = this;
}

VMFrame::~VMFrame() {
  AR_ASSERT(state.gc.vm_frames == this);
  state.gc.vm_frames = previous;
}

enum {
  OP_BAD = 0,
  OP_PUSH_CONSTANT = 1,
  OP_RETURN = 4
};

Value State::apply_vm(Value env, Value fn, Value args, Value src_exp, Value fn_name) {
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
        continue;
      }

      case OP_RETURN: {
        return f.stack[stack_i - 1];
      }

      case OP_BAD: AR_ASSERT(!"bad opcode");
    }
  }

  // std::cout << "STACK[0]: " << vmframe.stack[0] << std::endl;

  // Evaluate arguments and use them to initialize locals array
  // Figure out how to allocate locals array
  // Figure out how to allocate stack
  return Value::make_fixnum(5678);
}

}