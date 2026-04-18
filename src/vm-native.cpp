// vm-native.cpp - Native VM glue (opt-in + eligibility check).
//
// The DynASM-assembled dispatch core lives in src/vm-native-x64.cpp.dasc
// (added in step S2). This file holds the opt-in plumbing and runs on every
// platform: Scheme-visible builtins (native-vm-install!, native-vm-function?),
// the opcode-coverage eligibility check, and a stub apply_native_vm that
// forwards to apply_vm until the DynASM core is in place.
//
// See docs/Native VM.md for the rollout plan.

#include "arete.hpp"

#include <unordered_set>

namespace arete {

DefunGroup native_vm_fns("native-vm");

// Whether the shared DynASM dispatch core has been initialized. On platforms
// without a native dispatch core (non-x86-64, Windows, Emscripten for now),
// this stays false forever and eligibility always returns false.
static bool native_vm_ready = false;

size_t native_vm_stat_apply_vm_path = 0;
size_t native_vm_stat_apply_c_path = 0;
size_t native_vm_stat_apply_tail_vm_path = 0;
size_t native_vm_stat_apply_tail_c_path = 0;

// Opcodes the native VM can currently execute. Every opcode in a VMFunction's
// body must be present here for the function to be eligible.
//
// This set grows as each rollout step lands. Keeping it explicit means partial
// rollouts can't accidentally execute an unimplemented opcode.
static bool native_vm_opcode_supported(size_t op) {
  switch(op) {
    // S2:
    case OP_ARGC_EQ:
    case OP_PUSH_IMMEDIATE:
    case OP_RETURN:
    // S3:
    case OP_LOCAL_GET:
    case OP_LOCAL_SET:
    case OP_POP:
    case OP_PUSH_CONSTANT:
    case OP_ARGC_GTE:
    // S4:
    case OP_JUMP:
    case OP_JUMP_WHEN:
    case OP_JUMP_WHEN_POP:
    case OP_JUMP_UNLESS:
    // S5:
    case OP_GLOBAL_GET:
    case OP_GLOBAL_SET:
    // S6:
    case OP_FX_ADD:
    case OP_FX_SUB:
    case OP_FX_LT:
    case OP_EQ:
    case OP_NOT:
    case OP_FIXNUMP:
    case OP_CAR:
    case OP_CDR:
    case OP_TYPE_CHECK:
    case OP_ADD:
    case OP_SUB:
    case OP_LT:
    // S7:
    case OP_APPLY:
    // S8:
    case OP_APPLY_TAIL:
    // S9:
    case OP_UPVALUE_GET:
    case OP_UPVALUE_SET:
    case OP_CLOSE_OVER:
    case OP_UPVALUE_FROM_LOCAL:
    case OP_UPVALUE_FROM_CLOSURE:
    // S10:
    case OP_ARGV_REST:
    // Fused null?+conditional-jump (exp 3):
    case OP_JUMP_IF_NOT_NIL:
    case OP_JUMP_IF_NIL:
    // Fused pair?+conditional-jump (exp 3b):
    case OP_JUMP_IF_NOT_PAIR:
    case OP_JUMP_IF_PAIR:
      return true;
    default:
      return false;
  }
}

static bool native_vm_install_one(Value fn) {
  Value unboxed = fn.closure_unbox();
  if(unboxed.type() != VMFUNCTION) {
    return false;
  }

  VMFunction* vfn = unboxed.as_unsafe<VMFunction>();
  if(vfn->get_header_bit(Value::VMFUNCTION_NATIVE_VM_BIT)) {
    if(fn.heap_type_equals(CLOSURE)) {
      fn.as_unsafe<Closure>()->procedure_addr = (c_closure_t) &apply_native_vm;
    }
    return true;
  }

  if(!native_vm_function_eligible(vfn)) {
    return false;
  }

  vfn->set_header_bit(Value::VMFUNCTION_NATIVE_VM_BIT);
  vfn->procedure_addr = (c_closure_t) &apply_native_vm;
  if(fn.heap_type_equals(CLOSURE)) {
    fn.as_unsafe<Closure>()->procedure_addr = (c_closure_t) &apply_native_vm;
  }
  return true;
}

static void native_vm_install_tree_visit(Value fn,
                                         std::unordered_set<VMFunction*>& seen) {
  Value unboxed = fn.closure_unbox();
  if(unboxed.type() != VMFUNCTION) return;

  VMFunction* vfn = unboxed.as_unsafe<VMFunction>();
  if(!seen.insert(vfn).second) return;

  native_vm_install_one(fn);

  VectorStorage* constants = vfn->constants;
  for(size_t i = 0; i != constants->length; i++) {
    Value constant = constants->data[i];
    if(constant.type() == VMFUNCTION || constant.type() == CLOSURE) {
      native_vm_install_tree_visit(constant, seen);
    }
  }
}

static Value native_vm_stats_alist(State& state) {
  Value result = C_NIL;
  Value key = C_FALSE;
  Value val = C_FALSE;
  Value pair = C_FALSE;
  AR_FRAME(state, result, key, val, pair);

  auto push = [&](const char* name, size_t n) {
    key = state.get_symbol(name);
    val = Value::make_fixnum((ptrdiff_t) n);
    pair = state.make_pair(key, val);
    result = state.make_pair(pair, result);
  };

  push("apply-tail-c", native_vm_stat_apply_tail_c_path);
  push("apply-tail-vm", native_vm_stat_apply_tail_vm_path);
  push("apply-c", native_vm_stat_apply_c_path);
  push("apply-vm", native_vm_stat_apply_vm_path);
  return result;
}

bool native_vm_function_eligible(VMFunction* vfn) {
  if(!native_vm_ready) return false;
  if(vfn->get_header_bit(Value::VMFUNCTION_NATIVE_BIT)) return false;
  if(vfn->get_header_bit(Value::VMFUNCTION_MACRO_BIT)) return false;
  if(vfn->get_header_bit(Value::VMFUNCTION_IDENTIFIER_MACRO_BIT)) return false;

  size_t* code = vfn->code_pointer();
  size_t len = vfn->code->length;
  size_t i = 0;
  while(i < len) {
    size_t op = code[i];
    if(!native_vm_opcode_supported(op)) return false;
    switch(op) {
      case OP_POP:
      case OP_RETURN:
      case OP_NOT:
      case OP_EQ:
      case OP_CAR:
      case OP_CDR:
      case OP_LIST_REF:
      case OP_FIXNUMP:
      case OP_FX_LT:
      case OP_FX_ADD:
      case OP_FX_SUB:
      case OP_ARGV_REST:
        i += 1; break;

      case OP_PUSH_CONSTANT:
      case OP_PUSH_IMMEDIATE:
      case OP_GLOBAL_GET:
      case OP_LOCAL_GET:
      case OP_LOCAL_SET:
      case OP_UPVALUE_GET:
      case OP_UPVALUE_SET:
      case OP_CLOSE_OVER:
      case OP_UPVALUE_FROM_LOCAL:
      case OP_UPVALUE_FROM_CLOSURE:
      case OP_APPLY:
      case OP_APPLY_TAIL:
      case OP_JUMP:
      case OP_JUMP_WHEN:
      case OP_JUMP_WHEN_POP:
      case OP_JUMP_UNLESS:
      case OP_JUMP_IF_NOT_NIL:
      case OP_JUMP_IF_NIL:
      case OP_JUMP_IF_NOT_PAIR:
      case OP_JUMP_IF_PAIR:
      case OP_ARGC_EQ:
      case OP_ARGC_GTE:
      case OP_TYPE_CHECK:
      case OP_ADD:
      case OP_SUB:
      case OP_LT:
        i += 2; break;

      case OP_GLOBAL_SET:
        i += 3; break;

      default:
        return false;
    }
  }
  return true;
}

#if !(AR_64_BIT == 1) || defined(_MSC_VER) || defined(__EMSCRIPTEN__) || \
    !(defined(__x86_64__) || defined(_M_X64))
// Platforms without a native dispatch core — apply_native_vm just forwards.
// On x86-64 SysV these are defined in vm-native-x64.cpp (generated from .dasc).
Value apply_native_vm(State& state, size_t argc, Value* argv, void* fnp) {
  return apply_vm(state, argc, argv, fnp);
}

void init_native_vm(State&) {
  // native_vm_ready stays false — native-vm-install! will always return #f.
}
#endif

static Value fn_native_vm_install(State& state, size_t argc, Value* argv, void*) {
  static const char* fn_name = "native-vm-install!";
  AR_FN_ARGC_EQ(state, argc, 1);

  Value fn = argv[0];
  Value unboxed = fn.closure_unbox();
  if(unboxed.type() != VMFUNCTION) {
    return state.type_error("native-vm-install!: expected a vm function");
  }
  VMFunction* vfn = unboxed.as_unsafe<VMFunction>();

  if(vfn->get_header_bit(Value::VMFUNCTION_NATIVE_VM_BIT)) {
    return C_TRUE;
  }

  return Value::make_boolean(native_vm_install_one(fn));
}
AR_DEFUN("native-vm-install!", fn_native_vm_install, 1);

static Value fn_native_vm_install_tree(State& state, size_t argc, Value* argv, void*) {
  static const char* fn_name = "native-vm-install-tree!";
  AR_FN_ARGC_EQ(state, argc, 1);

  Value fn = argv[0];
  Value unboxed = fn.closure_unbox();
  if(unboxed.type() != VMFUNCTION) {
    return state.type_error("native-vm-install-tree!: expected a vm function");
  }

  std::unordered_set<VMFunction*> seen;
  native_vm_install_tree_visit(fn, seen);
  return C_TRUE;
}
AR_DEFUN("native-vm-install-tree!", fn_native_vm_install_tree, 1);

static Value fn_native_vm_function_p(State& state, size_t argc, Value* argv, void*) {
  static const char* fn_name = "native-vm-function?";
  AR_FN_ARGC_EQ(state, argc, 1);
  Value fn = argv[0].closure_unbox();
  if(fn.type() != VMFUNCTION) return C_FALSE;
  return Value::make_boolean(
      fn.heap->get_header_bit(Value::VMFUNCTION_NATIVE_VM_BIT));
}
AR_DEFUN("native-vm-function?", fn_native_vm_function_p, 1);

static Value fn_native_vm_eligible_p(State& state, size_t argc, Value* argv, void*) {
  static const char* fn_name = "native-vm-eligible?";
  AR_FN_ARGC_EQ(state, argc, 1);
  Value fn = argv[0].closure_unbox();
  if(fn.type() != VMFUNCTION) return C_FALSE;
  return Value::make_boolean(native_vm_function_eligible(fn.as_unsafe<VMFunction>()));
}
AR_DEFUN("native-vm-eligible?", fn_native_vm_eligible_p, 1);

static Value fn_native_vm_stats(State& state, size_t argc, Value* argv, void*) {
  static const char* fn_name = "native-vm-stats";
  AR_FN_ARGC_EQ(state, argc, 0);
  return native_vm_stats_alist(state);
}
AR_DEFUN("native-vm-stats", fn_native_vm_stats, 0);

static Value fn_native_vm_reset_stats(State& state, size_t argc, Value* argv, void*) {
  static const char* fn_name = "native-vm-reset-stats!";
  AR_FN_ARGC_EQ(state, argc, 0);
  native_vm_stat_apply_vm_path = 0;
  native_vm_stat_apply_c_path = 0;
  native_vm_stat_apply_tail_vm_path = 0;
  native_vm_stat_apply_tail_c_path = 0;
  return C_UNSPECIFIED;
}
AR_DEFUN("native-vm-reset-stats!", fn_native_vm_reset_stats, 0);

void native_vm_mark_ready() {
  native_vm_ready = true;
}

void load_native_vm(State& state) {
  native_vm_fns.install(state);
  // init_native_vm is called from State::boot_common so that the dispatch
  // core is also assembled when loading from a saved image.
}

}
