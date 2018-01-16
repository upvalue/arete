// vm.cpp - Virtual machine

#define NOMINMAX

#include "arete.hpp"

#define VM_NEXT_INSN() (*cp++)
#define VM_JUMP(offset) \
    cp = (size_t*)((((code + ((size_t) offset))))); \

// This code allows us to use a normal switch or the computed goto extension

#ifdef _MSC_VER
# define AR_COMPUTED_GOTO 0
#else
# define AR_COMPUTED_GOTO 1
#endif

#if AR_COMPUTED_GOTO
# define VM_CASE(label) LABEL_##label 
# define VM_DISPATCH() goto *dispatch_table[*cp++];
# define VM_SWITCH()
#else 
# define VM_CASE(label) case label 
# define VM_DISPATCH() break ;
# define VM_SWITCH() switch(*cp++)
#endif

#define VM2_EXCEPTION(type, msg) \
  { std::ostringstream __os; __os << msg ; exception = state.make_exception(type, __os.str()); \
    goto exception;}

#define AR_VM_LOG_ALWAYS false

namespace arete {

/**
 * Like SourceLocation, but includes a code offset
 */
struct VMSourceLocation {
  unsigned code, source, line, begin, length;
};

std::ostream& operator<<(std::ostream& os, const VMSourceLocation& loc) {
  os << "#<VMSourceLocation code-position: " << loc.code << ' ' <<
    "source: " << loc.source << " line: " << loc.line << " begin: " << loc.begin <<
    " length: " << loc.length << '>';
  return os;
}

struct VMFrame2 {
  VMFrame2(State& state_, Value fnp): state(state_) {
    closure = fnp;
    fn = closure.closure_unbox();
    AR_ASSERT(state.gc.live(closure.heap));
    AR_ASSERT(state.gc.live(fn.heap));
    AR_ASSERT("Wizard fight" && arete::current_state->gc.live(fn.heap));
    VMFunction* vfn = fn.as<VMFunction>();
    vm_stack_used = vfn->local_count + vfn->upvalue_count + vfn->stack_max;
    //state.gc.grow_stack(vm_stack_used);
  }

  ~VMFrame2() {
    VMFunction* vfn = fn.as<VMFunction>();
    Value* upvalues = &state.gc.vm_stack[state.gc.vm_stack_used - vm_stack_used + vfn->local_count];
    //std::cout << "closing over " << vfn->upvalue_count << " upvalues" << std::endl;
    for(size_t i = 0; i != vfn->upvalue_count; i++) {
      AR_ASSERT(upvalues[i].heap_type_equals(UPVALUE));
      upvalues[i].as_unsafe<Upvalue>()->U.converted = state.gc.vm_stack[upvalues[i].as_unsafe<Upvalue>()->U.vm_local_idx];
      upvalues[i].heap->set_header_bit(Value::UPVALUE_CLOSED_BIT);
    }
    state.gc.shrink_stack(vm_stack_used);
  }

  State& state;
  Value closure;
  Value fn;
  size_t vm_stack_used;
  size_t depth;
};

// It is possible for pointers to be moved around during program execution; however, putting them
// in an AR_FRAME tracking structure means they can't be stored in registers.

// We keep them out of registers and restore them using these macros after allocations

// VM2_RESTORE_GC restores only GC'd pointer and is called after allocations

#define VM2_RESTORE_GC() \
  vfn = f.fn.as_unsafe<VMFunction>(); \
  cp = (size_t*)((((size_t) vfn->code_pointer())) +  (((size_t) cp) - ((size_t) code))); \
  code = vfn->code_pointer();

// VM2_RESTORE restores GC and stack pointers and is called after applications

// TODO: Way to do this without division?

#define VM2_RESTORE() \
  vfn = f.fn.as_unsafe<VMFunction>(); \
  locals = &state.gc.vm_stack[sff_offset]; \
  /*std::cout << "stack offset:" << ((size_t)stack ) - ((size_t) sbegin) << std::endl; */\
  stack = (Value*)(((size_t)&state.gc.vm_stack[sff_offset + vfn->local_count + vfn->upvalue_count] + (((((size_t)stack) - ((size_t)sbegin)))))); \
  sbegin = &state.gc.vm_stack[sff_offset + vfn->local_count + vfn->upvalue_count]; \
  cp = (size_t*)((((size_t) vfn->code_pointer())) +  (((size_t) cp) - ((size_t) code))); \
  code = vfn->code_pointer();

#define AR_LOG_VM2(msg) \
  if(((ARETE_LOG_TAGS & ARETE_LOG_TAG_VM) && (AR_VM_LOG_ALWAYS || vfn->get_header_bit(Value::VMFUNCTION_LOG_BIT)))) { \
    ARETE_LOG((ARETE_LOG_TAG_VM), "vm", msg); \
  }

Value apply_vm(State& state, size_t argc, Value* argv, void* fnp) {
  //std::cout << "apply_vm called" << std::endl;

  size_t frames_lost = 0;

  // Function frame, allocated on stack
  size_t sff_offset = state.gc.vm_stack_used;
  Value exception;

tail:
  AR_ASSERT(state.gc.live((HeapValue*) fnp));
  VMFrame2 f(state, Value((HeapValue*) fnp));

  // If argv points to somewhere on the existing stack, we need to update it
  if(state.gc.stack_needs_realloc(f.vm_stack_used) && ((size_t)argv >= (size_t)state.gc.vm_stack && (size_t)argv <= ((size_t) state.gc.vm_stack + (size_t)(state.gc.vm_stack_used * sizeof(void*))))) {
    size_t argv_offset = ((size_t)argv) - ((size_t) state.gc.vm_stack);

    state.gc.grow_stack(f.vm_stack_used);

    argv = (Value*) ((((size_t) state.gc.vm_stack) + argv_offset));
  } else {
    state.gc.grow_stack(f.vm_stack_used);
  }

  // TODO: CHECK RECURSION LIMIT

  AR_FRAME(state, f.fn, f.closure);

  // We keep these pointers to garbage-collected values outside of the GC frame, because it allows
  // the compiler to store them in registers, providing a modest speedup.

  // But because of that, they have to be restored after anything that might call the GC

  // In addition, pointers to the VM stack (which is managed manually, but can be realloc) have to
  // be updated after anything that might result in another apply_vm call
  VMFunction* vfn = static_cast<VMFunction*>(f.fn.heap);
  Value *locals, *stack = 0, *sbegin = 0;
  size_t  *code = 0;

  size_t *cp = 0;
  // Calculate initial pointers to info
  VM2_RESTORE();

  // This has to be done in a somewhat funky order. Because allocations can occur here (if a function
  // has upvalues, or if a function has rest arguments). We have to take care to make sure everything
  // is garbage collected at the allocation points here. So we check function arity right before
  // starting the actual function.
  //std::cout << (ptrdiff_t) state.gc.vm_stack << std::endl;
  //std::cout << (ptrdiff_t) locals << std::endl;
  AR_ASSERT((size_t)locals >= (size_t)state.gc.vm_stack && (size_t)locals <= ((size_t) state.gc.vm_stack + (size_t)(state.gc.vm_stack_used * sizeof(void*))));

  // Problem: ARGV is moved after stack growth!

  size_t locals_size = std::min(argc, (size_t)vfn->max_arity) * sizeof(Value*);
  // Initialize local variables
  memcpy(locals, argv, std::min(argc, (size_t)vfn->max_arity) * sizeof(Value*));
  // Zero out whole stack
  // TODO: Necessary?
  memset((void*)(((size_t)locals) + locals_size), 0, (f.vm_stack_used * sizeof(Value*)) - locals_size);

  // Here we allocate all upvalues needed by functions that will be enclosed by this function.
  // Each upvalue is tied to a local until control exits this function, at which point they become
  // freestanding
  if(vfn->upvalue_count) {
    Value* upvalues = &state.gc.vm_stack[sff_offset + vfn->local_count];
    state.gc.protect_argc = argc;
    state.gc.protect_argv = argv;

    //AR_LOG_VM2("allocating space for " << vfn->free_variables->length << " upvalues");

    for(size_t i = 0; i != f.fn.as_unsafe<VMFunction>()->free_variables->length; i++) {
      upvalues[i] = state.gc.allocate(UPVALUE, sizeof(Upvalue));
      AR_ASSERT(state.gc.live(f.fn));
      AR_ASSERT(state.gc.live(upvalues[i]));
      size_t idx = ((size_t*) f.fn.as_unsafe<VMFunction>()->free_variables->data)[i];
      size_t vm_stack_idx = ((size_t)&locals[idx] - (size_t) state.gc.vm_stack) / sizeof(void*);


      AR_ASSERT(current_state->gc.live(upvalues[i]));
      upvalues[i].as<Upvalue>()->U.vm_local_idx = vm_stack_idx;
    }

    VM2_RESTORE_GC();
    state.gc.protect_argc = 0;
  }

#if AR_COMPUTED_GOTO
  static void* dispatch_table[] = {
    &&LABEL_OP_BAD, &&LABEL_OP_PUSH_CONSTANT, &&LABEL_OP_PUSH_IMMEDIATE, &&LABEL_OP_POP,
    &&LABEL_OP_GLOBAL_GET, &&LABEL_OP_GLOBAL_SET, &&LABEL_OP_LOCAL_GET, &&LABEL_OP_LOCAL_SET,
    &&LABEL_OP_UPVALUE_GET, &&LABEL_OP_UPVALUE_SET, &&LABEL_OP_CLOSE_OVER,
    &&LABEL_OP_APPLY, &&LABEL_OP_APPLY_TAIL,
    &&LABEL_OP_RETURN,
    &&LABEL_OP_JUMP, &&LABEL_OP_JUMP_WHEN, &&LABEL_OP_JUMP_WHEN_POP, &&LABEL_OP_JUMP_UNLESS,
    &&LABEL_OP_ARGC_EQ, &&LABEL_OP_ARGC_GTE, &&LABEL_OP_ARG_OPTIONAL, &&LABEL_OP_ARGV_REST,
    &&LABEL_OP_ARG_KEY, &&LABEL_OP_ARGV_KEYS,
    // Extended instructions
    &&LABEL_OP_ADD, &&LABEL_OP_SUB,
    &&LABEL_OP_LT,
    &&LABEL_OP_CAR,
    &&LABEL_OP_CDR,
    &&LABEL_OP_LIST_REF,
    &&LABEL_OP_NOT,
    &&LABEL_OP_EQ,
    &&LABEL_OP_FX_LT,
    &&LABEL_OP_FX_ADD,
    &&LABEL_OP_FX_SUB,
  };
#endif

  //std::cout << "Offset of frame storage: " << ((size_t)locals - (size_t)state.gc.vm_stack) << std::endl;
  //std::cout << "Offset of stack: " << ((size_t)stack - (size_t)state.gc.vm_stack) << std::endl;

  while(true) {
#if AR_COMPUTED_GOTO
    VM_DISPATCH();
#endif

    VM_SWITCH() {
      VM_CASE(OP_BAD): {
        AR_ASSERT(!"bad instruction");
      }

      VM_CASE(OP_PUSH_CONSTANT):  {
        size_t idx = VM_NEXT_INSN();
        (*stack++) = vfn->constants->data[idx];
        AR_LOG_VM2("push-constant idx: " << idx << "; " << vfn->constants->data[idx]);
        VM_DISPATCH();
      }

      VM_CASE(OP_PUSH_IMMEDIATE): {
        AR_LOG_VM2("push-immediate " << (size_t) *cp);
        // std::cout << (size_t) stack << ' ' << (size_t) sbegin << std::endl;
        (*stack++) = VM_NEXT_INSN();
        VM_DISPATCH();
      }

      VM_CASE(OP_POP): {
        AR_LOG_VM2("pop");
        (*stack--);
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_GET): {
        size_t idx = VM_NEXT_INSN();
        Value global = vfn->constants->data[idx];
        Value value = global.symbol_value();
        if(value == C_UNDEFINED) {
          VM2_EXCEPTION("eval", "reference to undefined variable " << global);
        }
        (*stack++) = global.symbol_value();
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_SET): {
        size_t err_on_undefined = VM_NEXT_INSN();
        size_t constant_id = VM_NEXT_INSN();

        //Value val = f.stack[f.stack_i - 2], key = f.stack[f.stack_i - 1];
        Value val = (*--stack);
        Value key = vfn->constants->data[constant_id];
        AR_LOG_VM2("global-set (" << (err_on_undefined ? "set!" : "define") << ") " << key << " = " << val);

        if(err_on_undefined && key.symbol_value() == C_UNDEFINED) {
          VM2_EXCEPTION("eval", "attempt to set! undefined variable " << key);
        }

        key.set_symbol_value(val);
        VM_DISPATCH();
      }

      VM_CASE(OP_LOCAL_GET): {
        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM2("local-get " << idx);
        (*stack++) = locals[idx];
        VM_DISPATCH();
      }
      
      VM_CASE(OP_LOCAL_SET): {
        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM2("local-set " << idx);

        locals[idx] = (*--stack);

        VM_DISPATCH();
      }

      VM_CASE(OP_UPVALUE_GET): {
        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM2("upvalue-get " << idx);
        Upvalue* upval = f.closure.as_unsafe<Closure>()->upvalues->data[idx].as_unsafe<Upvalue>();
        if(upval->get_header_bit(Value::UPVALUE_CLOSED_BIT)) {
          (*stack++) = upval->U.converted;
        } else {
          (*stack++) = state.gc.vm_stack[upval->U.vm_local_idx];
        }
        // (*stack++) = upval.upvalue();
        VM_DISPATCH();
      }

      VM_CASE(OP_UPVALUE_SET): {
        size_t idx = VM_NEXT_INSN();
        Upvalue* upval = f.closure.as_unsafe<Closure>()->upvalues->data[idx].as_unsafe<Upvalue>();
        Value val = (*--stack);
        if(upval->get_header_bit(Value::UPVALUE_CLOSED_BIT)) {
          upval->U.converted = val;
        } else {
          state.gc.vm_stack[upval->U.vm_local_idx] = val;
        }
        //upval.upvalue_set(val);
        AR_LOG_VM2("upvalue-set " << idx << " = " << val);
        VM_DISPATCH();
      }

      VM_CASE(OP_CLOSE_OVER): {
        size_t upvalue_count = VM_NEXT_INSN();
        AR_LOG_VM2("close-over " << upvalue_count);

        Value storage, closure;
        {
          AR_FRAME(state, storage, closure);

          storage = state.make_vector_storage(upvalue_count);

          VM2_RESTORE_GC();
          // VM ALLOCATION

          Value* upvalues = &state.gc.vm_stack[sff_offset + vfn->local_count];

          for(size_t i = 0; i != upvalue_count; i++) {
            size_t is_enclosed = VM_NEXT_INSN();
            size_t idx = VM_NEXT_INSN();

            if(is_enclosed) {
              AR_LOG_VM2("enclosing free variable " << i << " from closure idx " << idx);
              state.vector_storage_append(storage, f.closure.as<Closure>()->upvalues->data[idx]);
            } else {
              AR_LOG_VM2("enclosing local variable " << idx);
              AR_ASSERT(upvalues[idx].heap_type_equals(UPVALUE));
              state.vector_storage_append(storage, upvalues[idx]);
            }
            // VM ALLOCATION

            VM2_RESTORE_GC();

            AR_LOG_VM2("upvalue " << i << " = " << storage.as_unsafe<VectorStorage>()->data[i] << " " << storage.as_unsafe<VectorStorage>()->data[i].upvalue())
          }
          closure = state.gc.allocate(CLOSURE, sizeof(Closure));

          //closure.procedure_install(&State::apply_vm);
          closure.procedure_install((c_closure_t) & arete::apply_vm);

          closure.as_unsafe<Closure>()->upvalues = storage.as<VectorStorage>();

          closure.as_unsafe<Closure>()->function = *(stack-1);

          AR_ASSERT(closure.as_unsafe<Closure>()->function.heap_type_equals(VMFUNCTION));

          // VM ALLOCATION

          *(stack-1) = closure;
        }
        VM2_RESTORE_GC();
        VM_DISPATCH();  
      }

      VM_CASE(OP_APPLY): {
        size_t fargc = VM_NEXT_INSN();
        Value afn = *(stack - (fargc + 1));
        AR_LOG_VM2("apply " << fargc << " " << afn);
        if(AR_LIKELY(afn.procedurep())) {
          Value ret =  afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc, afn.heap);
          VM2_RESTORE();
          *(stack - (fargc + 1)) = ret;
          stack -= fargc;


          if((*(stack-1)).is_active_exception()) {
            exception = *(stack-1);
            goto exception;
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value" << afn);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_APPLY_TAIL): {
        size_t fargc = VM_NEXT_INSN();

        Value afn = *(stack - fargc - 1);
        Value to_apply = afn;
        AR_LOG_VM2("apply-tail fargc " << fargc << " fn: " << afn);

        if(AR_LIKELY(afn.procedurep())) {
          if(afn.heap_type_equals(VMFUNCTION) || afn.heap_type_equals(CLOSURE)) {
            frames_lost++;

            afn = afn.closure_unbox();

            state.temps.clear();
            state.temps.insert(state.temps.end(), stack - fargc, stack);

            argv = &state.temps[0];
            argc = fargc;
            fnp = (void*) to_apply.bits;

            goto tail;
          } else {
            Value ret =  afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc, (void*) to_apply.heap);
            VM2_RESTORE();
            *(stack - (fargc + 1)) = ret;
            stack -= fargc;

            if((*(stack-1)).is_active_exception()) {
              exception = *(stack-1);
              goto exception;
            }
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value " << afn);
        }
      }

      VM_CASE(OP_RETURN): {
        goto ret;
      }

      VM_CASE(OP_JUMP): {
        size_t jmp = VM_NEXT_INSN();
        AR_LOG_VM2("jump " << jmp);

        VM_JUMP(jmp);

        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_WHEN): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value val = *(stack-1);

        if(val == C_FALSE) {
          AR_LOG_VM2("jump-if-false jumping " << jmp_offset);
          VM_JUMP(jmp_offset);
        } else {
          AR_LOG_VM2("jump-if-false not jumping" << jmp_offset);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_WHEN_POP): {
        size_t jmp_offset = VM_NEXT_INSN();
        if((*--stack) == C_FALSE) {
          VM_JUMP(jmp_offset);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_UNLESS): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value val = *(stack-1);

        if(val == C_FALSE) {
          AR_LOG_VM2("jump-if-true not jumping " << jmp_offset);
        } else {
          AR_LOG_VM2("jump-if-true jumping " << jmp_offset);
          VM_JUMP(jmp_offset);
        }

        VM_DISPATCH();
      }


      VM_CASE(OP_ARGC_EQ): {
        size_t eargc = VM_NEXT_INSN();
        AR_LOG_VM2("argc-eq " << eargc);
        if(AR_UNLIKELY(argc != eargc)) {
          VM2_EXCEPTION("eval", "function expected exactly " << eargc << " arguments but got " << argc);
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_ARGC_GTE): {
        size_t eargc = VM_NEXT_INSN();

        AR_LOG_VM2("argc-gte " << eargc);

        if(AR_UNLIKELY(argc < eargc)) {
          VM2_EXCEPTION("eval", "function expected at least " << eargc << " arguments but got " << argc);
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_ARG_OPTIONAL): {
        size_t eargc = VM_NEXT_INSN();
        size_t offset = VM_NEXT_INSN();
        
        AR_LOG_VM2("arg-optional " << eargc << ' ' << offset << ' ' << (argc > eargc ? "jumping" : "not jumping"));
        if(argc > eargc) {
          VM_JUMP(offset);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_ARGV_REST): {
        if(argc <= vfn->max_arity) {
          locals[vfn->max_arity] = C_NIL;
        } else {
          // argv may be &temps[0] if this is a tail call
          if(argv == &state.temps[0]) {
            locals[vfn->max_arity] = state.temps_to_list(vfn->max_arity);
          } else {
            state.temps.clear();
            state.temps.insert(state.temps.end(), &argv[vfn->max_arity], &argv[argc]);

            locals[vfn->max_arity] = state.temps_to_list();
          }
        }
        VM2_RESTORE_GC();
        VM_DISPATCH();
      }

      VM_CASE(OP_ARG_KEY): {
        size_t local_idx = VM_NEXT_INSN();
        size_t key_constant = VM_NEXT_INSN();
        size_t label = VM_NEXT_INSN();


        // Attempt to find key in #!keys table
        Value keys = locals[vfn->max_arity];
        Value constant = vfn->constants->data[key_constant];
        AR_LOG_VM2("arg-key " << local_idx << " " << constant)

        bool found;
        Value result = state.table_get(keys, constant, found);

        std::cout << "keys: " << keys << std::endl;
        std::cout << "constant: " << constant << std::endl;

        if(found) {
          locals[local_idx] = result;
          AR_LOG_VM2("arg-key found key, jumping past default value expression");
          VM_JUMP(label);
        } 

        VM_DISPATCH();
      }

      VM_CASE(OP_ARGV_KEYS): {
        // Convert argv to table containing keywords
        Value table;
        {
          AR_FRAME(state, table);

          state.gc.protect_argc = argc;
          state.gc.protect_argv = argv;

          locals[vfn->max_arity] = state.make_table();

          VM2_RESTORE_GC();

          for(size_t i = vfn->max_arity; i != argc; i += 2) {
            if(i + 1 == argc) {
              VM2_EXCEPTION("eval", "odd number of arguments to keyword-accepting function");
            }

            Value k = argv[i], v = argv[i+1];

            if(!(k.heap_type_equals(SYMBOL) && k.symbol_keyword())) {
              VM2_EXCEPTION("eval", "expected keyword as argument " << i << " but got " << k.type());
            }

            state.table_set(locals[vfn->max_arity], k, v);
            VM2_RESTORE_GC();
          }
        }
        VM_DISPATCH();
      }

#define STACK_PICK(i) (*(stack - (i)))

      VM_CASE(OP_ADD): {
        size_t argc = VM_NEXT_INSN();
        size_t i;
        ptrdiff_t fx = 0;
        for(i = argc; i != 0; i--) {
          Value n(*(stack - i));
          if(n.fixnump()) {
            fx += n.fixnum_value();
          } else {
            break;
          }
        }
        if(i == 0) {
          stack -= argc;
          (*stack++) = Value::make_fixnum(fx);
        } else {
          double fl = (double) fx;
          for(; i != 0; i--) {
            Value n(*(stack - i));
            if(n.fixnump()) {
              fl += (double) n.fixnum_value();
            } else if(n.heap_type_equals(FLONUM)) {
              fl += n.flonum_value();
            } else {
              VM2_EXCEPTION("type", "vm primitive + expected a fixnum or flonum as argument " << i << " but got " << n.type());
            }
          }
          stack -= argc;
          (*stack++) = state.make_flonum(fl);
          VM2_RESTORE_GC();
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_SUB): {
        size_t argc = VM_NEXT_INSN();
        // TODO Separate unary operator
        if(argc == 0) {
          VM2_EXCEPTION("eval", "vm primitive - expects at least one argument");
        } else if(argc == 1) {
          if(STACK_PICK(1).fixnump()) {
            *(stack - 1) = Value::make_fixnum(-STACK_PICK(1).fixnum_value());
          } else if(STACK_PICK(1).heap_type_equals(FLONUM)) {
            *(stack - 1) = state.make_flonum(-STACK_PICK(1).flonum_value());
          } else {
            VM2_EXCEPTION("type", "vm primitive - expected fixnum or flonum as argument but got " << STACK_PICK(1).type());
          }
        } else {
          Value n(*(stack - argc));
          ptrdiff_t fx = 0;
          size_t i = 1;
          for(; i != argc; i++) {
            Value n2(*(stack - i));
            if(n2.fixnump()) {
              fx += n2.fixnum_value();
            } else {
              break;
            }
          }
          if(i == argc) {
            *(stack - argc) = Value::make_fixnum(n.fixnum_value() - fx);
          } else {
            double fl = (double) fx;
            for(; i != argc; i++) {
              Value n2(STACK_PICK(i));
              if(n2.fixnump()) {
                fl += n2.fixnum_value();
              } else if(n2.heap_type_equals(FLONUM)) {
                fl += n2.flonum_value();
              } else {
                VM2_EXCEPTION("type", "vm primitive - expected fixnum or flonum as argument but got " << STACK_PICK(i).type());
              }
            }
            if(n.fixnump()) {
              *(stack - argc) = state.make_flonum((double)n.fixnum_value() - fl);
            } else {
              *(stack - argc) = state.make_flonum(n.flonum_value() - fl);
            }
          }
          stack -= (argc - 1);
        }

        VM2_RESTORE_GC();
        VM_DISPATCH();
      }

      VM_CASE(OP_LT): {
        size_t argc = VM_NEXT_INSN();
        for(size_t i = 0; i != argc - 1; i++) {
          Value n(STACK_PICK(argc-i));
          Value n2(STACK_PICK(argc-i-1));
          if(!n.numeric()) {
            VM2_EXCEPTION("type", "< expects all numeric arguments but argument " << i+1 << " is a " << n.type());
          } else if(!n2.numeric()) {
            VM2_EXCEPTION("type", "< expects all numeric arguments but argument " << i+2 << " is a " << n2.type());
          }
          if(n.fixnump()) {
            if(n2.fixnump() && !(n.fixnum_value() < n2.fixnum_value())) {
              *(stack - argc) = C_FALSE;
              goto fals;
            } else if(n2.heap_type_equals(FLONUM) && !(n.fixnum_value() < n2.flonum_value())) {
              *(stack - argc) = C_FALSE;
              goto fals;
            } 
          } else {
            if(n2.fixnump() && !(n.flonum_value() < n2.fixnum_value())) {
              *(stack - argc) = C_FALSE;
              goto fals;
            } else if(n2.heap_type_equals(FLONUM) && !(n.flonum_value() < n2.flonum_value())) {
              *(stack - argc) = C_FALSE;
              goto fals;
            } 
          }
        }
        *(stack - argc) = C_TRUE;
        fals:
        stack -= (argc - 1);
        VM_DISPATCH();
      }

#define VM_TOP_PAIR_CHECK(name) \
  if(AR_UNLIKELY(!STACK_PICK(1).heap_type_equals(PAIR))) { \
    VM2_EXCEPTION("type", "vm primitive " name " expected a pair as its argument but got " << STACK_PICK(1).type()); \
  }

      VM_CASE(OP_CAR): {
        VM_TOP_PAIR_CHECK("car");
        *(stack - 1) = STACK_PICK(1).car();
        VM_DISPATCH();
      }

      VM_CASE(OP_CDR): {
        VM_TOP_PAIR_CHECK("cdr");
        *(stack - 1) = STACK_PICK(1).cdr();
        VM_DISPATCH();
      }


      VM_CASE(OP_LIST_REF): {
        VM_DISPATCH();
      }

      VM_CASE(OP_NOT): {
        *(stack - 1) = (*(stack - 1) == C_FALSE ? C_TRUE : C_FALSE);
        VM_DISPATCH();
      }
      
      VM_CASE(OP_EQ): {
        *(stack - 2) = Value::make_boolean(STACK_PICK(2).bits == STACK_PICK(1).bits);
        stack--;
        VM_DISPATCH();
      }

    #define VM_FX_CHECK(name) \
      if(!STACK_PICK(1).fixnump() || !STACK_PICK(2).fixnump()) { \
        VM2_EXCEPTION("type", "vm primitive " name " expected a fixnum as its arguments but got " << STACK_PICK(2).type() << ' ' << STACK_PICK(1).type()) \
      }

      VM_CASE(OP_FX_LT): {
        VM_FX_CHECK("fx<");
        *(stack - 2) = STACK_PICK(2).bits < STACK_PICK(1).bits ? C_TRUE : C_FALSE;
        stack--;
        //*(stack--) = ((*(stack - 1)).fixnump() && (*(stack - 2)).fixnump() && (*(stack - 2)).bits < *(stack -)
        VM_DISPATCH();
      }

      VM_CASE(OP_FX_ADD): {
        VM_FX_CHECK("fx+");
        *(stack - 2) = Value::make_fixnum(STACK_PICK(2).fixnum_value() + STACK_PICK(1).fixnum_value());
        stack--;
        VM_DISPATCH();
      }

      VM_CASE(OP_FX_SUB): {
        VM_FX_CHECK("fx-");
        *(stack - 2) = Value::make_fixnum(STACK_PICK(2).fixnum_value() - STACK_PICK(1).fixnum_value());
        stack--;
        VM_DISPATCH();
      }
    }

  }

  exception:
  if(exception.exception_trace()) {
    state.trace_function(f.fn, frames_lost, (((size_t) cp) - (size_t)f.fn.as_unsafe<VMFunction>()->code_pointer()) / sizeof(size_t));
  }

  return exception;
  ret:
  return *(stack - 1);
}

void State::trace_function(Value fn, size_t frames_lost, size_t code_offset) {
  Value sources(fn.as<VMFunction>()->sources);
  if(sources.type() == BYTEVECTOR && sources.bv_length() > 0) {
    // Source code information is available
    size_t i = 0;
    VMSourceLocation vmloc = sources.bv_ref<VMSourceLocation>(0);
    // If there is more than one source location,
    // we'll iterate through the source-locations until we find the one closest to the piece of
    // code we just errored out on
    if(sources.bv_length() != 5) {
      for(i = 1; i <= sources.bv_length() / 5; i++) {
        VMSourceLocation vmloc2 = sources.bv_ref<VMSourceLocation>(i);
        if(vmloc2.code >= code_offset) {
          break;
        }
        vmloc = vmloc2;
      }
    }

    SourceLocation loc;
    loc.source = vmloc.source;
    loc.line = vmloc.line;
    loc.begin = vmloc.begin;
    loc.length = vmloc.length;
    //AR_ASSERT(vmloc.source < source_names.size());

    std::ostringstream os;
    os << source_info(loc, fn.vm_function_name());
    if(frames_lost > 0) {
      os << std::endl << "-- " << frames_lost << " frames lost due to tail call optimization";
    }
    std::string line(os.str());
    if(stack_trace.size() > 0) {
      StackTrace& last = stack_trace.at(stack_trace.size() - 1);
      if(last.text.compare(line) == 0) {
        last.seen++;
      } else {
        stack_trace.push_back(line);
      }
    } else {
      stack_trace.push_back(line);
    }
  }
}

}
