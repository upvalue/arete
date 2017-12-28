// vm.cpp - Virtual machine

// TODO Disassembler

// TODO: Style. It's somewhat awkward and difficult to manipulate the stack by arithmetic
// e.g. after a function application there should probably be a simple way to save a position
// on the stack and refer to it later.

// TODO: Memory usage could be reduced by actual bytecode, but this would increase complexity

// TODO: Multiple return values cannot be implemented efficiently because we cannot grow the stack
// on-demand.


// Does add some runtime cost

// Since values is sort of a special form, we could maybe instrument the compiler to increase
// the stack-max for values storage.

#include "arete.hpp"

// TODO: Non C-stack improvement. There's no need to malloc every function call. Rather we should
// use more intelligent allocation. The problem is that realloc'ing on the fly is rather complex as
// the existing pointers need to be updated; I tried this approach and couldn't get it to work.

// Another approach might be to just chain together malloc'd blocks

// TODO: Are there more optimizations that can be done? With a little trouble, for example, we could
// use variables outside the VMFrame struct. Does putting everything in VMFrame prevent the compiler
// from storing them in registers?

// TODO: Branch prediction hinting

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

#define VM_CODE() (AR_ASSERT(gc.live((HeapValue*)code)), code)

//#define VM_NEXT_INSN() (VM_CODE()[code_offset++], (*cp++))
#define VM_NEXT_INSN() (*cp++)
#define VM_JUMP(offset) \
    cp = (size_t*)((((code + ((size_t) offset))))); \

#if AR_COMPUTED_GOTO
# define VM_CASE(label) LABEL_##label 
# define VM_DISPATCH() goto *dispatch_table[*cp++];
# define VM_SWITCH()
#else 
# define VM_CASE(label) case label 
# define VM_DISPATCH() break ;
# define VM_SWITCH() switch(*cp++)
#endif

#define VM_STACK_PUSH(expr) \
  AR_ASSERT("function stack_max is not high enough, probably a compiler bug" && \
    f.stack_i < f.fn->stack_max); \
  f.stack[f.stack_i++] = (expr)

#define VM_EXCEPTION(type, msg) \
  { std::ostringstream __os; __os << msg ; f.exception = make_exception(type, __os.str()); \
    goto exception;}

//#define VM_CODE() (assert(gc.live((HeapValue*)f.code)), f.code)
//#define VM_CODE() (f.code)


// Restore pointers after any potential garbage collection
#define VM_RESTORE() \
    cp = (size_t*)((((size_t) f.fn->code_pointer())) +  (((size_t) cp) - ((size_t) code))); \
    code = f.fn->code_pointer(); 

#define AR_VM_LOG_ALWAYS false

#define AR_LOG_VM(msg) \
  if(((ARETE_LOG_TAGS & ARETE_LOG_TAG_VM) && (AR_VM_LOG_ALWAYS || f.fn->get_header_bit(Value::VMFUNCTION_LOG_BIT)))) { \
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
  unsigned code, source, line, begin, length;
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

VMFrame::VMFrame(State& state_): state(state_), 
    //stack(0), locals(0), upvalues(0),
    exception(C_FALSE), stack_i(0),
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

  if(to_apply.heap_type_equals(CLOSURE)) {
    closure = to_apply.as_unsafe<Closure>();
    fn = closure->function.as_unsafe<VMFunction>();
  } else {
    closure = 0;
    fn = to_apply.as_unsafe<VMFunction>();
  }

  if(!AR_USE_C_STACK) {
    size_t upvalue_count = fn->free_variables ? fn->free_variables->length : 0;
    size_t alloc_size = (fn->stack_max + fn->local_count + upvalue_count);
    stack = (Value*) malloc(alloc_size * sizeof(void*));
    locals = (Value*)  (((char*) stack) + (fn->stack_max * sizeof(void*)));
    if(upvalue_count)
      upvalues = (Value*) (((char*) locals) + (fn->local_count * sizeof(void*)));
  }
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

  if(!AR_USE_C_STACK) {
    free(stack);
  }

  stack_i = 0;
  destroyed = true;
}


Value State::apply_vm(size_t argc, Value* argv, Value fn) {
#if AR_COMPUTED_GOTO
  static void* dispatch_table[] = {
    &&LABEL_OP_BAD, &&LABEL_OP_PUSH_CONSTANT, &&LABEL_OP_PUSH_IMMEDIATE, &&LABEL_OP_POP,
    
    &&LABEL_OP_GLOBAL_GET, 
    &&LABEL_OP_GLOBAL_SET, &&LABEL_OP_LOCAL_GET, &&LABEL_OP_LOCAL_SET, &&LABEL_OP_UPVALUE_GET,
    &&LABEL_OP_UPVALUE_SET,
    
    &&LABEL_OP_CLOSE_OVER, &&LABEL_OP_APPLY, &&LABEL_OP_APPLY_TAIL,

    &&LABEL_OP_RETURN, &&LABEL_OP_RETURN_END, &&LABEL_OP_JUMP, &&LABEL_OP_JUMP_WHEN, &&LABEL_OP_JUMP_WHEN_POP, &&LABEL_OP_JUMP_UNLESS,

    &&LABEL_OP_LOCAL_GET_0,

    &&LABEL_OP_ARGC_EQ,
    &&LABEL_OP_ARGC_GTE,
    &&LABEL_OP_ARGV_REST,

    // Primitives implemented directly in the VM
    &&LABEL_OP_ADD, &&LABEL_OP_SUB,
    &&LABEL_OP_LT,
    &&LABEL_OP_CAR,
    &&LABEL_OP_LIST_REF,
    &&LABEL_OP_NOT,
    &&LABEL_OP_EQ,
    &&LABEL_OP_FX_LT,
    &&LABEL_OP_FX_ADD,
    &&LABEL_OP_FX_SUB,
  };
#endif
  // Frames lost due to tail call optimization
  size_t frames_lost = 0;
  VMFrame f(*this);

  f.setup(fn);
 tail:

  AR_ASSERT(gc.vm_frames == &f);

  AR_LOG_VM("ENTERING FUNCTION " << f.fn->name << " closure: " << (f.closure == 0 ? "#f" : "#t")
     << " free_variables: " << f.fn->free_variables << " stack_max: " << f.fn->stack_max);

  // Allocate function's required storage
#if AR_USE_C_STACK
  size_t upvalue_count = f.fn->free_variables ? f.fn->free_variables->length : 0;
  // Add storage to VM frame
  void* stack[f.fn->stack_max];
  void* locals[f.fn->local_count];
  void* upvalues[upvalue_count];

  f.stack = (Value*) stack;
  f.locals = (Value*) locals;

  f.upvalues = upvalue_count ? (Value*) upvalues : 0;
  if(upvalue_count)
    f.upvalues = (Value*) upvalues;
#endif

  // This has to be done in a somewhat funky order. Because allocations can occur here (if a function
  // has upvalues, or if a function has rest arguments). We have to take care to make sure everything
  // is garbage collected at the allocation points here. So we check function arity right before
  // starting the actual function.

  // Initialize local variables
  memset(f.locals, 0, f.fn->local_count * sizeof(void*));
  memcpy(f.locals, argv, (argc > f.fn->max_arity ? f.fn->max_arity : argc) * sizeof(void*));

  // Clear upvalues array in case creation of rest arguments causes allocation
  if(f.fn->free_variables != 0) {
    memset(f.upvalues, 0, f.fn->free_variables->length * sizeof(Value));
  }

  if(f.fn->free_variables != 0) {
    // Allocate upvalues as needed
    AR_LOG_VM("Allocating space for " << f.fn->free_variables->length << " upvalues");

    for(size_t i = 0; i != f.fn->free_variables->length; i++) {
      f.upvalues[i] = gc.allocate(UPVALUE, sizeof(Upvalue));
      size_t idx = ((size_t*) f.fn->free_variables->data)[i];
      AR_LOG_VM("tying free variable " << i << " to local idx " << idx);
      f.upvalues[i].as<Upvalue>()->U.local = &f.locals[idx];
    }
  }

  tail_recur:

  // TODO: Try using a stack pointer instead of stack_i for perf

  // Forcing this into a register provides a decent speedup for the VM, but as we can't take its
  // address in that case, it is necessary to update the pointer manually after every potential
  // collection

#ifdef __GNUC__
# if ARETE_64_BIT
#  ifndef AR_USE_REGISTER
#   define AR_USE_REGISTER 1
#  endif
# endif
#endif

#ifndef AR_USE_REGISTER
# define AR_USE_REGISTER 0
#endif

#if AR_USE_REGISTER
  register size_t *cp asm ("r15") = (size_t*) f.fn->code_pointer();
#else
  size_t * cp = (size_t*) f.fn->code_pointer();
#endif

  // Always points to the beginning of the code array. Used to restore the code pointer.
  size_t * code = cp;

  if(f.depth > (size_t)get_global_value(G_RECURSION_LIMIT).fixnum_value_or_zero()) {
    std::ostringstream os;
    os << "non-tail recursive calls exceeded G_RECURSION_LIMIT (" << get_global_value(G_RECURSION_LIMIT) << ")";
    f.exception = eval_error(os.str());
    goto exception;
  }

  while(true) {
#if AR_COMPUTED_GOTO
    VM_DISPATCH();
#endif
    
    VM_SWITCH()  {
      VM_CASE(OP_PUSH_CONSTANT): {
        size_t idx = VM_NEXT_INSN();
        f.stack[f.stack_i++] = f.fn->constants->data[idx];
        AR_PRINT_STACK();
        AR_LOG_VM("push-constant idx: " << idx << "; " << f.fn->constants->data[idx]);
        VM_DISPATCH();
      }

      VM_CASE(OP_POP): {
        AR_LOG_VM("pop");
        AR_ASSERT(f.stack_i > 0 && "stack underflow");
        f.stack_i--;
        VM_DISPATCH();
      }

      VM_CASE(OP_PUSH_IMMEDIATE): {
        Value imm = VM_NEXT_INSN();
        AR_LOG_VM("push-immediate " << imm);
        f.stack[f.stack_i++] = imm;
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_GET): {
        size_t idx = VM_NEXT_INSN();
        Value sym = f.fn->constants->data[idx];
        AR_ASSERT(sym.heap_type_equals(SYMBOL));
        AR_LOG_VM("global-get idx: " << idx << " ;; " << f.fn->constants->data[idx]);
        // AR_ASSERT(sym.type() == SYMBOL && "global-get called against non-symbol");
        VM_STACK_PUSH(sym.as_unsafe<Symbol>()->value);
        if(f.stack[f.stack_i - 1] == C_UNDEFINED) {
          std::ostringstream os;
          os << "reference to undefined variable " << sym;//<< symbol_dequalify(sym);
          f.exception = eval_error(os.str());
          goto exception;
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_SET): {
        size_t err_on_undefined = VM_NEXT_INSN();
        size_t constant_id = VM_NEXT_INSN();
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

        f.stack_i -= 1;
        key.set_symbol_value(val);
        VM_DISPATCH();
      }

      VM_CASE(OP_LOCAL_GET): {
        size_t idx = VM_NEXT_INSN();
        f.stack[f.stack_i++] = f.locals[idx];
        AR_LOG_VM("local-get idx: " << idx << " = " << f.locals[idx]);
        VM_DISPATCH();
      }

      VM_CASE(OP_LOCAL_SET): {
        size_t idx = VM_NEXT_INSN();
        AR_ASSERT("stack underflow" && f.stack_i >= 1);
        Value val = f.stack[--f.stack_i];
        AR_LOG_VM("local-set idx: " << idx << " = " << val);
        f.locals[idx] = val;
        VM_DISPATCH();
      }

      VM_CASE(OP_UPVALUE_GET): {
        AR_ASSERT(f.closure);

        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM("upvalue-get " << idx);
        AR_ASSERT(f.closure->upvalues->data[idx].type() == UPVALUE);
        AR_ASSERT(gc.live(f.closure->upvalues->data[idx]));
        AR_ASSERT(gc.live(f.closure->upvalues->data[idx].upvalue()));
        f.stack[f.stack_i++] = f.closure->upvalues->data[idx].upvalue();
        VM_DISPATCH();
      }

      VM_CASE(OP_UPVALUE_SET): {
        AR_ASSERT(f.closure);
        size_t idx = VM_NEXT_INSN();
        AR_ASSERT("stack underflow" && f.stack_i >= 1);
        Value val = f.stack[--f.stack_i];
        Value upval = f.closure->upvalues->data[idx];
        upval.upvalue_set(val);
        AR_LOG_VM("upvalue-set " << idx << " = " << val);
        VM_DISPATCH();
      }

      VM_CASE(OP_APPLY_TAIL): {
        size_t fargc = VM_NEXT_INSN();

        Value afn = f.stack[f.stack_i - fargc - 1];
        Value to_apply = afn;
        AR_LOG_VM("apply-tail fargc " << fargc << " fn: " << afn);

        if(AR_LIKELY(afn.procedurep())) {
          // Stack frame is trivially reusable (same function, same argument count, no closures)
          if(afn.heap_type_equals(VMFUNCTION) || afn.heap_type_equals(CLOSURE)) {
            frames_lost++;

            afn = afn.closure_unbox();

            if((ptrdiff_t)f.fn == afn.bits && fargc == f.fn->min_arity && !f.upvalues) {
              memcpy(f.locals, &f.stack[f.stack_i - fargc], fargc * sizeof(void*));
              f.stack_i = 0;
              goto tail_recur;
            } else {
              temps.clear();
              for(size_t i = 0; i != fargc; i++) {
                temps.push_back(f.stack[f.stack_i - fargc + i]);
              }

              argv = &temps[0];

              argc = fargc;
              fn = to_apply;
              
              f.close_over();
              AR_ASSERT(f.destroyed);
              f.setup(fn);
              AR_ASSERT(!f.destroyed);

              goto tail;
            }
          } else {
            f.stack[f.stack_i - fargc - 1] =
              f.stack[f.stack_i - fargc - 1].as_unsafe<Procedure>()->procedure_addr(*this, fargc, &f.stack[f.stack_i - fargc], (void*) f.stack[f.stack_i - fargc - 1].bits);

            f.stack_i -= (fargc);

            VM_RESTORE();

            AR_ASSERT("stack underflow" && f.stack_i > 0);

            if(f.stack[f.stack_i - 1].is_active_exception()) {
              f.exception = f.stack[f.stack_i - 1];
              goto exception;
            }
          }
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_APPLY): {
        size_t fargc = VM_NEXT_INSN();

        Value afn = f.stack[f.stack_i - fargc - 1];

        AR_LOG_VM("apply fargc: " << fargc << " fn: " << afn);

        if(AR_LIKELY(afn.procedurep())) {
          f.stack[f.stack_i - fargc - 1] =
            f.stack[f.stack_i - fargc - 1].as_unsafe<Procedure>()->procedure_addr(*this, fargc, &f.stack[f.stack_i - fargc], (void*) f.stack[f.stack_i - fargc - 1].bits);

          f.stack_i -= (fargc);

          VM_RESTORE();

          AR_ASSERT("stack underflow" && f.stack_i > 0);

          if(f.stack[f.stack_i - 1].is_active_exception()) {
            f.exception = f.stack[f.stack_i - 1];
            goto exception;
          }
        } else {
          std::ostringstream os;
          AR_PRINT_STACK();
          os << "vm: attempt to apply non-applicable value " << afn;
          f.exception = eval_error(os.str());
          goto exception;
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_CLOSE_OVER): {
        size_t upvalues = VM_NEXT_INSN();
        AR_LOG_VM("close-over " << upvalues);

        AR_ASSERT(upvalues > 0);

        // OP_ENCLOSE_FREE_VARIABLE
        // OP_ENCLOSE_LOCAL_VARIABLE
        // OP_CLOSE_OVER

        Value storage, closure;
        {
          AR_FRAME(*this, storage, closure);

          storage = make_vector_storage(upvalues);

          VM_RESTORE();
          // VM ALLOCATION

          for(size_t i = 0; i != upvalues; i++) {
            size_t is_enclosed = VM_NEXT_INSN();
            size_t idx = VM_NEXT_INSN();

            if(is_enclosed) {
              AR_LOG_VM("enclosing free variable " << i << " from closure idx " << idx);
              AR_ASSERT(f.closure->upvalues->data[idx].heap_type_equals(UPVALUE));
              vector_storage_append(storage, f.closure->upvalues->data[idx]);
            } else {
              vector_storage_append(storage, f.upvalues[idx]);
            }
            // VM ALLOCATION

            VM_RESTORE();

            AR_LOG_VM("upvalue " << i << " = " << storage.as_unsafe<VectorStorage>()->data[i] << " " << storage.as_unsafe<VectorStorage>()->data[i].upvalue())
          }
          closure = gc.allocate(CLOSURE, sizeof(Closure));

          closure.procedure_install(&State::apply_vm);

          closure.as_unsafe<Closure>()->upvalues = storage.as<VectorStorage>();
          closure.as_unsafe<Closure>()->function = f.stack[f.stack_i-1];

          // VM ALLOCATION

          f.stack[f.stack_i-1] = closure;
        }
        VM_RESTORE();
        VM_DISPATCH();
      }

      ///// FLOW CONTROL

      VM_CASE(OP_RETURN):
      VM_CASE(OP_RETURN_END): {
        AR_LOG_VM("return " << f.stack_i);
        AR_PRINT_STACK();
        Value ret = f.stack_i == 0 ? C_UNSPECIFIED : f.stack[f.stack_i - 1];
        f.close_over();
        AR_ASSERT(f.destroyed);
        return ret;
      }

      VM_CASE(OP_JUMP): {
        size_t jmp = VM_NEXT_INSN();
        AR_LOG_VM("jump " << jmp);

        VM_JUMP(jmp);

        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_WHEN): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value val = f.stack[f.stack_i-1];

        if(val == C_FALSE) {
          AR_LOG_VM("jump-if-false jumping " << jmp_offset);
          VM_JUMP(jmp_offset);
        } else {
          AR_LOG_VM("jump-if-false not jumping" << jmp_offset);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_WHEN_POP): {
        size_t jmp_offset = VM_NEXT_INSN();
        if(f.stack[--f.stack_i] == C_FALSE) {
          VM_JUMP(jmp_offset);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_JUMP_UNLESS): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value val = f.stack[f.stack_i-1];

        if(val == C_FALSE) {
          AR_LOG_VM("jump-if-true not jumping " << jmp_offset);
        } else {
          AR_LOG_VM("jump-if-true jumping " << jmp_offset);
          VM_JUMP(jmp_offset);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_LOCAL_GET_0): {
        f.stack[f.stack_i++] = f.locals[0];
        VM_DISPATCH();
      }

      VM_CASE(OP_ARGC_EQ): {
        // Check that argc equals a specific number
        size_t eargc = VM_NEXT_INSN();

        AR_LOG_VM("argc-eq " << eargc);

        if(argc != eargc) {
          std::ostringstream os;
          os << "function " << f.fn << " expected exactly " << f.fn->min_arity
            << " arguments but got " << argc << std::endl;
          f.exception = eval_error(os.str());
          goto exception;
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_ARGC_GTE): {
        size_t eargc = VM_NEXT_INSN();

        if(argc < eargc) {
          std::ostringstream os;
          os << "function " << f.fn << " expected at least " << f.fn->min_arity << 
            " arguments but got " << argc << std::endl;
          f.exception = eval_error(os.str());
          goto exception;
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_ARGV_REST): {
        // argv may be &temps[0] if this is a tail call
        if(argv == &temps[0]) {
          f.locals[f.fn->max_arity] = temps_to_list(f.fn->max_arity);
        } else {
          temps.clear();
          unsigned i;
          for(i = f.fn->max_arity; i != argc; i++) {
            //std::cout << "Creating rest with argc i " << i  << ' ' << argv[i] << std::endl;
            temps.push_back(argv[i]);
          }

          f.locals[f.fn->max_arity] = temps_to_list();

          //std::cout << argv[f.fn->max_arity] << std::endl;
        }
        VM_RESTORE();
        VM_DISPATCH();
      }

      ///// OPEN CODED PRIMITIVES

      VM_CASE(OP_ADD): {
        size_t argc = VM_NEXT_INSN();
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

        // Return fixnum result
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

        VM_RESTORE();
        VM_DISPATCH();
      }

      VM_CASE(OP_SUB): {
        size_t argc = VM_NEXT_INSN();
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
              // VM ALLOCATION
              VM_RESTORE();
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
        // VM ALLOCATION
        VM_RESTORE();
        VM_DISPATCH();
      }

      VM_CASE(OP_LT): {
        size_t argc = VM_NEXT_INSN();
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

        if(AR_UNLIKELY(!idx.fixnump())) {
          VM_EXCEPTION("type", "vm primitive list-ref expected a fixnum as its second argument but got " << idx.type());
        }

        if(AR_UNLIKELY(!lst.heap_type_equals(PAIR))) {
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

      VM_CASE(OP_NOT): {
        f.stack[f.stack_i - 1] = f.stack[f.stack_i - 1] == C_FALSE ? C_TRUE : C_FALSE;
        VM_DISPATCH();
      }

      VM_CASE(OP_EQ): {
        f.stack[f.stack_i - 2] = Value::make_boolean(f.stack[f.stack_i - 2].bits == f.stack[f.stack_i - 1].bits);
        f.stack_i--;
        VM_DISPATCH();
      }

    #define VM_FX_CHECK(name) \
        if(!f.stack[f.stack_i - 2].fixnump() || !f.stack[f.stack_i - 1].fixnump()) { \
          VM_EXCEPTION("type", "vm primitive " name " expected a fixnum as its arguments but got " << f.stack[f.stack_i - 2].type() << ' ' << f.stack[f.stack_i - 1].type()) \
        }

      VM_CASE(OP_FX_LT): {
        VM_FX_CHECK("fx<");
        f.stack[f.stack_i-2] = Value::make_boolean(f.stack[f.stack_i-2].bits < f.stack[f.stack_i-1].bits);
        f.stack_i--;
        VM_DISPATCH();
      }

      VM_CASE(OP_FX_ADD): {
        VM_FX_CHECK("fx+");
        f.stack[f.stack_i-2] = Value::make_fixnum(f.stack[f.stack_i-1].fixnum_value() + f.stack[f.stack_i-2].fixnum_value());
        f.stack_i--;
        VM_DISPATCH();
      }

      VM_CASE(OP_FX_SUB): {
        VM_FX_CHECK("fx-");
        f.stack[f.stack_i-2] = Value::make_fixnum(f.stack[f.stack_i-2].fixnum_value() - f.stack[f.stack_i-1].fixnum_value());
        f.stack_i--;
        VM_DISPATCH();
      }

      VM_CASE(OP_BAD): {
#ifndef AR_COMPUTED_GOTO
      default:
#endif
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

    if(f.exception.exception_trace()) {
      trace_function(f.fn, frames_lost, (((size_t) cp) - (size_t)f.fn->code_pointer()) / sizeof(size_t));
    }

    AR_ASSERT(!f.destroyed);
    Value exc = f.exception;
    f.close_over();
    AR_ASSERT(exc.is_active_exception());
    return exc;
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
