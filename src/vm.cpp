// vm.cpp - Virtual machine

#ifndef NOMINMAX
# define NOMINMAX
#endif

#include <algorithm>

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

extern Value fn_apply(State& state, size_t argc, Value* argv, void* v);

/**
 * Like SourceLocation, but includes a code offset. Used by trace_function
 * regardless of which dispatch core is active.
 */
struct VMSourceLocation {
  unsigned code, source, line, begin, length;
};

#if !NATIVE_VM_ONLY
static Value vm_flatten_apply_arguments(State& state, size_t argc, Value* argv, Value& fn) {
  fn = argv[0];

  if(!fn.procedurep()) {
    return state.type_error("apply: argument 1 is not a procedure");
  }

  Value lst = argv[argc - 1];
  Value original_lst = lst;
  AR_FRAME(state, fn, lst, original_lst);

  size_t lst_length = (argc - 2) + original_lst.list_length();
  if(original_lst != C_NIL && lst_length == 0) {
    return state.type_error("apply: argument 2 is not a proper list");
  }

  state.temps.clear();
  if(argc > 2) {
    state.temps.insert(state.temps.end(), &argv[1], &argv[argc - 1]);
  }

  while(lst.heap_type_equals(PAIR)) {
    state.temps.push_back(lst.car());
    lst = lst.cdr();
  }

  AR_ASSERT(state.temps.size() == lst_length);
  return C_FALSE;
}

std::ostream& operator<<(std::ostream& os, const VMSourceLocation& loc) {
  os << "#<VMSourceLocation code-position: " << loc.code << ' ' <<
    "source: " << loc.source << " line: " << loc.line << " begin: " << loc.begin <<
    " length: " << loc.length << '>';
  return os;
}

/**
 * Close any still-open Boxes of the frame whose locals start at frame_base.
 * Mirrors the old ~VMFrame2 logic: copy the backing vm_stack slot into the
 * Box's `converted` field, innermost-first on unwind.
 */
static inline void vm_close_frame_boxes(State& state, VMFunction* vfn, size_t frame_base) {
  if(!vfn->box_count) return;
  Value* boxes = &state.gc.vm_stack[frame_base + vfn->local_count];
  for(size_t i = 0; i != vfn->box_count; i++) {
    if(!boxes[i].heap_type_equals(BOX) || boxes[i].box_closed()) {
      continue;
    }

    size_t idx = boxes[i].as_unsafe<Box>()->U.vm_local_idx;
    if(idx >= state.gc.vm_stack_used) {
      continue;
    }

    boxes[i].as_unsafe<Box>()->U.converted = state.gc.vm_stack[idx];
    boxes[i].heap->set_header_bit(Value::BOX_CLOSED_BIT);
  }
}

// Pointers to garbage-collected values (vfn, code, cp, cur_closure) are kept
// in locals so the compiler can put them in registers; the rooted copy lives
// in the top VMCallFrame record. They must be restored after anything that can
// allocate (GC moves the VMFunction and its bytecode).

// VM2_RESTORE_GC restores only GC'd pointers and is called after allocations.
// vm_stack itself is malloc'd and never moves during a collection, so locals/
// stack stay valid.

#define VM2_RESTORE_GC() { \
  size_t __cp_off = (size_t)(cp - code); \
  cur_closure = state.gc.vm_frames.back().closure; \
  vfn = cur_closure.closure_unbox().as_unsafe<VMFunction>(); \
  code = vfn->code_pointer(); \
  cp = code + __cp_off; }

// VM2_RESTORE additionally recomputes vm_stack pointers from offsets and is
// called after applications: a C callee can re-enter the VM and grow
// (realloc) vm_stack. stack_off must be saved with VM2_SAVE_STACK() before
// the call.

#define VM2_SAVE_STACK() stack_off = (size_t)(stack - state.gc.vm_stack)

#define VM2_RESTORE() { \
  VM2_RESTORE_GC(); \
  locals = &state.gc.vm_stack[sff_offset]; \
  stack = &state.gc.vm_stack[stack_off]; }

#define AR_LOG_VM2(msg) \
  if(((AR_LOG_TAGS & AR_LOG_TAG_VM) && (AR_VM_LOG_ALWAYS || vfn->get_header_bit(Value::VMFUNCTION_LOG_BIT)))) { \
    AR_LOG((AR_LOG_TAG_VM), "vm", msg); \
  }

extern Value compile_native(State&, Value);

Value apply_vm(State& state, size_t argc, Value* argv, void* fnp) {
  PerfScope perf_scope(state, PERF_VM, &state.perf.vm_calls);

  // Frames at or above this height belong to this C-level invocation
  // ("session"). OP_RETURN/unwind never pop past it; only the session-base
  // frame's return actually returns from this C function.
  const size_t session_base = state.gc.vm_frames.size();

  Value exception, return_value, cur_closure;

  // Registers for the active frame. The rooted copy of cur_closure lives in
  // the top VMCallFrame; vfn/code/cp must be re-derived after anything that
  // can GC (VM2_RESTORE_GC), and locals/stack after anything that can grow
  // vm_stack (VM2_RESTORE).
  VMFunction* vfn = 0;
  Value *locals = 0, *stack = 0;
  size_t *code = 0, *cp = 0;
  size_t sff_offset = 0, stack_off = 0;

  // Call linkage for the frame about to be entered, consumed by enter:.
  // link_frame_base == VM_FRESH_FRAME means "allocate above vm_stack_used";
  // otherwise it is the vm_stack offset the new frame's locals start at
  // (zero-copy calls: arg0's slot; tail calls: the recycled frame's base).
#define VM_FRESH_FRAME ((size_t) -1)
  size_t link_return_cp = 0, link_result_slot = 0;
  size_t link_frame_base = VM_FRESH_FRAME;
  uint32_t link_frames_lost = 0;
  bool link_is_base = true;

enter:
  AR_ASSERT(fnp != (void*) C_EOF); // for debugging native code
  AR_ASSERT(state.gc.live((HeapValue*) fnp));
  {
    VMFunction* callee_vfn = Value((HeapValue*) fnp).closure_unbox().as_unsafe<VMFunction>();
    size_t frame_slots = callee_vfn->local_count + callee_vfn->box_count + callee_vfn->stack_max;

    // Check the recursion limit before touching any state: on failure the
    // registers still describe the frame that issued the call, which is what
    // the unwind path needs. The +1 mirrors the old VMFrame2 ordering, which
    // counted the callee frame before checking.
    if(state.vm_depth + 1 >= state.recursion_limit) {
      std::ostringstream os;
      os << " non-tail recursive calls exceeded RECURSION-LIMIT (" <<
        state.recursion_limit << ')';
      exception = state.make_exception(State::S_EVAL_ERROR, os.str());
      if(state.gc.vm_frames.size() == session_base) return exception;
      goto exception;
    }

    // vm_stack_used is a session high-water mark: it never shrinks except at
    // session exit. Any slot below it is either zero or a valid Value that
    // every GC has kept updated, so growth (always into the entering frame's
    // fully-initialized region) is the only event that needs care.
    size_t frame_base;
    bool args_in_place = false;
    if(link_frame_base != VM_FRESH_FRAME) {
      frame_base = link_frame_base;
      args_in_place = (argv == &state.gc.vm_stack[frame_base]);
      if(args_in_place && argc > (size_t) callee_vfn->max_arity) {
        // The extra args would sit where the box/eval region goes. Stash
        // them in temps and take the copy path into the SAME base — falling
        // back to a fresh frame here would climb the high-water mark on
        // every over-arity (tail) call and leak stack space.
        state.temps.clear();
        state.temps.insert(state.temps.end(), argv, argv + argc);
        argv = state.temps.empty() ? nullptr : state.temps.data();
        args_in_place = false;
      }
    } else {
      frame_base = state.gc.vm_stack_used;
    }

    size_t extent = frame_base + frame_slots;
    if(extent > state.gc.vm_stack_used) {
      size_t old_used = state.gc.vm_stack_used;
      size_t delta = extent - old_used;
      // If argv points into the existing stack, it has to survive the realloc
      if(state.gc.stack_needs_realloc(delta) && ((size_t)argv >= (size_t)state.gc.vm_stack && (size_t)argv <= ((size_t) state.gc.vm_stack + (size_t)(state.gc.vm_stack_used * sizeof(void*))))) {
        size_t argv_offset = ((size_t)argv) - ((size_t) state.gc.vm_stack);

        state.gc.grow_stack(delta);

        argv = (Value*) ((((size_t) state.gc.vm_stack) + argv_offset));
      } else {
        state.gc.grow_stack(delta);
      }
      // Slots above the old high-water mark are realloc garbage, or stale
      // values a GC ran past while a nested session had shrunk the mark.
      // Everything below vm_stack_used must be scannable, so zero exactly
      // the newly included region.
      memset((void*) &state.gc.vm_stack[old_used], 0, delta * sizeof(Value*));
    }

    locals = &state.gc.vm_stack[frame_base];

    // Only the locals and box slots need initialization: the eval region is
    // write-before-read by the stack discipline, and every slot below the
    // high-water mark already holds a GC-scannable Value (zero or stale-but-
    // updated-by-every-collection).
    size_t frame_init = (size_t) callee_vfn->local_count + callee_vfn->box_count;
    if(args_in_place) {
      // Args are already the first locals; zero box slots and unsupplied
      // locals beyond them.
      if(frame_init > argc) {
        memset((void*) (locals + argc), 0, (frame_init - argc) * sizeof(Value*));
      }
    } else {
      size_t ncopy = std::min(argc, (size_t)callee_vfn->max_arity);
      // Initialize local variables
      memcpy(locals, argv, ncopy * sizeof(Value*));
      if(frame_init > ncopy) {
        memset((void*) (locals + ncopy), 0, (frame_init - ncopy) * sizeof(Value*));
      }
    }

    // The frame record roots the closure across the box allocations below and
    // across the whole frame lifetime.
    VMCallFrame rec;
    rec.closure = Value((HeapValue*) fnp);
    rec.frame_base = frame_base;
    rec.return_cp = link_return_cp;
    rec.result_slot = link_result_slot;
    rec.frames_lost = link_frames_lost;
    rec.flags = link_is_base ? (uint32_t) VMCallFrame::SESSION_BASE : 0;
    state.gc.vm_frames.push_back(rec);
    state.vm_depth++;

    sff_offset = frame_base;
    cur_closure = rec.closure;
    vfn = callee_vfn;

    // Allocate all Boxes needed for this frame's mutable captures. Each Box
    // starts "open" pointing at its backing local slot; when control exits
    // this function, the frame-pop logic closes any still-open Box into its
    // own `converted` field.
    if(vfn->box_count) {
      Value* boxes = &state.gc.vm_stack[sff_offset + vfn->local_count];
      state.gc.protect_argc = argc;
      state.gc.protect_argv = argv;

      for(size_t i = 0; i != vfn->free_variables->length; i++) {
        boxes[i] = state.gc.allocate(BOX, sizeof(Box));
        // The allocation may have moved the VMFunction; re-derive it from the
        // rooted record before touching it again.
        cur_closure = state.gc.vm_frames.back().closure;
        vfn = cur_closure.closure_unbox().as_unsafe<VMFunction>();
        AR_ASSERT(state.gc.live(cur_closure));
        AR_ASSERT(state.gc.live(boxes[i]));
        size_t idx = ((size_t*) vfn->free_variables->data)[i];

        boxes[i].as<Box>()->U.vm_local_idx = sff_offset + idx;
      }

      state.gc.protect_argc = 0;
    }

    code = vfn->code_pointer();
    cp = code;
    stack = &state.gc.vm_stack[sff_offset + vfn->local_count + vfn->box_count];
  }

#if AR_COMPUTED_GOTO
  static void* dispatch_table[] = {
    &&LABEL_OP_BAD, &&LABEL_OP_PUSH_CONSTANT, &&LABEL_OP_PUSH_IMMEDIATE, &&LABEL_OP_POP,
    &&LABEL_OP_GLOBAL_GET, &&LABEL_OP_GLOBAL_SET, &&LABEL_OP_LOCAL_GET, &&LABEL_OP_LOCAL_SET,
    &&LABEL_OP_BOX_GET, &&LABEL_OP_BOX_SET,
    &&LABEL_OP_CLOSE_OVER, &&LABEL_OP_BOX_FROM_LOCAL, &&LABEL_OP_BOX_FROM_CLOSURE,
    &&LABEL_OP_APPLY, &&LABEL_OP_APPLY_TAIL,
    &&LABEL_OP_RETURN,
    &&LABEL_OP_JUMP, &&LABEL_OP_JUMP_WHEN, &&LABEL_OP_JUMP_WHEN_POP, &&LABEL_OP_JUMP_UNLESS,
    &&LABEL_OP_ARGC_EQ, &&LABEL_OP_ARGC_GTE, &&LABEL_OP_ARGV_REST,
    // Extended instructions
    &&LABEL_OP_TYPE_CHECK, &&LABEL_OP_FIXNUMP,
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
    &&LABEL_OP_JUMP_IF_NOT_NIL,
    &&LABEL_OP_JUMP_IF_NIL,
    &&LABEL_OP_JUMP_IF_NOT_PAIR,
    &&LABEL_OP_JUMP_IF_PAIR,
    &&LABEL_OP_JUMP_IF_EQ_IMM,
    &&LABEL_OP_JUMP_IF_NOT_EQ_IMM,
    &&LABEL_OP_CADR,
    &&LABEL_OP_CDDR,
    &&LABEL_OP_CAAR,
    &&LABEL_OP_CDAR,
    &&LABEL_OP_CADDR,
    &&LABEL_OP_APPLY_GLOBAL,
    &&LABEL_OP_APPLY_TAIL_GLOBAL,
    &&LABEL_OP_APPLY_LOCAL,
    &&LABEL_OP_APPLY_TAIL_LOCAL,
    &&LABEL_OP_CAPTURE_FROM_LOCAL,
    &&LABEL_OP_CAPTURE_FROM_CLOSURE,
    &&LABEL_OP_CAPTURE_GET,
    &&LABEL_OP_VALUE_TYPE,
    &&LABEL_OP_VALUE_HEADER_BIT,
  };
#endif


#define STACK_PICK(i) (*(stack - (i)))

#if !AR_COMPUTED_GOTO
  dispatch_top:
#endif
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
        stack--;
        VM_DISPATCH();
      }

      VM_CASE(OP_GLOBAL_GET): {
        size_t idx = VM_NEXT_INSN();
        Value global = vfn->constants->data[idx];
        Value value = global.symbol_value();
        if(value == C_UNDEFINED) {
          VM2_EXCEPTION("eval", "reference to undefined variable " << global);
        }
        (*stack++) = value;
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

      VM_CASE(OP_BOX_GET): {
        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM2("box-get " << idx);
        Value slot = cur_closure.as_unsafe<Closure>()->captures->data[idx];
        if(!slot.heap_type_equals(BOX)) {
          (*stack++) = slot;
          VM_DISPATCH();
        }

        Box* box = slot.as_unsafe<Box>();
        if(box->get_header_bit(Value::BOX_CLOSED_BIT)) {
          (*stack++) = box->U.converted;
        } else {
          if(box->get_header_bit(Value::BOX_POINTER_BIT)) {
            (*stack++) = box->U.local->bits;
          } else {
            (*stack++) = state.gc.vm_stack[box->U.vm_local_idx];
          }
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_BOX_SET): {
        size_t idx = VM_NEXT_INSN();
        Value val = (*--stack);
        Value slot = cur_closure.as_unsafe<Closure>()->captures->data[idx];
        if(!slot.heap_type_equals(BOX)) {
          cur_closure.as_unsafe<Closure>()->captures->data[idx] = val;
          AR_LOG_VM2("box-set " << idx << " = " << val);
          VM_DISPATCH();
        }

        Box* box = slot.as_unsafe<Box>();
        if(box->get_header_bit(Value::BOX_CLOSED_BIT)) {
          box->U.converted = val;
        } else {
          state.gc.vm_stack[box->U.vm_local_idx] = val;
        }
        AR_LOG_VM2("box-set " << idx << " = " << val);
        VM_DISPATCH();
      }

      VM_CASE(OP_CLOSE_OVER): {
        size_t capture_count = VM_NEXT_INSN();
        AR_LOG_VM2("close-over " << capture_count);

        {
          Value storage, closure;
          AR_FRAME(state, storage, closure);
          storage = state.make_vector_storage(capture_count);
          // A slot may hold a Box (mutable) or a raw Value (display capture).
          for(size_t i = capture_count; i != 0; i--) {
            state.vector_storage_append(storage, STACK_PICK(i));
          }
          closure = state.gc.allocate(CLOSURE, sizeof(Closure));
          closure.as_unsafe<Closure>()->captures = storage.as<VectorStorage>();
          stack -= capture_count;
          closure.as_unsafe<Closure>()->function = *(stack - 1);
          // We have to extract procedure_addr because this function may be native-compiled
          closure.procedure_install(*(stack-1)->as_unsafe<Procedure>()->procedure_addr);

          AR_ASSERT(closure.as_unsafe<Closure>()->function.heap_type_equals(VMFUNCTION));
          AR_ASSERT(closure.heap_type_equals(CLOSURE));
          AR_ASSERT((*(stack - 1)).heap_type_equals(VMFUNCTION));

          AR_ASSERT(state.gc.live(closure.heap));
          AR_ASSERT(state.gc.live((*(stack - 1)).heap));

          *(stack - 1) = closure;
        }

        VM2_RESTORE_GC();
        VM_DISPATCH();  
      }

      VM_CASE(OP_BOX_FROM_CLOSURE): {
        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM2("box-from-closure " << idx);
        (*stack++) = cur_closure.as<Closure>()->captures->data[idx];
        AR_ASSERT((*(stack - 1)).heap_type_equals(BOX));
        VM_DISPATCH();
      }

      VM_CASE(OP_BOX_FROM_LOCAL): {
        size_t idx = VM_NEXT_INSN();
        (*stack++) = state.gc.vm_stack[sff_offset + vfn->local_count + idx];
        AR_LOG_VM2("box-from-local " << idx << ' ' << *(stack - 1));
        AR_ASSERT((*(stack - 1)).heap_type_equals(BOX));
        VM_DISPATCH();
      }

      VM_CASE(OP_CAPTURE_FROM_LOCAL): {
        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM2("capture-from-local " << idx);
        (*stack++) = locals[idx];
        VM_DISPATCH();
      }

      // FROM_CLOSURE (emitted in parent at close-over) and GET (emitted in
      // child at access) both read `current_closure->captures[idx]` raw. The
      // distinction is which closure is current at the time — two opcodes
      // for one code path keeps disassembly legible.
      VM_CASE(OP_CAPTURE_FROM_CLOSURE):
      VM_CASE(OP_CAPTURE_GET): {
        size_t idx = VM_NEXT_INSN();
        AR_LOG_VM2("capture-from-closure/get " << idx);
        (*stack++) = cur_closure.as_unsafe<Closure>()->captures->data[idx];
        VM_DISPATCH();
      }

      VM_CASE(OP_APPLY): {
        size_t fargc = VM_NEXT_INSN();
        Value afn = *(stack - (fargc + 1));
        AR_LOG_VM2("apply " << fargc << " " << afn);
        if(afn.procedurep()) {
          if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
            // Flat Scheme->Scheme call: push a frame record and continue in
            // this dispatch loop. No C recursion.
            if(state.perf_report_enabled) state.perf.vm_calls++;
            link_return_cp = (size_t)(cp - code);
            link_result_slot = (size_t)((stack - (fargc + 1)) - state.gc.vm_stack);
            link_frame_base = (size_t)((stack - fargc) - state.gc.vm_stack);
            link_frames_lost = 0;
            link_is_base = false;
            argc = fargc;
            argv = stack - fargc;
            fnp = (void*) afn.bits;
            goto enter;
          }
          VM2_SAVE_STACK();
          Value ret =  afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc, afn.heap);
          VM2_RESTORE();
          *(stack - (fargc + 1)) = ret;
          stack -= fargc;


          if((*(stack-1)).is_active_exception()) {
            exception = *(stack-1);
            goto exception;
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value " << afn);
        }

        VM_DISPATCH();
      }

      VM_CASE(OP_APPLY_TAIL): {
        size_t fargc = VM_NEXT_INSN();
        size_t apply_fargc = fargc;

        Value afn = *(stack - fargc - 1);
        Value to_apply = afn;
        AR_LOG_VM2("apply-tail fargc " << fargc << " fn: " << afn);

        if(afn.procedurep()) {
          if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
            // Flat tail call: recycle this frame's record (return linkage is
            // inherited, frames_lost bumped) and re-enter with the callee.
            vm_close_frame_boxes(state, vfn, sff_offset);
            {
              VMCallFrame& top = state.gc.vm_frames.back();
              link_return_cp = top.return_cp;
              link_result_slot = top.result_slot;
              link_frames_lost = top.frames_lost + 1;
              link_is_base = (top.flags & VMCallFrame::SESSION_BASE) != 0;
              link_frame_base = top.frame_base;
            }
            state.gc.vm_frames.pop_back();
            state.vm_depth--;
            // Slide the args down onto the recycled frame's base (regions can
            // overlap); they become the callee's first locals in place.
            memmove(&state.gc.vm_stack[link_frame_base], stack - fargc, fargc * sizeof(Value));

            argv = &state.gc.vm_stack[link_frame_base];
            argc = fargc;
            fnp = (void*) to_apply.bits;

            goto enter;
          } else {
            Value ret;
            if(afn.heap_type_equals(CFUNCTION) &&
               afn.as_unsafe<Procedure>()->procedure_addr == &fn_apply) {
              Value flat_error = vm_flatten_apply_arguments(state, fargc, stack - fargc, to_apply);
              if(flat_error != C_FALSE) {
                exception = flat_error;
                goto exception;
              }

              afn = to_apply;
              fargc = state.temps.size();
              Value* flat_argv = state.temps.empty() ? nullptr : state.temps.data();

              if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
                vm_close_frame_boxes(state, vfn, sff_offset);
                {
                  VMCallFrame& top = state.gc.vm_frames.back();
                  link_return_cp = top.return_cp;
                  link_result_slot = top.result_slot;
                  link_frames_lost = top.frames_lost + 1;
                  link_is_base = (top.flags & VMCallFrame::SESSION_BASE) != 0;
                  link_frame_base = top.frame_base;
                }
                state.gc.vm_frames.pop_back();
                state.vm_depth--;

                argv = flat_argv;
                argc = fargc;
                fnp = (void*) to_apply.bits;

                goto enter;
              }

              VM2_SAVE_STACK();
              ret = afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, flat_argv,
                (void*) to_apply.heap);
            } else {
              VM2_SAVE_STACK();
              ret = afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc,
                (void*) to_apply.heap);
            }
            VM2_RESTORE();
            *(stack - (apply_fargc + 1)) = ret;
            stack -= apply_fargc;

            if((*(stack-1)).is_active_exception()) {
              exception = *(stack-1);
              goto exception;
            }
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value " << afn);
        }
        return_value = *(stack - 1);
        goto do_return;
      }

      // Exp 8: fused callee-load + apply. Operands: {callee-source-idx, argc}.
      // Callee is read directly (no operand-stack push) from its source:
      //   OP_APPLY_{_TAIL_}GLOBAL -> vfn->constants[idx].symbol_value()
      //   OP_APPLY_{_TAIL_}LOCAL  -> locals[idx]
      // Semantics otherwise identical to OP_APPLY / OP_APPLY_TAIL.

      VM_CASE(OP_APPLY_GLOBAL): {
        size_t idx = VM_NEXT_INSN();
        size_t fargc = VM_NEXT_INSN();
        Value global = vfn->constants->data[idx];
        Value afn = global.symbol_value();
        if(afn == C_UNDEFINED) {
          VM2_EXCEPTION("eval", "reference to undefined variable " << global);
        }
        AR_LOG_VM2("apply-global " << fargc << " " << afn);
        if(afn.procedurep()) {
          if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
            // No fn slot on the operand stack for the fused variants: the
            // result lands where arg0 sits.
            if(state.perf_report_enabled) state.perf.vm_calls++;
            link_return_cp = (size_t)(cp - code);
            link_result_slot = (size_t)((stack - fargc) - state.gc.vm_stack);
            link_frame_base = (size_t)((stack - fargc) - state.gc.vm_stack);
            link_frames_lost = 0;
            link_is_base = false;
            argc = fargc;
            argv = stack - fargc;
            fnp = (void*) afn.bits;
            goto enter;
          }
          VM2_SAVE_STACK();
          Value ret =  afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc, afn.heap);
          VM2_RESTORE();
          *(stack - fargc) = ret;
          stack -= (fargc - 1);

          if((*(stack-1)).is_active_exception()) {
            exception = *(stack-1);
            goto exception;
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value " << afn);
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_APPLY_TAIL_GLOBAL): {
        size_t idx = VM_NEXT_INSN();
        size_t fargc = VM_NEXT_INSN();
        size_t apply_fargc = fargc;

        Value global = vfn->constants->data[idx];
        Value afn = global.symbol_value();
        if(afn == C_UNDEFINED) {
          VM2_EXCEPTION("eval", "reference to undefined variable " << global);
        }
        Value to_apply = afn;
        AR_LOG_VM2("apply-tail-global fargc " << fargc << " fn: " << afn);

        if(afn.procedurep()) {
          if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
            vm_close_frame_boxes(state, vfn, sff_offset);
            {
              VMCallFrame& top = state.gc.vm_frames.back();
              link_return_cp = top.return_cp;
              link_result_slot = top.result_slot;
              link_frames_lost = top.frames_lost + 1;
              link_is_base = (top.flags & VMCallFrame::SESSION_BASE) != 0;
              link_frame_base = top.frame_base;
            }
            state.gc.vm_frames.pop_back();
            state.vm_depth--;
            // Slide the args down onto the recycled frame's base (regions can
            // overlap); they become the callee's first locals in place.
            memmove(&state.gc.vm_stack[link_frame_base], stack - fargc, fargc * sizeof(Value));
            argv = &state.gc.vm_stack[link_frame_base];
            argc = fargc;
            fnp = (void*) to_apply.bits;
            goto enter;
          } else {
            Value ret;
            if(afn.heap_type_equals(CFUNCTION) &&
               afn.as_unsafe<Procedure>()->procedure_addr == &fn_apply) {
              Value flat_error = vm_flatten_apply_arguments(state, fargc, stack - fargc, to_apply);
              if(flat_error != C_FALSE) {
                exception = flat_error;
                goto exception;
              }

              afn = to_apply;
              fargc = state.temps.size();
              Value* flat_argv = state.temps.empty() ? nullptr : state.temps.data();

              if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
                vm_close_frame_boxes(state, vfn, sff_offset);
                {
                  VMCallFrame& top = state.gc.vm_frames.back();
                  link_return_cp = top.return_cp;
                  link_result_slot = top.result_slot;
                  link_frames_lost = top.frames_lost + 1;
                  link_is_base = (top.flags & VMCallFrame::SESSION_BASE) != 0;
                  link_frame_base = top.frame_base;
                }
                state.gc.vm_frames.pop_back();
                state.vm_depth--;
                argv = flat_argv;
                argc = fargc;
                fnp = (void*) to_apply.bits;
                goto enter;
              }

              VM2_SAVE_STACK();
              ret = afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, flat_argv,
                (void*) to_apply.heap);
            } else {
              VM2_SAVE_STACK();
              ret = afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc,
                (void*) to_apply.heap);
            }
            VM2_RESTORE();
            *(stack - apply_fargc) = ret;
            stack -= (apply_fargc - 1);
            if((*(stack-1)).is_active_exception()) {
              exception = *(stack-1);
              goto exception;
            }
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value " << afn);
        }
        return_value = *(stack - 1);  // tail-call from CFUNCTION path returns top-of-stack
        goto do_return;
      }

      VM_CASE(OP_APPLY_LOCAL): {
        size_t idx = VM_NEXT_INSN();
        size_t fargc = VM_NEXT_INSN();
        Value afn = locals[idx];
        AR_LOG_VM2("apply-local " << fargc << " " << afn);
        if(afn.procedurep()) {
          if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
            if(state.perf_report_enabled) state.perf.vm_calls++;
            link_return_cp = (size_t)(cp - code);
            link_result_slot = (size_t)((stack - fargc) - state.gc.vm_stack);
            link_frame_base = (size_t)((stack - fargc) - state.gc.vm_stack);
            link_frames_lost = 0;
            link_is_base = false;
            argc = fargc;
            argv = stack - fargc;
            fnp = (void*) afn.bits;
            goto enter;
          }
          VM2_SAVE_STACK();
          Value ret =  afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc, afn.heap);
          VM2_RESTORE();
          *(stack - fargc) = ret;
          stack -= (fargc - 1);

          if((*(stack-1)).is_active_exception()) {
            exception = *(stack-1);
            goto exception;
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value " << afn);
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_APPLY_TAIL_LOCAL): {
        size_t idx = VM_NEXT_INSN();
        size_t fargc = VM_NEXT_INSN();
        size_t apply_fargc = fargc;

        Value afn = locals[idx];
        Value to_apply = afn;
        AR_LOG_VM2("apply-tail-local fargc " << fargc << " fn: " << afn);

        if(afn.procedurep()) {
          if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
            vm_close_frame_boxes(state, vfn, sff_offset);
            {
              VMCallFrame& top = state.gc.vm_frames.back();
              link_return_cp = top.return_cp;
              link_result_slot = top.result_slot;
              link_frames_lost = top.frames_lost + 1;
              link_is_base = (top.flags & VMCallFrame::SESSION_BASE) != 0;
              link_frame_base = top.frame_base;
            }
            state.gc.vm_frames.pop_back();
            state.vm_depth--;
            // Slide the args down onto the recycled frame's base (regions can
            // overlap); they become the callee's first locals in place.
            memmove(&state.gc.vm_stack[link_frame_base], stack - fargc, fargc * sizeof(Value));
            argv = &state.gc.vm_stack[link_frame_base];
            argc = fargc;
            fnp = (void*) to_apply.bits;
            goto enter;
          } else {
            Value ret;
            if(afn.heap_type_equals(CFUNCTION) &&
               afn.as_unsafe<Procedure>()->procedure_addr == &fn_apply) {
              Value flat_error = vm_flatten_apply_arguments(state, fargc, stack - fargc, to_apply);
              if(flat_error != C_FALSE) {
                exception = flat_error;
                goto exception;
              }

              afn = to_apply;
              fargc = state.temps.size();
              Value* flat_argv = state.temps.empty() ? nullptr : state.temps.data();

              if(afn.as_unsafe<Procedure>()->procedure_addr == &apply_vm) {
                vm_close_frame_boxes(state, vfn, sff_offset);
                {
                  VMCallFrame& top = state.gc.vm_frames.back();
                  link_return_cp = top.return_cp;
                  link_result_slot = top.result_slot;
                  link_frames_lost = top.frames_lost + 1;
                  link_is_base = (top.flags & VMCallFrame::SESSION_BASE) != 0;
                  link_frame_base = top.frame_base;
                }
                state.gc.vm_frames.pop_back();
                state.vm_depth--;
                argv = flat_argv;
                argc = fargc;
                fnp = (void*) to_apply.bits;
                goto enter;
              }

              VM2_SAVE_STACK();
              ret = afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, flat_argv,
                (void*) to_apply.heap);
            } else {
              VM2_SAVE_STACK();
              ret = afn.as_unsafe<Procedure>()->procedure_addr(state, fargc, stack - fargc,
                (void*) to_apply.heap);
            }
            VM2_RESTORE();
            *(stack - apply_fargc) = ret;
            stack -= (apply_fargc - 1);
            if((*(stack-1)).is_active_exception()) {
              exception = *(stack-1);
              goto exception;
            }
          }
        } else {
          VM2_EXCEPTION("eval", "vm: attempt to apply non-applicable value " << afn);
        }
        return_value = *(stack - 1);
        goto do_return;
      }

      VM_CASE(OP_RETURN): {
        return_value = *(stack - 1);
        goto do_return;
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

      // Fused null?+jump: pop value, jump if it is NOT nil.
      // Emitted by compile-if for (if (null? X) then else).
      VM_CASE(OP_JUMP_IF_NOT_NIL): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value v = *--stack;
        if(v != C_NIL) {
          VM_JUMP(jmp_offset);
        }
        VM_DISPATCH();
      }

      // Fused (not (null? X))+jump: pop value, jump if it IS nil.
      // Emitted by compile-if for (if (not (null? X)) then else), which is
      // what unless expands to for a null? test.
      VM_CASE(OP_JUMP_IF_NIL): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value v = *--stack;
        if(v == C_NIL) {
          VM_JUMP(jmp_offset);
        }
        VM_DISPATCH();
      }

      // Fused pair?+jump: pop value, jump if it is NOT a pair.
      // Emitted by compile-if for (if (pair? X) then else).
      VM_CASE(OP_JUMP_IF_NOT_PAIR): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value v = *--stack;
        if(!v.heap_type_equals(PAIR)) {
          VM_JUMP(jmp_offset);
        }
        VM_DISPATCH();
      }

      // Fused (not (pair? X))+jump: pop value, jump if it IS a pair.
      // Emitted by compile-if for (if (not (pair? X)) then else), which is
      // what unless expands to for a pair? test.
      VM_CASE(OP_JUMP_IF_PAIR): {
        size_t jmp_offset = VM_NEXT_INSN();
        Value v = *--stack;
        if(v.heap_type_equals(PAIR)) {
          VM_JUMP(jmp_offset);
        }
        VM_DISPATCH();
      }

      // Fused eq?-to-safe-immediate-literal + jump (exp 3d).
      // JUMP_IF_NOT_EQ_IMM: operand = {const-idx, jmp-target}. Pops value,
      // jumps iff popped value is NOT eq? to constants[idx]. Replaces the
      // (push-constant idx; eq?; jump-when-pop L) sequence after (if (eq? X 'LIT) ...).
      VM_CASE(OP_JUMP_IF_NOT_EQ_IMM): {
        size_t idx = VM_NEXT_INSN();
        size_t jmp_offset = VM_NEXT_INSN();
        Value lit = vfn->constants->data[idx];
        Value v = *--stack;
        if(v != lit) {
          VM_JUMP(jmp_offset);
        }
        VM_DISPATCH();
      }

      // JUMP_IF_EQ_IMM: as above but jump iff IS eq?. Emitted for
      // (if (not (eq? X 'LIT)) ...).
      VM_CASE(OP_JUMP_IF_EQ_IMM): {
        size_t idx = VM_NEXT_INSN();
        size_t jmp_offset = VM_NEXT_INSN();
        Value lit = vfn->constants->data[idx];
        Value v = *--stack;
        if(v == lit) {
          VM_JUMP(jmp_offset);
        }
        VM_DISPATCH();
      }


      VM_CASE(OP_ARGC_EQ): {
        size_t eargc = VM_NEXT_INSN();
        AR_LOG_VM2("argc-eq " << eargc);
        if(argc != eargc) {
          VM2_EXCEPTION("eval", "function expected exactly " << eargc << " arguments but got " << argc);
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_ARGC_GTE): {
        size_t eargc = VM_NEXT_INSN();

        AR_LOG_VM2("argc-gte " << eargc);

        if(argc < eargc) {
          VM2_EXCEPTION("eval", "function expected at least " << eargc << " arguments but got " << argc);
        }
        VM_DISPATCH();
      }

      VM_CASE(OP_ARGV_REST): {
        // Snapshot max_arity before any allocation: temps_to_list() can
        // trigger a GC that moves the VMFunction, invalidating `vfn`.
        // (Assignment RHS is sequenced before LHS in C++17, so reading
        // vfn->max_arity on the LHS would use a stale pointer.)
        size_t max_arity = vfn->max_arity;
        if(argc <= max_arity) {
          locals[max_arity] = C_NIL;
        } else {
          // argv may point into temps if this is a tail call
          Value lst;
          if(!state.temps.empty() && argv == state.temps.data()) {
            lst = state.temps_to_list(max_arity);
          } else {
            state.temps.clear();
            state.temps.insert(state.temps.end(), &argv[max_arity], &argv[argc]);
            lst = state.temps_to_list();
          }
          locals[max_arity] = lst;
        }
        VM2_RESTORE_GC();
        VM_DISPATCH();
      }

      VM_CASE(OP_TYPE_CHECK): {
        Value s(*(stack - 1));
        size_t tipe = VM_NEXT_INSN();
        *(stack - 1) = s.type() == tipe;
        VM_DISPATCH();
      }

      VM_CASE(OP_FIXNUMP): {
        *(stack - 1) = Value::make_boolean((*(stack - 1)).fixnump());
        VM_DISPATCH();
      }

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
  if(!STACK_PICK(1).heap_type_equals(PAIR)) { \
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

      // Exp 7: fused composite pair accessors. Each performs 2 (or 3 for
      // CADDR) PAIR-type-checked dereferences in a single opcode dispatch.
      // Error behavior matches the equivalent OP_CAR/OP_CDR sequence: the
      // first non-pair intermediate value raises the same type-error the
      // corresponding single op would, named after the step that tripped.
      VM_CASE(OP_CADR): {
        // (car (cdr X)): cdr step first, then car step.
        VM_TOP_PAIR_CHECK("cdr");
        Value v = STACK_PICK(1).cdr();
        if(!v.heap_type_equals(PAIR)) {
          VM2_EXCEPTION("type", "vm primitive car expected a pair as its argument but got " << v.type());
        }
        *(stack - 1) = v.car();
        VM_DISPATCH();
      }

      VM_CASE(OP_CDDR): {
        // (cdr (cdr X))
        VM_TOP_PAIR_CHECK("cdr");
        Value v = STACK_PICK(1).cdr();
        if(!v.heap_type_equals(PAIR)) {
          VM2_EXCEPTION("type", "vm primitive cdr expected a pair as its argument but got " << v.type());
        }
        *(stack - 1) = v.cdr();
        VM_DISPATCH();
      }

      VM_CASE(OP_CAAR): {
        // (car (car X))
        VM_TOP_PAIR_CHECK("car");
        Value v = STACK_PICK(1).car();
        if(!v.heap_type_equals(PAIR)) {
          VM2_EXCEPTION("type", "vm primitive car expected a pair as its argument but got " << v.type());
        }
        *(stack - 1) = v.car();
        VM_DISPATCH();
      }

      VM_CASE(OP_CDAR): {
        // (cdr (car X))
        VM_TOP_PAIR_CHECK("car");
        Value v = STACK_PICK(1).car();
        if(!v.heap_type_equals(PAIR)) {
          VM2_EXCEPTION("type", "vm primitive cdr expected a pair as its argument but got " << v.type());
        }
        *(stack - 1) = v.cdr();
        VM_DISPATCH();
      }

      VM_CASE(OP_CADDR): {
        // (car (cdr (cdr X))) — three derefs in one dispatch.
        VM_TOP_PAIR_CHECK("cdr");
        Value v1 = STACK_PICK(1).cdr();
        if(!v1.heap_type_equals(PAIR)) {
          VM2_EXCEPTION("type", "vm primitive cdr expected a pair as its argument but got " << v1.type());
        }
        Value v2 = v1.cdr();
        if(!v2.heap_type_equals(PAIR)) {
          VM2_EXCEPTION("type", "vm primitive car expected a pair as its argument but got " << v2.type());
        }
        *(stack - 1) = v2.car();
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

      VM_CASE(OP_VALUE_TYPE): {
        *(stack - 1) = Value::make_fixnum((ptrdiff_t) STACK_PICK(1).type());
        VM_DISPATCH();
      }

      VM_CASE(OP_VALUE_HEADER_BIT): {
        Value bit = STACK_PICK(1);
        Value v = STACK_PICK(2);
        if(!bit.fixnump()) {
          VM2_EXCEPTION("type", "vm primitive value-header-bit? expected a fixnum bit index");
        }
        if(v.immediatep()) {
          VM2_EXCEPTION("eval", "value-header bit got immediate object");
        }
        *(stack - 2) = Value::make_boolean(
            v.heap->get_header_bit(1u << (unsigned) bit.fixnum_value()));
        stack--;
        VM_DISPATCH();
      }
    }

  }

  do_return:
  {
    // Pop the active frame: close its boxes, release its vm_stack region.
    vm_close_frame_boxes(state, vfn, sff_offset);

    VMCallFrame fr = state.gc.vm_frames.back();
    state.gc.vm_frames.pop_back();
    state.vm_depth--;

    if(fr.flags & VMCallFrame::SESSION_BASE) {
      // Control returns to the C caller of this apply_vm invocation. Session
      // exit is the only point vm_stack_used shrinks: everything at or above
      // this frame's base is dead, and the base equals the outer session's
      // high-water mark (or 0), so the outer invariant is restored exactly.
      state.gc.vm_stack_used = fr.frame_base;
      return return_value;
    }

    // Resume the calling frame inside this dispatch loop.
    cur_closure = state.gc.vm_frames.back().closure;
    vfn = cur_closure.closure_unbox().as_unsafe<VMFunction>();
    sff_offset = state.gc.vm_frames.back().frame_base;
    locals = &state.gc.vm_stack[sff_offset];
    code = vfn->code_pointer();
    cp = code + fr.return_cp;
    state.gc.vm_stack[fr.result_slot] = return_value;
    stack = &state.gc.vm_stack[fr.result_slot + 1];

    // Match the call-site exception check the C path performs on results.
    if(return_value.is_active_exception()) {
      exception = return_value;
      goto exception;
    }
  }
#if AR_COMPUTED_GOTO
  VM_DISPATCH();
#else
  goto dispatch_top;
#endif

  exception:
  {
    // Unwind this session's frames innermost-first, closing boxes and
    // emitting one stack-trace entry per frame. The top frame traces at the
    // current instruction; parent frames trace at their call sites.
    size_t trace_off = (size_t)(cp - code);
    while(true) {
      VMCallFrame fr = state.gc.vm_frames.back();
      Value frame_fn = fr.closure.closure_unbox();

      if(exception.exception_trace()) {
        state.trace_function(frame_fn, fr.frames_lost, trace_off);
      }

      vm_close_frame_boxes(state, frame_fn.as_unsafe<VMFunction>(), fr.frame_base);
      state.gc.vm_frames.pop_back();
      state.vm_depth--;

      if(fr.flags & VMCallFrame::SESSION_BASE) {
        // See do_return: vm_stack_used only shrinks at session exit.
        state.gc.vm_stack_used = fr.frame_base;
        return exception;
      }

      trace_off = fr.return_cp;
    }
  }
}
#else
// NATIVE_VM_ONLY=1: the C++ bytecode interpreter is dead code. apply_vm stays
// as a linkable symbol that aborts if reached — that means a VMFunction's
// procedure_addr was left pointing here (coverage bug, not a fallback).
Value apply_vm(State& state, size_t argc, Value* argv, void* fnp) {
  (void) state; (void) argc; (void) argv; (void) fnp;
  AR_ASSERT(0 && "apply_vm called in NATIVE_VM_ONLY build");
  std::abort();
}
#endif // NATIVE_VM_ONLY

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
