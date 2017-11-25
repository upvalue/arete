// builtins.cpp - builtin functionality

#include <algorithm>
#include <fstream>
#include <iostream>

#include "arete.hpp"

namespace arete {

// TODO: Thread safety.

DefunGroup builtins("builtins");

// Equality

Value fn_eq(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "eq?";
  return Value::make_boolean(argv[0].bits == argv[1].bits);
}
AR_DEFUN("eq?", fn_eq, 2);

Value fn_eqv(State& state, size_t argc, Value* argv) {
  if(argv[0].type() == FLONUM && argv[1].type() == FLONUM) {
    return Value::make_boolean(argv[0].flonum_value() == argv[1].flonum_value());
  }

  if(argv[0].type() == CHARACTER && argv[1].type() == CHARACTER) {
    return Value::make_boolean(argv[0].character() == argv[1].character());
  }

  return fn_eq(state, argc, argv);
}
AR_DEFUN("eqv?", fn_eqv, 2);

Value fn_equal(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(state.equals(argv[0], argv[1]));
}
AR_DEFUN("equal?", fn_equal, 2);

///// PREDICATES

Value fn_procedurep(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].procedurep());
}
AR_DEFUN("procedure?", fn_procedurep, 1);

Value fn_value_type(State& state, size_t argc, Value* argv) {
  return Value::make_fixnum(argv[0].type());
}
AR_DEFUN("value-type", fn_value_type, 1);

Value fn_macrop(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == FUNCTION && argv[0].function_is_macro());
}
AR_DEFUN("macro?", fn_macrop, 1);

//
///// LISTS
//

Value fn_cons(State& state, size_t argc, Value* argv) {
  return state.make_pair(argv[0], argv[1]);
}
AR_DEFUN("cons", fn_cons, 2);

/** cons that copies source information from a given expression */
Value fn_cons_source(State& state, size_t argc, Value* argv) {
  const char* fn_name = "cons-source";
  Value pare, src, kar, kdr;
  AR_FRAME(state, pare, kar, kdr);
  src = argv[0];
  Type type = src.type();

  if(type == PAIR && !src.pair_has_source()) {
    return state.make_pair(argv[1], argv[2]);
  }

  AR_FN_ASSERT_ARG(state, 0, "to be a pair with source information",
    (type == PAIR && src.pair_has_source()));
  kar = argv[1];
  kdr = argv[2];
  SourceLocation loc;
  if(type == PAIR) {
    loc = (src.pair_src());
  } 
  pare = state.make_src_pair(kar, kdr, loc);
  return pare;
}
AR_DEFUN("cons-source", fn_cons_source, 3);

/** Create a list from arguments. Somewhat complex due to the need to GC track
 * variables, and handle the transfer of source code information for the macroexpander. */
static Value fn_list_impl(State& state, size_t argc, Value* argv, bool copy_source) {
  Value lst = C_NIL;
  AR_FRAME(state, lst);
  state.temps.clear();

  size_t list_begin = copy_source ? 1 : 0;
  SourceLocation loc;

  if(copy_source) {
    if(argv[0].type() == PAIR && argv[0].pair_has_source()) {
      loc = argv[0].pair_src();
    } else {
      copy_source = false;
    }
  }


  for(size_t i = list_begin; i < argc; i++) {
    state.temps.push_back(argv[i]);
  }

  while(argc > list_begin) {
    lst = state.make_pair(state.temps[--argc - list_begin], lst);
  }

  if(argc > list_begin) {
    if(copy_source) {
      lst = state.make_pair(state.temps[0], lst);//, loc);
    } else {
      lst = state.make_pair(state.temps[0], lst);
    }
  }

  return lst;
}

#if 0
// TODO REMOVE
Value fn_list(State& state, size_t argc, Value* argv) {
  return fn_list_impl(state, argc, argv, false);
}
AR_DEFUN("list", fn_list, 0, 0, true);
#endif

Value fn_list_source(State& state, size_t argc, Value* argv) {
  return fn_list_impl(state, argc, argv, true);
}
AR_DEFUN("list-source", fn_list_source, 1, 1, true);

// (cons* 1) => 1
// (cons* 1 2 3) => (1 2 . 3)
Value fn_cons_star(State& state, size_t argc, Value* argv) {
  //static const char* fn_name = "cons*";
  if(argc == 1) return argv[0];

  state.temps.clear();
  for(size_t i = 0; i != argc - 1; i++) {
    state.temps.push_back(argv[i]);
  }

  Value lst, elt;
  AR_FRAME(state, lst, elt);
  elt = argv[argc-1];
  for(size_t i = state.temps.size(); i != 0; i--) {
    lst = state.make_pair(state.temps[i-1], elt);
    elt = lst;
  }
  return lst;
}
AR_DEFUN("cons*", fn_cons_star, 1, 1, true);

/** Returns the length of a list */
Value fn_length(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "length";
  if(argv[0] == C_NIL)
    return Value::make_fixnum(0);

  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);

  size_t length = 0;

  Value next = argv[0];
  while(true) {
    next = next.cdr();
    length++;
    if(next == C_NIL) break;
    if(!next.heap_type_equals(PAIR)) {
      return state.type_error("length got a dotted list as its argument");
    }
  }

  return Value::make_fixnum(length);
}
AR_DEFUN("length", fn_length, 1);

Value fn_listp(State& state, size_t argc, Value* argv) {
  // return argv[0] == C_NIL || (argv[0].type() == PAIR && argv[0].list_length() > 
  if(argv[0] == C_NIL) return C_TRUE;
  while(argv[0].heap_type_equals(PAIR)) {
    if(argv[0].cdr() == C_NIL) return C_TRUE;
    argv[0] = argv[0].cdr();
  }
  return C_FALSE;
}
AR_DEFUN("list?", fn_listp, 1);

Value fn_list_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "list-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_ASSERT_ARG(state, 1, "to be a positive number", argv[1].fixnum_value() >= 0);

  Value h = argv[0];
  size_t idx = argv[1].fixnum_value();
  
  while(idx--) {
    if(!h.cdr().heap_type_equals(PAIR)) {
      return state.type_error("list-ref ran into a dotted list");
    }
    h = h.cdr();
  }

  return h.car();
}
AR_DEFUN("list-ref", fn_list_ref, 2);

Value fn_list_join(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "list-join";

  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);
  AR_FN_ASSERT_ARG(state, 0, "to be a list", (argv[0].list_length() > 0));

  Value lst = argv[0], join_char = argv[1];
  std::ostringstream ss;

  while(lst.type() == PAIR) {
    ss << lst.car();
    lst = lst.cdr();
    if(lst != C_NIL) {
      ss << join_char.string_data();
    }
  }

  return state.make_string(ss.str());
}
AR_DEFUN("list-join", fn_list_join, 2);

/** 
 * Underlying implementation of for-each and map. If improper is true, it will handle dotted lists,
 * if ret is true it will allocate and return a list of the results of function application.
 */
Value fn_map_impl(State& state, size_t argc, Value* argv, const char* fn_name, bool improper, bool ret, bool indice) {
  AR_FN_ASSERT_ARG(state, 0, "to be a function", (argv[0].procedurep()));

  if(argv[1] == C_NIL) return ret ? C_NIL : C_UNSPECIFIED;
  AR_FN_EXPECT_TYPE(state, argv, 1, PAIR);

  if(!improper) {
    AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1].list_length() > 0));
  } else {
    AR_FN_EXPECT_TYPE(state, argv, 1, PAIR);
  }

  Value tmp, lst = argv[1], fn = argv[0], arg;
  ListAppender nlst;
  AR_FRAME(state, nlst.head, nlst.tail,  lst, fn, arg, tmp);
  
  size_t i = 0;

  while(lst.heap_type_equals(PAIR)) {
    if(indice) {
      Value argv[2] = {Value::make_fixnum(i), lst.car()};
      tmp = state.apply(fn, 2, argv);
    } else {
      Value argv[1] = {lst.car()};
      tmp = state.apply(fn, 1, argv);
    }

    if(tmp.is_active_exception()) return tmp;
    if(ret) {
      // Try to preserve source information if it's available
      if(lst.pair_has_source()) {
        SourceLocation src = (lst.pair_src());
        tmp = state.make_src_pair(tmp, C_NIL, src);
      } else{
        tmp = state.make_pair(tmp, C_NIL);
      }

      nlst.append_cell(tmp);
    }
    i++;
    lst = lst.cdr();
  }

  // Optionally handle an improper list
  if(lst != C_NIL) {
    if(improper) {
      if(indice) {
        Value argv[2] = {Value::make_fixnum(i), lst};
        tmp = state.apply(fn, 2, argv);
      } else {
        Value argv[1] = {lst};
        tmp = state.apply(fn, 1, argv);
      }

      if(tmp.is_active_exception()) return tmp;
      if(ret && nlst.head != C_NIL) {
        nlst.tail.set_cdr(tmp);
      }
    } else {
      std::ostringstream os;
      os << fn_name << " got improper list";
      return state.type_error(os.str());
    }
  }

  return ret ? nlst.head : C_UNSPECIFIED;
}

Value fn_map_proper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "map1", false, true, false);
}
AR_DEFUN("map1", fn_map_proper, 2);

Value fn_map_improper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "map-improper", true, true, false);
}
AR_DEFUN("map-improper", fn_map_improper, 2);

Value fn_foreach_proper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each", true, false, false);
}
AR_DEFUN("for-each", fn_foreach_proper, 2);

Value fn_foreach_improper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each-improper", true, false, false);
}
AR_DEFUN("for-each-improper", fn_foreach_improper, 2);

Value fn_foreach_improper_i(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each-improper", true, false, true);
}
AR_DEFUN("for-each-improper-i", fn_foreach_improper_i, 2);

Value fn_map_proper_i(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "map-i", false, true, true);
}
AR_DEFUN("map-i", fn_map_proper_i, 2);

Value fn_for_each_proper_i(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each-i", false, false, true);
}
AR_DEFUN("for-each-i", fn_for_each_proper_i, 2);

Value fn_filter(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "filter";
  AR_FN_ASSERT_ARG(state, 0, "to be applicable", (argv[0].applicable()));
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1] == C_NIL || argv[1].list_length() > 0));
  
  Value fn = argv[0], lst = argv[1], result = C_NIL, tmp;
  ListAppender a;
  AR_FRAME(state, fn, lst, result, tmp, a.head, a.tail);

  while(lst.heap_type_equals(PAIR)) {
    Value argv[1] = {lst.car()};
    tmp = state.apply(fn, 1, argv);

    if(tmp.is_active_exception()) return tmp;

    if(tmp == C_FALSE) {
      lst = lst.cdr();
      continue;
    }

    a.append(state, lst.car());
    lst = lst.cdr();
  }

  return a.head;
}
AR_DEFUN("filter", fn_filter, 2);

enum Mem { MEMQ, MEMV, MEMBER };

Value fn_mem_impl(const char* fn_name, Mem method, State& state, size_t argc, Value* argv) {
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1] == C_NIL || argv[1].list_length() > 0));

  if(argv[1] == C_NIL) {
    return C_FALSE;
  }

  Value lst = argv[1], obj = argv[0];
  while(lst.heap_type_equals(PAIR)) {
    switch(method) {
      case MEMQ:
        if(lst.car() == obj) return lst;
        break;
      case MEMV:
        if(lst.car().eqv(obj)) return lst;
        break;
      case MEMBER:
        if(state.equals(lst.car(), obj)) return lst;
        break;
    }
    lst = lst.cdr();
  }

  return C_FALSE;
}

Value fn_memq(State& state, size_t argc, Value* argv) {
  return fn_mem_impl("memq", MEMQ, state, argc, argv);
}
AR_DEFUN("memq", fn_memq, 2);

Value fn_memv(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "memv";
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1] == C_NIL || argv[1].list_length() > 0));

  Value lst = argv[1], obj = argv[0];
  while(lst.heap_type_equals(PAIR)) {
    if(lst.car().eqv(obj)) return lst;
    lst = lst.cdr();
  }

  return C_FALSE;
}
AR_DEFUN("memv", fn_memv, 2);

Value fn_member(State& state, size_t argc, Value* argv) {
  return fn_mem_impl("member", MEMBER, state, argc, argv);
}
AR_DEFUN("member", fn_member, 2);

Value fn_reverse(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "reverse";
  if(argv[0] == C_NIL) return C_NIL;
  size_t length = argv[0].list_length();
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (length > 0));

  state.temps.clear();
  for(size_t i = 0; i != length; i++) {
    state.temps.push_back(argv[0].car());
      argv[0] = argv[0].cdr();
  }
  std::reverse(state.temps.begin(), state.temps.end());
  return state.temps_to_list();
}
AR_DEFUN("reverse", fn_reverse, 1);

Value fn_apply(State& state, size_t argc, Value* argv) {
  const char* fn_name = "apply";

  AR_FN_ASSERT_ARG(state, 0, "to be a function", (argv[0].procedurep()));
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1] == C_NIL || argv[1].list_length() > 0));

  size_t length = argv[1].list_length();
  Value sub_argv;

  Value fn = argv[0], args = argv[1], tmp;
  AR_FRAME(state, fn, args, tmp, sub_argv);

  sub_argv = state.make_vector_storage(length);
  while(args.heap_type_equals(PAIR)) {
    state.vector_storage_append(sub_argv, args.car());
    args = args.cdr();
  }

  tmp = state.apply_vector_storage(fn, sub_argv);

  return tmp;
}
AR_DEFUN("apply", fn_apply, 2);

///// PAIRS

Value fn_car(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "car";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  return argv[0].car();
}
AR_DEFUN("car", fn_car, 1);

Value fn_cdr(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "cdr";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  return argv[0].cdr();
}
AR_DEFUN("cdr", fn_cdr, 1);

Value fn_set_car(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "set-car!";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  argv[0].set_car(argv[1]);
  return C_UNSPECIFIED;
}
AR_DEFUN("set-car!", fn_set_car, 2);

Value fn_set_cdr(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "set-cdr!";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  argv[0].set_cdr(argv[1]);
  return C_UNSPECIFIED;
}
AR_DEFUN("set-cdr!", fn_set_cdr, 2);

///// VECTORS

Value fn_make_vector(State& state, size_t argc, Value* argv) {
  const char* fn_name = "make-vector";
  size_t size= 0, capacity = 2;
  Value vec, fill = C_FALSE;
  AR_FRAME(state, vec, fill);
  if(argc > 0) {
    AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
    AR_FN_EXPECT_POSITIVE(state, argv, 0);

    size = argv[0].fixnum_value();
    if(argc == 2) {
      fill = argv[1];
    }
  }

  vec = state.make_vector(size < capacity ? capacity : size);
  vec.vector_storage().as<VectorStorage>()->length = size;
  for(size_t i = 0; i != size; i++) {
    vec.vector_set(i, fill);
  }
  return vec;
}
AR_DEFUN("make-vector", fn_make_vector, 0, 2);

Value fn_vector_set(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "vector-set!";
  AR_FN_EXPECT_TYPE(state, argv, 0, VECTOR);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_POSITIVE(state, argv, 1);

  size_t position = argv[1].fixnum_value();

  if(argv[0].vector_length() <= (size_t) argv[1].fixnum_value()) {
    std::ostringstream os;
    os << "vector-set! bounds error, attempted to set position " << argv[1].fixnum_value() << " on a vector of length " << argv[0].vector_length();
    return state.eval_error(os.str());
  }

  argv[0].vector_set(position, argv[2]);
  return C_UNSPECIFIED;
}
AR_DEFUN("vector-set!", fn_vector_set, 3);

Value fn_vector_append(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "vector-append!";
  AR_FN_EXPECT_TYPE(state, argv, 0, VECTOR);

  state.vector_append(argv[0], argv[1]);
  return C_UNSPECIFIED;
}
AR_DEFUN("vector-append!", fn_vector_append, 2)

Value fn_vector_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "vector-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, VECTOR);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_POSITIVE(state, argv, 1);

  size_t position = argv[1].fixnum_value();

  if(argv[0].vector_length() <= position) {
    std::ostringstream os;
    os << "vector-ref bounds error, attempted to get position " << argv[1].fixnum_value() << " on a vector of length " << argv[0].vector_length();
    return state.eval_error(os.str());
  }

  return argv[0].vector_ref(argv[1].fixnum_value());
}
AR_DEFUN("vector-ref", fn_vector_ref, 2);

Value fn_vector_length(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "vector-length";
  AR_FN_EXPECT_TYPE(state, argv, 0, VECTOR);

  return Value::make_fixnum(argv[0].vector_length());
}
AR_DEFUN("vector-length", fn_vector_length, 1);

///// MACROEXPANSION SUPPORT

/** Generate a unique, unused symbol */
Value fn_gensym(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "gensym";

  std::string prefix("g");

  if(argc == 1) {
    AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
    prefix = argv[0].symbol_name_data();
  }

  return state.gensym(state.get_symbol(prefix));
}
AR_DEFUN("gensym", fn_gensym, 0, 1);

Value fn_gensymp(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "gensym?";

  return Value::make_boolean(argv[0].type() == SYMBOL &&
    argv[0].heap->get_header_bit(Value::SYMBOL_GENSYM_BIT));
}
AR_DEFUN("gensym?", fn_gensymp, 1);

// Strip qualifications from a symbol e.g. ##user#x becomes x, mainly for error messages
Value fn_symbol_dequalify(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "symbol-dequalify";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);

  return state.symbol_dequalify(argv[0]);
}
AR_DEFUN("symbol-dequalify", fn_symbol_dequalify, 1);

/** Macro that asserts an argument is a valid environment */
#define AR_FN_EXPECT_ENV(state, n) \
 AR_FN_ASSERT_ARG((state), (n), "to be a valid environment (vector or #f)", argv[(n)].type() == VECTOR || argv[(n)].type() == TABLE || argv[(n)] == C_FALSE);

#define AR_FN_EXPECT_IDENT(state, n) \
  AR_FN_ASSERT_ARG((state), (n), "to be a valid identifier (symbol or rename)", argv[(n)].identifierp())

Value fn_set_function_name(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-function-name!";
  AR_FN_EXPECT_TYPE(state, argv, 1, SYMBOL);

  switch(argv[0].type()) {
    case FUNCTION: argv[0].as_unsafe<Function>()->name = argv[1]; break;
    case VMFUNCTION: argv[0].as_unsafe<VMFunction>()->name = argv[1]; break;
    default: break;
  }
  return C_UNSPECIFIED;
}
AR_DEFUN("set-function-name!", fn_set_function_name, 2);

Value fn_set_vmfunction_name(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-vmfunction-name!";
  AR_FN_EXPECT_TYPE(state, argv, 0, VMFUNCTION);

  VMFunction* fn = argv[0].as<VMFunction>();
  fn->name = argv[1];

  return C_UNSPECIFIED;
}
AR_DEFUN("set-vmfunction-name!", fn_set_vmfunction_name, 2);

Value fn_set_vmfunction_macro_env(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-vmfunction-macro-env!";
  AR_FN_EXPECT_TYPE(state, argv, 0, VMFUNCTION);

  VMFunction* fn = argv[0].as<VMFunction>();
  fn->macro_env = argv[1];

  return C_UNSPECIFIED;
}
AR_DEFUN("set-vmfunction-macro-env!", fn_set_vmfunction_macro_env, 2);

Value fn_set_vmfunction_log(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-vmfunction-log!";
  AR_FN_EXPECT_TYPE(state, argv, 1, CONSTANT);

  Value fn = argv[0];

  if(argv[0].type() == CLOSURE) {
    fn = fn.closure_function();
  }

  if(fn.type() != VMFUNCTION) return C_FALSE;

  if(argv[1] == C_FALSE) {
    fn.heap->unset_header_bit(Value::VMFUNCTION_LOG_BIT);
  } else {
    if(!fn.heap->get_header_bit(Value::VMFUNCTION_LOG_BIT))
      fn.heap->set_header_bit(Value::VMFUNCTION_LOG_BIT);
  }

  return C_UNSPECIFIED;
}
AR_DEFUN("set-vmfunction-log!", fn_set_vmfunction_log, 2);

Value fn_function_env(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "function-env";
  switch(argv[0].type()) {
    case FUNCTION: return argv[0].function_parent_env();
    case CLOSURE:
    case VMFUNCTION: return argv[0].vm_function_macro_env();
    default: return state.type_error("function-env expected valid macro");
  }
}
AR_DEFUN("function-env", fn_function_env, 1);

Value fn_set_function_macro_bit(State& state, size_t argc, Value* argv) {
  switch(argv[0].type()) {
    case FUNCTION:
      argv[0].heap->set_header_bit(Value::FUNCTION_MACRO_BIT);
      break;
    case CLOSURE:
    case VMFUNCTION: {
      Value fn = argv[0].closure_unbox();
      if(fn.heap->get_header_bit(Value::VMFUNCTION_MACRO_BIT)) return argv[0];
      fn.heap->set_header_bit(Value::VMFUNCTION_MACRO_BIT);
      break;
    }
    default: break;
  }
  return argv[0];
}
AR_DEFUN("set-function-macro-bit!", fn_set_function_macro_bit, 1);

Value fn_function_is_macro(State& state, size_t argc, Value* argv) {
  switch(argv[0].type()) {
    case FUNCTION:
      return Value::make_boolean(argv[0].heap->get_header_bit(Value::FUNCTION_MACRO_BIT));
    case VMFUNCTION: {
      Value fn = argv[0].closure_unbox();
      return Value::make_boolean(fn.heap->get_header_bit(Value::VMFUNCTION_MACRO_BIT));
    }
    default: break;
  }
  return C_FALSE;
}
AR_DEFUN("function-macro?", fn_function_is_macro, 1);

Value fn_function_is_identifier_macro(State& state, size_t argc, Value* argv) {
  switch(argv[0].type()) {
    case FUNCTION:
      return Value::make_boolean(argv[0].heap->get_header_bit(Value::FUNCTION_IDENTIFIER_MACRO_BIT));
    case CLOSURE:
    case VMFUNCTION: {
      Value fn = argv[0].closure_unbox();
      return Value::make_boolean(fn.heap->get_header_bit(Value::VMFUNCTION_IDENTIFIER_MACRO_BIT));
    }
    default: break;
  }

  return C_FALSE;
}
AR_DEFUN("function-identifier-macro?", fn_function_is_identifier_macro, 1);

Value fn_set_function_identifier_macro_bit(State& state, size_t argc, Value* argv) {
  switch(argv[0].type()) {
    case FUNCTION:
      if(argv[0].heap->get_header_bit(Value::FUNCTION_IDENTIFIER_MACRO_BIT)) return argv[0];
      argv[0].heap->set_header_bit(Value::FUNCTION_IDENTIFIER_MACRO_BIT);
      break;
    case CLOSURE:
    case VMFUNCTION: {
      Value fn = argv[0].closure_unbox();
      if(fn.heap->get_header_bit(Value::VMFUNCTION_IDENTIFIER_MACRO_BIT)) return argv[0];
      fn.heap->set_header_bit(Value::VMFUNCTION_IDENTIFIER_MACRO_BIT);
      break;
    }
    default: break;
  }
  return argv[0];
}
AR_DEFUN("set-function-identifier-macro-bit!", fn_set_function_identifier_macro_bit, 1);

Value fn_function_min_arity(State& state, size_t argc, Value* argv) {
  switch(argv[0].type()) {
    case FUNCTION: {
      return Value::make_fixnum(argv[0].function_arguments().list_length());
    }
    case CLOSURE:
    case VMFUNCTION: {
      Value x = argv[0].closure_unbox();

      return Value::make_fixnum(x.vm_function_min_arity());
    }
    default:
      return Value::make_fixnum(1);
  }
}
AR_DEFUN("function-min-arity", fn_function_min_arity, 1);

Value fn_function_body(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-body";
  AR_FN_EXPECT_HEAP_TYPE(state, argv, 0, FUNCTION);

  return argv[0].as<Function>()->body;
}
AR_DEFUN("function-body", fn_function_body, 1);

Value fn_function_name(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-name";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  return argv[0].as<Function>()->name;
}
AR_DEFUN("function-name", fn_function_name, 1);

Value fn_function_arguments(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-arguments";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  return argv[0].as<Function>()->arguments;
}
AR_DEFUN("function-arguments", fn_function_arguments, 1);

Value fn_function_rest_arguments(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-rest-arguments";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);
  
  return argv[0].as<Function>()->rest_arguments;
}
AR_DEFUN("function-rest-arguments", fn_function_rest_arguments, 1);

Value fn_top_level_value(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "top-level-value";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);

  Value r = argv[0].symbol_value();
  if(r == C_UNDEFINED) return C_UNSPECIFIED;
  return r;
}
AR_DEFUN("top-level-value", fn_top_level_value, 1);

Value fn_top_level_bound(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "top-level-bound?";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);

  return Value::make_boolean(argv[0].symbol_value() != C_UNDEFINED);
}
AR_DEFUN("top-level-bound?", fn_top_level_bound, 1);

/** Iterate over everything that might be a toplevel function. */
Value fn_top_level_for_each(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "top-level-for-each";
  AR_FN_EXPECT_APPLICABLE(state, argv, 0);
  size_t count = 0;

  Value fn = argv[0], key, value;
  AR_FRAME(state, fn, key, value);

  std::vector<std::string> keys;
  for(auto it = state.symbol_table->begin(); it != state.symbol_table->end(); it++) {
    keys.push_back(it->first);
  }

  for(size_t i = 0; i != keys.size(); i++) {
    key = state.get_symbol(keys[i]);
    value = key.symbol_value();

    // Scheme code can't touch these
    if(value == C_UNDEFINED || value == C_SYNTAX)
      value = C_UNSPECIFIED;

    Value argv[2] = {key, value};
    
    Value tst = state.apply(fn, 2, argv);

    if(tst.is_active_exception()) return tst;
    if(tst == C_TRUE) count++;
  }


  if(count > 0) return Value::make_fixnum(count);
  return Value::make_fixnum(0);
}
AR_DEFUN("top-level-for-each", fn_top_level_for_each, 1);

Value fn_set_top_level_value(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-top-level-value!";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  argv[0].set_symbol_value(argv[1]);
  return C_UNSPECIFIED;
}
AR_DEFUN("set-top-level-value!", fn_set_top_level_value, 2);

Value fn_make_rename(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "make-rename";
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_EXPECT_TYPE(state, argv, 1, SYMBOL);

  return state.make_rename(argv[1], argv[0]);
}
AR_DEFUN("make-rename", fn_make_rename, 2);

Value fn_rename_set_gensym(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-gensym!";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);

  Value renam = argv[0], sym;
  AR_FRAME(state, renam, sym);

  sym = state.gensym(renam.rename_expr());

  renam.as<Rename>()->gensym = sym;
  return C_UNSPECIFIED;
}
AR_DEFUN("rename-gensym!", fn_rename_set_gensym, 1);

Value fn_rename_env(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-env";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);
  return argv[0].rename_env();
}
AR_DEFUN("rename-env", fn_rename_env, 1);

Value fn_rename_expr(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-env";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);
  return argv[0].rename_expr();
}
AR_DEFUN("rename-expr", fn_rename_expr, 1);

Value fn_rename_gensym(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-gensym";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);
  return argv[0].rename_gensym();
}
AR_DEFUN("rename-gensym", fn_rename_gensym, 1);

Value fn_eval(State& state, size_t argc, Value* argv) {
  Value env = argc == 2 ? argv[1] : C_FALSE, exp = argv[0];
  AR_FRAME(state, env, exp);
  exp = state.make_pair(exp, C_NIL);
  return state.eval_list(exp, false, env);
}
AR_DEFUN("eval", fn_eval, 1, 2);

///// MISC

Value fn_raise(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "raise";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);
  Value tag = argv[0], message = argv[1], irritants = argc == 3 ? argv[2] : C_FALSE, exc;
  AR_FRAME(state, tag, message, irritants, exc);
  exc = state.make_exception(tag, message, irritants);
  AR_ASSERT(exc.is_active_exception());
  return exc;
}
AR_DEFUN("raise", fn_raise, 2, 3);

Value fn_raise_source(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "raise-source";

  AR_FN_EXPECT_TYPE(state, argv, 1, SYMBOL);
  AR_FN_EXPECT_TYPE(state, argv, 2, STRING);

  Value tag = argv[1], message = argv[2], irritants = argv[3], exc;

  std::ostringstream os;

  if(argv[0].type() == PAIR && argv[0].pair_has_source()) {
    state.print_src_pair(os, argv[0]);
  }

  os << std::endl << message.string_data();

  exc = state.make_exception(tag, os.str(), irritants);

  return exc;
}
AR_DEFUN("raise-source", fn_raise_source, 4);

///// RECORDS
Value fn_register_record_type(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "register-record-type";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_POSITIVE(state, argv, 1);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);

  Value name = argv[0], data = argv[2], fields = argv[3], parent = argv[4];
  ptrdiff_t field_count = argv[1].fixnum_value();

  if(parent != C_FALSE) {
    field_count += parent.as<RecordType>()->field_count;
  }

  // TODO: Is it possible this doesn't copy string data and could potentially be moved?
  // Should string_data return char* ?
  std::string cname(name.string_data());

  size_t tag = (size_t)state.register_record_type(cname, (unsigned)field_count, (unsigned)data.fixnum_value(), fields, parent);

  return state.globals[tag];
}
AR_DEFUN("register-record-type", fn_register_record_type, 5);


Value fn_set_record_type_printer(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-record-type-printer!";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_APPLICABLE(state, argv, 1);
  RecordType* rt = argv[0].as<RecordType>();
  rt->print = argv[1]; 
  return C_UNSPECIFIED;
}
AR_DEFUN("set-record-type-printer!", fn_set_record_type_printer, 2);

Value fn_set_record_type_apply(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-record-type-apply!";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_APPLICABLE(state, argv, 1);
  RecordType* rt = argv[0].as<RecordType>();
  rt->apply = argv[1]; 
  return C_UNSPECIFIED;
}
AR_DEFUN("set-record-type-apply", fn_set_record_type_apply, 2);

// Macro for comparing records in the unusual case we already have the RecordType
// (essentially in cases where record accessors have been generated by define-record)

#define _AR_FN_EXPECT_RECORD_ISA(state, expected_type, record) \
  if(!((record).record_isa((expected_type)))) { \
    std::ostringstream msg; \
    msg << fn_name << "expected a record of type " << \
      (expected_type).record_type_name().string_data() << " but got a record of type " <<  \
      (record).record_type().record_type_name().string_data(); \
    return (state).type_error(msg.str()); \
  }

Value fn_record_set(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "record-set!";

  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_TYPE(state, argv, 1, RECORD);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);

  _AR_FN_EXPECT_RECORD_ISA(state, argv[0], argv[1]);

  argv[1].record_set((unsigned)argv[2].fixnum_value(), argv[3]);

  return C_UNSPECIFIED;
}
AR_DEFUN("record-set!", fn_record_set, 4);

Value fn_record_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "record-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_TYPE(state, argv, 1, RECORD);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);

  _AR_FN_EXPECT_RECORD_ISA(state, argv[0], argv[1]);

  return argv[1].record_ref((unsigned)argv[2].fixnum_value());
}
AR_DEFUN("record-ref", fn_record_ref, 3);

Value fn_make_record(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "make-record";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);

  return state.make_record(argv[0].as<RecordType>());
}
AR_DEFUN("make-record", fn_make_record, 1);

Value fn_record_type_descriptor(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "record-type-descriptor";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD);
  return argv[0].as<Record>()->type;
}
AR_DEFUN("record-type-descriptor", fn_record_type_descriptor, 1);

Value fn_record_isa(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "record-isa?";
  if(argv[0].type() != RECORD)
    return C_FALSE;
  Value rec = argv[0], rtd = argv[1];

  return Value::make_boolean(rec.record_isa(rtd));
}
AR_DEFUN("record-isa?", fn_record_isa, 2);

// Compiler
Value fn_list_get_source(State& state, size_t argc, Value* argv) {
  if(argv[0].type() == PAIR && argv[0].pair_has_source()) {
    Value storage;
    AR_FRAME(state, storage);

    SourceLocation src(argv[0].pair_src());
    storage = state.make_vector_storage(4);


    state.vector_storage_append(storage, Value::make_fixnum(src.source));
    state.vector_storage_append(storage, Value::make_fixnum(src.line));
    state.vector_storage_append(storage, Value::make_fixnum(src.begin));
    state.vector_storage_append(storage, Value::make_fixnum(src.length));

    return state.make_vector(storage);
  }
  return C_FALSE;
}
AR_DEFUN("list-get-source", fn_list_get_source, 1);

Value fn_openfn_to_procedure(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "OpenFn->procedure";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD);
  // TODO: Could type check more thoroughly here.

  size_t size = sizeof(VMFunction);
  Value name, insns, constants, sources, stack_size, rec = argv[0], fn, free_vars, free_vars_blob = C_FALSE,
     sources_blob;
  AR_FRAME(state, name, insns, constants, sources, stack_size, rec, fn, free_vars, free_vars_blob,
    sources_blob);

  name = rec.record_ref(0);
  insns = rec.record_ref(1);
  constants = rec.record_ref(2);

  // Convert free variables vector to size_ts
  // This is done above the creation of the actual function
  // because it allocates
  free_vars = rec.record_ref(15);

  if(free_vars.type() == VECTOR && free_vars.vector_length() > 0) {
    size_t length = free_vars.vector_length();
    free_vars_blob = state.make_blob<size_t>(length);
    for(size_t i = 0; i != length; i++) {
      free_vars_blob.blob_set<size_t>(i, free_vars.vector_ref(i).fixnum_value());
      AR_ASSERT(((size_t*) free_vars_blob.as<Blob>()->data)[i] ==
        (size_t)free_vars.vector_ref(i).fixnum_value());
    }

    AR_ASSERT(free_vars_blob.blob_length() == length);
  }

  sources = rec.record_ref(3);
  if(sources.type() == VECTOR && sources.vector_length() > 0) {
    size_t length = sources.vector_length();
    sources_blob = state.make_blob<unsigned>(length);
    for(size_t i = 0; i != length; i++) {
      sources_blob.blob_set<unsigned>(i, sources.vector_ref(i).fixnum_value());
    }
  }

  size_t insn_count = insns.vector_length();
  size_t bytecode_size = (size_t) insn_count * sizeof(size_t);
  unsigned constant_count = (unsigned) constants.vector_length();

  // Determine actual size of VMFunction
  size += (constant_count * sizeof(Value));
  size += (bytecode_size);

  VMFunction* vfn = static_cast<VMFunction*>(state.gc.allocate(VMFUNCTION, size));

  vfn->constant_count = constant_count;
  vfn->min_arity = (unsigned)rec.record_ref(9).fixnum_value();
  vfn->max_arity = (unsigned)rec.record_ref(10).fixnum_value();
  vfn->bytecode_size = (unsigned)bytecode_size;
  vfn->stack_max = (unsigned)rec.record_ref(6).fixnum_value();
  vfn->local_count = (unsigned)rec.record_ref(5).fixnum_value();
  vfn->name = rec.record_ref(0);
  vfn->free_variables = static_cast<Blob*>(free_vars_blob.heap);
  vfn->sources = static_cast<Blob*>(sources_blob.heap);

  // Check for variable arity
  if(rec.record_ref(11) == C_TRUE) {
    vfn->set_header_bit(Value::VMFUNCTION_VARIABLE_ARITY_BIT);
  }

  fn = vfn;

  vfn->constants = rec.record_ref(2).vector_storage().as<VectorStorage>();

  // Copy bytecode
  size_t* code_array = (size_t*) fn.vm_function_code();

  AR_ASSERT(((char*) code_array) > ((char*) &fn.as_unsafe<VMFunction>()->constants));
  
  for(size_t i = 0; i != insn_count; i++) {
    (*code_array++) = insns.vector_ref(i).fixnum_value();
  }

  // Check we didn't go over the end of the object
  AR_ASSERT((char*) code_array <= ((char*) (vfn)) + vfn->size);

  return fn;
}
AR_DEFUN("OpenFn->procedure", fn_openfn_to_procedure, 1);

Value fn_value_bits(State& state, size_t argc, Value* argv) {
  return Value::make_fixnum(argv[0].bits);
}
AR_DEFUN("value-bits", fn_value_bits, 1);

Value fn_value_make(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "value-make";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  Value v;
  v.bits = argv[0].fixnum_value();
  if(!v.immediatep()) {
    return state.eval_error("value-make resulted in non-immediate value");
  }
  return v;
}
AR_DEFUN("value-make", fn_value_make, 1);

Value fn_value_copy(State& state, size_t argc, Value* argv) {
  Value v1 = argv[0], v2;
  if(v1.immediatep()) return v1;
  AR_FRAME(state, v1, v2);
  v2 = state.gc.allocate(BLOB, v1.heap->size);
  memcpy(v2.heap, v1.heap, v1.heap->size);
  return v2;
}
AR_DEFUN("value-copy", fn_value_copy, 1);

Value fn_value_header_bit(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "value-header-bit?";
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  if(argv[0].immediatep()) {
    return state.eval_error("value-header bit got immediate object");
  }
  return Value::make_boolean(argv[0].heap->get_header_bit(1 << argv[1].fixnum_value()));
}
AR_DEFUN("value-header-bit?", fn_value_header_bit, 2)

// Garbage collector

Value fn_gc_collect(State& state, size_t argc, Value* argv) {
  state.gc.collect();
  return C_UNSPECIFIED;
}
AR_DEFUN("gc:collect", fn_gc_collect, 0);

Value fn_exit(State& state, size_t argc, Value* argv) {
  if(argv[1] == C_TRUE) {
    exit(EXIT_SUCCESS);
  } else {
    exit(EXIT_FAILURE);
  }
  return C_UNSPECIFIED;
}
AR_DEFUN("exit", fn_exit, 1);

Value fn_save_image(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "save-image";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  state.save_image(argv[0].string_data());

  exit(EXIT_SUCCESS);
  return C_UNSPECIFIED; // make the compiler happy :)
}
AR_DEFUN("save-image", fn_save_image, 1);

Value fn_repl(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(state.enter_repl());
}
AR_DEFUN("repl", fn_repl, 0);

void State::load_builtin_functions() {
  builtins.install(*this);
}

} // namespace arete
