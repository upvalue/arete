// arete.cpp - arete builtin functions

#include "arete.hpp"

namespace arete {

size_t gc_collect_timer = 0;
State* current_state = 0;

// Casting arithmetic
#define OP(name, cname, boolean, operator) \
  Value name (State& state, size_t argc, Value* argv) { \
    static const char* fn_name = cname; \
    AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].type() == FIXNUM || argv[0].type() == FLONUM); \
    AR_FN_ASSERT_ARG(state, 1, "to be a number", argv[1].type() == FIXNUM || argv[1].type() == FLONUM); \
    if(argv[0].type() == FLONUM || argv[1].type() == FLONUM) { \
      double n1 = argv[0].type() == FLONUM ? argv[0].flonum_value() : argv[0].fixnum_value(); \
      double n2 = argv[1].type() == FLONUM ? argv[1].flonum_value() : argv[1].fixnum_value(); \
      return (boolean) ? Value::make_boolean(n1 operator n2) : state.make_flonum(n1 operator n2); \
    } \
    return (boolean) ? Value::make_boolean(argv[0].fixnum_value() operator argv[1].fixnum_value()) : \
        Value::make_fixnum(argv[0].fixnum_value() operator argv[1].fixnum_value()); \
  }

OP(fn_add, "+", false, +)
OP(fn_sub, "-", false, -)
OP(fn_mul, "*", false, *)
OP(fn_div, "/", false, /)
OP(fn_num_equal, "=", false, ==)

#undef OP

// Fixnum arithmetic

Value fn_fx_sub(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx-";

  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  if(argc == 1) {
    return Value::make_fixnum(0 - argv[0].fixnum_value());
  }

  ptrdiff_t result = argv[0].fixnum_value();

  for(size_t i = 1; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    result -= argv[i].fixnum_value();
  }

  return Value::make_fixnum(result);
}

Value fn_fx_add(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx+";
  ptrdiff_t result = 0;

  for(size_t i = 0; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    result += argv[i].fixnum_value();
  }

  return Value::make_fixnum(result);
}

Value fn_fx_equals(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx=";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].fixnum_value() == argv[1].fixnum_value());
}

Value fn_fx_lt(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx<";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].fixnum_value() < argv[1].fixnum_value());
}

Value fn_fx_gt(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx>";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].fixnum_value() > argv[1].fixnum_value());
}

#define AR_ARITHMETIC(name, op) \
  Value fn_##name (State& state, size_t argc, Value* argv) { \
    const char* fn_name = #name; \
  }

Value fn_eq(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "eq?";
  return Value::make_boolean(argv[0].bits == argv[1].bits);
}

Value fn_eqv(State& state, size_t argc, Value* argv) {
  return fn_eq(state, argc, argv);
}

Value fn_equal(State& state, size_t argc, Value* argv) {
  if(argv[0].type() == VECTOR && argv[1].type() == VECTOR) {
    return C_FALSE;
  } else if(argv[0].type() == PAIR && argv[1].type() == PAIR) {
    Value lst = argv[0], lst2 = argv[1];

    while(lst.type() == PAIR && lst2.type() == PAIR) {
      if(lst.car().bits != lst2.car().bits) {
        return C_FALSE;
      }
      lst = lst.cdr();
      lst2 = lst2.cdr();
    }

    if(lst != C_NIL || lst2 != C_NIL) {
      return Value::make_boolean(lst.bits == lst2.bits);
    }

    return C_TRUE;
  } else {
    return fn_eq(state, argc, argv);
  }
}

Value fn_display(State& state, size_t argc, Value* argv) {
  std::cout << argv[0];
  return C_UNSPECIFIED;
}

Value fn_newline(State& state, size_t argc, Value* argv) {
  std::cout << std::endl;
  return C_UNSPECIFIED;
}


void fn_print_impl(State& state, size_t argc, Value* argv, std::ostream& os) {
  for(size_t i = 0; i != argc; i++) {
    if(argv[i].type() == STRING) {
      os << argv[i].string_data();
    } else {
      os << argv[i];
    }

    if(i != argc - 1)  {
      os << ' ';
    }
  }
}

Value fn_print(State& state, size_t argc, Value* argv) {
  fn_print_impl(state, argc, argv, std::cout);
  std::cout << std::endl;
  return C_UNSPECIFIED;
}

Value fn_print_string(State& state, size_t argc, Value* argv) {
  std::ostringstream os;
  fn_print_impl(state, argc, argv, os);
  return state.make_string(os.str());
}

///// PREDICATES

Value fn_nullp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0] == C_NIL);
}

Value fn_renamep(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == RENAME);
}

Value fn_procedurep(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].procedurep());
}

Value fn_pairp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == PAIR);
}

Value fn_symbolp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == SYMBOL);
}

Value fn_boxp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == BOX);
}

Value fn_flonump(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == FLONUM);
}

Value fn_fixnump(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == FIXNUM);
}

Value fn_charp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == CHARACTER);
}

Value fn_macrop(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == FUNCTION && argv[0].function_is_macro());
}

Value fn_self_evaluatingp(State& state, size_t argc, Value* argv) {
  if(argv[0].immediatep()) return C_TRUE;

  switch(argv[0].type()) {
    case STRING:
    case VECTOR: 
    case FLONUM:
      return C_TRUE;
    default:
      return C_FALSE;
  }
}

Value fn_identifierp(State& state, size_t argc, Value* argv) {
  Type tipe = argv[0].type();
  return Value::make_boolean(tipe == RENAME || tipe == SYMBOL);
}

///// LISTS

Value fn_cons(State& state, size_t argc, Value* argv) {
  Value kar, kdr;
  AR_FRAME(state, kar, kdr);
  kar = argv[0];
  kdr = argv[1];
  return state.make_pair(kar, kdr);
}

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

  AR_FN_ASSERT_ARG(state, 0, "to be a box or pair with source information",
    (type == PAIR && src.pair_has_source() || type == BOX && src.box_has_source()));
  kar = argv[1];
  kdr = argv[2];
  SourceLocation loc;
  if(type == PAIR) {
    loc = *(src.pair_src());
  } else {
    loc = *(src.box_src());
  }
  pare = state.make_src_pair(kar, kdr, loc);
  return pare;
}

Value fn_list_impl(State& state, size_t argc, Value* _argv, bool copy_source) {
  // static const char* fn_name = "list";
  AR_FRAME_ARRAY(state, argc, _argv, argv);

  Value head = C_NIL, current, tmp;
  SourceLocation loc;

  AR_FRAME(state, head, current, tmp);

  if(copy_source) {
    if(_argv[0].type() == PAIR && _argv[0].pair_has_source()) {
      loc = *_argv[0].pair_src();
    }
  }

  for(size_t i = copy_source ? 1 : 0; i < argc; i++) {
    Value v = argv[i];
    if(head == C_NIL) {
      if(copy_source) {
        head = current = state.make_src_pair(v, C_NIL, loc);
      } else {
        head = current = state.make_pair(v, C_NIL);
      }
    } else {
      tmp = state.make_pair(v, C_NIL);
      current.set_cdr(tmp);
      current = tmp;
    }
  }

  delete[] argv;
  delete[] __ar_roots;
  return head;
}

Value fn_list(State& state, size_t argc, Value* argv) {
  return fn_list_impl(state, argc, argv, false);
}

Value fn_list_source(State& state, size_t argc, Value* argv) {
  return fn_list_impl(state, argc, argv, true);
}

/** Returns the length of a list */
Value fn_length(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "length";
  if(argv[0] == C_NIL)
    return Value::make_fixnum(0);

  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);

  size_t length = 0;

  Value head = argv[0], next;
  while(head != C_NIL) {
    next = head.cdr();
    if(next != C_NIL && next.type() != PAIR) {
      return state.type_error("length got a dotted list as its argument");
    }
    length++;
    head = next;
  }

  return Value::make_fixnum(length);
}

Value fn_listp(State& state, size_t argc, Value* argv) {
  // return argv[0] == C_NIL || (argv[0].type() == PAIR && argv[0].list_length() > 
  if(argv[0] == C_NIL) return C_TRUE;
  while(argv[0].type() == PAIR) {
    if(argv[0].cdr() == C_NIL) return C_TRUE;
    argv[0] = argv[0].cdr();
  }
  return C_FALSE;
}

Value fn_list_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "list-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_ASSERT_ARG(state, 1, "to be a positive number", argv[1].fixnum_value() >= 0);

  Value h = argv[0];
  size_t idx = argv[1].fixnum_value();
  
  while(idx--) {
    if(h.cdr().type() != PAIR) {
      return state.type_error("list-ref ran into a dotted list");
    }
    h = h.cdr();
  }

  return h.car().maybe_unbox();
}

Value fn_map(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "map"; 

  AR_FN_ASSERT_ARG(state, 0, "to be a function", (argv[0].procedurep()));

  if(argv[1] == C_NIL) return C_NIL;
  AR_FN_EXPECT_TYPE(state, argv, 1, PAIR);
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1].list_length() > 0));

  Value nlst_head, nlst_current = C_NIL, tmp, lst = argv[1], fn = argv[0], arg;
  AR_FRAME(state, nlst_head, nlst_current, lst, fn, arg);

  while(lst.type() == PAIR) {
    arg = state.make_pair(lst.car().maybe_unbox(), C_NIL);
    tmp = state.apply_generic(fn, arg, false);
    if(tmp.is_active_exception()) return tmp;
    if(nlst_current == C_NIL) {
      nlst_head = nlst_current = state.make_pair(tmp, C_NIL);
    } else {
      tmp = state.make_pair(tmp, C_NIL);
      nlst_current.set_cdr(tmp);
      nlst_current = tmp;
    }
    lst = lst.cdr();
  }

  return nlst_head;
}

Value fn_for_each_dot(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "for-each.";

  AR_FN_ASSERT_ARG(state, 0, "to be a function", (argv[0].procedurep()));

  if(argv[1] == C_NIL) return C_NIL;
  AR_FN_EXPECT_TYPE(state, argv, 1, PAIR);

  Value lst = argv[1], fn = argv[0], arg, tmp;
  AR_FRAME(state, lst, fn, arg, tmp);

  while(lst.type() == PAIR) {
    arg = state.make_pair(lst.car().maybe_unbox(), C_NIL);
    tmp = state.apply_generic(fn, arg, false);
    if(tmp.is_active_exception()) return tmp;
    lst = lst.cdr();
  }

  if(lst != C_NIL) {
    arg = state.make_pair(lst.maybe_unbox(), C_NIL);
    return state.apply_generic(fn, arg, false);
  }

  return C_UNSPECIFIED;
}

Value fn_eval(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "eval"; (void) fn_name;

  return state.eval_toplevel(argv[0]);
}

Value fn_apply(State& state, size_t argc, Value* argv) {
  const char* fn_name = "apply";

  AR_FN_ASSERT_ARG(state, 0, "to be a function", (argv[0].procedurep()));
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1] == C_NIL || argv[1].list_length() > 0));

  Value fn = argv[0], args = argv[1], tmp;
  AR_FRAME(state, fn, args, tmp);

  tmp = state.apply_generic(fn, args, false);

  return tmp;
}

///// PAIRS

Value fn_car(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "car";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  return argv[0].car().maybe_unbox();
}

Value fn_cdr(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "cdr";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  return argv[0].cdr().maybe_unbox();
}

///// VECTORS

Value fn_make_vector(State& state, size_t argc, Value* argv) {
  const char* fn_name = "make-vector";
  size_t size = 0;
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

  vec = state.make_vector(size);
  vec.vector_storage().as<VectorStorage>()->length = size;
  for(size_t i = 0; i != size; i++) {
    vec.vector_set(i, fill);
  }
  return vec;
}

Value fn_vector_set(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "vector-set!";
  AR_FN_EXPECT_TYPE(state, argv, 0, VECTOR);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_POSITIVE(state, argv, 1);

  size_t position = argv[1].fixnum_value();

  if(argv[0].vector_length() <= argv[1].fixnum_value()) {
    std::ostringstream os;
    os << "vector-set! bounds error, attempted to set position " << argv[1].fixnum_value() << " on a vector of length " << argv[0].vector_length();
    return state.eval_error(os.str());
  }

  argv[0].vector_set(position, argv[2]);
  return C_UNSPECIFIED;
}

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

///// MACROEXPANSION SUPPORT

/** Generate a unique, unused symbol */
Value fn_gensym(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "gensym";

  Value sym;

  std::ostringstream os;

  os << "#:";

  if(argc == 1) {
    AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
    os << argv[0].symbol_name_bytes();
  } else {
    os << "g";
  }

  os << state.gensym_counter;

  sym = state.get_symbol(os.str());
  state.gensym_counter++;
  return sym;  
}

/** Macro that asserts an argument is a valid environment */
#define AR_FN_EXPECT_ENV(state, n) \
  AR_FN_ASSERT_ARG((state), (n), "to be a valid environment (vector or #f)", argv[(n)].type() == VECTOR || argv[(n)] == C_FALSE);

Value fn_source_location(State& state, size_t argc, Value* argv) {
  if(argv[0].type() != BOX && argv[0].type() != PAIR)
    return C_FALSE;

  return C_FALSE;
}

Value fn_env_make(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-make";
  AR_FN_EXPECT_ENV(state, 0);
  return state.make_env(argv[0]);
}

Value fn_env_define(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-define";
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_ASSERT_ARG(state, 1, "to be a valid identifier (symbol or rename)", argv[1].type() == RENAME || argv[1].type() == SYMBOL);

  Value env = argv[0], name = argv[1], value = argv[2];
  AR_FRAME(state, env, name, value);

  if(argv[0] == C_FALSE) {
    name.as<Symbol>()->value = value;
  } else {
    state.vector_append(env, name);
    state.vector_append(env, value);
  }

  return C_UNSPECIFIED;
}

Value fn_env_compare(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-compare";

  Type type1 = argv[1].type(), type2 = argv[2].type();

  if((type1 != SYMBOL && type1 != RENAME) || (type2 != SYMBOL && type2 != RENAME)) {
    return Value::make_boolean(argv[1].bits == argv[2].bits);
  }

  //std::cout << argv[0] << std::endl;
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_ASSERT_ARG(state, 1, "to be a valid identifier (symbol or rename)", argv[1].type() == RENAME || argv[1].type() == SYMBOL);
  AR_FN_ASSERT_ARG(state, 2, "to be a valid identifier (symbol or rename)", argv[2].type() == RENAME || argv[2].type() == SYMBOL);

  Value env = argv[0];
  Value id1 = argv[1];
  Value id2 = argv[2];

  if(state.identifier_equal(id1, id2)) {
    return C_TRUE;
  }

  // Ensure that id1 holds the rename
  if(id2.type() == RENAME) {
    std::swap(id1, id2);
  }

  Value rename_env = id1.rename_env();

  state.env_lookup_impl(rename_env, id1.rename_expr(), false);
  state.env_lookup_impl(env, id2, false);

  return Value::make_boolean(rename_env == env && id1.rename_expr() == id2);
}

Value fn_env_lookup(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-lookup";
  AR_FN_ASSERT_ARG(state, 0, "to be a valid environment (vector or #f)", argv[0].type() == VECTOR || argv[0] == C_FALSE);
  AR_FN_ASSERT_ARG(state, 1, "to be a valid identifier (symbol or rename)", argv[1].type() == RENAME || argv[1].type() == SYMBOL);
  
  return state.env_lookup(argv[0], argv[1]);
}

Value fn_env_syntaxp(State& state, size_t argc, Value* argv) {
  Value result = fn_env_lookup(state, argc, argv);

  if(result.is_active_exception()) return result;
  
  if(result == C_SYNTAX) return C_TRUE;

  if(result.type() == FUNCTION && result.function_is_macro()) return C_TRUE;

  return C_FALSE;
}

Value fn_set_function_name(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-function-name!";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);
  AR_FN_EXPECT_TYPE(state, argv, 1, SYMBOL);

  Function* fn = argv[0].as<Function>();
  fn->name = argv[1];

  return C_UNSPECIFIED;
}

Value fn_function_env(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-env";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  return argv[0].function_parent_env();
}

Value fn_set_function_macro_bit(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-function-macro-bit!";

  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  Function* fn = argv[0].as<Function>();
  fn->set_header_bit(Value::FUNCTION_MACRO_BIT);

  return fn;
}

Value fn_install_macroexpander(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "install-macroexpander";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  state.macroexpander = argv[0];

  return C_UNSPECIFIED;

}

Value fn_make_rename(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "make-rename";
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_EXPECT_TYPE(state, argv, 1, SYMBOL);

  return state.make_rename(argv[1], argv[0]);
}

Value fn_rename_env(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-env";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);
  return argv[0].rename_env();
}

Value fn_rename_expr(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-env";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);
  return argv[0].rename_expr();
}

Value fn_eval_lambda(State& state, size_t argc, Value* argv) {
  return state.eval_lambda(argv[1], argv[0]);
}

///// MISC

Value fn_raise(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "raise";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);
  Value tag = argv[0], message = argv[1], irritants = argv[2], exc;
  AR_FRAME(state, tag, message, irritants, exc);
  exc = state.make_exception(tag, message, irritants);
  return exc;
}


void State::install_builtin_functions() {
  // Numbers
  defun("fx+", fn_fx_add, 1, 1, true);
  defun("fx=", fn_fx_equals, 2, 2, true);
  defun("fx-", fn_fx_sub, 1, 1, true);
  defun("fx<", fn_fx_lt, 2);
  defun("fx>", fn_fx_gt, 2);

  // TODO Variadic arguments
  

  defun("+", fn_add, 2);
  defun("-", fn_sub, 2);
  defun("/", fn_div, 2);
  defun("*", fn_mul, 2);
  defun("=", fn_num_equal, 2);

  // Predicates
  defun("null?", fn_nullp, 1);
  defun("char?", fn_charp, 1);
  defun("procedure?", fn_procedurep, 1);
  defun("pair?", fn_pairp, 1);
  defun("symbol?", fn_symbolp, 1);
  defun("rename?", fn_renamep, 1);
  defun("macro?", fn_macrop, 1);
  defun("self-evaluating?", fn_self_evaluatingp, 1);
  defun("identifier?", fn_identifierp, 1);
  defun("box?", fn_boxp, 1);
  defun("fixnum?", fn_fixnump, 1);
  defun("flonum?", fn_flonump, 1);
  //defun("constant=?", fn_constantp, 1);

  // Lists
  defun("cons", fn_cons, 2);
  defun("list", fn_list, 0, 0, true);
  defun("list?", fn_listp, 1);
  defun("list-ref", fn_list_ref, 2);
  defun("length", fn_length, 1);
  defun("map", fn_map, 2);
  defun("for-each.", fn_for_each_dot, 2);
  defun("apply", fn_apply, 2);
  defun("eval", fn_eval, 1);

  // Vectors
  defun("make-vector", fn_make_vector, 0, 2);
  defun("vector-ref", fn_vector_ref, 2);
  defun("vector-set!", fn_vector_set, 3);

  // Equality
  defun("eq?", fn_eq, 2);
  defun("eqv?", fn_eqv, 2);
  defun("equal?", fn_equal, 2);

  // I/O
  defun("display", fn_display, 1, 1, false);
  defun("newline", fn_newline, 0);
  defun("print", fn_print, 0, 0, true);
  defun("print-string", fn_print_string, 0, 0, true);

  // Pairs
  defun("car", fn_car, 1, 1);
  defun("cdr", fn_cdr, 1, 1);

  // Exceptions
  defun("raise", fn_raise, 3);

  // Macroexpansion support functionality
  defun("cons-source", fn_cons_source, 3);
  defun("list-source", fn_list_source, 0, 0, true);
  defun("source-location", fn_source_location, 1);

  defun("env-make", fn_env_make, 1);
  defun("env-define", fn_env_define, 3);
  defun("env-compare", fn_env_compare, 3);
  defun("env-lookup", fn_env_lookup, 2);
  defun("env-syntax?", fn_env_syntaxp, 2);

  defun("gensym", fn_gensym, 0, 1);

  // TODO: Full eval/apply necessary? Probably...
  defun("install-macroexpander", fn_install_macroexpander, 1);
  defun("make-rename", fn_make_rename, 2);
  defun("rename-env", fn_rename_env, 1);
  defun("rename-expr", fn_rename_expr, 1);
  defun("eval-lambda", fn_eval_lambda, 2);
  defun("set-function-name!", fn_set_function_name, 2);
  defun("set-function-macro-bit!", fn_set_function_macro_bit, 1);
  defun("function-env", fn_function_env, 1);
}

}
