// builtins.cpp - builtin functionality

#include <fstream>
#include <iostream>

#include "arete.hpp"

namespace arete {

size_t gc_collect_timer = 0;
State* current_state = 0;

Value State::load_stream(std::istream& input, size_t source) {
  XReader reader(*this, input);
  Value x;
  AR_FRAME(this, x);

  if(source > 0) {
    reader.source = source;
  }

  while(true) {
    x = reader.read();
    if(x == C_EOF) {
      break;
    }

    if(x.is_active_exception()) {
      return x;
    } else {
      x = eval_toplevel(x);
      if(x.is_active_exception()) {
        return x;
      }
    }
  }
  return x;
}

Value State::load_file(const std::string& path) {
  std::ifstream handle(path);

  // std::cout << ";; loading module " << path << std::endl; 

  if(!handle.good()) { 
    std::ostringstream os;
    os << "Could not load file " << path;
    return make_exception(globals[S_FILE_ERROR], os.str());
  }

  return load_stream(handle);
}

// Various state methods that rely on forward declarations
Value State::load_module(const std::string& identifier) {
  return C_FALSE;
}

// Casting arithmetic

// This is actually pretty tricky due to the need to cast from fixnum from flonum, the desire to
// avoid code duplication, and the need to avoid allocation because there's no good way currently
// to track variadic arguments

#define OPV(name, cname, operator, sub, nozero) \
  Value name(State& state, size_t argc, Value* argv) { \
    static const char* fn_name = cname; \
    ptrdiff_t fxresult = 0; \
    size_t i = 0; \
    if(argc == 1) { \
      AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].numeric()); \
      if(sub) return argv[0].type() == FLONUM ? state.make_flonum(0 - argv[0].flonum_value()) : Value::make_fixnum(0 - argv[0].fixnum_value()); \
      return argv[0]; \
    } \
    for(i = 0; i != argc; i++) { \
      if(argv[i].type() == FLONUM) break; \
      AR_FN_ASSERT_ARG(state, i, "to be a number", argv[i].type() == FIXNUM); \
      if(nozero && argv[i].fixnum_value() == 0) return state.type_error("divide by zero"); \
      if(i == 0) { fxresult = argv[i].fixnum_value(); } else { fxresult = fxresult operator argv[i].fixnum_value(); } \
    } \
    if(i == argc) return Value::make_fixnum(fxresult); \
    double flresult = (double) fxresult; \
    for(; i != argc; i++) { \
      AR_FN_ASSERT_ARG(state, i, "to be a number", argv[i].type() == FIXNUM || argv[i].type() == FLONUM); \
      if(i == 0) { flresult = argv[i].flonum_value(); continue; } \
      if(argv[i].type() == FIXNUM) { \
        if(nozero && argv[i].fixnum_value() == 0) return state.type_error("divide by zero"); \
        flresult = flresult operator (double) argv[i].fixnum_value(); \
      } else if(argv[i].type() == FLONUM) { \
        if(nozero && argv[i].flonum_value() == 0.0) return state.type_error("divide by zero"); \
        flresult = flresult operator argv[i].flonum_value(); \
      } \
    } \
    return state.make_flonum(flresult); \
  }

OPV(fn_add, "+", +, false, false)
OPV(fn_sub, "-", -, true, false)
OPV(fn_mul, "*", *, false, false)
OPV(fn_div, "/", /, false, true)

#undef OPV

#define OPV_BOOL(name, cname, operator) \
  Value name(State& state, size_t argc, Value* argv) { \
    static const char* fn_name = cname; \
    AR_FN_ASSERT_ARG(state, 0, "to be numeric", argv[0].numeric()); \
    for(size_t i = 0; i != argc - 1; i++) { \
      AR_FN_ASSERT_ARG(state, (i+1), "to be numeric", argv[i+1].numeric()); \
      if(argv[i].type() == FIXNUM) { \
        if(argv[i+1].type() == FIXNUM) { \
          if(!(argv[i].bits operator argv[i+1].bits)) return C_FALSE; \
        } else { \
          if(!(argv[i].fixnum_value() operator argv[i+1].flonum_value())) return C_FALSE; \
        } \
      } \
    } \
    Value lhs = argv[argc-2], rhs = argv[argc-1]; \
    if(lhs.type() == FLONUM && rhs.type() == FLONUM) \
      return Value::make_boolean(lhs.flonum_value() operator rhs.flonum_value()); \
    else if(lhs.type() == FLONUM && rhs.type() == FIXNUM) \
      return Value::make_boolean(lhs.flonum_value() operator rhs.fixnum_value()); \
    else if(lhs.type() == FIXNUM && rhs.type() == FLONUM) \
      return Value::make_boolean(lhs.fixnum_value() operator rhs.flonum_value()); \
    else if(lhs.type() == FLONUM && rhs.type() == FIXNUM) \
      return Value::make_boolean(lhs.flonum_value() operator rhs.fixnum_value()); \
    return C_TRUE; \
  }

// After writing all this out I see why the Lua/JS guys gave up and used floating point math only

OPV_BOOL(fn_num_equal, "=", ==)
OPV_BOOL(fn_lt, "<", <)
OPV_BOOL(fn_lte, "<=", <=)
OPV_BOOL(fn_gte, ">=", >=)
OPV_BOOL(fn_gt, ">", >)

#undef OPV_BOOL
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

// Conversions
Value fn_string_to_symbol(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string->symbol";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  return state.get_symbol(argv[0]);
}

Value fn_symbol_to_string(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "symbol->string";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  std::string str(argv[0].symbol_name_data());
  return state.make_string(str);
}

// Equality

Value fn_eq(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "eq?";
  return Value::make_boolean(argv[0].bits == argv[1].bits);
}

Value fn_eqv(State& state, size_t argc, Value* argv) {
  if(argv[0].type() == FLONUM && argv[1].type() == FLONUM)
    return Value::make_boolean(argv[0].flonum_value() == argv[1].flonum_value());
  return fn_eq(state, argc, argv);
}


Value fn_equal(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(state.equals(argv[0], argv[1]));
}

Value fn_display(State& state, size_t argc, Value* argv) {
  if(argv[0].type() == STRING)
    std::cout << argv[0].string_data();
  else
    std::cout << argv[0];
  return C_UNSPECIFIED;
}

Value fn_write(State& state, size_t argc, Value* argv) {
  std::cout << argv[0];
  return C_UNSPECIFIED;
}

Value fn_newline(State& state, size_t argc, Value* argv) {
  std::cout << std::endl;
  return C_UNSPECIFIED;
}

Value fn_print_impl(State& state, size_t argc, Value* argv, std::ostream& os, bool whitespace) {
  for(size_t i = 0; i != argc; i++) {
    if(argv[i].type() == STRING) {
      os << argv[i].string_data();
    } else {
      Value x = state.print(argv[i], os);
      if(x.is_active_exception()) return x;
    }

    if(whitespace && i != argc - 1)  {
      os << ' ';
    }
  }

  return C_UNSPECIFIED;
}

Value fn_print(State& state, size_t argc, Value* argv) {
  Value chk = fn_print_impl(state, argc, argv, std::cout, true);
  std::cout << std::endl;
  return chk;
}

Value fn_print_string(State& state, size_t argc, Value* argv) {
  std::ostringstream os;
  Value chk = fn_print_impl(state, argc, argv, os, true);
  if(chk.is_active_exception()) return chk;
  return state.make_string(os.str());
}

Value fn_string_append(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-append";
  for(size_t i = 0; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, STRING);
  }
  std::ostringstream os;
  fn_print_impl(state, argc, argv, os, false);
  return state.make_string(os.str());
}

Value fn_print_table_verbose(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "print-table-verbose";
  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);

  state.print_table_verbose(argv[0]);
  
  return C_UNSPECIFIED;
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

Value fn_flonump(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == FLONUM);
}

Value fn_tablep(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == TABLE);
}

Value fn_fixnump(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == FIXNUM);
}

Value fn_charp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == CHARACTER);
}

Value fn_stringp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == STRING);
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

Value fn_list_impl(State& state, size_t argc, Value* _argv, bool copy_source) {
  // static const char* fn_name = "list";
  AR_FRAME_ARRAY(state, argc, _argv, argv);

  Value head = C_NIL, current, tmp;
  SourceLocation loc;

  AR_FRAME(state, head, current, tmp);

  if(copy_source) {
    if(_argv[0].type() == PAIR && _argv[0].pair_has_source()) {
      loc = _argv[0].pair_src();
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

Value fn_appendm(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "append!";
  (void) fn_name;

  return C_FALSE;
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

  return h.car();
}

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

/** 
 * Underlying implementation of for-each and map. If improper is true, it will handle dotted lists,
 * if ret is true it will allocate and return a list of the results of function application.
 */
Value fn_map_impl(State& state, size_t argc, Value* argv, const char* fn_name, bool improper, bool ret) {

  AR_FN_ASSERT_ARG(state, 0, "to be a function", (argv[0].procedurep()));

  if(argv[1] == C_NIL) return ret ? C_NIL : C_UNSPECIFIED;
  AR_FN_EXPECT_TYPE(state, argv, 1, PAIR);

  if(!improper) {
    AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1].list_length() > 0));
  } else {
    AR_FN_EXPECT_TYPE(state, argv, 1, PAIR);
  }

  Value nlst_head, nlst_current = C_NIL, tmp, lst = argv[1], fn = argv[0], arg;
  AR_FRAME(state, nlst_head, nlst_current, lst, fn, arg, tmp);

  while(lst.type() == PAIR) {
    arg = state.make_pair(lst.car(), C_NIL);
    tmp = state.apply_generic(fn, arg, false);
    if(tmp.is_active_exception()) return tmp;
    if(ret) {
      // Try to preserve source information if it's available
      if(lst.pair_has_source()) {
        SourceLocation src = (lst.pair_src());
        tmp = state.make_src_pair(tmp, C_NIL, src);
      } else{
        tmp = state.make_pair(tmp, C_NIL);
      }

      if(nlst_current == C_NIL) {
        nlst_head = nlst_current = tmp; // state.make_pair(tmp, C_NIL);
      } else {
        nlst_current.set_cdr(tmp);
        nlst_current = tmp;
      }
    }
    lst = lst.cdr();
  }

  if(lst != C_NIL) {
    if(improper) {
      arg = state.make_pair(lst, C_NIL);
      tmp = state.apply_generic(fn, arg, false);
      if(tmp.is_active_exception()) return tmp;
      if(ret) {
        nlst_current.set_cdr(tmp);
      }
    } else {
      std::ostringstream os;
      os << fn_name << " got improper list";
      return state.type_error(os.str());
    }
  }

  return ret ? nlst_head : C_UNSPECIFIED;
}

Value fn_map_proper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "map", false, true);
}

Value fn_map_improper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "map-improper", true, true);
}

Value fn_foreach_proper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each", true, false);
}

Value fn_foreach_improper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each-improper", true, false);
}

enum Mem { MEMQ, MEMV, MEMBER };

Value fn_mem_impl(const char* fn_name, Mem method, State& state, size_t argc, Value* argv) {
  AR_FN_ASSERT_ARG(state, 1, "to be a list", (argv[1] == C_NIL || argv[1].list_length() > 0));

  if(argv[1] == C_NIL) {
    return C_FALSE;
  }

  Value lst = argv[1], obj = argv[0];
  while(lst.type() == PAIR) {
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

Value fn_memv(State& state, size_t argc, Value* argv) {
  return fn_mem_impl("memv", MEMV, state, argc, argv);
}

Value fn_member(State& state, size_t argc, Value* argv) {
  return fn_mem_impl("member", MEMBER, state, argc, argv);
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
  return argv[0].car();
}

Value fn_cdr(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "cdr";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  return argv[0].cdr();
}

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

Value fn_vector_append(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "vector-append!";
  AR_FN_EXPECT_TYPE(state, argv, 0, VECTOR);

  state.vector_append(argv[0], argv[1]);
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

Value fn_make_table(State& state, size_t argc, Value* argv) {
  //static const char* fn_name = "make-table";
  return state.make_table();
}

Value fn_table_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-ref";

  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);
  AR_FN_ASSERT_ARG(state, 1, "to be hashable", argv[1].hashable());

  bool found;
  Value result = state.table_get(argv[0], argv[1], found);
  if(!found) return C_FALSE;
  return result;
}

Value fn_table_set(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-set!";

  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);
  AR_FN_ASSERT_ARG(state, 1, "to be hashable", argv[1].hashable());

  return state.table_set(argv[0], argv[1], argv[2]);

}

///// STRINGS

Value fn_string_copy(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-copy";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  return state.string_copy(argv[0]);
}

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

/** Macro that asserts an argument is a valid environment */
#define AR_FN_EXPECT_ENV(state, n) \
 AR_FN_ASSERT_ARG((state), (n), "to be a valid environment (vector or #f)", argv[(n)].type() == VECTOR || argv[(n)] == C_FALSE);

#define AR_FN_EXPECT_IDENT(state, n) \
  AR_FN_ASSERT_ARG((state), (n), "to be a valid identifier (symbol or rename)", argv[(n)].identifierp())

Value fn_env_make(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-make";
  AR_FN_EXPECT_ENV(state, 0);
  return state.make_env(argv[0]);
}

Value fn_env_define(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-define";
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_EXPECT_IDENT(state, 1);

  Value env = argv[0], name = argv[1], value = argv[2];
  AR_FRAME(state, env, name, value);

  if(argv[0] == C_FALSE) {
    if(argc == 4 && argv[3] == C_TRUE) {
      name.as<Symbol>()->value = value;
    }

  } else {
    state.vector_append(env, name);
    state.vector_append(env, value);
  }

  return C_UNSPECIFIED;
}

/** env-resolve takes an environment and identifier and returns an appropriate symbol for runtime
 * For example, references to arete.core functions like car become ##arete.core#car,
 * renames in lambda lists become gensyms, and so on. If it can't resolve a symbol, it is eitehr
 * an undefined variable or a toplevel variable and is returned unmodified. */
Value fn_env_resolve(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-resolve";
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_EXPECT_IDENT(state, 1);

  Value env = argv[0], name, rename_key, result, renames, qname, imports, arg1 = argv[1], mname;
  AR_FRAME(state, env, name, rename_key, result, renames, qname, imports, arg1, mname);

  //Value env = argv[0];

  // (env-resolve (<table> vars) name)

  // If this is a vector, these bindings have been found in a lambda body.

  // Symbols and renames may refer to table-level variables, e.g. (print "something")
  // Must become ##arete.core#print
  // As might (make-rename <core> 'print)

  // Or they might reference variables introduced in the argument list of a lambda, which means that
  // symbols should be returned unmodified, and renames should become gensyms

  if(argv[0].type() == VECTOR) {
    // Note that env_lookup_impl takes env and rename_key as references
    bool found;
    result = state.env_lookup_impl(env, argv[1], rename_key, found);
    (void) result;

    // If a rename was introduced in the bindings of a lambda, it needs to resolve to a gensym
    if(env.type() != TABLE) {
      if(argv[1].type() == RENAME) {
        if(rename_key != C_FALSE) {
          return rename_key.rename_gensym();
        }
      }
      return argv[1];
    } 
  }

  return argv[1];
}

Value fn_env_compare(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-compare";

  Type type1 = argv[1].type(), type2 = argv[2].type();

  if((type1 != SYMBOL && type1 != RENAME) || (type2 != SYMBOL && type2 != RENAME)) {
    return Value::make_boolean(argv[1].bits == argv[2].bits);
  }

  //std::cout << argv[0] << std::endl;
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_EXPECT_IDENT(state, 1);
  AR_FN_EXPECT_IDENT(state, 2);

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
  Value rename_key;

  bool found1, found2;
  state.env_lookup_impl(rename_env, id1.rename_expr(), rename_key, found1);
  state.env_lookup_impl(env, id2, rename_key, found2);

  // If these both refer to a toplevel variable (i.e. not defined), this is true
  return Value::make_boolean((rename_env == env || (found1 && found1 == found2)) && id1.rename_expr() == id2);
}

// env-lookup
Value fn_env_lookup(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "env-lookup";
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_ASSERT_ARG(state, 1, "to be a valid identifier (symbol or rename)", argv[1].identifierp());

  Value arg0 = argv[0], arg1 = argv[1];
  Value env = argv[0], qname, result;
  AR_FRAME(state, env, qname, result, arg0, arg1);

  qname = fn_env_resolve(state, argc, argv);

  result = state.env_lookup(env, qname);
  
  if(result != C_UNDEFINED) {
    return result;
  }

  return state.env_lookup(arg0, arg1);
}

/** Check if a name is syntax (a builtin such as define, or a macro) in a particular environment.
  * Necessary because C_SYNTAX values cannot be handled by Scheme code directly. */
Value fn_env_syntaxp(State& state, size_t argc, Value* argv) {
  Value env, name, qname;
  AR_FRAME(state, env, qname, name);

  Value result = fn_env_lookup(state, argc, argv);

  if(result == C_SYNTAX) return C_TRUE;
  return Value::make_boolean(result.type() == FUNCTION && result.function_is_macro());
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

Value fn_function_min_arity(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-min-arity";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  return Value::make_fixnum(argv[0].function_arguments().list_length());
}

Value fn_top_level_value(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "top-level-value";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);

  Value r = argv[0].symbol_value();
  if(r == C_UNDEFINED) return C_UNSPECIFIED;
  return r;
}

Value fn_set_top_level_value(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-top-level-value!";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  argv[0].set_symbol_value(argv[1]);
  return C_UNSPECIFIED;
}

Value fn_make_rename(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "make-rename";
  AR_FN_EXPECT_ENV(state, 0);
  AR_FN_EXPECT_TYPE(state, argv, 1, SYMBOL);

  return state.make_rename(argv[1], argv[0]);
}

Value fn_rename_set_gensym(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-gensym!";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);

  Value renam = argv[0], sym;
  AR_FRAME(state, renam, sym);

  sym = state.gensym(renam.rename_expr());

  renam.as<Rename>()->gensym = sym;
  return C_UNSPECIFIED;
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

Value fn_rename_gensym(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "rename-gensym";
  AR_FN_EXPECT_TYPE(state, argv, 0, RENAME);
  return argv[0].rename_gensym();
}

Value fn_eval(State& state, size_t argc, Value* argv) {
  return state.eval(argv[1], argv[0]);
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

///// RECORDS
Value fn_register_record_type(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "register-record-type";
  Value parent = C_FALSE;
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_POSITIVE(state, argv, 2);

  Value name = argv[0], data = argv[2];
  ptrdiff_t field_count = argv[1].fixnum_value();

  if(argc == 4) {
    parent = argv[3];
    AR_FN_EXPECT_TYPE(state, argv, 3, RECORD_TYPE);
    field_count += parent.as<RecordType>()->field_count;
  }

  // TODO: Is it possible this doesn't copy string data and could potentially be moved?
  // Should string_data return char* ?
  std::string cname(name.string_data());

  size_t tag = state.register_record_type(cname, field_count, data.fixnum_value(), parent);

  return state.globals[tag];
}


#define AR_FN_EXPECT_APPLICABLE(state, argv, arg) \
  if(!((argv)[(arg)].applicable())) { \
    std::ostringstream os; \
    os << fn_name << " expected argument " << (arg) << " to be applicable but got a non-applicable " << (argv)[(arg)].type(); \
    return state.type_error(os.str()); \
  }

Value fn_set_record_type_printer(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-record-type-printer!";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_APPLICABLE(state, argv, 1);
  RecordType* rt = argv[0].as<RecordType>();
  rt->print = argv[1]; 
  return C_UNSPECIFIED;
}

Value fn_set_record_type_apply(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-record-type-apply!";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_APPLICABLE(state, argv, 1);
  RecordType* rt = argv[0].as<RecordType>();
  rt->apply = argv[1]; 
  return C_UNSPECIFIED;
}

#define AR_FN_EXPECT_RECORD_ISA(state, expected_type, record) \
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

  AR_FN_EXPECT_RECORD_ISA(state, argv[0], argv[1]);

  argv[1].record_set(argv[2].fixnum_value(), argv[3]);

  return C_UNSPECIFIED;
}

Value fn_record_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "record-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_TYPE(state, argv, 1, RECORD);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);

  AR_FN_EXPECT_RECORD_ISA(state, argv[0], argv[1]);

  return argv[1].record_ref(argv[2].fixnum_value());
}

Value fn_make_record(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "make-record";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);

  return state.make_record(argv[0].as<RecordType>());
}

Value fn_record_type_descriptor(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "record-type-descriptor";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD);
  return argv[0].as<Record>()->type;
}

Value fn_record_isa(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "record-isa?";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD);
  Value rec = argv[0], rtd = argv[1];

  return Value::make_boolean(rec.record_isa(rtd));
}

Value fn_gc_collect(State& state, size_t argc, Value* argv) {
  state.gc.collect();
  return C_UNSPECIFIED;
}

void State::install_core_functions() {
  // Numbers
  defun_core("fx+", fn_fx_add, 1, 1, true);
  defun_core("fx=", fn_fx_equals, 2, 2, true);
  defun_core("fx-", fn_fx_sub, 1, 1, true);
  defun_core("fx<", fn_fx_lt, 2);
  defun_core("fx>", fn_fx_gt, 2);

  // TODO Variadic arguments
  defun_core("+", fn_add, 1, 1, true);
  defun_core("-", fn_sub, 1, 1, true);
  defun_core("/", fn_div, 2, 2, true);
  defun_core("*", fn_mul, 1, 1, true);
  defun_core("=", fn_num_equal, 2, 2, true);
  defun_core("<", fn_lt, 2, 2, true);
  defun_core(">", fn_gt, 2, 2, true);
  defun_core("<=", fn_lte, 2, 2, true);
  defun_core(">=", fn_gte, 2, 2, true);

  // Conversion
  defun_core("string->symbol", fn_string_to_symbol, 1);
  defun_core("symbol->string", fn_symbol_to_string, 1);

  // Predicates
  defun_core("null?", fn_nullp, 1);
  defun_core("char?", fn_charp, 1);
  defun_core("procedure?", fn_procedurep, 1);
  defun_core("pair?", fn_pairp, 1);
  defun_core("symbol?", fn_symbolp, 1);
  defun_core("rename?", fn_renamep, 1);
  defun_core("macro?", fn_macrop, 1);
  defun_core("self-evaluating?", fn_self_evaluatingp, 1);
  defun_core("identifier?", fn_identifierp, 1);
  defun_core("fixnum?", fn_fixnump, 1);
  defun_core("flonum?", fn_flonump, 1);
  defun_core("table?", fn_tablep, 1);
  defun_core("string?", fn_stringp, 1);

  // Strings
  defun_core("string-copy", fn_string_copy, 1);

  // Lists
  defun_core("cons", fn_cons, 2);
  defun_core("list", fn_list, 0, 0, true);
  defun_core("list?", fn_listp, 1);
  defun_core("list-ref", fn_list_ref, 2);
  defun_core("list-join", fn_list_join, 2);
  defun_core("length", fn_length, 1);
  defun_core("append!", fn_appendm, 2);

  defun_core("map", fn_map_improper, 2);
  defun_core("map-improper", fn_map_improper, 2);
  defun_core("for-each", fn_foreach_proper, 2);
  defun_core("for-each-improper", fn_foreach_improper, 2);

  defun_core("memq", fn_memq, 2);
  defun_core("memv", fn_memv, 2);
  defun_core("member", fn_member, 2);
  
  defun_core("apply", fn_apply, 2);
  defun_core("eval", fn_eval, 2);

  // Vectors
  defun_core("make-vector", fn_make_vector, 0, 2);
  defun_core("vector-ref", fn_vector_ref, 2);
  defun_core("vector-set!", fn_vector_set, 3);
  defun_core("vector-append!", fn_vector_append, 2);

  // Tables
  defun_core("make-table", fn_make_table, 0);
  defun_core("table-ref", fn_table_ref, 2);
  defun_core("table-set!", fn_table_set, 3);

  // Equality
  defun_core("eq?", fn_eq, 2);
  defun_core("eqv?", fn_eqv, 2);
  defun_core("equal?", fn_equal, 2);

  // I/O
  defun_core("display", fn_display, 1, 1, false);
  defun_core("write", fn_write, 1, 1, false);
  defun_core("newline", fn_newline, 0);
  defun_core("print", fn_print, 0, 0, true);
  defun_core("print-string", fn_print_string, 0, 0, true);
  defun_core("string-append", fn_string_append, 0, 0, true);
  defun_core("print-table-verbose", fn_print_table_verbose, 1);

  // Pairs
  defun_core("car", fn_car, 1, 1);
  defun_core("cdr", fn_cdr, 1, 1);

  // Exceptions
  defun_core("raise", fn_raise, 3);

  ///// Expansion support functionality
  defun_core("cons-source", fn_cons_source, 3);
  defun_core("list-source", fn_list_source, 0, 0, true);

  // Environments
  defun_core("env-make", fn_env_make, 1);
  defun_core("env-resolve", fn_env_resolve, 2);
  defun_core("env-define", fn_env_define, 3, 4);
  defun_core("env-compare", fn_env_compare, 3);
  defun_core("env-lookup", fn_env_lookup, 2);
  defun_core("env-syntax?", fn_env_syntaxp, 2);
  
  // Renames
  defun_core("make-rename", fn_make_rename, 2);
  defun_core("rename-gensym!", fn_rename_set_gensym, 1);
  defun_core("rename-env", fn_rename_env, 1);
  defun_core("rename-expr", fn_rename_expr, 1);
  defun_core("rename-gensym", fn_rename_gensym, 1);
  defun_core("gensym", fn_gensym, 0, 1);

  // Function modification
  defun_core("set-function-name!", fn_set_function_name, 2);
  defun_core("set-function-macro-bit!", fn_set_function_macro_bit, 1);
  defun_core("function-min-arity", fn_function_min_arity, 1);
  defun_core("function-env", fn_function_env, 1);

  defun_core("top-level-value", fn_top_level_value, 1);
  defun_core("set-top-level-value!", fn_set_top_level_value, 2);

  // Records
  defun_core("register-record-type", fn_register_record_type, 3, 4);
  defun_core("set-record-type-printer!", fn_set_record_type_printer, 2);
  defun_core("set-record-type-apply!", fn_set_record_type_apply, 2);

  defun_core("make-record", fn_make_record, 1);
  defun_core("record-ref", fn_record_ref, 3);
  defun_core("record-set!", fn_record_set, 4);
  defun_core("record-type-descriptor", fn_record_type_descriptor, 1);
  defun_core("record-isa?", fn_record_isa, 2);


  // Garbage collector
  defun_core("gc:collect", fn_gc_collect, 0);
}

}