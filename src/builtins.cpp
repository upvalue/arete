// builtins.cpp - builtin functionality

#include <algorithm>
#include <fstream>
#include <iostream>
#ifndef _MSC_VER
# include <sys/time.h>
#endif

#include "arete.hpp"

namespace arete {

Value State::load_file(const std::string& path) {
  Value x, tmp;
  AR_FRAME(*this, x, tmp);

  x = slurp_file(path);

  AR_FN_STATE_CHECK(x);

  x = eval_toplevel_list(x);

  return x;
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
  if(argv[0].type() == CHARACTER && argv[1].type() == CHARACTER) {
    return Value::make_boolean(argv[0].character() == argv[1].character());
  }
  return fn_eq(state, argc, argv);
}

Value fn_equal(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(state.equals(argv[0], argv[1]));
}

Value fn_string_append(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-append";
  std::ostringstream os;
  for(size_t i = 0; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, STRING);
    os << argv[i].string_data();
  }

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

Value fn_value_type(State& state, size_t argc, Value* argv) {
  return Value::make_fixnum(argv[0].type());
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

Value fn_list(State& state, size_t argc, Value* argv) {
  return fn_list_impl(state, argc, argv, false);
}

Value fn_list_source(State& state, size_t argc, Value* argv) {
  return fn_list_impl(state, argc, argv, true);
}

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
Value fn_map_impl(State& state, size_t argc, Value* argv, const char* fn_name, bool improper, bool ret, bool indice) {
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
  
  size_t i = 0;

  while(lst.type() == PAIR) {
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

      if(nlst_current == C_NIL) {
        nlst_head = nlst_current = tmp; // state.make_pair(tmp, C_NIL);
      } else {
        nlst_current.set_cdr(tmp);
        nlst_current = tmp;
      }
    }
    i++;
    lst = lst.cdr();
  }

  // Optionally handle an improper list
  if(lst != C_NIL) {
    if(improper) {
      Value argv[1] = {lst};
      tmp = state.apply(fn, 1, argv);

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
  return fn_map_impl(state, argc, argv, "map", false, true, false);
}

Value fn_map_improper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "map-improper", true, true, false);
}

Value fn_foreach_proper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each", true, false, false);
}

Value fn_foreach_improper(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each-improper", true, false, false);
}

Value fn_map_proper_i(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "map-i", false, true, true);
}

Value fn_for_each_proper_i(State& state, size_t argc, Value* argv) {
  return fn_map_impl(state, argc, argv, "for-each-i", false, false, true);
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

  size_t length = argv[1].list_length();
  Value sub_argv;

  Value fn = argv[0], args = argv[1], tmp;
  AR_FRAME(state, fn, args, tmp, sub_argv);

  args = argv[1];

  sub_argv = state.make_vector_storage(length);
  while(args.type() == PAIR) {
    state.vector_storage_append(sub_argv, args.car());
    args = args.cdr();
  }

  tmp = state.apply_vector_storage(fn, sub_argv);

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

Value fn_set_car(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "set-car!";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  argv[0].set_car(argv[1]);
  return C_UNSPECIFIED;
}

Value fn_set_cdr(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "set-cdr!";
  AR_FN_EXPECT_TYPE(state, argv, 0, PAIR);
  argv[0].set_cdr(argv[1]);
  return C_UNSPECIFIED;
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

Value fn_vector_length(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "vector-length";
  AR_FN_EXPECT_TYPE(state, argv, 0, VECTOR);

  return Value::make_fixnum(argv[0].vector_length());
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

Value fn_table_entries(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-entries";
  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);
  return Value::make_fixnum(argv[0].as<Table>()->entries);
}

Value fn_table_for_each(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-for-each";

  AR_FN_ASSERT_ARG(state, 0, "to be applicable", argv[0].applicable());
  AR_FN_EXPECT_TYPE(state, argv, 1, TABLE);


  Value table = argv[1], fn = argv[0], chain, arg, tmp;
  AR_FRAME(state, table, fn, chain, arg);

  for(size_t i = 0; i != table.as<Table>()->chains->length; i++) {
    chain = table.as<Table>()->chains->data[i];
    if(chain != C_FALSE) {
      while(chain != C_NIL) {
        arg = state.make_pair(chain.cdar(), C_NIL);
        arg = state.make_pair(chain.caar(), arg);

        Value argv[2] = {chain.caar(), chain.cdar()};
        tmp = state.apply(fn, 2, argv);
        if(tmp.is_active_exception()) return tmp;
        chain = chain.cdr();
      }
    }
  }

  return C_UNSPECIFIED;
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

Value fn_gensymp(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "gensym?";

  return Value::make_boolean(argv[0].type() == SYMBOL &&
    argv[0].heap->get_header_bit(Value::SYMBOL_GENSYM_BIT));
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
          AR_ASSERT(rename_key.rename_gensym() != C_FALSE);
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

  if((type1 != SYMBOL && type1 != RENAME) || (type2 != SYMBOL && type2 != RENAME)
    || (type1 == SYMBOL && type2 == SYMBOL)) {
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
  switch(result.type()) {
    case FUNCTION:
      return Value::make_boolean(result.function_is_macro());
    case CLOSURE: case VMFUNCTION:
      return Value::make_boolean(result.closure_unbox().vm_function_is_macro());
    default: return C_FALSE;
  }
}

Value fn_set_function_name(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-function-name!";
  AR_FN_EXPECT_TYPE(state, argv, 1, SYMBOL);

  switch(argv[0].type()) {
    case FUNCTION: argv[0].as_unsafe<Function>()->name = argv[0]; break;
    case VMFUNCTION: argv[0].as_unsafe<VMFunction>()->name = argv[1]; break;
    default: break;
  }
  return C_UNSPECIFIED;
}

Value fn_set_vmfunction_name(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "set-vmfunction-name!";
  AR_FN_EXPECT_TYPE(state, argv, 0, VMFUNCTION);

  VMFunction* fn = argv[0].as<VMFunction>();
  fn->name = argv[1];

  return C_UNSPECIFIED;
}

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
    fn.heap->set_header_bit(Value::VMFUNCTION_LOG_BIT);
  }

  return C_UNSPECIFIED;
}

Value fn_function_env(State& state, size_t argc, Value* argv) {
  // static const char* fn_name = "function-env";
  switch(argv[0].type()) {
    case FUNCTION: return argv[0].function_parent_env();
    case CLOSURE:
    case VMFUNCTION: return argv[0].vm_function_macro_env();
    default: return state.type_error("function-env expected valid macro");
  }
}

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

Value fn_function_body(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-body";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  return argv[0].as<Function>()->body;
}

Value fn_function_name(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-name";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  return argv[0].as<Function>()->name;
}

Value fn_function_arguments(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-arguments";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);

  return argv[0].as<Function>()->arguments;
}

Value fn_function_rest_arguments(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "function-rest-arguments";
  AR_FN_EXPECT_TYPE(state, argv, 0, FUNCTION);
  
  return argv[0].as<Function>()->rest_arguments;
}

Value fn_top_level_value(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "top-level-value";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);

  Value r = argv[0].symbol_value();
  if(r == C_UNDEFINED) return C_UNSPECIFIED;
  return r;
}

Value fn_top_level_bound(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "top-level-bound?";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);

  return Value::make_boolean(argv[0].symbol_value() != C_UNDEFINED);
}

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
  return Value::make_fixnum(keys.size());
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
  Value env = argv[1], exp = argv[0];
  AR_FRAME(state, env, exp);
  exp = state.make_pair(exp, C_NIL);
  return state.eval_toplevel_list(exp);
  //return state.eval(C_FALSE, argv[0]);
}

///// MISC

Value fn_raise(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "raise";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);
  Value tag = argv[0], message = argv[1], irritants = argv[2], exc;
  AR_FRAME(state, tag, message, irritants, exc);
  exc = state.make_exception(tag, message, irritants);
  AR_ASSERT(exc.is_active_exception());
  return exc;
}

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

  size_t tag = state.register_record_type(cname, field_count, data.fixnum_value(), fields, parent);

  return state.globals[tag];
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

  argv[1].record_set(argv[2].fixnum_value(), argv[3]);

  return C_UNSPECIFIED;
}

Value fn_record_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "record-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, RECORD_TYPE);
  AR_FN_EXPECT_TYPE(state, argv, 1, RECORD);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);

  _AR_FN_EXPECT_RECORD_ISA(state, argv[0], argv[1]);

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
  // static const char* fn_name = "record-isa?";
  if(argv[0].type() != RECORD)
    return C_FALSE;
  Value rec = argv[0], rtd = argv[1];

  return Value::make_boolean(rec.record_isa(rtd));
}

// Compiler
Value fn_list_get_source(State& state, size_t argc, Value* argv) {
 //  static const char* fn_name = "list-get-source";
  Value vec, lst = argv[0];
  if(argv[0].type() == PAIR && argv[0].pair_has_source()) {
    AR_FRAME(state, lst, vec);

    vec = state.make_vector();

    SourceLocation src(lst.pair_src());

    state.vector_append(vec, Value::make_fixnum(src.source));
    state.vector_append(vec, Value::make_fixnum(src.line));
    state.vector_append(vec, Value::make_fixnum(src.begin));
    state.vector_append(vec, Value::make_fixnum(src.length));

    return vec;
  }
  return C_FALSE;
}

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
    sources_blob = state.make_blob<size_t>(length);
    for(size_t i = 0; i != length; i++) {
      sources_blob.blob_set<size_t>(i, sources.vector_ref(i).fixnum_value());
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
  vfn->min_arity = rec.record_ref(9).fixnum_value();
  vfn->max_arity = rec.record_ref(10).fixnum_value();
  vfn->bytecode_size = bytecode_size;
  vfn->stack_max = rec.record_ref(6).fixnum_value();
  vfn->local_count = rec.record_ref(5).fixnum_value();
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

Value fn_value_bits(State& state, size_t argc, Value* argv) {
  return Value::make_fixnum(argv[0].bits);
}

Value fn_value_header_bit(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "value-header-bit";
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  if(argv[0].immediatep()) {
    return state.eval_error("value-header bit got immediate object");
  }
  return Value::make_boolean(argv[0].heap->get_header_bit(argv[1].fixnum_value() > 0));
}

// Garbage collector

Value fn_gc_collect(State& state, size_t argc, Value* argv) {
  state.gc.collect();
  return C_UNSPECIFIED;
}

Value fn_current_millisecond(State& state, size_t argc, Value* argv) {
#ifdef _MSC_VER
	return Value::make_fixnum(0);
#else
  struct timeval te; 
  gettimeofday(&te, NULL); // get current time
  size_t milliseconds = te.tv_sec*1000LL + te.tv_usec/1000; // caculate milliseconds

  return Value::make_fixnum(milliseconds);
#endif
}

Value fn_exit(State& state, size_t argc, Value* argv) {
  if(argv[1] == C_TRUE) {
    exit(EXIT_SUCCESS);
  } else {
    exit(EXIT_FAILURE);
  }
  return C_UNSPECIFIED;
}

void State::defun_core(const std::string& cname, c_function_t addr, size_t min_arity, size_t max_arity, bool variable_arity) {
  Value cfn, sym, name;

  AR_FRAME(this, cfn, sym, name);
  name = make_string(cname);
  cfn = make_c_function(name, addr, min_arity, max_arity, variable_arity);

  sym = get_symbol(name);
  sym.set_symbol_value(cfn);

  // sym.heap->set_header_bit(Value::SYMBOL_IMMUTABLE_BIT);
}

Value fn_char_to_integer(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "char->integer";
  AR_FN_EXPECT_TYPE(state, 0, argv, CHARACTER);
  return Value::make_fixnum(argv[0].character());
}

Value fn_integer_to_char(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "integer->char";
  AR_FN_EXPECT_TYPE(state, 0, argv, FIXNUM);
  return state.make_char(argv[0].fixnum_value());
}

void State::load_builtin_functions() {
  // Conversion
  defun_core("string->symbol", fn_string_to_symbol, 1);
  defun_core("symbol->string", fn_symbol_to_string, 1);
  defun_core("char->integer", fn_char_to_integer, 1);
  defun_core("integer->char", fn_integer_to_char, 1);

  // Predicates
  defun_core("procedure?", fn_procedurep, 1);
  defun_core("macro?", fn_macrop, 1);

  // Operations on raw objects
  defun_core("value-type", fn_value_type, 1);
  defun_core("value-bits", fn_value_bits, 1);
  defun_core("value-header-bit?", fn_value_header_bit, 2);

  // Strings
  defun_core("string-copy", fn_string_copy, 1);

  // Lists
  defun_core("cons", fn_cons, 2);
  defun_core("list", fn_list, 0, 0, true);
  defun_core("cons*", fn_cons_star, 1, 1, true);
  defun_core("list?", fn_listp, 1);
  defun_core("list-ref", fn_list_ref, 2);
  defun_core("list-join", fn_list_join, 2);
  defun_core("length", fn_length, 1);

  defun_core("map-i", fn_map_proper_i, 2);
  defun_core("for-each-i", fn_for_each_proper_i, 2);
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
  defun_core("vector-length", fn_vector_length, 1);
  defun_core("vector-set!", fn_vector_set, 3);
  defun_core("vector-append!", fn_vector_append, 2);

  // Tables
  defun_core("make-table", fn_make_table, 0);
  defun_core("table-ref", fn_table_ref, 2);
  defun_core("table-set!", fn_table_set, 3);
  defun_core("table-for-each", fn_table_for_each, 2);
  defun_core("table-entries", fn_table_entries, 1);

  // Equality
  defun_core("eq?", fn_eq, 2);
  defun_core("eqv?", fn_eqv, 2);
  defun_core("equal?", fn_equal, 2);

  // I/O
  defun_core("string-append", fn_string_append, 0, 0, true);
  //defun_core("load", fn_load_file, 1);

  // Pairs
  defun_core("car", fn_car, 1, 1);
  defun_core("cdr", fn_cdr, 1, 1);
  defun_core("set-car!", fn_set_car, 2, 2);
  defun_core("set-cdr!", fn_set_car, 2, 2);

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
  defun_core("gensym?", fn_gensymp, 1);

  // Function modification
  defun_core("set-function-name!", fn_set_function_name, 2);
  defun_core("set-function-macro-bit!", fn_set_function_macro_bit, 1);
  defun_core("function-min-arity", fn_function_min_arity, 1);
  defun_core("function-env", fn_function_env, 1);
  defun_core("function-body", fn_function_body, 1);
  defun_core("function-name", fn_function_name, 1);
  defun_core("function-arguments", fn_function_arguments, 1);
  defun_core("function-rest-arguments", fn_function_rest_arguments, 1);
  defun_core("set-vmfunction-name!", fn_set_vmfunction_name, 2);
  defun_core("set-vmfunction-log!", fn_set_vmfunction_log, 2);

  defun_core("top-level-value", fn_top_level_value, 1);
  defun_core("top-level-bound?", fn_top_level_bound, 1);
  defun_core("set-top-level-value!", fn_set_top_level_value, 2);

  defun_core("top-level-for-each", fn_top_level_for_each, 1);

  // Records
  defun_core("register-record-type", fn_register_record_type, 5);
  defun_core("set-record-type-printer!", fn_set_record_type_printer, 2);
  defun_core("set-record-type-apply!", fn_set_record_type_apply, 2);

  defun_core("make-record", fn_make_record, 1);
  defun_core("record-ref", fn_record_ref, 3);
  defun_core("record-set!", fn_record_set, 4);
  defun_core("record-type-descriptor", fn_record_type_descriptor, 1);
  defun_core("record-isa?", fn_record_isa, 2);

  // Compiler
  defun_core("list-get-source", fn_list_get_source, 1);
  defun_core("OpenFn->procedure", fn_openfn_to_procedure, 1);


  // Garbage collector
  defun_core("gc:collect", fn_gc_collect, 0);

  // Misc
  defun_core("exit", fn_exit, 1);
  defun_core("current-millisecond", fn_current_millisecond, 0);

}

} // namespace arete
