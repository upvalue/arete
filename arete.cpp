// arete.cpp - arete builtin functions

#include "arete.hpp"

namespace arete {

State* current_state = 0;

Value fn_fx_sub(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx-";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_fixnum(argv[0].fixnum_value() - argv[1].fixnum_value());
}

Value fn_fx_add(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx+";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_fixnum(argv[0].fixnum_value() + argv[1].fixnum_value());
}

Value fn_fx_equals(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx=";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].fixnum_value() == argv[1].fixnum_value());
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
  return fn_eq(state, argc, argv);
}

Value fn_display(State& state, size_t argc, Value* argv) {
  std::cout << argv[0];
  return C_UNSPECIFIED;
}

Value fn_newline(State& state, size_t argc, Value* argv) {
  std::cout << std::endl;
  return C_UNSPECIFIED;
}

Value fn_print(State& state, size_t argc, Value* argv) {
  for(size_t i = 0; i != argc; i++) {
    if(argv[i].type() == STRING) {
      std::cout << argv[i].string_data();
    } else {
      std::cout << argv[i];
    }

    if(i != argc - 1)  {
      std::cout << ' ';
    }
  }
  std::cout << std::endl;
  return C_UNSPECIFIED;
}

///// PREDICATES

Value fn_nullp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0] == C_NIL);
}

Value fn_procedurep(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == FUNCTION || argv[0].type() == CFUNCTION);
}

Value fn_pairp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == PAIR);
}

Value fn_charp(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0].type() == CHARACTER);
}

Value fn_listp(State& state, size_t argc, Value* argv) {
  // return argv[0] == C_NIL || (argv[0].type() == PAIR && argv[0].list_length() > 
  if(argv[0] == C_NIL) return C_FALSE;
  while(argv[0].type() == PAIR) {
    if(argv[0].cdr() == C_NIL) return C_TRUE;
    argv[0] = argv[0].cdr();
  }
  return C_FALSE;
}

Value fn_self_evaluatingp(State& state, size_t argc, Value* argv) {
  if(argv[0].immediatep()) return C_TRUE;

  switch(argv[0].type()) {
    case VECTOR: 
      return C_TRUE;
    default:
      return C_FALSE;
  }
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

///// MISC

Value fn_not(State& state, size_t argc, Value* argv) {
  return Value::make_boolean(argv[0] == C_FALSE);
}

void State::install_builtin_functions() {
  // Numbers
  defun("fx+", fn_fx_add, 2);
  defun("fx=", fn_fx_equals, 2);
  defun("fx-", fn_fx_sub, 2);

  // Predicates
  defun("null?", fn_nullp, 1);
  defun("char?", fn_charp, 1);
  defun("procedure?", fn_procedurep, 1);
  defun("pair?", fn_pairp, 1);
  defun("self-evaluating?", fn_self_evaluatingp, 1);
  
  // Lists
  defun("list?", fn_listp, 1);

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

  // Pairs
  defun("car", fn_car, 1, 1);
  defun("cdr", fn_cdr, 1, 1);

  // Misc
  defun("not", fn_not, 1, 1);
}

}