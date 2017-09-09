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
    std::cout << argv[i];
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

void State::install_builtin_functions() {
  // Numbers
  defun("fx+", fn_fx_add, 2, 2);
  defun("fx=", fn_fx_equals, 2, 2);
  defun("fx-", fn_fx_sub, 2, 2);

  // Predicates
  defun("null?", fn_nullp, 1, 1);
  defun("char?", fn_charp, 1, 1);
  defun("procedure?", fn_procedurep, 1, 1);
  defun("pair?", fn_pairp, 1, 1);
  
  // Lists
  defun("list?", fn_listp, 1, 1);

  // Equality
  defun("eq?", fn_eq, 2, 2);
  defun("eqv?", fn_eqv, 2, 2);
  defun("equal?", fn_equal, 2, 2);

  // I/O
  defun("display", fn_display, 1, 1, false);
  defun("newline", fn_newline, 1, 1, false);
  defun("print", fn_print, 0, 0, true);

  // Pairs
  defun("car", fn_car, 1, 1);
  defun("cdr", fn_cdr, 1, 1);
}

}