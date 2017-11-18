// strings.cpp - Strings and characters

#include "arete.hpp"

namespace arete {

DefunGroup strings("strings");

// Conversions
Value fn_string_to_symbol(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string->symbol";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  return state.get_symbol(argv[0]);
}
AR_DEFUN("string->symbol", fn_string_to_symbol, 1);

Value fn_symbol_to_string(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "symbol->string";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  std::string str(argv[0].symbol_name_data());
  return state.make_string(str);
}
AR_DEFUN("symbol->string", fn_symbol_to_string, 1);

Value fn_char_to_integer(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "char->integer";
  AR_FN_EXPECT_TYPE(state, 0, argv, CHARACTER);
  return Value::make_fixnum(argv[0].character());
}
AR_DEFUN("char->integer", fn_char_to_integer, 1);

Value fn_integer_to_char(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "integer->char";
  AR_FN_EXPECT_TYPE(state, 0, argv, FIXNUM);
  return state.make_char(argv[0].fixnum_value());
}
AR_DEFUN("integer->char", fn_integer_to_char, 1);

Value fn_char_numeric(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "char-numeric?";
  AR_FN_EXPECT_TYPE(state, 0, argv, CHARACTER);
  return Value::make_boolean(argv[0].character() >= '0' && argv[0].character() <= '9');
}
AR_DEFUN("char-numeric?", fn_char_numeric, 1);


Value fn_string_append(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-append";
  std::ostringstream os;
  for(size_t i = 0; i != argc; i++) {
    AR_FN_EXPECT_HEAP_TYPE(state, argv, i, STRING);
    os << argv[i].string_data();
  }

  return state.make_string(os.str());
}
AR_DEFUN("string-append", fn_string_append, 0, 0, true);

Value fn_string_copy(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-copy";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  return state.string_copy(argv[0]);
}
AR_DEFUN("string-copy", fn_string_copy, 1);

Value fn_string_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  // (string-ref "asd" 3)
  if(argv[0].string_bytes() <= (size_t)argv[1].fixnum_value()) {
    std::ostringstream os;
    os << "attempted to get index " << argv[1].fixnum_value() << " in a string of length " << argv[0].string_bytes();
    return state.type_error(os.str());
  }

  return state.make_char(argv[0].string_data()[((size_t)argv[1].fixnum_value())]);
}
AR_DEFUN("string-ref", fn_string_ref, 2);

void load_string_functions(State& state) {
  strings.install(state);
}

}