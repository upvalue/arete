// strings.cpp - Strings, characters, and bytevectors

#include "arete.hpp"

namespace arete {

Value State::make_string(const std::string& body) {
  String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + body.size()));
  heap->bytes = body.size();
  strncpy(heap->data, body.c_str(), body.size());
  AR_ASSERT(heap->data[heap->bytes] == '\0');

  return heap;
}

Value State::make_string(size_t length) {
  String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + length));
  heap->bytes = length;
  memset(heap->data, 'a', length);
  AR_ASSERT(heap->data[heap->bytes] == '\0');
  return heap;
}

Value State::string_copy(Value x) {
  AR_FRAME(this, x);
  String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + x.string_bytes()));
  strncpy(heap->data, x.string_data(), x.string_bytes());
  heap->bytes = x.string_bytes();
  heap->data[x.string_bytes()] = '\0';
  return heap;
}

///// SCHEME FUNCTIONS

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
  return state.make_char((char)argv[0].fixnum_value());
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

Value fn_make_string(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "make-string";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  char fill = '$';
  if(argc == 2) {
    AR_FN_EXPECT_TYPE(state, argv, 1, CHARACTER);
    fill = (char) argv[1].character();
  }
  Value str = state.make_string((size_t) argv[0].fixnum_value());

  memset(str.as_unsafe<String>()->data, fill, str.string_bytes());

  return str;
}
AR_DEFUN("make-string", fn_make_string, 1, 2);

Value fn_string_set(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-set!";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, CHARACTER);
  AR_FN_CHECK_BOUNDS(state, "string", argv[0].string_bytes(), argv[1].fixnum_value());

  argv[0].string_data_mod()[(size_t) argv[1].fixnum_value()] = argv[2].character();

  return C_UNSPECIFIED;
}
AR_DEFUN("string-set!", fn_string_set, 3);

Value fn_string_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_CHECK_BOUNDS(state, "string", argv[0].string_bytes(), argv[1].fixnum_value());

  return state.make_char(argv[0].string_data()[((size_t)argv[1].fixnum_value())]);
}
AR_DEFUN("string-ref", fn_string_ref, 2);

Value fn_substring(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "substring";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_POSITIVE(state, argv, 1);
  AR_FN_EXPECT_POSITIVE(state, argv, 2);

  AR_FN_CHECK_BOUNDS(state, "string", argv[0].string_bytes(), argv[1].fixnum_value());
  AR_FN_CHECK_BOUNDS(state, "string", argv[0].string_bytes() + 1, argv[2].fixnum_value());

  Value str = argv[0];
  size_t end = (size_t) argv[2].fixnum_value(), start = (size_t)argv[1].fixnum_value();
  AR_FRAME(state, str);

  Value substr = state.make_string(end - start);
  const char* substring = (str.string_data() + start);

  memcpy(substr.string_data_mod(), substring, end - start);

  return substr;
}
AR_DEFUN("substring", fn_substring, 3);

Value fn_string_equals(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string=?";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);
  if(argv[0].string_bytes() != argv[1].string_bytes()) return C_FALSE;

  return Value::make_boolean(strncmp(argv[0].string_data(), argv[1].string_data(), argv[0].string_bytes()) == 0);
}
AR_DEFUN("string=?", fn_string_equals, 2);

Value fn_string_length(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "string-length";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  return Value::make_fixnum((ptrdiff_t) argv[0].string_bytes());
}
AR_DEFUN("string-length", fn_string_length, 1);

Value fn_make_bytevector(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "make-bytevector";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  size_t sz = static_cast<size_t>(argv[0].fixnum_value());
  Value bv = state.make_bytevector<uint8_t>(sz);

  if(argc == 2) {
    AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
    size_t fill = static_cast<size_t>(argv[1].fixnum_value());
    if(fill != 0) {
      for(size_t i = 0; i != sz; i++) {
        bv.bv_set<uint8_t>(i, fill);
      }
    }
  }
  return bv;
}
AR_DEFUN("make-bytevector", fn_make_bytevector, 1, 2);

Value fn_bytevector_length(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "bytevector-length";
  AR_FN_EXPECT_TYPE(state, argv, 0, BYTEVECTOR);
  return argv[0].bv_length();
}
AR_DEFUN("bytevector-length", fn_bytevector_length, 1);

Value fn_bytevector_u8_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "bytevector-u8-ref";
  AR_FN_EXPECT_TYPE(state, argv, 0, BYTEVECTOR);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_CHECK_BOUNDS(state, "bytevector", argv[0].bv_length(), argv[1].fixnum_value());
  return Value::make_fixnum(argv[0].bv_ref<uint8_t>(argv[1].fixnum_value()));
}
AR_DEFUN("bytevector-u8-ref", fn_bytevector_u8_ref, 2);

Value fn_bytevector_u8_set(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "bytevector-u8-set!";
  AR_FN_EXPECT_TYPE(state, argv, 0, BYTEVECTOR);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_CHECK_BOUNDS(state, "bytevector", argv[0].bv_length(), argv[1].fixnum_value());
  ptrdiff_t value = argv[2].fixnum_value();

  AR_FN_CHECK_FX_RANGE(state, 2, argv[2], 0, 255);

  argv[0].bv_set<uint8_t>((size_t) argv[1].fixnum_value(), static_cast<uint8_t>(value));

  return C_UNSPECIFIED;
}

AR_DEFUN("bytevector-u8-set!", fn_bytevector_u8_set, 3);

// AR_DEFUN("fixnum-vector->bytevector")

void load_string_functions(State& state) {
  strings.install(state);
}

}
