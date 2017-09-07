// arete.cpp - arete builtin functions

#include "arete.hpp"

namespace arete {

Value fn_fx_add(State& state, size_t argc, Value* argv) {
  const char* fn_name = "fx+";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  return Value::make_fixnum(argv[0].fixnum_value() + argv[1].fixnum_value());
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

void State::install_builtin_functions() {
  defun("fx+", fn_fx_add, 2, 2);
  defun("print", fn_print, 0, 0, true);
}

}