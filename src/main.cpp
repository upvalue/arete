// main.cpp - Entry point for standalone builds
#include "arete.hpp"

using namespace arete;

int do_main(int argc, char* argv[]) {
  arete::State* state = new arete::State();
  int return_code = state->enter_cli(argc, argv);
  delete state;
  arete_free_function_tables();
  return return_code;
}

#ifndef __EMSCRIPTEN__
int main(int argc, char *argv[]) {
  int result = do_main(argc, argv);
  return result;
}
#else

arete::State *main_state = 0;

extern "C" {

void ar_init() {
  main_state = new arete::State();
}

void ar_eval_and_print(const char* str) {

}

void ar_free() {
  delete main_state;
}

}

#endif
