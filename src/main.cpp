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

int main(int argc, char *argv[]) {
  int result = do_main(argc, argv);
  return result;
}
