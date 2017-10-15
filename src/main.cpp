// main.cpp - Entry point for standalone builds
#include "arete.hpp"

int do_main(int argc, char* argv[]) {
  arete::State* state = new arete::State();
  state->boot();
  int return_code = state->enter_cli(argc, argv);
  delete state;
  return return_code;
}

int main(int argc, char *argv[]) {
  return do_main(argc, argv);
}
