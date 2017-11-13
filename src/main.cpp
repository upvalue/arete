// main.cpp - Entry point for standalone builds
#include "arete.hpp"

using namespace arete;

int do_main(int argc, char* argv[]) {
  arete::State* state = new arete::State();
  int return_code = state->enter_cli(argc, argv);
  delete state;
  return return_code;
}

int main(int argc, char *argv[]) {
#if 0
  arete::State* state = new arete::State();

  state->boot();
  state->load_file("bootstrap.scm");
  //state->enter_repl();

  state->save_image("heap.boot");

  delete state;
  state = new arete::State();

  const char* result = state->boot_from_image("heap.boot");

  state->enter_repl();

  if(result) std::cerr << result << std::endl;

  delete state;

  return 0;
#endif
  return do_main(argc, argv);
}
