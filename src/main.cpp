// main.cpp - Entry point for standalone builds and Emscripten interface
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

#include <emscripten.h>

static arete::State *state = 0;
static size_t i = 0;

extern "C" {

EMSCRIPTEN_KEEPALIVE
extern void ar_init() {
}

EMSCRIPTEN_KEEPALIVE
extern void ar_eval_and_print(const char* image, const char* str) {
  if(state == 0) {
    state = new arete::State();
    state->boot_from_image(image);
  }

  // std::cout << "evaluating: " << str << std::endl;

  std::stringstream ss;
  ss >> std::noskipws;
  ss << str << std::endl;
  
  std::ostringstream src_name;
  src_name << "repl-line-" << i++;
  XReader reader(*state, ss, false, src_name.str());

  Value x;
  AR_FRAME(state, x);
  while(true) {
    x = reader.read();
    if(x.is_active_exception()) {
      state->print_exception(std::cerr, x);
    }

    //std::cout << "reader: " << x << std::endl;

    if(x == C_EOF) break;

    x = state->make_src_pair(x, C_NIL, x);
    //std::cout << "evaluating: " << x << std::endl;
    x = state->eval_list(x);

    if(x.is_active_exception()) {
      //std::cout << "got exception" << std::endl;
      state->print_exception(std::cerr, x);
      break;
    }

    if(x != C_UNSPECIFIED)
      std::cout << x << std::endl;
    else 
      std::cout.flush();
  }
}

EMSCRIPTEN_KEEPALIVE
extern void howdy() {
  std::cout << "HELLO :)" << std::endl;
}

EMSCRIPTEN_KEEPALIVE
int ar_free() {
  delete state;
  return 1;
}

}

#endif
