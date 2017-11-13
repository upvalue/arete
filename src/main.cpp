// main.cpp - Entry point for standalone builds
#include "arete.hpp"

using namespace arete;

int do_main(int argc, char* argv[]) {
  arete::State* state = new arete::State();
  state->boot();
  int return_code = state->enter_cli(argc, argv);
  delete state;
  return return_code;
}

int main(int argc, char *argv[]) {
  //return do_main(argc, argv);
  arete::State* state = new arete::State();

  state->boot();

  Value v1 = state->make_pair(C_FALSE, C_FALSE);
  Value v2 = state->make_pair(v1, Value::make_fixnum(2));
  state->make_pair(v2, Value::make_fixnum(3));
  Value vec = state->make_vector_storage(4);
  state->vector_storage_append(vec, Value::make_fixnum(0));
  state->vector_storage_append(vec, Value::make_fixnum(1));
  state->vector_storage_append(vec, Value::make_fixnum(2));
  state->vector_storage_append(vec, v2);

  state->get_symbol("okay");
  Value vec2 = state->make_vector(vec);

  //state->globals.push_back(state->get_symbol("okay"));

  //state->globals.push_back(Value::make_fixnum(5));

  state->enter_repl();

  state->save_image("heap.boot");


  //state->globals.clear();
  //state->gc.block_cursor = 0;
  delete state;

  state = new arete::State();

  const char* result = state->load_image("heap.boot");

  state->boot();

  state->enter_repl();
  //std::cout << "globals: " << state->globals.at(0) << std::endl;

  if(result) std::cerr << result << std::endl;

  delete state;

  return 0;
}
