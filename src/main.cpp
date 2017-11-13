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
  /*
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

  Value exc = state->load_file("scheme/syntax.scm");

  if(exc.is_active_exception()) {
    state->print_exception(std::cerr, exc);
    return EXIT_FAILURE;
  }

  state->save_image("heap.boot");

  delete state;

  state = new arete::State();

  const char* result = state->boot_from_image("heap.boot");

  //state->boot();

  state->enter_repl();

  if(result) std::cerr << result << std::endl;

  delete state;

  state->enter_repl();

  return 0;
  */
  return do_main(argc, argv);
}
