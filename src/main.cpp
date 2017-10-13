// main.cpp - Entry point for standalone builds
#include "arete.hpp"

int do_main(int argc, char* argv[]) {
  arete::State state;
  state.boot();
  return state.enter_cli(argc, argv);
}

int main(int argc, char *argv[]) {
  return do_main(argc, argv);
}
