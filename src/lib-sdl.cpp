// lib-sdl.cpp - SDL bindings for Arete.

#include "SDL/SDL.h"

#include "arete.hpp"

namespace arete {

Value sdl_init(State& state, size_t argc, Value* argv) {

  if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0) {

  }

  return C_FALSE;

}

Value load_sdl(State& state) {
  state.defun_core("sdl:init", sdl_init, 1, 1);
  
  return C_UNSPECIFIED;
}

}
