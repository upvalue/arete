// lib-sdl.cpp - SDL bindings for Arete.

// TODO: This is tossing away the re-entrant quality of the runtime. While it doesn't matter for 
// SDL, it would be good to think about how to construct a more modular interface for when R7RS
// modules exist

// I envision something like Module m("sdl"), state.register_module(m);

// Modules could carry around data like record type tags, handles and other things, and be passed to
// each function invocation (how to do this without changing the signature of every function? -- or
// should we change the signature of every function? I've been thinking about adding some kind of
// CClosure anyway)

// Another option might be to pass modules or closure variables as the first argument to a function
// These variables could be attached to CFunction directly.

#include "SDL2/SDL.h"

#include "arete.hpp"

static SDL_Window* window = 0;
static size_t sdl_event_record_tag = 0;
static SDL_Event* event = 0;

namespace arete {

static Value sdl_error(State& state) {
  std::string msg(SDL_GetError());
  return state.make_exception("sdl", msg);
}

Value sdl_init(State& state, size_t argc, Value* argv) {

  if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_EVENTS) != 0) {
    return sdl_error(state);
  }

  window = SDL_CreateWindow("title", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
    640, 480, SDL_WINDOW_OPENGL);

  if(window == 0)
    return sdl_error(state);

  return C_TRUE;
}

Value sdl_quit(State& state, size_t argc, Value* argv) {
  SDL_Quit();
  return C_UNSPECIFIED;
}

Value sdl_make_event(State& state, size_t argc, Value* argv) {
  Value v = state.make_record(sdl_event_record_tag);
  return v;
}

Value sdl_poll_event(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:poll-event";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_event_record_tag);

  SDL_Event* e = state.record_data<SDL_Event>(sdl_event_record_tag, argv[0]);

  // TODO HERE: EVENT TYPES.

  return Value::make_boolean(SDL_PollEvent(e));

  return C_UNSPECIFIED;
}

Value sdl_event_type(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-type";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_event_record_tag);

  SDL_Event* e = state.record_data<SDL_Event>(sdl_event_record_tag, argv[0]);
  switch(e->type) {
    case SDL_QUIT:
      return state.get_symbol("quit");
    default:
      return C_FALSE;
  }
}

Value sdl_event_type_descriptor(State& state, size_t argc, Value* argv) {
  return Value::make_fixnum(sdl_event_record_tag);
}

Value sdl_delay(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:delay";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  SDL_Delay(argv[0].fixnum_value());
  return C_UNSPECIFIED;
}

Value load_sdl(State& state) {
  state.defun_core("sdl:init", sdl_init, 0);
  state.defun_core("sdl:quit", sdl_quit, 0);
  state.defun_core("sdl:make-event", sdl_make_event, 0);
  state.defun_core("sdl:poll-event", sdl_poll_event, 1);
  state.defun_core("sdl:event-type", sdl_event_type, 1);

  state.defun_core("sdl:event-type-descriptor", sdl_event_type_descriptor, 0);

  state.defun_core("sdl:delay", sdl_delay, 1);

  sdl_event_record_tag = state.register_record_type("sdl:event", 0, sizeof(SDL_Event));
  
  return C_UNSPECIFIED;
}

}
