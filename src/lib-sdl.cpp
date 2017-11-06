// lib-sdl.cpp - SDL bindings for Arete.

#if AR_LIB_SDL

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

// Also, we do have a spot on the stack for this, since in #<function> #<arg1> #<arg2> etc,
// we can replace function before calling function. That's probably the easiest way that doesn't
// involve modifying other code, OTOH that's it for extensibility...maybe we should support
// two sorts of cfunction calling conventions.

#include "SDL2/SDL.h"
#include "SDL2/SDL_opengl.h"
#include "SDL2/SDL_ttf.h"

#include "arete.hpp"

static SDL_Window* window = 0;
static SDL_Renderer *renderer = 0;
static size_t sdl_event_record_tag = 0;
static size_t sdl_ttf_font_tag = 0;
static SDL_Color draw_color = {0, 0, 0, 255};

#define CHECK_SDL(expr) \
  if((expr) == 0) return sdl_error(state);

namespace arete {

static Value sdl_error(State& state) {
  std::string msg(SDL_GetError());
  return state.make_exception("sdl", msg);
}

Value sdl_init(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:init";

  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_EVENTS) != 0) {
    return sdl_error(state);
  }

  if(TTF_Init() != 0) {
    return sdl_error(state);
  }

  window = SDL_CreateWindow("title", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
    (int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(), SDL_WINDOW_OPENGL);

  CHECK_SDL(window);

  renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);

  CHECK_SDL(renderer);

  //SDL_RenderSetLogicalSize(renderer, (int) argv[0].fixnum_value(), (int) argv[1].fixnum_value());
  SDL_RenderSetLogicalSize(renderer, (int) argv[0].fixnum_value(), (int) argv[1].fixnum_value());

  return C_TRUE;
}

Value sdl_clear(State& state, size_t argc, Value* argv) {
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
  SDL_RenderClear(renderer);

  return C_UNSPECIFIED;
}

Value sdl_quit(State& state, size_t argc, Value* argv) {
  if(window != 0) {
    SDL_DestroyWindow(window);
    SDL_DestroyRenderer(renderer);
  }
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


Value sdl_render(State& state, size_t argc, Value* argv) {
  SDL_RenderPresent(renderer);
  return C_UNSPECIFIED;
}

Value sdl_set_color(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:set-color";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);

  if(argc == 4) {
    AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);
  }
  
  unsigned char r = (unsigned char) argv[0].fixnum_value(), g = (unsigned char) argv[1].fixnum_value(),
    b = (unsigned char)argv[2].fixnum_value(), a = (unsigned char) ((argc == 4) ? argv[3].fixnum_value() : 255);

  draw_color = {r, g, b, a};
  SDL_SetRenderDrawColor(renderer, draw_color.r, draw_color.g, draw_color.b, draw_color.a);
  return C_UNSPECIFIED;
}

Value sdl_fill_rect(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:fill-rect";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_Rect rect = {(int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int)argv[2].fixnum_value(), (int)argv[3].fixnum_value()};

  AR_ASSERT(&rect);
  SDL_RenderFillRect(renderer, &rect);

  return C_UNSPECIFIED;
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

///// FONTS

void ttf_font_finalizer(State& state, Value sfont) {
  TTF_Font** font = state.record_data<TTF_Font*>(sdl_ttf_font_tag, sfont);
  if(*font) {
    TTF_CloseFont(*font);
    (*font) = 0;
  }
}

Value sdl_open_font(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:open-font";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  std::string path(argv[0].string_data());

  TTF_Font* cfont = TTF_OpenFont(path.c_str(), (int)argv[1].fixnum_value());

  Value v = state.make_record(sdl_ttf_font_tag);

  (*state.record_data<TTF_Font*>(sdl_ttf_font_tag, v)) = cfont;

  return v;
}

Value sdl_draw_text(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:draw-text";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_ttf_font_tag);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);

  TTF_Font* cfont = (*state.record_data<TTF_Font*>(sdl_ttf_font_tag, argv[0]));

  if(cfont) {
    SDL_Color white = {255,255,255,255};
    SDL_Surface* solid = TTF_RenderText_Blended(cfont, argv[1].string_data(), white);
    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, solid);

    SDL_Rect rect = {0, 0, 0, 0};
    SDL_QueryTexture(texture, 0, 0, &rect.w, &rect.h);

    SDL_RenderCopy(renderer, texture, 0, &rect);
    SDL_FreeSurface(solid);
  }

  return C_UNSPECIFIED;
}

Value sdl_close_font(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:close-font";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_ttf_font_tag);

  TTF_Font** font = state.record_data<TTF_Font*>(sdl_ttf_font_tag, argv[0]);
  if(font != 0) {
    TTF_CloseFont(*font);
    (*font) = 0;
  }

  argv[0].record_set_finalized();

  return C_UNSPECIFIED;

}
Value load_sdl(State& state) {
  state.defun_core("sdl:init", sdl_init, 2);
  state.defun_core("sdl:quit", sdl_quit, 0);
  state.defun_core("sdl:make-event", sdl_make_event, 0);
  state.defun_core("sdl:poll-event", sdl_poll_event, 1);
  state.defun_core("sdl:event-type", sdl_event_type, 1);

  state.defun_core("sdl:event-type-descriptor", sdl_event_type_descriptor, 0);

  state.defun_core("sdl:clear", sdl_clear, 0);
  state.defun_core("sdl:render", sdl_render, 0);

  state.defun_core("sdl:delay", sdl_delay, 1);

  state.defun_core("sdl:set-color", sdl_set_color, 3, 4);
  state.defun_core("sdl:fill-rect", sdl_fill_rect, 4);

  state.defun_core("sdl:open-font", sdl_open_font, 2);
  state.defun_core("sdl:close-font", sdl_close_font, 1);
  state.defun_core("sdl:draw-text", sdl_draw_text, 2);

  sdl_event_record_tag = state.register_record_type("sdl:event", 0, sizeof(SDL_Event));

  // Font handling
  sdl_ttf_font_tag = state.register_record_type("sdl:font", 0, sizeof(TTF_Font*));
  state.record_type_set_finalizer(sdl_ttf_font_tag, ttf_font_finalizer);
  
  return C_UNSPECIFIED;
}

}

#endif