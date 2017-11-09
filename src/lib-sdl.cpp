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

// This would also make it easier to store GC'd variables. We could have something like

// struct MyModule : ExtensibleRecord {}

// And then just put the Values up at the top.

// TODO: Also, there's a lot to unpack here...if my C++Fu were better and I had the time
// I'd much rather have something ike
// accessor(FIXNUM, "sdl:event-mouse-x", &SDL_Event->button.x) or something magical like that, rather than
// writing all these out by hand, which is pretty tedious!

#include <stdint.h>

#include "SDL2/SDL.h"
#include "SDL2/SDL_opengl.h"
#include "SDL2/SDL_ttf.h"

#include "arete.hpp"

static SDL_Window* window = 0;
static SDL_Renderer *renderer = 0;
static size_t sdl_event_record_tag = 0;
static size_t sdl_timer_tag = 0;
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

  if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_EVENTS | SDL_INIT_TIMER) != 0) {
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

  return Value::make_boolean(SDL_PollEvent(e));
}

Value sdl_event_type(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-type";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_event_record_tag);

  SDL_Event* e = state.record_data<SDL_Event>(sdl_event_record_tag, argv[0]);
  switch(e->type) {
    case SDL_QUIT:
      return state.get_symbol("quit");
    case SDL_KEYDOWN:
      return state.get_symbol("key-down");
    case SDL_MOUSEBUTTONDOWN:
      return state.get_symbol("mouse-down");
    case SDL_USEREVENT: {
      return state.get_symbol("timer");
    }
    default:
      return C_FALSE;
  }
}

Value sdl_event_mouse_x(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-mouse-x";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_event_record_tag);

  SDL_Event* e = state.record_data<SDL_Event>(sdl_event_record_tag, argv[0]);

  return Value::make_fixnum(e->button.x);
}

Value sdl_event_mouse_y(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-mouse-y";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_event_record_tag);

  SDL_Event* e = state.record_data<SDL_Event>(sdl_event_record_tag, argv[0]);

  return Value::make_fixnum(e->button.y);
}

Value sdl_event_timer_tag(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-timer-tag";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_event_record_tag);

  SDL_Event* e = state.record_data<SDL_Event>(sdl_event_record_tag, argv[0]);
  Handle* h = static_cast<Handle*>(e->user.data1);

  if(e->type != SDL_USEREVENT || &h->state != &state) {
    return state.make_exception("sdl", "sdl:event-timer-tag called against bad event");
  }
  
  return h->ref;
}

Value sdl_event_key(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-key";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_event_record_tag);

  SDL_Event* e = state.record_data<SDL_Event>(sdl_event_record_tag, argv[0]);

  std::cout << "Scancode: " << e->key.keysym.scancode << std::endl;
  switch(e->key.keysym.scancode) {
    case SDL_SCANCODE_RETURN:
      return state.get_symbol("return");
    case SDL_SCANCODE_BACKSPACE:
      return state.get_symbol("backspace");
    case SDL_SCANCODE_ESCAPE:
      return state.get_symbol("escape");
    default: break;
  }

  return state.make_char((char) e->key.keysym.sym);
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

  SDL_RenderFillRect(renderer, &rect);

  return C_UNSPECIFIED;
}

Value sdl_draw_rect(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:draw-rect";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_Rect rect = {(int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int)argv[2].fixnum_value(), (int)argv[3].fixnum_value()};

  SDL_RenderDrawRect(renderer, &rect);

  return C_UNSPECIFIED;
}

Value sdl_draw_line(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:draw-line";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_RenderDrawLine(renderer, (int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int) argv[2].fixnum_value(), (int) argv[3].fixnum_value());
  
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

//
///// FONTS
//

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
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  TTF_Font* cfont = (*state.record_data<TTF_Font*>(sdl_ttf_font_tag, argv[0]));

  if(cfont) {
    SDL_Surface* solid = TTF_RenderText_Blended(cfont, argv[1].string_data(), draw_color);
    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, solid);

    std::cout << argv[2].fixnum_value() << ' ' << argv[3].fixnum_value() << std::endl;
    SDL_Rect rect = {(int)argv[2].fixnum_value(), (int)argv[3].fixnum_value(), 0, 0};
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

//
///// TIMERS
//

uint32_t sdl_timer_callback(uint32_t interval, void* parameter) {
  SDL_Event event;
  SDL_UserEvent userevent;

  userevent.type = SDL_USEREVENT;
  userevent.code = 0;
  userevent.data1 = parameter;
  userevent.data2 = 0;

  event.type = SDL_USEREVENT;
  event.user = userevent;

  SDL_PushEvent(&event);
  
  return interval;
}


Value sdl_add_timer(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:add-timer";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  ptrdiff_t milliseconds = argv[1].fixnum_value();
  Handle *symbol_handle = new Handle(state, argv[0]);
  SDL_AddTimer((uint32_t) milliseconds, sdl_timer_callback, symbol_handle);

  Value rec = state.make_record(sdl_timer_tag);

  Handle** ptr = state.record_data<Handle*>(sdl_timer_tag, rec);
  (*ptr) = symbol_handle;

  return rec;
}

Value sdl_free_timer(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:free-timer";

  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, sdl_timer_tag);

  Handle** ptr = state.record_data<Handle*>(sdl_timer_tag, argv[0]);

  if(*ptr != 0) {
    delete *ptr;
    (*ptr) = 0;
  }

  argv[0].record_set_finalized();

  return C_UNSPECIFIED;
}

void timer_finalizer(State& state, Value timer) {
  Handle** handle = state.record_data<Handle*>(sdl_timer_tag, timer);
  if(*handle) {
    delete (*handle);
    (*handle) = 0;
  }
}

Value load_sdl(State& state) {
  state.defun_core("sdl:init", sdl_init, 2);
  state.defun_core("sdl:quit", sdl_quit, 0);
  state.defun_core("sdl:make-event", sdl_make_event, 0);
  state.defun_core("sdl:poll-event", sdl_poll_event, 1);
  state.defun_core("sdl:event-type", sdl_event_type, 1);
  state.defun_core("sdl:event-timer-tag", sdl_event_timer_tag, 1);
  state.defun_core("sdl:event-key", sdl_event_key, 1);
  state.defun_core("sdl:event-mouse-x", sdl_event_mouse_x, 1);
  state.defun_core("sdl:event-mouse-y", sdl_event_mouse_y, 1);
  state.defun_core("sdl:event-type-descriptor", sdl_event_type_descriptor, 0);

  state.defun_core("sdl:add-timer", sdl_add_timer, 2);
  state.defun_core("sdl:free-timer", sdl_free_timer, 1);

  state.defun_core("sdl:clear", sdl_clear, 0);
  state.defun_core("sdl:render", sdl_render, 0);

  state.defun_core("sdl:delay", sdl_delay, 1);

  state.defun_core("sdl:set-color", sdl_set_color, 3, 4);
  state.defun_core("sdl:fill-rect", sdl_fill_rect, 4);
  state.defun_core("sdl:draw-rect", sdl_draw_rect, 4);
  state.defun_core("sdl:draw-line", sdl_draw_line, 4);

  state.defun_core("sdl:open-font", sdl_open_font, 2);
  state.defun_core("sdl:close-font", sdl_close_font, 1);
  state.defun_core("sdl:draw-text", sdl_draw_text, 4, 6);

  sdl_event_record_tag = state.register_record_type("sdl:event", 0, sizeof(SDL_Event));

  sdl_timer_tag = state.register_record_type("#<sdl:timer>", 0, sizeof(Handle*));
  state.record_type_set_finalizer(sdl_timer_tag, timer_finalizer);

  // Font handling
  sdl_ttf_font_tag = state.register_record_type("#<sdl:font>", 0, sizeof(TTF_Font*));
  state.record_type_set_finalizer(sdl_ttf_font_tag, ttf_font_finalizer);
  
  return C_UNSPECIFIED;
}

}

#endif