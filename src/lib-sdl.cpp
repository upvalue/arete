// lib-sdl.cpp - SDL bindings for Arete.

#if AR_LIB_SDL

// TODO: Also, there's a lot to unpack here...if my C++Fu were better and I had the time
// I'd much rather have something ike
// accessor(FIXNUM, "sdl:event-mouse-x", &SDL_Event->button.x) or something magical like that,
// rather than writing all these out by hand, which is pretty tedious!

#include <stdint.h>

#include "SDL.h"
#include "SDL_opengl.h"
#include "SDL_ttf.h"

#include "arete.hpp"

#define CHECK_SDL(expr) \
  if((expr) == 0) return sdl_error(state);

namespace arete {

struct SDLModuleData {
  SDL_Renderer* renderer;
  SDL_Window* window;
  SDL_Color draw_color;
  size_t event_tag, timer_tag, font_tag;
};

struct SDLModule : CRecord {
  SDLModuleData data;
};

static Value sdl_error(State& state) {
  std::string msg(SDL_GetError());
  return state.make_exception("sdl", msg);
}

Value sdl_init(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:init";

  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_EVENTS | SDL_INIT_TIMER) != 0) {
    return sdl_error(state);
  }

  if(TTF_Init() != 0) {
    return sdl_error(state);
  }

  module->data.window = SDL_CreateWindow("title", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
    (int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(), SDL_WINDOW_OPENGL);

  CHECK_SDL(module->data.window);

  module->data.renderer = SDL_CreateRenderer(module->data.window, -1, SDL_RENDERER_ACCELERATED);

  CHECK_SDL(module->data.renderer);

  //SDL_RenderSetLogicalSize(renderer, (int) argv[0].fixnum_value(), (int) argv[1].fixnum_value());
  SDL_RenderSetLogicalSize(module->data.renderer, (int) argv[0].fixnum_value(), (int) argv[1].fixnum_value());

  return C_TRUE;
}

Value sdl_clear(State& state, SDLModule* module, size_t argc, Value* argv) {
  SDL_SetRenderDrawColor(module->data.renderer, 0, 0, 0, 255);
  SDL_RenderClear(module->data.renderer);

  return C_UNSPECIFIED;
}

Value sdl_quit(State& state, SDLModule* module, size_t argc, Value* argv) {
  if(module->data.window != 0) {
    SDL_DestroyRenderer(module->data.renderer);
    SDL_DestroyWindow(module->data.window);
    module->data.window = 0;
  }
  SDL_Quit();
  return C_UNSPECIFIED;
}

Value sdl_make_event(State& state, SDLModule* module, size_t argc, Value* argv) {
  Value v = state.make_record(module->data.event_tag);
  return v;
}

Value sdl_poll_event(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:poll-event";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

  return Value::make_boolean(SDL_PollEvent(e));
}

Value sdl_event_type(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-type";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);
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

Value sdl_event_mouse_x(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-mouse-x";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

  return Value::make_fixnum(e->button.x);
}

Value sdl_event_mouse_y(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-mouse-y";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

  return Value::make_fixnum(e->button.y);
}

Value sdl_event_timer_tag(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-timer-tag";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);
  Handle* h = static_cast<Handle*>(e->user.data1);

  if(e->type != SDL_USEREVENT || &h->state != &state) {
    return state.make_exception("sdl", "sdl:event-timer-tag called against bad event");
  }
  
  return h->ref;
}

Value sdl_event_key(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:event-key";
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

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

Value sdl_render(State& state, SDLModule* module, size_t argc, Value* argv) {
  SDL_RenderPresent(module->data.renderer);
  return C_UNSPECIFIED;
}

Value sdl_set_color(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:set-color";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);

  if(argc == 4) {
    AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);
  }
  
  unsigned char r = (unsigned char) argv[0].fixnum_value(), g = (unsigned char) argv[1].fixnum_value(),
    b = (unsigned char)argv[2].fixnum_value(), a = (unsigned char) ((argc == 4) ? argv[3].fixnum_value() : 255);

  SDL_Color draw_color = {r, g, b, a};
  module->data.draw_color = draw_color;
  SDL_SetRenderDrawColor(module->data.renderer, r,g,b,a);
  return C_UNSPECIFIED;
}

Value sdl_fill_rect(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:fill-rect";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_Rect rect = {(int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int)argv[2].fixnum_value(), (int)argv[3].fixnum_value()};

  SDL_RenderFillRect(module->data.renderer, &rect);

  return C_UNSPECIFIED;
}

Value sdl_draw_rect(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:draw-rect";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_Rect rect = {(int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int)argv[2].fixnum_value(), (int)argv[3].fixnum_value()};

  SDL_RenderDrawRect(module->data.renderer, &rect);

  return C_UNSPECIFIED;
}

Value sdl_draw_line(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:draw-line";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_RenderDrawLine(module->data.renderer, (int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int) argv[2].fixnum_value(), (int) argv[3].fixnum_value());
  
  return C_UNSPECIFIED;
}

Value sdl_event_type_descriptor(State& state, SDLModule* module, size_t argc, Value* argv) {
  return Value::make_fixnum(module->data.event_tag);
}

Value sdl_delay(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:delay";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  SDL_Delay(argv[0].fixnum_value());
  return C_UNSPECIFIED;
}

//
///// FONTS
//

void ttf_font_finalizer(State& state, Value sfont) {
  TTF_Font** font = sfont.record_data<TTF_Font*>();
  if(*font) {
    TTF_CloseFont(*font);
    (*font) = 0;
  }
}


Value sdl_open_font(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:open-font";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  size_t font_tag = module->data.font_tag;

  std::string path(argv[0].string_data());

  TTF_Font* cfont = TTF_OpenFont(path.c_str(), (int)argv[1].fixnum_value());

  //Value v = state.make_record(sdl_ttf_font_tag);
  Value v = state.make_record(font_tag);

  (*state.record_data<TTF_Font*>(font_tag, v)) = cfont;

  return v;
}

Value sdl_draw_text(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:draw-text";
  size_t font_tag = module->data.font_tag;

  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, font_tag);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  TTF_Font* cfont = (*state.record_data<TTF_Font*>(font_tag, argv[0]));

  if(cfont) {
    SDL_Surface* solid = TTF_RenderText_Blended(cfont, argv[1].string_data(), module->data.draw_color);
    SDL_Texture* texture = SDL_CreateTextureFromSurface(module->data.renderer, solid);

    std::cout << argv[2].fixnum_value() << ' ' << argv[3].fixnum_value() << std::endl;
    SDL_Rect rect = {(int)argv[2].fixnum_value(), (int)argv[3].fixnum_value(), 0, 0};
    SDL_QueryTexture(texture, 0, 0, &rect.w, &rect.h);

    SDL_RenderCopy(module->data.renderer, texture, 0, &rect);
    SDL_FreeSurface(solid);
  }

  return C_UNSPECIFIED;
}

Value sdl_close_font(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:close-font";
  size_t font_tag = module->data.font_tag;
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, font_tag);

  TTF_Font** font = state.record_data<TTF_Font*>(font_tag, argv[0]);
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


Value sdl_add_timer(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:add-timer";
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  size_t timer_tag = module->data.timer_tag;
  ptrdiff_t milliseconds = argv[1].fixnum_value();
  Handle *symbol_handle = new Handle(state, argv[0]);
  SDL_AddTimer((uint32_t) milliseconds, sdl_timer_callback, symbol_handle);

  Value rec = state.make_record(timer_tag);

  Handle** ptr = state.record_data<Handle*>(timer_tag, rec);
  (*ptr) = symbol_handle;

  return rec;
}

Value sdl_free_timer(State& state, SDLModule* module, size_t argc, Value* argv) {
  static const char* fn_name = "sdl:free-timer";

  size_t timer_tag = module->data.timer_tag;

  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, timer_tag);

  Handle** ptr = state.record_data<Handle*>(timer_tag, argv[0]);

  if(*ptr != 0) {
    delete *ptr;
    (*ptr) = 0;
  }

  argv[0].record_set_finalized();

  return C_UNSPECIFIED;
}

void timer_finalizer(State& state, Value timer) {
  Handle** handle = timer.record_data<Handle*>();
  if(*handle) {
    delete (*handle);
    (*handle) = 0;
  }
}

Value load_sdl(State& state) {
  // Register record types
  size_t sdl_module_tag = state.register_record_type("#<sdl:module>", 0, sizeof(SDLModuleData));

  // Font handling
  size_t font_tag = state.register_record_type("#<sdl:font>", 0, sizeof(TTF_Font*));
  state.record_type_set_finalizer(font_tag, ttf_font_finalizer);

  // Event handling
  size_t event_tag = state.register_record_type("sdl:event", 0, sizeof(SDL_Event));

  // Timer handling
  size_t timer_tag = state.register_record_type("#<sdl:timer>", 0, sizeof(Handle*));
  state.record_type_set_finalizer(timer_tag, timer_finalizer);

  // Set up module information
  Value module = state.make_record(state.globals[sdl_module_tag]);
  SDLModuleData* data = state.record_data<SDLModuleData>(sdl_module_tag, module);
  data->font_tag = font_tag;
  data->event_tag = event_tag;
  data->timer_tag = timer_tag;

  AR_ASSERT(module.type() == RECORD);
  AR_FRAME(state, module);

  // Install functions
  state.defun_core_closure("sdl:init", module, (c_closure_t)sdl_init, 2);
  state.defun_core_closure("sdl:quit", module, (c_closure_t)sdl_quit, 0);

  state.defun_core_closure("sdl:make-event", module, (c_closure_t)sdl_make_event, 0);
  state.defun_core_closure("sdl:poll-event", module, (c_closure_t)sdl_poll_event, 1);
  state.defun_core_closure("sdl:event-type", module, (c_closure_t)sdl_event_type, 1);
  state.defun_core_closure("sdl:event-timer-tag", module, (c_closure_t)sdl_event_timer_tag, 1);
  state.defun_core_closure("sdl:event-key", module, (c_closure_t)sdl_event_key, 1);
  state.defun_core_closure("sdl:event-mouse-x", module, (c_closure_t)sdl_event_mouse_x, 1);
  state.defun_core_closure("sdl:event-mouse-y", module, (c_closure_t)sdl_event_mouse_y, 1);
  state.defun_core_closure("sdl:event-type-descriptor", module, (c_closure_t)sdl_event_type_descriptor, 0);

  state.defun_core_closure("sdl:add-timer", module, (c_closure_t)sdl_add_timer, 2);
  state.defun_core_closure("sdl:free-timer", module, (c_closure_t)sdl_free_timer, 1);

  state.defun_core_closure("sdl:clear", module, (c_closure_t)sdl_clear, 0);
  state.defun_core_closure("sdl:render", module, (c_closure_t)sdl_render, 0);

  state.defun_core_closure("sdl:delay", module, (c_closure_t)sdl_delay, 1);

  state.defun_core_closure("sdl:set-color", module, (c_closure_t)sdl_set_color, 3, 4);
  state.defun_core_closure("sdl:fill-rect", module, (c_closure_t)sdl_fill_rect, 4);
  state.defun_core_closure("sdl:draw-rect", module, (c_closure_t)sdl_draw_rect, 4);
  state.defun_core_closure("sdl:draw-line", module, (c_closure_t)sdl_draw_line, 4);

  state.defun_core_closure("sdl:open-font", module, (c_closure_t) sdl_open_font, 2);
  state.defun_core_closure("sdl:close-font", module, (c_closure_t) sdl_close_font, 1);
  state.defun_core_closure("sdl:draw-text", module, (c_closure_t) sdl_draw_text, 4, 6);

  return C_UNSPECIFIED;
}

}

#endif