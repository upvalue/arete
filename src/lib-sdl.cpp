// lib-sdl.cpp - SDL2 bindings for Arete.

#if AR_LIB_SDL

// TODO: There's a lot to unpack here...if my C++Fu were better and I had the time
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

DefunGroup sdl_functions("sdl");

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

Value sdl_init(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure,  SDLModule*, module);
  static const char* fn_name = "sdl:init";

  AR_FN_ARGC_EQ(state, argc, 2);
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
AR_DEFUN("init", sdl_init, 2);

Value sdl_quit(State& state, size_t argc, Value* argv, void* closure) {
  static const char* fn_name = "sdl:quit";
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  AR_FN_ARGC_EQ(state, argc, 0);
  if(module->data.window != 0) {
    SDL_DestroyRenderer(module->data.renderer);
    SDL_DestroyWindow(module->data.window);
    module->data.window = 0;
  }
  SDL_Quit();
  return C_UNSPECIFIED;
}
AR_DEFUN("quit", sdl_quit, 0);

Value sdl_clear(State& state, size_t argc, Value* argv, void *closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:clear";
  AR_FN_ARGC_EQ(state, argc, 0);
  SDL_SetRenderDrawColor(module->data.renderer, 0, 0, 0, 255);
  SDL_RenderClear(module->data.renderer);

  return C_UNSPECIFIED;
}
AR_DEFUN("clear", sdl_clear, 0);

Value sdl_make_event(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:make-event";
  AR_FN_ARGC_EQ(state, argc, 0);
  std::cerr << "sdl:make-event" << std::endl;
  std::cerr << module->data.event_tag << std::endl;
  Value v = state.make_record(module->data.event_tag);
  return v;
}
AR_DEFUN("make-event", sdl_make_event, 0);

Value sdl_poll_event(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:poll-event";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

  return Value::make_boolean(SDL_PollEvent(e));
}
AR_DEFUN("poll-event", sdl_poll_event, 1);

Value sdl_event_type(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:event-type";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);
  switch(e->type) {
    case SDL_QUIT: return state.get_symbol("quit");
    case SDL_KEYDOWN: return state.get_symbol("key-down");
    case SDL_MOUSEBUTTONDOWN: return state.get_symbol("mouse-down");
    case SDL_USEREVENT: return state.get_symbol("timer");
    case SDL_WINDOWEVENT: {
      switch(e->window.event) {
        case SDL_WINDOWEVENT_SHOWN: return state.get_symbol("window-shown");
        case SDL_WINDOWEVENT_FOCUS_GAINED: return state.get_symbol("window-focus-gained");
        default: break;
      }
    }
    default:
      return C_FALSE;
  }
  return C_FALSE;
}
AR_DEFUN("event-type", sdl_event_type, 1);

Value sdl_event_mouse_x(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:event-mouse-x";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

  return Value::make_fixnum(e->button.x);
}
AR_DEFUN("event-mouse-x", sdl_event_mouse_x, 1);

Value sdl_event_mouse_y(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:event-mouse-y";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

  return Value::make_fixnum(e->button.y);
}
AR_DEFUN("event-mouse-y", sdl_event_mouse_y, 1);

Value sdl_event_timer_tag(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:event-timer-tag";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);
  Handle* h = static_cast<Handle*>(e->user.data1);

  if(e->type != SDL_USEREVENT || &h->state != &state) {
    return state.make_exception("sdl", "sdl:event-timer-tag called against non SDL_USEREVENT");
  }
  
  return h->ref;
}
AR_DEFUN("event-timer-tag", sdl_event_timer_tag, 1);

Value sdl_event_key_modifiers(State& state, size_t argc, Value* argv, void* closure) {
  // AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:event-key-modifiers";
  AR_FN_ARGC_EQ(state, argc, 1);
  return C_UNSPECIFIED;
}
AR_DEFUN("event-key-modifiers", sdl_event_key_modifiers, 1);

Value sdl_event_key(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:event-key";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.event_tag);

  SDL_Event* e = state.record_data<SDL_Event>(module->data.event_tag, argv[0]);

  // If the scancode can be represented as a character
  if(e->key.keysym.scancode <= SDL_SCANCODE_CAPSLOCK) {
    return state.make_char((char) e->key.keysym.sym);
  }

  //std::cout << "Scancode: " << e->key.keysym.scancode << std::endl;
  switch(e->key.keysym.scancode) {
    case SDL_SCANCODE_RETURN: return state.make_char('\r');
    case SDL_SCANCODE_BACKSPACE: return state.make_char('\b');
    case SDL_SCANCODE_ESCAPE: return state.get_symbol(27);
    case SDL_SCANCODE_F1: return state.get_symbol("f1");
    case SDL_SCANCODE_F2: return state.get_symbol("f2");
    case SDL_SCANCODE_F3: return state.get_symbol("f3");
    case SDL_SCANCODE_F4: return state.get_symbol("f4");
    case SDL_SCANCODE_F5: return state.get_symbol("f5");
    case SDL_SCANCODE_F6: return state.get_symbol("f6");
    case SDL_SCANCODE_F7: return state.get_symbol("f7");
    case SDL_SCANCODE_F8: return state.get_symbol("f8");
    case SDL_SCANCODE_F9: return state.get_symbol("f9");
    case SDL_SCANCODE_F10: return state.get_symbol("f10");
    case SDL_SCANCODE_F11: return state.get_symbol("f11");
    case SDL_SCANCODE_F12: return state.get_symbol("f12");
    case SDL_SCANCODE_HOME: return state.get_symbol("home");
    case SDL_SCANCODE_DELETE: return state.get_symbol("delete");
    case SDL_SCANCODE_UP: return state.get_symbol("up");
    case SDL_SCANCODE_LEFT: return state.get_symbol("left");
    case SDL_SCANCODE_RIGHT: return state.get_symbol("right");
    case SDL_SCANCODE_DOWN: return state.get_symbol("down");
    case SDL_SCANCODE_LCTRL: return state.get_symbol("lctrl");
    case SDL_SCANCODE_LSHIFT: return state.get_symbol("lshift");
    case SDL_SCANCODE_LALT: return state.get_symbol("lalt");
    case SDL_SCANCODE_RCTRL: return state.get_symbol("rctrl");
    case SDL_SCANCODE_RALT: return state.get_symbol("ralt");
    case SDL_SCANCODE_RSHIFT: return state.get_symbol("rshift");
    case SDL_SCANCODE_LGUI: return state.get_symbol("gui");
    case SDL_SCANCODE_KP_0: return state.get_symbol("kp0");
    case SDL_SCANCODE_KP_1: return state.get_symbol("kp1");
    case SDL_SCANCODE_KP_2: return state.get_symbol("kp2");
    case SDL_SCANCODE_KP_3: return state.get_symbol("kp3");
    case SDL_SCANCODE_KP_4: return state.get_symbol("kp4");
    case SDL_SCANCODE_KP_5: return state.get_symbol("kp5");
    case SDL_SCANCODE_KP_6: return state.get_symbol("kp6");
    case SDL_SCANCODE_KP_7: return state.get_symbol("kp7");
    case SDL_SCANCODE_KP_8: return state.get_symbol("kp8");
    case SDL_SCANCODE_KP_9: return state.get_symbol("kp9");
    case SDL_SCANCODE_KP_ENTER: return state.get_symbol("kp-enter");
    case SDL_SCANCODE_KP_MULTIPLY: return state.get_symbol("kp-multiply");
    case SDL_SCANCODE_KP_DIVIDE: return state.get_symbol("kp-divide");
    case SDL_SCANCODE_KP_PLUS: return state.get_symbol("kp-plus");
    case SDL_SCANCODE_KP_MINUS: return state.get_symbol("kp-minus");
    default: break;
  }

  return state.get_symbol("unknown");

}
AR_DEFUN("event-key", sdl_event_key, 1);

Value sdl_render(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  SDL_RenderPresent(module->data.renderer);
  return C_UNSPECIFIED;
}
AR_DEFUN("render", sdl_render, 0);

Value sdl_set_color(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:set-color";
  AR_FN_ARGC_BETWEEN(state, argc, 3, 4);
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
AR_DEFUN("set-color", sdl_set_color, 3, 4);

Value sdl_fill_rect(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:fill-rect";
  AR_FN_ARGC_EQ(state, argc, 4);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_Rect rect = {(int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int)argv[2].fixnum_value(), (int)argv[3].fixnum_value()};

  SDL_RenderFillRect(module->data.renderer, &rect);

  return C_UNSPECIFIED;
}
AR_DEFUN("fill-rect", sdl_fill_rect, 4);

Value sdl_draw_rect(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:draw-rect";
  AR_FN_ARGC_EQ(state, argc, 4);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_Rect rect = {(int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int)argv[2].fixnum_value(), (int)argv[3].fixnum_value()};

  SDL_RenderDrawRect(module->data.renderer, &rect);

  return C_UNSPECIFIED;
}
AR_DEFUN("draw-rect", sdl_draw_rect, 4);

Value sdl_draw_line(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:draw-line";
  AR_FN_ARGC_EQ(state, argc, 4);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  SDL_RenderDrawLine(module->data.renderer, (int)argv[0].fixnum_value(), (int)argv[1].fixnum_value(),
    (int) argv[2].fixnum_value(), (int) argv[3].fixnum_value());
  
  return C_UNSPECIFIED;
}
AR_DEFUN("draw-line", sdl_draw_line, 4);

Value sdl_delay(State& state, size_t argc, Value* argv, void* closure) {
  //AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:delay";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  SDL_Delay((unsigned) argv[0].fixnum_value());
  return C_UNSPECIFIED;
}
AR_DEFUN("delay", sdl_delay, 1);

//
///// FONTS
//

Value ttf_font_finalizer(State& state, size_t argc, Value* argv, void* sfontp) {
  Value sfont((HeapValue*) sfontp);
  TTF_Font** font = sfont.record_data<TTF_Font*>();
  if(*font) {
    TTF_CloseFont(*font);
    (*font) = 0;
  }
  return C_UNSPECIFIED;
}

Defun _font_finalizer((void*) ttf_font_finalizer);

Value sdl_open_font(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:open-font";
  AR_FN_ARGC_EQ(state, argc, 2);
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
AR_DEFUN("open-font", sdl_open_font, 2);

Value sdl_draw_text(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:draw-text";
  size_t font_tag = module->data.font_tag;

  AR_FN_ARGC_BETWEEN(state, argc, 4, 6);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, font_tag);
  AR_FN_EXPECT_TYPE(state, argv, 1, STRING);
  AR_FN_EXPECT_TYPE(state, argv, 2, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 3, FIXNUM);

  TTF_Font* cfont = (*state.record_data<TTF_Font*>(font_tag, argv[0]));

  if(cfont) {
    SDL_Surface* solid = TTF_RenderText_Blended(cfont, argv[1].string_data(), module->data.draw_color);
    SDL_Texture* texture = SDL_CreateTextureFromSurface(module->data.renderer, solid);

    //std::cout << argv[2].fixnum_value() << ' ' << argv[3].fixnum_value() << std::endl;
    SDL_Rect rect = {(int)argv[2].fixnum_value(), (int)argv[3].fixnum_value(), 0, 0};
    SDL_QueryTexture(texture, 0, 0, &rect.w, &rect.h);

    SDL_RenderCopy(module->data.renderer, texture, 0, &rect);
    SDL_FreeSurface(solid);
  }

  return C_UNSPECIFIED;
}
AR_DEFUN("draw-text", sdl_draw_text, 4, 6);

Value sdl_close_font(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:close-font";
  size_t font_tag = module->data.font_tag;
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, font_tag);

  TTF_Font** font = state.record_data<TTF_Font*>(font_tag, argv[0]);
  if(font != 0) {
    TTF_CloseFont(*font);
    (*font) = 0;
  }

  argv[0].record_set_finalized();

  return C_UNSPECIFIED;
}
AR_DEFUN("close-font", sdl_close_font, 1);

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

struct TimerData {
  Handle* symbol;
  SDL_TimerID id;
};

Value sdl_add_timer(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:add-timer";
  AR_FN_ARGC_EQ(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, SYMBOL);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);

  size_t timer_tag = module->data.timer_tag;
  ptrdiff_t milliseconds = argv[1].fixnum_value();

  Handle *symbol_handle = new Handle(state, argv[0]);
  SDL_TimerID id = SDL_AddTimer((uint32_t) milliseconds, sdl_timer_callback, symbol_handle);

  CHECK_SDL(id);

  Value rec = state.make_record(timer_tag);

  TimerData* data = state.record_data<TimerData>(timer_tag, rec);
  data->symbol = symbol_handle;
  data->id = id;

  //Handle** ptr = state.record_data<Handle*>(timer_tag, rec);
  //(*ptr) = symbol_handle;

  return rec;
}
AR_DEFUN("add-timer", sdl_add_timer, 2);

Value sdl_remove_timer(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, SDLModule*, module);
  static const char* fn_name = "sdl:remove-timer";

  AR_FN_ARGC_EQ(state, argc, 1);

  size_t timer_tag = module->data.timer_tag;

  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, timer_tag);

  TimerData* data = state.record_data<TimerData>(timer_tag, argv[0]);

  bool result = false;

  if(data->symbol != nullptr) {
    delete data->symbol;
    data->symbol = nullptr;
    result = SDL_RemoveTimer(data->id) == SDL_TRUE;
  }

  argv[0].record_set_finalized();
  return Value::make_boolean(result);
}
AR_DEFUN("remove-timer", sdl_remove_timer, 1);

Value timer_finalizer(State& state, size_t argc, Value* argv, void* timerp) {
  Value timer((HeapValue*) timerp);
  TimerData* timer_data = timer.record_data<TimerData>();
  if(timer_data->symbol != nullptr) {
    delete timer_data->symbol;
    timer_data->symbol = nullptr;
    if(SDL_WasInit(SDL_INIT_TIMER) & SDL_INIT_TIMER) {
      SDL_RemoveTimer(timer_data->id);
    }
  }
  return C_UNSPECIFIED;
}

Defun _timer_finalizer((void*) timer_finalizer);

void load_sdl(State& state) {
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

  sdl_functions.install_module(state, "sdl", module);
}

}

#endif
