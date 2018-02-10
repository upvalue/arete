//  lib-uv.cpp - libuv bindings for Arete
#if AR_LIB_UV

#include "uv.h"

#include "arete.hpp"

namespace arete {

DefunGroup uv_functions("uv");

struct UVModuleData {
  Value default_loop;
  // 1 GC'd value
  size_t tcp_tag;
  size_t loop_tag;
};

struct UVModule : CRecord {
  UVModuleData data;
};

Value default_loop(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, UVModule*, module);

  Value mod(module);

  if(module->data.default_loop == C_FALSE) {
    Value val;
    AR_FRAME(state, val, mod);
    size_t loop_tag = module->data.loop_tag;
    AR_ASSERT(loop_tag);

    val = state.make_record(loop_tag);
    uv_loop_t** loop = state.record_body<uv_loop_t*>(loop_tag, val);
    (*loop) = uv_default_loop();
    mod.record_body<UVModuleData>()->default_loop = val;
  }

  return mod.record_body<UVModuleData>()->default_loop;
}
AR_DEFUN("default-loop", default_loop, 1);

Value make_tcp(State& state, size_t argc, Value* argv, void* closure) {
  AR_FN_CLOSURE(state, closure, UVModule*, module);

  Value val;
  size_t tcp_tag = module->data.tcp_tag;

  val = state.make_record(tcp_tag);
  uv_tcp_t** data = state.record_data<uv_tcp_t*>(tcp_tag, val);

  (*data) = (uv_tcp_t*) calloc(1, sizeof(uv_tcp_t));
  return val;
}
AR_DEFUN("make-tcp", make_tcp, 1);

void load_uv(State& state) {
  size_t uv_module_tag = state.register_record_type("#<uv:module>", 1, sizeof(UVModuleData));

  size_t loop_tag = state.register_record_type("#<uv:loop>", 0, sizeof(uv_loop_t*));
  size_t tcp_tag = state.register_record_type("#<uv:tcp>", 0, sizeof(uv_tcp_t*));

  Value module = state.make_record(state.globals[uv_module_tag]);
  UVModuleData* data = state.record_body<UVModuleData>(uv_module_tag, module);
  data->tcp_tag = tcp_tag;
  data->loop_tag = loop_tag;

  uv_functions.install_module(state, "uv", module);
}

}

#endif