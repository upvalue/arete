//  lib-uv.cpp - libuv bindings for Arete
#if AR_LIB_UV

#include "uv.h"

#include "arete.hpp"

#define CHECK_UV(expr) \
  { \
    int r = (expr); \
    if (r != 0) { return uv_error(state, r); } \
    \
  } 

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

Value uv_error(State& state, int r) {
  return state.make_exception("uv", uv_strerror(r));
}

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

Value tcp_init(State& state, size_t argc, Value* argv, void* closure) {
  static const char* fn_name = "uv:tcp-init";
  AR_FN_CLOSURE(state, closure, UVModule*, module);

  size_t tcp_tag = module->data.tcp_tag;

  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.loop_tag);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 1, module->data.tcp_tag);

  Value loop = argv[0], val = argv[1];

  uv_tcp_t** data = state.record_body<uv_tcp_t*>(tcp_tag, val);
  uv_tcp_init((*loop.record_body<uv_loop_t*>()), (*data));
  
  return val;
}
AR_DEFUN("tcp-init", tcp_init, 1);

Value run(State& state, size_t argc, Value* argv, void* closure) {
  static const char* fn_name = "uv:run";
  AR_FN_CLOSURE(state, closure, UVModule*, module);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 0, module->data.loop_tag)

  int ret = uv_run((*argv[0].record_body<uv_loop_t*>()), UV_RUN_DEFAULT);

  return Value::make_fixnum(ret);
}
AR_DEFUN("run", run, 1);

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

void on_new_connection(uv_tcp_t* server, int status) {

}

Value tcp_bind(State& state, size_t, Value* argv, void* closure) {
  // <port:fixnum> #<uv:tcp> maxconn callback
  static const char* fn_name = "uv:tcp-bind";
  AR_FN_CLOSURE(state, closure, UVModule*, module);

  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_RECORD_ISA(state, argv, 1, module->data.tcp_tag);

  sockaddr_in addr;
  uv_ip4_addr("0.0.0.0", argv[0].fixnum_value(), &addr);
  uv_tcp_t* server = *argv[1].record_body<uv_tcp_t*>();
  uv_tcp_bind(server, (const struct sockaddr*)&addr, 0);

  // TODO: Now what? 
  // How do we do a callback here?

  // Could re-use DynASM here, use it to create a function encoded with a Handle which passes
  // on arguments as appropriate. 
  CHECK_UV(uv_listen((uv_stream_t*) server, 128, on_new_connection));

  return C_UNSPECIFIED;

}

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