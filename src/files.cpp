// files.cpp - Operations on files

#include <algorithm>
#include <fstream>

#include "arete.hpp"

namespace arete {

Value State::make_input_file_port(const char* cpath, std::istream* fs) {
  Value path = make_string(cpath);
  AR_FRAME(this, path);
  FilePort* port = static_cast<FilePort*>(gc.allocate(FILE_PORT, sizeof(FilePort)));
  port->set_header_bit(Value::FILE_PORT_INPUT_BIT);
  port->set_header_bit(Value::FILE_PORT_NEVER_CLOSE_BIT);
  port->input_handle = fs;
  port->path = path;
  return port;
}

Value State::make_input_file_port(Value path) {
  AR_TYPE_ASSERT(path.type() == STRING);
  std::string cpath(path.string_data());
  std::ifstream* fs = new std::ifstream(cpath);
  if(!fs) {
    std::ostringstream os;
    os << "failed to open file \"" << path << "\"";
    return make_exception(globals[S_FILE_ERROR], os.str());
  }
  AR_FRAME(this, path);
  FilePort* port = static_cast<FilePort*>(gc.allocate(FILE_PORT, sizeof(FilePort)));
  port->set_header_bit(Value::FILE_PORT_INPUT_BIT);
  port->path = string_copy(path);
  port->input_handle = fs;
  gc.finalizers.push_back(port);
  Value res = port;
  AR_ASSERT(res.file_port_readable());
  return res;
}

void State::finalize(Type object_type, Value object, bool called_by_gc) {
  if(called_by_gc) {
    std::cerr << "arete: warning: finalizer called by GC. Files and other objects should always be closed in program code." << std::endl;;
  }

  switch(object_type) {
    case FILE_PORT: {
      FilePort* fp = object.as_unsafe<FilePort>();
      if(fp->reader) {
        delete fp->reader;
        fp->reader = 0;
      } 

      if(fp->get_header_bit(Value::FILE_PORT_NEVER_CLOSE_BIT))
        return;

      if(object.file_port_readable() && fp->input_handle) {
        delete fp->input_handle;
        fp->input_handle = 0;
      } else if(object.file_port_writable() && fp->output_handle) {
        delete fp->output_handle;
        fp->output_handle = 0;
      }

      break;
    }
    default: break;
  }
}

Value fn_open_input_file(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "open-input-file";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  return state.make_input_file_port(argv[0]);
}

// Extract a file port from an argument, or return *current-input-port*

#define  AR_MAYBE_INPUT_PORT(state, argv, argc, argi, name) \
  Value name ; \
  if(((argc) > (argi)) && ((argv)[(argi)].type() != FILE_PORT || !(argv[(argi)].file_port_readable()))) { \
    std::ostringstream os; os << fn_name << " expected an input-file port but got " << \
      (argv)[(argi)].type(); \
    return state.type_error(os.str()); \
  } else if((argc) > (argi)) { \
    name = (argv)[(argi)]; \
  } else {  \
    name = state.get_global_value(State::G_CURRENT_INPUT_PORT); \
  }

Value fn_read_char(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "read-char";

  AR_MAYBE_INPUT_PORT(state, argv, argc, 0, port);

  char c;
  std::istream* is = port.file_port_input_handle();
  if(!is && is->eof()) return C_EOF;
  is->read(&c, 1);
  return state.make_char(c);
}

Value fn_read(State& state, size_t argc, Value* argv) {
  if(argv[0].type() != FILE_PORT || !argv[0].file_port_readable()) {
    std::ostringstream os("read expected an input-file-port as its first argument but got ");
    os << argv[0].type();
    return state.type_error(os.str());
  }

  FilePort* fp = argv[0].as_unsafe<FilePort>();
  if(!fp->reader) {
    std::string name(fp->path.string_data());
    fp->reader = new XReader(state, *fp->input_handle, true, name);
  }

  return fp->reader->read();
}

Value fn_slurp_file(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "slurp-file";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  std::string path(argv[0].string_data());
  std::ifstream fs(path);

  if(!fs.good()) {
    std::ostringstream os;
    os << "could not open file " << argv[0].string_data();
    return state.make_exception(state.globals[State::S_READ_ERROR], os.str());
  }

  Value x, lst = C_NIL;

  AR_FRAME(state, x, lst);

  XReader reader(state, fs, true, path);
  state.temps.clear();
  while(true) {
    x = reader.read();

    if(x.is_active_exception()) {
      return x;
    }

    if(x == C_EOF) break;

    state.temps.push_back(x);
  }

  std::reverse(state.temps.begin(), state.temps.end());

  for(size_t i = 0; i != state.temps.size(); i++) {
    lst = state.make_pair(state.temps[i], lst);
  }

  return lst;
}

void State::load_file_functions() {
  defun_core("slurp-file", fn_slurp_file, 1);
  defun_core("open-input-file", fn_open_input_file, 1);
  defun_core("read", fn_read, 1);
  defun_core("read-char", fn_read_char, 0, 1);
}

}