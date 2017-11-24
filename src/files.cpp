// files.cpp - Operations on files, input and output from Scheme code

#include <algorithm>
#include <fstream>

#include "arete.hpp"

namespace arete {

DefunGroup files("files");

Value State::make_input_file_port(const char* cpath, std::istream* fs) {
  Value path = make_string(cpath);
  AR_FRAME(this, path);
  FilePort* port = static_cast<FilePort*>(gc.allocate(FILE_PORT, sizeof(FilePort)));
  port->set_header_bit(Value::FILE_PORT_INPUT_BIT);
  port->set_header_bit(Value::FILE_PORT_NEVER_CLOSE_BIT);
  port->input_handle = fs;
  port->path = path;
  // Although we'll never close stdin, it's necessary to do this in case (read) is called
  // against it as an XReader will be allocated on the heap
  gc.finalizers.push_back(port);
  return port;
}

Value State::make_output_file_port(const char* cpath, std::ostream* fs) {
  Value path = make_string(cpath);
  AR_FRAME(this, path);
  FilePort* port = static_cast<FilePort*>(gc.allocate(FILE_PORT, sizeof(FilePort)));
  port->set_header_bit(Value::FILE_PORT_OUTPUT_BIT);
  port->set_header_bit(Value::FILE_PORT_NEVER_CLOSE_BIT);
  port->output_handle = fs;
  port->path = path;
  gc.finalizers.push_back(port);
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

Value State::make_output_file_port(Value path) {
  AR_TYPE_ASSERT(path.type() == STRING);
  std::string cpath(path.string_data());
  std::ofstream* fs = new std::ofstream(cpath);
  if(!fs) {
    std::ostringstream os;
    os << "failed to open file \"" << path << "\"";
    return make_exception(globals[S_FILE_ERROR], os.str());
  }
  AR_FRAME(this, path);
  FilePort* port = static_cast<FilePort*>(gc.allocate(FILE_PORT, sizeof(FilePort)));
  port->set_header_bit(Value::FILE_PORT_OUTPUT_BIT);
  port->path = string_copy(path);
  port->output_handle = fs;
  gc.finalizers.push_back(port);
  Value res = port;
  AR_ASSERT(res.file_port_writable());
  return res;
}

Value fn_open_output_file(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "open-output-file";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  return state.make_output_file_port(argv[0]);
}
AR_DEFUN("open-output-file", fn_open_output_file, 1);

Value fn_open_input_file(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "open-input-file";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  return state.make_input_file_port(argv[0]);
}
AR_DEFUN("open-input-file", fn_open_input_file, 1);

Value fn_close_port(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "close-port";
  AR_FN_EXPECT_TYPE(state, argv, 0, FILE_PORT);

  state.finalize(FILE_PORT, argv[0], false);

  return C_UNSPECIFIED;
}
AR_DEFUN("close-output-port", fn_close_port, 1);
AR_DEFUN("close-input-port", fn_close_port, 1);

Value fn_flush_port(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "flush-port";
  AR_FN_EXPECT_TYPE(state, argv, 0, FILE_PORT);

  if(argv[0].file_port_writable()) {
    std::ostream* handle = argv[0].file_port_output_handle();
    handle->flush();
  }

  return C_UNSPECIFIED;
}
AR_DEFUN("flush-output-port", fn_flush_port, 1);

// Extract a file port from an argument, or return *current-input-port*
#define  AR_MAYBE_INPUT_PORT(state, argv, argc, argi, name) \
  Value name ; \
  if(((argc) > (argi)) && ((argv)[(argi)].type() != FILE_PORT || !(argv[(argi)].file_port_readable()))) { \
    std::ostringstream os; os << fn_name << " expected an input-file-port but got " << \
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
  if(is->eof()) return C_EOF;
  return state.make_char(c);
}
AR_DEFUN("read-char", fn_read_char, 0, 1);

Value fn_peek_char(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "peek-char";

  AR_MAYBE_INPUT_PORT(state, argv, argc, 0, port);

  std::istream* is = port.file_port_input_handle();
  if(!is && is->eof()) return C_EOF;
  char c = is->peek();
  if(is->eof()) return C_EOF;
  return state.make_char(c);
}
AR_DEFUN("peek-char", fn_peek_char, 0, 1);

Value fn_read(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "read";

  AR_MAYBE_INPUT_PORT(state, argv, argc, 0, port);

  std::istream* is = port.file_port_input_handle();

  if(!is) return C_EOF;

  FilePort* fp = port.as_unsafe<FilePort>();
  if(!fp->reader) {
    fp->reader = new XReader(state, *is, false);
  }

  return fp->reader->read();
}
AR_DEFUN("read", fn_read, 0, 1);

// Extract a file port from an argument, or return *current-input-port*
#define  AR_MAYBE_OUTPUT_PORT(state, argv, argc, argi, name) \
  Value name ; \
  if(((argc) > (argi)) && ((argv)[(argi)].type() != FILE_PORT || !(argv[(argi)].file_port_writable()))) { \
    std::ostringstream os; os << fn_name << " expected an output-file-port but got " << \
      (argv)[(argi)].type(); \
    return state.type_error(os.str()); \
  } else if((argc) > (argi)) { \
    name = (argv)[(argi)]; \
  } else {  \
    name = state.get_global_value(State::G_CURRENT_OUTPUT_PORT); \
  }

#define AR_CHECK_OUTPUT_PORT_OPEN(state, port) \
  if(!port.file_port_output_handle() || port.file_port_output_handle()->eof()) { \
    std::ostringstream os; os << fn_name << " called against closed output port"; \
    return state.file_error(os.str()); \
  }

Value fn_write_char(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "write-char";

  AR_FN_EXPECT_TYPE(state, argv, 0, CHARACTER);
  AR_MAYBE_OUTPUT_PORT(state, argv, argc, 1, port);

  std::ostream* os = port.file_port_output_handle();

  if(!os)
    return state.make_exception(state.globals[State::S_FILE_ERROR],
      "write-char called against closed port");

  char c = argv[0].character();
  os->write(&c, 1);
  
  return C_UNSPECIFIED;
}
AR_DEFUN("write-char", fn_write_char, 1, 2);

Value fn_write(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "write";
  AR_MAYBE_OUTPUT_PORT(state, argv, argc, 1, port);
  AR_CHECK_OUTPUT_PORT_OPEN(state, port);
  std::ostream& os = *port.file_port_output_handle();
  os << argv[0];
  if(&os == &std::cout) os.flush();
  return C_UNSPECIFIED;
}
AR_DEFUN("write", fn_write, 1, 2);

Value fn_display(State& state, size_t argc, Value* argv) {
  static const char *fn_name = "display";
  AR_MAYBE_OUTPUT_PORT(state, argv, argc, 1, port);
  AR_CHECK_OUTPUT_PORT_OPEN(state, port);
  std::ostream& os = *port.file_port_output_handle();
  if(argv[0].type() == STRING)
    os << argv[0].string_data();
  else
    os << argv[0];
  return C_UNSPECIFIED;
}
AR_DEFUN("display", fn_display, 1, 2);

Value fn_newline(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "newline";
  AR_MAYBE_OUTPUT_PORT(state, argv, argc, 0, port);
  AR_CHECK_OUTPUT_PORT_OPEN(state, port);

  std::ostream& os = *port.file_port_output_handle();
  os << std::endl;
  os.flush();
  if(&os == &std::cout) os.flush();

  return C_UNSPECIFIED;
}
AR_DEFUN("newline", fn_newline, 0, 1);

Value fn_print_impl(State& state, size_t argc, Value* argv, std::ostream& os, bool whitespace, bool pretty) {
  for(size_t i = 0; i != argc; i++) {
    if(argv[i].type() == STRING) {
      os << argv[i].string_data();
    } else {
      if(pretty) {
        state.pretty_print(os, argv[i]);
      } else {
        os << argv[i];
      }
    }

    if(whitespace && i != argc - 1)  {
      os << ' ';
    }
  }

  return C_UNSPECIFIED;
}

Value fn_print_source(State& state, size_t argc, Value* argv) {
  if(argv[0].type() == PAIR && argv[0].pair_has_source()) {
    state.print_src_pair(std::cerr, argv[0]);
    std::cerr << std::endl;
    fn_print_impl(state, argc-1, &argv[1], std::cerr, true, false);
    std::cerr << std::endl;
  }
  return C_UNSPECIFIED;
}
AR_DEFUN("print-source", fn_print_source, 1, 1, true);

Value fn_source_name(State& state, size_t argc, Value* argv) {
  if(argv[0].heap_type_equals(PAIR) && argv[0].pair_has_source()) {
    SourceLocation loc = argv[0].pair_src();
    if(state.source_names.size() > loc.source) {
      return state.make_string(state.source_names[loc.source]);
    }
  }
  return C_FALSE;
}
AR_DEFUN("source-name", fn_source_name, 1);

Value fn_print(State& state, size_t argc, Value* argv) {
  Value chk = fn_print_impl(state, argc, argv, std::cout, true, false);
  std::cout << std::endl;
  return chk;
}
AR_DEFUN("print", fn_print, 0, 0, true);

Value fn_print_string(State& state, size_t argc, Value* argv) {
  std::ostringstream os;
  Value chk = fn_print_impl(state, argc, argv, os, true, false);
  if(chk.is_active_exception()) return chk;
  return state.make_string(os.str());
}
AR_DEFUN("print-string", fn_print_string, 0, 0, true);

static Value fn_pretty_print(State& state, size_t argc, Value* argv) {
  Value chk = fn_print_impl(state, argc, argv, std::cout, true, true);
  std::cout << std::endl;
  return chk;
}
AR_DEFUN("pretty-print", fn_pretty_print, 0, 0, true);

Value State::slurp_file(const std::string& path) {
  std::ifstream fs(path);

  if(!fs.good()) {
    std::ostringstream os;
    os << "could not open file " << path;
    return make_exception(globals[State::S_READ_ERROR], os.str());
  }

  Value x, lst = C_NIL;

  AR_FRAME(*this, x, lst);

  XReader reader(*this, fs, true, path);
  temps.clear();
  while(true) {
    x = reader.read();

    if(x.is_active_exception()) {
      return x;
    }

    if(x == C_EOF) break;

    temps.push_back(x);
  }

  std::reverse(temps.begin(), temps.end());

  for(size_t i = 0; i != temps.size(); i++) {
    SourceLocation loc;
    loc.source = reader.source;
    lst = make_src_pair(temps[i], lst, loc);
  }

  return lst;
}

Value fn_slurp_file(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "slurp-file";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  std::string path(argv[0].string_data());

  return state.slurp_file(path);
}
AR_DEFUN("slurp-file", fn_slurp_file, 1);

Value fn_print_table_verbose(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "print-table-verbose";
  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);

  state.print_table_verbose(argv[0]);
  
  return C_UNSPECIFIED;
}
AR_DEFUN("print-table-verbose", fn_print_table_verbose, 1);

Value fn_load_file(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "load";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  
  std::string path(argv[0].string_data());

  Value mod, res;
  AR_FRAME(state, mod, res);
  
  mod = state.get_global_value(State::G_CURRENT_MODULE);
  res = state.load_file(path);
  state.set_global_value(State::G_CURRENT_MODULE, mod);
  // Allow *push-module* to override the current module. This is so the expander can set up
  // and install the user module in which interaction happens by default
  if(state.get_global_value(State::G_PUSH_MODULE) != C_UNDEFINED) {
    state.set_global_value(State::G_CURRENT_MODULE, state.get_global_value(State::G_PUSH_MODULE));
    state.set_global_value(State::G_PUSH_MODULE, C_UNDEFINED);
  }
  return res;
}
AR_DEFUN("load", fn_load_file, 1);

Value fn_load_module(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "load-module";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);

  // auto => auto.sld, auto.scm
  // 

  //return state.load_file(path);

  return C_UNSPECIFIED;
}
AR_DEFUN("load-module", fn_load_module, 1);

void State::load_file_functions() {
  files.install(*this);
}

}