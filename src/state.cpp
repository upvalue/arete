// state.cpp - various bits of runtime functionality
// including: boot, object constructors, source code location tracking

#include "arete.hpp"

namespace arete {

State* current_state = 0;

State::State():
  gc(*this),
  gensym_counter(0),
  symbol_table(),
  source_names(),
  source_contents(),
  globals(),
  temps(),

  shared_objects_begin(0),
  shared_objects_i(0) {

  symbol_table = new symbol_table_t();
  current_state = this;
}

State::~State() {
  // Finalize all objects (close ports, free memory, etc)
  gc.run_finalizers(true);
  delete symbol_table;
  current_state = 0;
}
  
void State::boot() {
  source_names.push_back("unknown");
  source_names.push_back("anonymous");
  source_contents.push_back("");
  source_contents.push_back("");

  static const char* _symbols[] = {
    // C_SYNTAX values
    "quote", "begin", "define", "lambda", "if", "cond", "cond-expand", "and", "or", "set!",
    "define-syntax", "let-syntax", "letrec-syntax", "define-library", "import",
    // Used by interpreter
    "else",
    // Used by reader      
    "quasiquote", "unquote", "unquote-splicing", "rename",
    // Modules
    "unqualified", "only", "except", "prefix",
    // Tags for errors that may be thrown by the runtime
    "file", "read", "eval", "type", "expand", "syntax",
    // Various variables
    "*command-line*",
    "EXPANDER-PRINT",
    "expander",
    "compiler",
    // I/O
    "*current-input-port*",
    "*current-output-port*",
  };

  AR_ASSERT((sizeof(_symbols) / sizeof(const char*)) == G_END &&
    "did you forget to change _symbols to match enum Global?");

    // TODO: This is suitably confusing and there should probably be multiple vectors:
    // for symbols used directly, for symbols used to store values, and for other values (strings)
  Value s;
  for(size_t i = 0; i != G_END; i++) {
    if(i >= G_END) {
      s = make_string(_symbols[i]);
    } else {
      s = get_symbol(_symbols[i]);

      if(i <= S_LETREC_SYNTAX) {
        s.as<Symbol>()->value = C_SYNTAX;
      } else {
        s.as<Symbol>()->value = C_UNDEFINED;
      }
    }
    globals.push_back(s);
  }


  load_builtin_functions();
  load_file_functions();
  load_numeric_functions();
  load_sdl(*this);

  set_global_value(G_COMMAND_LINE, C_NIL);
  set_global_value(G_CURRENT_INPUT_PORT, make_input_file_port("stdin", &std::cin));
  set_global_value(G_CURRENT_OUTPUT_PORT, make_output_file_port("stdout", &std::cout));

  // Uncomment to see allocations after boot
  // gc.allocations = 0;
}

std::string State::source_info(const SourceLocation loc, Value fn_name,
    bool from_eval) {
  std::ostringstream ss;
  if(from_eval) ss << "eval: ";
  ss << source_names[loc.source] << ':' << loc.line;
  if(fn_name == C_TRUE) 
    ss << " in toplevel";
  else if(fn_name != C_FALSE)
    ss << " in " << fn_name;
  return ss.str();
}


}