// state.cpp - various bits of runtime functionality
// including: boot, object constructors, source code location tracking

#include "arete.hpp"

namespace arete {

State* current_state = 0;

State::State():
  gc(*this),
  gensym_counter(0),
  booted(false),
  symbol_table(),
  source_names(),
  source_contents(),
  globals(),
  temps(),
  stack_trace()
  {

  symbol_table = new symbol_table_t();
  current_state = this;
}

State::~State() {
  // Finalize all objects (close ports, free memory, etc)
  gc.run_finalizers(true);
  delete symbol_table;
  current_state = 0;
}

void State::boot_common() {
  AR_ASSERT(!booted);

  source_names.push_back("unknown");
  source_names.push_back("anonymous");
  source_contents.push_back("");
  source_contents.push_back("");

  set_global_value(G_CURRENT_INPUT_PORT, make_input_file_port("stdin", &std::cin));
  set_global_value(G_CURRENT_OUTPUT_PORT, make_output_file_port("stdout", &std::cout));
  
  booted = true;
}
  
void State::boot() {
  AR_ASSERT(!booted);

  static const char* _symbols[] = {
    // C_SYNTAX values
    "quote", "begin", "define", "lambda", "if", "cond",  "and", "or", "set!", "let",
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
    "TCO-ENABLED",
    "*features*",
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
  register_feature("sdl");

  set_global_value(G_COMMAND_LINE, C_NIL);
  set_global_value(G_FEATURES, C_NIL);

  register_feature("arete");

  boot_common();

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

void State::register_feature(const std::string& name) {
  Value pare, sym;
  AR_FRAME(*this, pare, sym);
  pare = get_global_value(G_FEATURES);
  sym = get_symbol(name);
  set_global_value(G_FEATURES, make_pair(sym, pare));
}

Value State::load_file(const std::string& path) {
  Value x, tmp;
  AR_FRAME(this, x, tmp);

  x = slurp_file(path);

  AR_FN_STATE_CHECK(x);

  x = eval2_list(x);
  //x = eval_toplevel_list(x);

  return x;
}


}