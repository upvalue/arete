// state.cpp - various bits of runtime functionality
// including: boot, object constructors, source code location tracking

#include "arete.hpp"

#if AR_OS == AR_POSIX
# include <sys/mman.h>
#else
# include <Windows.h>
#endif

namespace arete {

State* current_state = 0;

extern void load_table_functions(State&);
extern void load_string_functions(State&);
extern void load_platform_functions(State&);
extern void load_sdl(State&);
extern void load_native_compiler(State&);

State::State():
  gc(*this, AR_HEAP_SIZE),
  gensym_counter(0),
  tco_enabled(true),
  booted(false),
  symbol_table(),
  source_names(),
  source_contents(),
  globals(),
  temps(),
  stack_trace(),
  vm_depth(0)
  {

  symbol_table = new symbol_table_t();
  current_state = this;
}

State::~State() {
  // Finalize all objects (close ports, free memory, etc)
  for(size_t i = 0; i != native_code.size(); i++) {
    delete native_code[i];
  }
  gc.run_finalizers(true);
  delete symbol_table;
  current_state = 0;
}

void State::boot_common() {
  AR_ASSERT(!booted);

  set_global_value(G_CURRENT_INPUT_PORT, make_input_file_port("stdin", &std::cin));
  set_global_value(G_CURRENT_OUTPUT_PORT, make_output_file_port("stdout", &std::cout));
  set_global_value(G_STR_MODULE_NAME, make_string("module-name"));
  set_global_value(G_STR_MODULE_EXPORTS, make_string("module-exports"));
  
  booted = true;
}
  
void State::boot() {
  AR_ASSERT(!booted);

  source_names.push_back("unknown");
  source_names.push_back("anonymous");
  source_contents.push_back("");
  source_contents.push_back("");

  static const char* _symbols[] = {
    // C_SYNTAX values
    "quote", "begin", "define", "lambda", "if", "cond",  "and", "or", "set!", "let",
    "identifier-transformer", "combined-transformer",
    "module", "define-library", "import", "define-syntax", "let-syntax", "letrec-syntax",
    // Used by interpreter
    "else",
    // Used by reader      
    "quasiquote", "unquote", "unquote-splicing", "rename",
    // Modules
    "unqualified", "only", "except", "prefix",
    // Tags for errors that may be thrown by the runtime
    "continuation",
    "file", "read", "eval", "type", "expand", "syntax",
    // Various variables
    "*features*",
    "*command-line*",
    "expander",
    "compiler",
    // Flags
    "RECURSION-LIMIT",
    "TCO-ENABLED",
    "VM-LOG-REPL",
    "EXPANDER-PRINT",
    "FORBID-INTERPRETER",
    // Printer flags
    "PRINT-READABLY",
    "PRINT-TABLE-MAX",
    // I/O
    "*current-input-port*",
    "*current-output-port*",
    // modules
    "module-table",
    "*current-module*",
    "*push-module*",
    "*core-module*",
    // module fields
    "module-name",
    "module-exports",
  };

  AR_ASSERT((sizeof(_symbols) / sizeof(const char*)) == G_END &&
    "did you forget to change _symbols to match enum Global? (or miss a comma when you did? ;)");

    // TODO: This is suitably confusing and there should probably be multiple vectors:
    // for symbols used directly, for symbols used to store values, and for other values (strings)
  Value s;
  for(size_t i = 0; i != G_END; i++) {
    if(i >= G_END) {
      s = make_string(_symbols[i]);
    } else {
      s = get_symbol(_symbols[i]);
      s.heap->set_header_bit(Value::SYMBOL_ROOT_BIT);

      if(i <= S_LETREC_SYNTAX) {
        s.as<Symbol>()->value = C_SYNTAX;
      } else {
        s.as<Symbol>()->value = C_UNDEFINED;
      }
    }
    globals.push_back(s);
  }

  boot_common();

  set_global_value(G_MODULE_TABLE, make_table());

  load_builtin_functions();
  load_file_functions();
  load_numeric_functions();
  load_table_functions(*this);
  load_string_functions(*this);
  load_platform_functions(*this);
  load_native_compiler(*this);

#if AR_LIB_SDL
  load_sdl(*this);
  register_feature("sdl");
#endif

  set_global_value(G_RECURSION_LIMIT, Value::make_fixnum(1500));
  set_global_value(G_COMMAND_LINE, C_NIL);
  set_global_value(G_FEATURES, C_NIL);
  set_global_value(G_TCO_ENABLED, C_TRUE);
  set_global_value(G_PRINT_READABLY, C_FALSE);
  set_global_value(G_PRINT_TABLE_MAX, Value::make_fixnum(0));

  register_feature("arete");

  // Uncomment to see allocations after boot
  // gc.allocations = 0;
}

std::string State::source_info(const SourceLocation loc, Value fn_name,
    bool from_eval) {
  std::ostringstream ss;
  if(from_eval) ss << "eval: ";
  AR_ASSERT(loc.source < source_names.size());
  ss << source_names.at(loc.source) << ':' << loc.line;
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

  x = eval_list(x);

  return x;
}

void* State::allocate_native_code(size_t size_) {
  // TODO: Do a better job of this.
  size_t size = size_ > 4096 ? size_ : 4096;
  if(!native_code.size() || ((native_code_cursor + size) > native_code[native_code.size()-1]->size)) {
    Block* b = new Block(size, true);
    native_code.push_back(b);
    native_code_cursor = 0;
  }
  Block* b = native_code[native_code.size() - 1];

  char* ptr = (b->data + native_code_cursor);
  native_code_cursor += size;
  //AR_ASSERT(native_code_cursor != 4096);

  return ptr;
}

}
