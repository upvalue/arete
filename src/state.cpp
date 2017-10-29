// state.cpp - various bits of runtime functionality
// including: boot, object constructors, source code location tracking

#include "arete.hpp"

namespace arete {

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
    "EXPANDER-PRINT",
    "expander"
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
  load_numeric_functions();

  // Uncomment to see allocations after boot
  // gc.allocations = 0;
}

}