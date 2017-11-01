// cli.cpp - arete command line interface and REPL

#include <fstream>
#include <iostream>
#include <stdlib.h>
#include <stdio.h>

#include "arete.hpp"

#if AR_LINENOISE
#include "linenoise.h"
#endif

using namespace arete;
using namespace std;

namespace arete {

static const char* help[] = {
  "Note: Arguments are evaluated left to right, e.g. arete <file1> --repl <file2>",
  "will cause <file1> to be loaded, a REPL to be opened, and <file2> to be loaded after it is closed.\n",
  "  --help: Print this message",
  "  -- <arguments...>: Arguments after -- will be pass to Scheme as a list of strings named *command-line*",
  "  --set <variable> <expr>: Set a top-level variable to an expression (only read, not evaluated)",
  "  Helpful top-level variables:",
  "     --set EXPANDER-PRINT t ",
  "        Print all expansions to current output port as they occur",
  "  --read <file>: Read and print S-expressions from a file without expanding or evaluating them",
  "  --read --repl: Same but with REPL",
  "  --repl: Open REPL",
  "  --debug-gc: Forces a collection after every allocation, used to flush out GC bugs"
};

static void  print_help() {
  std::cout << "arete 0.0.1" << std::endl << std::endl;
  for(size_t i = 0; i != sizeof(help) / sizeof(const char*); i++) {
    std::cout << help[i] << std::endl;
  }
}

#define EXPECT_NEXT_ARG() \
  std::string arg2; \
  if(i + 1 == argc) { \
    std::cerr << "Expected argument after " << arg << " but got nothing."; \
    print_help(); \
    return 1; \
  } \
  arg2 = argv[++i];

static bool do_repl(State& state, bool read_only) {
  Value x = C_FALSE, tmp;
  AR_FRAME(state, x, tmp);

  std::cout << ";; arete 0.1" << std::endl;
  std::cout << ";; Everything is a nail." << std::endl;

  std::ostringstream hist_file;

	if(getenv("HOME")) {
		hist_file << getenv("HOME") << "/.arete_history";
	} else if(getenv("HOMEPATH")) {
		hist_file << getenv("HOMEPATH") << "/.arete_history";
	}


  size_t i = 0;

#if AR_LINENOISE
  linenoiseHistorySetMaxLen(1024);
  linenoiseHistoryLoad(hist_file.str().c_str());
#endif

  static const char* prompt = "> ";

  while(++i) {
    std::ostringstream line_name;
    line_name << "repl-line-" << i;

    std::stringstream liness;
    liness >> std::noskipws;
    bool history_add = true;
#if AR_LINENOISE
    char *line = linenoise(prompt);

    if(!line) {
      break;
    }

#else
    std::cout << prompt;
    size_t size = 0;
    //char* line = 0;
		char line[512];
    // size = getline(&line, &size, stdin);
		if(!std::cin.getline((char*)&line, 512)) break;

    if(!line || feof(stdin)) break;

		std::cout << "Read me a line " << line << std::endl;
    
    // std::cout << prompt;
    // std::string line;

    //std::getline(std::cin, line);
    // if(std::cin.eof())
    //  break;
#endif
    liness << line;

    XReader reader(state, liness, true, line_name.str());

    x = reader.read();
    // Don't add comments/expressionless lines to history
    if(x == C_EOF) {
      history_add = false;
    }

    for(; x != C_EOF; x = reader.read()) {
      if(x.is_active_exception()) {
        std::cerr << x.exception_message().string_data() << std::endl;
        break;
      }

      if(read_only) {
        std::cout << x << std::endl;
        continue;
      }

      tmp = state.make_pair(x, C_NIL);
      tmp = state.eval_toplevel_list(tmp);

      if(tmp.is_active_exception()) {
        state.print_exception(std::cerr, tmp);
        continue;
      }

      if(tmp != C_UNSPECIFIED) {
        state.pretty_print(std::cout, tmp);
        std::cout << std::endl;
      }

    }

#if AR_LINENOISE
    if(history_add) {
      linenoiseHistoryAdd(line);
    }
    free(line);
#endif
  }

  state.print_gc_stats(std::cout);

#if AR_LINENOISE
  linenoiseHistorySave(hist_file.str().c_str());
  linenoiseHistoryFree();
#endif
  return true;
}

bool do_file(State& state, std::string path, bool read_only) {
  Value x = C_FALSE, tmp = C_FALSE;

  AR_FRAME(state, x, tmp);

  if(read_only) {
    x = state.slurp_file(path);
    while(x.type() == PAIR) {
      if(x.car().is_active_exception()) {
        break;
      }
      std::cout << x.car() << std::endl;
      x = x.cdr();
    }
  } else {
    x = state.load_file(path);
  }


  if(x.is_active_exception()) {
    state.print_exception(std::cerr, x);
    return false;
  }

  return true;
}

static int cli_exception(State* state, Value exc, const std::string& desc) {
  state->print_exception(std::cerr, exc);
  std::cerr << desc << std::endl;
  return EXIT_FAILURE;
}

int State::enter_cli(int argc_, char* argv[]) {
  unsigned argc = (unsigned) argc_;
  static std::string read("--read");
  static std::string help("--help");
  static std::string repl("--repl");
  static std::string bad("--");
  static std::string debug_gc("--debug-gc");
  static std::string set("--set");
  static std::string eval("--eval");
  static std::string stats("--stats");
  static std::string rest("--");

  if(argc == 1) {
    if(do_repl(*this, false) == false) {
      return EXIT_FAILURE;
    }
  }

  // Find Scheme *command-line* arguments after --
  for(size_t i = 1; i != argc; i++) {
    if(rest.compare(argv[i]) == 0) {

      Value lst = C_NIL, tmp;
      AR_FRAME(*this, lst, tmp);
      for(size_t j = argc - 1; j != i; j--) {
        tmp = make_string(argv[j]);
        lst = make_pair(tmp, lst);
      }

      set_global_value(G_COMMAND_LINE, lst);

      argc = i;
      break;
    } 
  }

  // Prepend program name to *command-line*
  Value tmp = make_string(argv[0]);
  tmp = make_pair(tmp, get_global_value(G_COMMAND_LINE));
  set_global_value(G_COMMAND_LINE, tmp);

  for(size_t i = 1; i != argc; i++) {
    const char* arg = argv[i];

    if(help.compare(arg) == 0) {
      print_help();
      return EXIT_SUCCESS;
    } else if(read.compare(arg) == 0) {
      EXPECT_NEXT_ARG();
      if(repl.compare(arg2) == 0) {
        if(!do_repl(*this, true))
          return EXIT_FAILURE;
      } else {
        if(!do_file(*this, arg2, true))
          return EXIT_FAILURE;
      }
    } else if(stats.compare(arg) == 0) {
      print_gc_stats(std::cout);
    } else if(eval.compare(arg) == 0) {
      // Evaluate following string
      if((i + 1) >= argc) {
        std::cerr << "Expected at least one argument after --eval" << std::endl;
        return EXIT_FAILURE;
      }

      std::stringstream ss;
      ss >> std::noskipws;
      ss << argv[i+1] << std::endl;

      i += 1;

      XReader reader(*this, ss, false, "eval argument");

      Value x;

      AR_FRAME(this, x);

      while(true) {
        x = reader.read();
        if(x.is_active_exception()) 
          return cli_exception(this, x, "--eval argument resulted in a reader error");

        if(x == C_EOF) break;

        x = make_pair(x, C_NIL);
        x = eval_toplevel_list(x);

        if(x.is_active_exception()) 
          return cli_exception(this, x, "--eval argument resulted in an evaluation error");
      }

    } else if(set.compare(arg) == 0) {

      if((i + 2) >= argc) {
        std::cerr << "Expected at least two arguments after --set" << std::endl;
        return EXIT_FAILURE;
      }

      std::stringstream ss;
      ss >> std::noskipws;
      ss << argv[i+1] << std::endl;
      ss << argv[i+2] << std::endl;

      i += 2;

      XReader reader(*this, ss, "set variable");
      Value name, value;

      AR_FRAME(this, name, value);

      name = reader.read();
      value = reader.read();

      if(name.type() != SYMBOL) {
        std::cerr << "--set first argument must be a symbol" << std::endl;
      } else if(name.is_active_exception()) {
        print_exception(std::cerr, name);
        std::cerr << "--set first argument resulted in an exception" << std::endl;
        return EXIT_FAILURE;
      }

      if(value.is_active_exception()) {
        print_exception(std::cerr, value);
        std::cerr << "--set second argument resulted in an exception" << std::endl;
        return EXIT_FAILURE;
      }

      name.set_symbol_value(value);
    } else if(repl.compare(arg) == 0) {
      if(do_repl(*this, false) == false) {
        return EXIT_FAILURE;
      }
    } else if(debug_gc.compare(arg) == 0) {
      gc.collect_before_every_allocation = true;
    } else {
      std::string cxxarg(arg);
      std::string badc(cxxarg.substr(0, bad.size()));

      if(badc.compare(bad) == 0) {
        std::cerr << "Unknown -- option \"" << cxxarg << "\"" << std::endl << std::endl;
        print_help();
        return EXIT_FAILURE;
      } else {
        std::string path(argv[i]);
        if(!do_file(*this, path, false))
          return EXIT_FAILURE;
      }
    }
  }

  return EXIT_SUCCESS;
}

}
