// cli.cpp - arete command line interface and REPL

#include <fstream>
#include <iostream>
#include <stdlib.h>

#include "lest.hpp"
#include "linenoise.h"

#include "arete.hpp"

using namespace arete;
using namespace std;

namespace arete {

lest::tests& specification() {
  static lest::tests tests;
  return tests;
}

const char* help[] = {
  "Note: Arguments are evaluated left to right, e.g. arete <file1> --repl <file2>",
  "will cause <file1> to be loaded, a REPL to be opened, and <file2> to be loaded after it is closed.\n",
  "  --help: Print this message",
  "  --set <variable> <expr>: Set a top-level variable to an expression (only read, not evaluated)",
  "  Important variable variables:",
  "     --set \"*show-expand*\" #t ",
  "        Print all expansions to current output port as they occur",
  "  --read <file>: Read and print S-expressions from a file without expanding or evaluating them",
  "  --read --repl: Same but with REPL",
  "  --repl: Open REPL",
  "  --test: Run builtin test suite, if compiled in; all arguments after this will be passed to lest",
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
  char* line = 0;
  Value x = C_FALSE, tmp;
  AR_FRAME(state, x, tmp);

  std::ostringstream hist_file;

  hist_file << getenv("HOME") << "/.arete_history";
  size_t i = 0;

  linenoiseHistorySetMaxLen(1024);
  linenoiseHistoryLoad(hist_file.str().c_str());

  static const char* prompt = "> ";

  while(++i) {
    line = linenoise(prompt);

    if(!line) {
      break;
    }

    std::ostringstream line_name;
    line_name << "repl-line-" << i;

    std::stringstream liness;
    liness >> std::noskipws;
    liness << line;

    XReader reader(state, liness, line_name.str());

    for(x = reader.read(); x != C_EOF; x = reader.read()) {
      if(x.is_active_exception()) {
        std::cerr << x.exception_message().string_data() << std::endl;
        break;
      }

      if(read_only) {
        std::cout << x << std::endl;
        /*
        if(x.type() == PAIR) {
          if(x.list_length() > 1) {
            state.print_src_pair(std::cout, x.cadr());
            state.print_src_pair(std::cout, x.cdr());
            std::cout << std::endl;
          }

          state.print_src_pair(std::cout, x);
          std::cout << std::endl;
        }
        */
        continue;
      }

      tmp = state.eval_toplevel(x);

      if(tmp.is_active_exception()) {
        state.print_exception(std::cout, tmp);
        continue;
      }

      std::cout << tmp << std::endl;

    }

    linenoiseHistoryAdd(line);
  }

  linenoiseHistorySave(hist_file.str().c_str());
  return true;
}

bool do_file(State& state, std::string path, bool read_only) {
  Value x;

  AR_FRAME(state, x);

  std::ifstream fs(path);
  if(!fs.good()) {
    std::cerr << "Could not open file " << path << std::endl;
    return false;
  }

  XReader reader(state, fs, path);
  
  while(x != C_EOF) {
    x = reader.read();
    if(x.is_active_exception()) {
      std::cerr << x.exception_message().string_data() << std::endl;
      return false;
    } else if(x != C_EOF) {
      std::cout << x << std::endl;
    }
  }


  return true;
}

int enter_cli(State& state, int argc, char* argv[]) {
  static std::string test("--test");
  static std::string read("--read");
  static std::string help("--help");
  static std::string repl("--repl");
  static std::string bad("--");
  static std::string debug_gc("--debug-gc");

  for(size_t i = 0; i != argc; i++) {
    const char* arg = argv[i];

    if(help.compare(arg) == 0) {
      print_help();
      return EXIT_SUCCESS;
    } else if(read.compare(arg) == 0) {
      EXPECT_NEXT_ARG();
      if(repl.compare(arg2) == 0) {
        if(!do_repl(state, true))
          return EXIT_FAILURE;
      } else {
        if(!do_file(state, arg2, true))
          return EXIT_FAILURE;
      }
    } else if(repl.compare(arg) == 0) {
      if(do_repl(state, false) == false) {
        return 1;
      }
    } else if(debug_gc.compare(arg) == 0) {
      state.gc.collect_before_every_allocation = true;
    } else {
      std::string cxxarg(arg);
      std::string badc(cxxarg.substr(0, bad.size()));

      if(badc.compare(bad) == 0) {
        std::cerr << "Unknown -- option " << cxxarg << std::endl << std::endl;
        print_help();
        return EXIT_FAILURE;
      }
    }

    if(test.compare(arg) == 0) {
      return lest::run(specification(), argc - i, &argv[i]);
    }
  }

  return EXIT_SUCCESS;
}

}
