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
  "  --set <variable> <expr>: Set a top-level variable to an expression",
  "  Important variable examples:",
  "     --set \"*show-expand*\" #t ",
  "        Print all expansions to current output port as they occur",
  "  --read <file>: Read and print S-expressions from a file without expanding or evaluating them",
  "  --read --repl: Same but with REPL",
  "  --repl: Open REPL",
  "  --test: Run builtin test suite, if compiled in; all arguments after this will be passed to lest",
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
  Value x = C_FALSE;
  AR_FRAME(state, x);

  std::ostringstream hist_file;

  hist_file << getenv("HOME") << "/.arete_history";
  size_t i = 1;


  linenoiseHistorySetMaxLen(1024);
  linenoiseHistoryLoad(hist_file.str().c_str());

  static const char* prompt = "> ";

  while(i++) {
    line = linenoise(prompt);

    if(!line) {
      break;
    }

    XStringReader reader(state, line);

    for(x = reader->read(); x != C_EOF; x = reader->read()) {

      if(x.is_active_exception()) {
        std::cerr << x.exception_message().string_data() << std::endl;
        break;
      }

      if(read_only) {
        std::cout << x << std::endl;
        continue;
      }

    }



    linenoiseHistoryAdd(line);
  }


  linenoiseHistorySave(hist_file.str().c_str());
  return true;
}

int enter_cli(State& state, int argc, char* argv[]) {
  std::string test("--test");
  std::string read("--read");
  std::string help("--help");
  std::string repl("--repl");
  std::string bad("--");

  for(size_t i = 0; i != argc; i++) {
    const char* arg = argv[i];

    if(help.compare(arg) == 0) {
      print_help();
      return 0;
    } else if(read.compare(arg) == 0) {
      EXPECT_NEXT_ARG();
      if(repl.compare(arg2) == 0) {
        if(do_repl(state, true) == false) {
          return 1;
        }
      }
    } else if(repl.compare(arg) == 0) {
      if(do_repl(state, false) == false) {
        return 1;
      }
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
  return 0;
}

}

#if 0

State state = State();

static bool QUIET = false;
static bool SHOW_READ = false;

bool do_file(const char* file_path, bool eval ) {
  Value x, vec;
  AR_FRAME(state, x, vec);

  vec = state.make_vector();
  std::ifstream file_handle(file_path, std::ios::in);
  if(!file_handle.good()) {
    std::cerr << "Could not read " << file_path << ". Does it exist?" << std::endl;
    return false;
  }
  Reader reader(state, file_handle);
  reader.file = state.register_file(file_path, file_handle);
  while(true) {
    x = reader.read();
    if(x == C_EOF) {
      break;
    }

    if(x.is_active_exception()) {
      std::cerr << "Reader error: " << x.exception_message().string_data() << std::endl;
      return false;
    } else {
      if(SHOW_READ) {
        std::cout << x << std::endl;
      }
      if(eval) {
        x = state.eval_toplevel(x);
        if(x.is_active_exception()) {
          std::cerr << "Evaluation error: " << x.exception_message().string_data() << std::endl;
          state.print_stack_trace();
          return false;
        }
      } else {
        // state.vector_append(vec, x);
        std::cout << x << std::endl;
      }
    }
  }
  // std::cout << vec << std::endl;
  return true;
}

bool do_repl() {
  Value x, vec;
  AR_FRAME(state, x, vec);
  size_t i = 1;

  std::ostringstream hist_file;
  hist_file << getenv("HOME") << "/.arete_history";

  linenoiseHistorySetMaxLen(1024);
  linenoiseHistoryLoad(hist_file.str().c_str());

  char* line = 0;

  std::ostringstream prompt;

  std::cout << ";) Arete 0.1" << std::endl;

  while(i++) {
    prompt << "> ";
    line = linenoise(prompt.str().c_str());
    prompt.str("");
    prompt.clear();

    if(!line) {
      break;
    }

    linenoiseHistoryAdd(line);

    std::ostringstream os;
    os << "repl-line-" << i - 1;
    StringReader reader(state, line, os.str());
    
    while(x != C_EOF) { 
      x = reader->read();
      if(x == C_EOF) {
        break;
      }

      if(x.is_active_exception()) {
        std::cerr << "Reader error: " << x.exception_message().string_data() << std::endl;
        break;
      } else {
        if(SHOW_READ) {
          std::cout << x << std::endl;
        }
        x = state.eval_toplevel(x);
        if(x.type() == EXCEPTION) {
          std::cerr << "Evaluation error: " << x.exception_message().string_data() << std::endl;
          state.print_stack_trace();
          break;
        } else {
          if(x != C_UNSPECIFIED)
            std::cout << x << std::endl;
        }
      }
    }
    
    x = C_UNSPECIFIED;
    free(line); line = 0;
  }

  if(line) free(line);

  linenoiseHistorySave(hist_file.str().c_str());
  return true;
}

#endif

#ifdef ARETE_DEV

int main(int argc, char *argv[]) {
  State state;
  state.boot();
  return enter_cli(state, argc, argv);
  /*
  while(true) {
    char *thing = linenoise("> ");
    if(!thing) break;
  }
  */
#if 0
  return lest::run(specification(), argc, argv, std::cout);
  // state.gc.collect_before_every_allocation = true;
  
  state.boot();

  state.load_paths.push_back(".");

  std::string open_repl("--repl");
  std::string gcdebug("--gcdebug");
  std::string quiet("--quiet");
  std::string showread("--show-read");
  std::string showexpand("--show-expand");
  std::string loadpath("--push-load-path=");

  if(argc > 1) {
    // read files
    for(int i = 1; i != argc; i++) {
      if(open_repl.compare(argv[i]) == 0) {
        do_repl();
      } else if(quiet.compare(argv[i]) == 0) {
        QUIET = true;
      } else if(showread.compare(argv[i]) == 0) {
        SHOW_READ = true;
      } else if(gcdebug.compare(argv[i]) == 0) {
        state.gc.collect_before_every_allocation = true;
      } else if(showexpand.compare(argv[i]) == 0) {
        state.print_expansions = true;
      } else {
        // Flag arguments
        std::string str(argv[i]);

        std::string loadpathc(str.substr(0, loadpath.size()));

        if(loadpathc.compare(loadpath) == 0) {
          std::string new_load_path(str.substr(loadpath.size(), str.size()));

          state.load_paths.push_back(new_load_path);

          continue;
        }

        // assume it's a file
        if(!do_file(argv[i], true)) {
          return EXIT_FAILURE;
        }
      }
    }
  } else {
    do_repl();
  }

  if(!QUIET) {
    state.print_gc_stats(std::cout);
  }

  state.gc.collect();

  return 0;
#endif 
}

#endif
