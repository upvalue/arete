// cli.cpp - arete command line interface 

#include <fstream>
#include <iostream>
#include <stdlib.h>

#include "vendor/linenoise.h"

#include "arete.cpp"

using namespace arete;

State state = State();

static bool QUIET = false;
static bool SHOW_READ = false;

bool do_file(const char* file_path, bool eval ) {
  Value x, vec;
  AR_FRAME(state, x, vec);

  vec = state.make_vector();
  std::ifstream file_handle(file_path, std::ios::in);
  std::stringstream ss;
  // CHECK IF FILE EXISTS
  ss << file_handle.rdbuf();
  Reader reader(state, ss);
  reader.file = state.register_file(file_path);
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
        if(x.type() == EXCEPTION) {
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

  std::cout << "; Arete 0.1" << std::endl;

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

int main(int argc, const char **argv) {
  // state.gc.collect_before_every_allocation = true;
  state.boot();

  std::string open_repl("--repl");
  std::string gcdebug("--gcdebug");
  std::string quiet("--quiet");
  std::string showread("--show-read");
  std::string showexpand("--show-expand");

  if(argc > 1) {
    // read files
    for(size_t i = 1; i != argc; i++) {
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
}
