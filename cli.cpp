// cli.cpp - arete command line interface 

//#define ARETE_LOG_TAGS (ARETE_LOG_TAG_GC)
//#define ARETE_GC_STRATEGY ARETE_GC_SEMISPACE
//#define ARETE_GC_DEBUG 1

#include <fstream>
#include <iostream>

#include "vendor/linenoise.h"

#include "arete.cpp"

using namespace arete;

State state = State();

void do_file(const char* file_path, bool eval ) {
  Value x, vec;
  AR_FRAME(state, x, vec);

  vec = state.make_vector();
  std::ifstream file_handle(file_path, std::ios::in);
  std::stringstream ss;
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
      break;
    } else {
      if(eval) {
        x = state.eval_toplevel(x);
        if(x.type() == EXCEPTION) {
          std::cerr << "Evaluation error: " << x.exception_message().string_data() << std::endl;
          state.print_stack_trace();
          break;
        }
      } else {
        // state.vector_append(vec, x);
        std::cout << x << std::endl;
      }
    }
  }
  // std::cout << vec << std::endl;
}

void do_repl() {
  Value x, vec;
  AR_FRAME(state, x, vec);
  size_t i = 1;

  char* line = 0;
  linenoiseHistorySetMaxLen(1024);

  std::ostringstream prompt;

  while(i++) {
    prompt << (i - 1) << "> ";
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

      if(x.type() == EXCEPTION) {
        std::cout << "Reader error: " << x.exception_message().string_data() << std::endl;
      } else {
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
}

int main(int argc, const char **argv) {
  state.gc.collect_before_every_allocation = true;
  state.boot();

  std::string open_repl("--repl");

  if(argc > 1) {
    // read files
    for(size_t i = 1; i != argc; i++) {
      if(open_repl.compare(argv[i]) == 0) {
        do_repl();
        continue;
      }
      do_file(argv[i], true);
    }
  } else {
    do_repl();
 
  }
  state.gc.collect();

  state.print_gc_stats(std::cout);
  return 0;
}
