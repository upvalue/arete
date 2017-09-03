// cli.cpp - arete command line interface 

#include <fstream>
#include <iostream>

#include "linenoise.h"

#include "arete.hpp"

using namespace arete;

int main(int argc, const char **argv) {
  State state;
  state.boot();

  Value x;
  AR_FRAME(state, x);

  if(argc > 1) {
    // read a file
    const char* file_path = argv[1];

    std::ifstream file_handle(file_path, std::ios::in);
    std::stringstream ss;
    ss << file_handle.rdbuf();
    Reader reader(state, ss);
    reader.file = state.register_file(argv[1]);
    while(true) {
      x = reader.read();
      if(x == C_EOF) {
        break;
      }

      if(x.type() == EXCEPTION) {
        std::cerr << "Reader error: " << x.exception_message().string_data() << std::endl;
        break;
      } else {
        std::cout << x << std::endl;
      }
    }
  } else {
    size_t i = 1;

    std::cout << "Arete 0.1" << std::endl;

    char* line = 0;
    linenoiseHistorySetMaxLen(1024);

    while(i++) {
      line = linenoise("> ");

      if(!line) {
        break;
      }

      linenoiseHistoryAdd(line);

      std::ostringstream os;
      os << "repl:" << i - 1;
      StringReader reader(state, line, os.str());
      
      while(x != C_EOF) { 
        x = reader->read();
        if(x == C_EOF) {
          break;
        }

        if(x.type() == EXCEPTION) {
          std::cout << "Reader error: " << x.exception_message().string_data() << std::endl;
        } else {
          std::cout << x << std::endl;

        }
      }
      
      x = C_UNSPECIFIED;
      free(line); line = 0;
    }

    if(line) free(line);
  }

  return 0;
}
