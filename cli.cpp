// cli.cpp - arete command line interface 

#include "arete.hpp"

#include "linenoise.h"

using namespace arete;
int main(void) {
  State state;

  Value x;
  size_t i = 1;

  std::cout << "Arete 0.1" << std::endl;

  AR_FRAME(state, x);

  while(i++) {
    char* line = linenoise("> ");

    if(!line) {
      break;
    }

    std::ostringstream os;
    os << "repl:" << i;
    StringReader reader(state, line, os.str());
    
    while(x != C_EOF) { 
      x = reader->read();
      if(x == C_EOF) {
        break;
      }
      std::cout << x << std::endl;
    }
    
    x = C_UNSPECIFIED;
  }

  return 0;
}
