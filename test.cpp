// test.cpp - in-progress C++ stuff
#include <assert.h>

#include "arete.hpp"
#include "arete.cpp"

int main(void) {
  using namespace arete;

  State state;

  Value a, b, c, d;

  Value table = state.make_table();

  AR_FRAME(state, table, a, b, c, d);

  a = state.make_string("hello world");
  b = state.get_symbol("hello world");

  for(size_t i = 0; i != 100; i++) {
    std::ostringstream number_string;
    number_string << i;
    c = state.make_string(number_string.str());

    state.table_insert(table, c, Value::make_fixnum(i));

    // NB should tables copy strings? yes.
    Value cell = state.table_get_cell(table, c);
    std::cout << cell << std::endl;
  }


  state.print_table_verbose(table);

  return 0;
}
