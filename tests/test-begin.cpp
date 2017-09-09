#include <sstream>

#include "arete.cpp"

using namespace arete;

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

// A fixture that creates, boots and deletes a new state for each test
struct ASB {
  ASB() {
    state.boot();
  }
  ~ASB() {}

  State state;
};

// A fixture that creates and deletes a new state for each test. 
struct AS {
  AS() {
#if ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE
    state.gc.collect_before_every_allocation = true;
#endif 
  }

  ~AS() {

  }
  State state;
};

TEST_CASE("info") {
  std::cout << "sizeof(Value): " << sizeof(Value) << std::endl;
  std::cout << "sizeof(State): " << sizeof(State) << std::endl;
  std::cout << "sizeof(HeapValue): " << sizeof(HeapValue) << std::endl;
}

///// (TYPE) Testing value representation

TEST_CASE("fixnum representation") {
  Value x = Value::make_fixnum(12345);

  CHECK(x.type_unsafe() == FIXNUM);
  CHECK(x.fixnum_value() == 12345);
  
  for(size_t i = 0; i != 100; i++) {
    Value y = Value::make_fixnum(i);
    CHECK(y.type_unsafe() == FIXNUM);
    CHECK(y.fixnum_value() == i);
  }
}

TEST_CASE("constant representation") {
  Value t = C_TRUE, f = C_FALSE, nil = C_NIL, eof = C_EOF, unspec = C_UNSPECIFIED;
  
  CHECK(t.type_unsafe() == CONSTANT);
  CHECK(f.type_unsafe() == CONSTANT);
  CHECK(nil.type_unsafe() == CONSTANT);
  CHECK(eof.type_unsafe() == CONSTANT);
  CHECK(unspec.type_unsafe() == CONSTANT);

  // Comparison operator
  CHECK(t == C_TRUE);

  CHECK(t.bits == 2);
  CHECK(f.bits == 6);
  CHECK(nil.bits == 10);
  CHECK(nil.bits != f.bits);
  CHECK(eof.bits == 18);
  CHECK(unspec.bits == 14);
}

TEST_CASE("gc alignment works") {
  CHECK(GCCommon::align(8, 63) == 64);
  CHECK(GCCommon::align(4096, 4095) == 4096);
}

TEST_CASE("frames successfully save pointers to stack values") {
  State state;
  Value v = Value::make_fixnum(12345);
  Value y = Value::make_fixnum(555);
  AR_FRAME(state, v);

  (*state.gc.frames[0]->values[0]) = (HeapValue*) y.bits;
  CHECK(v.fixnum_value() == 555);
}

TEST_CASE("handles work") {
  State state;
  {
    Handle h1(state, Value::make_fixnum(1));
    Handle h2(state, Value::make_fixnum(2));
    Handle h3(state, Value::make_fixnum(3));

    CHECK(state.gc.handles.back() == &h3);
    CHECK(state.gc.handles.front() == &h1);
    CHECK(state.gc.handles.size() == 3);
  }
}

TEST_CASE("heap values work") {
  HeapValue v;
  v.initialize(FLONUM, 0, sizeof(Flonum));

  CHECK(v.get_type() == FLONUM);
  CHECK(v.get_mark_bit() == 0);
  CHECK(v.size == sizeof(Flonum));

  v.flip_mark_bit();

  CHECK(v.get_mark_bit() == 1);
}

#define FLONUMS_PER_BLOCK (ARETE_BLOCK_SIZE / GCCommon::align(8, sizeof(Flonum)))