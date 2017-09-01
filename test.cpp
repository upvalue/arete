// test.cpp - arete runtime tests with doctest

#define ARETE_LOG_TAGS (ARETE_LOG_TAG_GC)

#include <assert.h>

#include "arete.hpp"

using namespace arete;

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

// Enum usage causes bizarre errors with doctest for some reason
static const int TFIXNUM = FIXNUM, TCONSTANT = CONSTANT, TFLONUM = FLONUM;

TEST_CASE("fixnum representation") {
  Value x = Value::make_fixnum(12345);

  CHECK(x.type() == TFIXNUM);
  CHECK(x.fixnum_value() == 12345);
  
  for(size_t i = 0; i != 100; i++) {
    Value y = Value::make_fixnum(i);
    CHECK(y.type() == TFIXNUM);
    CHECK(y.fixnum_value() == i);
  }
}

TEST_CASE("constant representation") {
  Value t = Value::t(), f = Value::f(), nil = Value::nil();
  
  CHECK(t.type() == TCONSTANT);
  CHECK(f.type() == TCONSTANT);
  CHECK(nil.type() == TCONSTANT);

  CHECK(t.bits == 2);
  CHECK(f.bits == 6);
  CHECK(nil.bits == 10);

  std::cout << t << ' ' << f << ' ' << nil << std::endl;
}

TEST_CASE("gc alignment works") {
  CHECK(GC::align(8, 63) == 64);
  CHECK(GC::align(4096, 4095) == 4096);
}

TEST_CASE("frames successfully save pointers to stack values") {
  State state;
  Value v = Value::make_fixnum(12345);
  Value y = Value::make_fixnum(555);
  AR_FRAME(state, v);

  (*state.gc.frames[0]->values[0]) = (HeapValue*) y.bits;
  CHECK(v.fixnum_value() == 555);
}

TEST_CASE("heap values work") {
  HeapValue v;
  v.initialize(FLONUM, 0, sizeof(Flonum));

  CHECK(v.get_type() == TFLONUM);
  CHECK(v.get_mark_bit() == 0);
  CHECK(v.size == sizeof(Flonum));

  v.flip_mark_bit();

  CHECK(v.get_mark_bit() == 1);
}

// A fixture that creates and deletes a new state for each test. 
struct AS {
  State state;
};

TEST_CASE_FIXTURE(AS, "gc allocation") {
  Value f = state.make_flonum(0.0);
  CHECK(f.bits);
}

#define FLONUMS_PER_BLOCK (ARETE_BLOCK_SIZE / sizeof(Flonum))

TEST_CASE_FIXTURE(AS, "gc allocation correctly adds a block") {
  arete::Block* b = new arete::Block(ARETE_BLOCK_SIZE, state.gc.mark_bit);
  state.gc.blocks.push_back(b);

  CHECK(state.gc.blocks.size() == 2);

  for(size_t i = 0; i != (FLONUMS_PER_BLOCK) + 1; i++) {
    state.make_flonum(0.0);
  }

  CHECK(state.gc.collections == 0);
}

TEST_CASE_FIXTURE(AS, "gc collection") {
  // Exhaust memory by allocating more than initial block
  for(size_t i = 0; i != (ARETE_BLOCK_SIZE / sizeof(Flonum)) + 1; i++) {
    state.make_flonum(0.0);
  }

  CHECK(state.gc.collections == 1);
}

TEST_CASE_FIXTURE(AS, "gc marking simple immediate values") {
  Value f, f2;
  // Intentionally leave out f2; only f should be marked as live
  AR_FRAME(state, f);

  f = state.make_flonum(0.0);
  f2 = state.make_flonum(0.0);

  // Objects should be marked upon allocation
  CHECK(f.heap->get_mark_bit() == state.gc.mark_bit);
  CHECK(f.heap->get_mark_bit() == 1);
  CHECK(f2.heap->get_mark_bit() == state.gc.mark_bit);

  state.gc.collect();

  // And now f2 should be dead
  CHECK(f.heap->get_mark_bit() == state.gc.mark_bit);
  CHECK(f2.heap->get_mark_bit() != state.gc.mark_bit);
  CHECK(!state.gc.live(f2.heap));
}

TEST_CASE_FIXTURE(AS, "gc marking recursive values") {
  Value p, f1, f2;
  AR_FRAME(state, p, f1, f2);

  f1 = state.make_flonum(0.0);
  f2 = state.make_flonum(0.0);

  p = state.make_pair(f1, f2);

  state.gc.collect();

  std::cout << f1 << std::endl;
  CHECK(state.gc.live(p.heap));
  CHECK(state.gc.live(f1.heap));
  CHECK(state.gc.live(f2.heap));
  std::cout << p << std::endl;
}

TEST_CASE_FIXTURE(AS, "gc collection failure") {
  // Exhaust memory by allocating a bunch of living objects that can't be collected

  Value vars[FLONUMS_PER_BLOCK];
  HeapValue** roots[FLONUMS_PER_BLOCK];
  memset(roots, 0, FLONUMS_PER_BLOCK * sizeof(HeapValue*));
  Frame f(state, FLONUMS_PER_BLOCK, roots);

  {
    CHECK(state.gc.frames.size() == 1);
    for(size_t i = 0; i != FLONUMS_PER_BLOCK; i++) {
      vars[i] = state.make_flonum(0.0);
      static_cast<Flonum*>(vars[i].heap)->number = i;
      roots[i] = &(vars[i].heap);
    }

    state.gc.collect();
  }

  CHECK(state.gc.frames.size() == 1);
  CHECK_MESSAGE(state.gc.live_objects_after_collection == FLONUMS_PER_BLOCK, "all fixnums survived collection");
  CHECK_MESSAGE(state.gc.blocks.size() == 2, "additional block was allocated");
}

TEST_CASE_FIXTURE(AS, "gc allocating dead objects works correctly") {
  // Allocate fixnums and make half of them dead
  Value vars[FLONUMS_PER_BLOCK];
  memset(vars, 0, FLONUMS_PER_BLOCK * sizeof(Value));
  HeapValue** roots[FLONUMS_PER_BLOCK];
  memset(roots, 0, FLONUMS_PER_BLOCK * sizeof(HeapValue*));
  Frame f(state, FLONUMS_PER_BLOCK, roots);

  for(size_t i = 0; i != FLONUMS_PER_BLOCK; i++) {
    vars[i] = state.make_flonum(i);
    if((i % 2) == 0) {
      roots[i] = &vars[i].heap;
    } else {
      vars[i].bits = 0;
    }
  }
  state.gc.collect();

  Value vars2[FLONUMS_PER_BLOCK / 2];
  memset(vars2, 0, (FLONUMS_PER_BLOCK / 2) * sizeof(Value));
  HeapValue** roots2[FLONUMS_PER_BLOCK / 2];
  memset(roots2, 0, (FLONUMS_PER_BLOCK / 2) * sizeof(HeapValue*));
  Frame f2(state, FLONUMS_PER_BLOCK / 2, roots2);

  for(size_t i = 0; i != (FLONUMS_PER_BLOCK / 2); i++) { 
    // New fixnums will have negative values
    vars2[i] = state.make_flonum(0 - i);
    roots2[i] = &vars2[i].heap;
  }

  state.gc.collect();
  for(size_t i = 0; i != FLONUMS_PER_BLOCK; i++) {
    if(i % 2 == 0) {
      CHECK_MESSAGE(((Flonum*)vars[i].heap)->number == i, "live objects were not overwritten");
    } 
  }

  for(size_t i = 0; i != FLONUMS_PER_BLOCK / 2; i++) {
    CHECK_MESSAGE((((Flonum*)vars2[i].heap)->number == -i), "new objects were correctly allocated over dead objects");
  }
}

/*
TODO Large object allocation
TEST_CASE_FIXTURE(AS, "gc large object allocation") {
  Fixnum* f = 0;
  ARETE_FRAME(state, f);

  f = state.gc.allocate<Fixnum>(ARETE_BLOCK_SIZE * 2);
}
*/

TEST_CASE_FIXTURE(AS, "symbol interning") {
  Value x, y;
  AR_FRAME(state, x, y);
  x = state.get_symbol("hello-world");
  y = state.get_symbol("hello-world");
  CHECK_MESSAGE(x.bits == y.bits, "symbols intern");
}