#define ARETE_LOG_TAGS (ARETE_LOG_TAG_GC)
#define ARETE_LINK

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
  Value t = Value::t(), f = Value::f();
  
  CHECK(t.type() == TCONSTANT);
  CHECK(f.type() == TCONSTANT);

  CHECK(t.bits == 2);
  CHECK(f.bits == 6);

  std::cout << t << ' ' << f << std::endl;
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

TEST_CASE("gc successfully allocates") {

}



/*

// A fixture that creates and deletes a new state for each test. 
struct AS {
  State state;
};

TEST_CASE_FIXTURE(AS, "gc allocation") {
  Fixnum *f = state.gc.allocate<Fixnum>();
  CHECK(f != (Fixnum*)0);
}

#define FIXNUMS_PER_BLOCK ARETE_BLOCK_SIZE / sizeof(Fixnum)

TEST_CASE_FIXTURE(AS, "gc allocation with multiple blocks") {
  arete::Block* b = new arete::Block(ARETE_BLOCK_SIZE, state.gc.mark_bit);
  state.gc.blocks.push_back(b);

  CHECK(state.gc.blocks.size() == 2);

  for(size_t i = 0; i != (FIXNUMS_PER_BLOCK) + 1; i++) {
    state.gc.allocate<Fixnum>();
  }

  CHECK(state.gc.collections == 0);
}

TEST_CASE_FIXTURE(AS, "gc collection") {
  // Exhaust memory by allocating more than initial block
  for(size_t i = 0; i != (ARETE_BLOCK_SIZE / sizeof(Fixnum)) + 1; i++) {
    state.gc.allocate<Fixnum>();
  }

  CHECK(state.gc.collections == 1);
}

TEST_CASE_FIXTURE(AS, "gc frame tracking") {
  Fixnum* f = 0;
  ARETE_FRAME(state, f);

  CHECK(state.gc.frames.size() == 1);
  CHECK(state.gc.frames[0]->size == 1);
  CHECK_MESSAGE(((ptrdiff_t)state.gc.frames[0]->roots[0]) == (ptrdiff_t)&f, "frame successfully captures pointer to stack variable");

  f = (Fixnum*) 12345;
  CHECK_MESSAGE(((ptrdiff_t)*state.gc.frames[0]->roots[0]) == (ptrdiff_t)12345, "frame successfully captures pointer to stack variable");
}

TEST_CASE_FIXTURE(AS, "gc marking simple immediate values") {
  Fixnum* f = 0, *f2 = 0;
  // Intentionally leave out f2; only f should be marked as live
  ARETE_FRAME(state, f);

  f = state.gc.allocate<Fixnum>();
  f2 = state.gc.allocate<Fixnum>();

  CHECK(f->mark_bit == state.gc.mark_bit);
  CHECK(f->mark_bit == 1);
  CHECK(f2->mark_bit == state.gc.mark_bit);

  state.gc.collect();

  CHECK(f->mark_bit == state.gc.mark_bit);
  CHECK(f->mark_bit == 0);
  // f2 is dead
  CHECK(f2->mark_bit != state.gc.mark_bit);
}

TEST_CASE_FIXTURE(AS, "gc marking recursive values") {
  Pair* p = 0;
  Fixnum *f1 = 0, *f2 = 0;
  ARETE_FRAME(state, p);

  f1 = state.gc.allocate<Fixnum>();
  f2 = state.gc.allocate<Fixnum>();
  p = state.make_pair(f1, f2);

  state.gc.collect();
  CHECK(p->mark_bit == state.gc.mark_bit);
  CHECK(f1->mark_bit == state.gc.mark_bit);
  CHECK(f2->mark_bit == state.gc.mark_bit);
}

TEST_CASE_FIXTURE(AS, "gc collection failure") {
  // Exhaust memory by allocating a bunch of living objects that can't be collected

  Value* vars[FIXNUMS_PER_BLOCK];
  Value** roots[FIXNUMS_PER_BLOCK];
  memset(roots, 0, FIXNUMS_PER_BLOCK);
  Frame f(state, FIXNUMS_PER_BLOCK, roots);

  {
    CHECK(state.gc.frames.size() == 1);

    for(size_t i = 0; i != FIXNUMS_PER_BLOCK; i++) {
      vars[i] = state.gc.allocate<Fixnum>();
      ((Fixnum*) vars[i])->data = i;
      roots[i] = &vars[i];
    }

    state.gc.collect();
  }
  CHECK(state.gc.frames.size() == 1);
  // Memory should grow at this point.
  CHECK_MESSAGE(state.gc.live_objects_after_collection == FIXNUMS_PER_BLOCK, "all fixnums survived collection");

  CHECK_MESSAGE(state.gc.blocks.size() == 2, "additional block was allocated");
}

TEST_CASE_FIXTURE(AS, "gc allocating dead objects works correctly") {
  // Allocate fixnums and make half of them dead
  Value* vars[FIXNUMS_PER_BLOCK];
  memset(vars, 0, FIXNUMS_PER_BLOCK * sizeof(Value*));
  Value** roots[FIXNUMS_PER_BLOCK];
  memset(roots, 0, FIXNUMS_PER_BLOCK * sizeof(Value*));
  Frame f(state, FIXNUMS_PER_BLOCK, roots);

  for(size_t i = 0; i != FIXNUMS_PER_BLOCK; i++) {
    vars[i] = state.gc.allocate<Fixnum>();
    ((Fixnum*) vars[i])->data =i;
    if((i % 2) == 0) {
      roots[i] = &vars[i];
    } else {
      vars[i] = 0;
    }
  }
  state.gc.collect();

  Value* vars2[FIXNUMS_PER_BLOCK / 2];
  memset(vars2, 0, (FIXNUMS_PER_BLOCK / 2) * sizeof(Value*));
  Value** roots2[FIXNUMS_PER_BLOCK / 2];
  memset(roots2, 0, (FIXNUMS_PER_BLOCK / 2) * sizeof(Value*));
  Frame f2(state, FIXNUMS_PER_BLOCK / 2, roots2);

  for(size_t i = 0; i != (FIXNUMS_PER_BLOCK / 2); i++) { 
    // New fixnums will have negative values
    vars2[i] = state.gc.allocate<Fixnum>();
    ((Fixnum*) vars2[i])->data = (0 - i);
    roots2[i] = &vars2[i];
  }

  state.gc.collect();
  for(size_t i = 0; i != FIXNUMS_PER_BLOCK; i++) {
    if(i % 2 == 0) {
      CHECK_MESSAGE(((Fixnum*)vars[i])->data == i, "live objects were not overwritten");
    } 
  }

  for(size_t i = 0; i != FIXNUMS_PER_BLOCK / 2; i++) {
    CHECK_MESSAGE((((Fixnum*)vars2[i])->data == -i), "new objects were correctly allocated over dead objects");
  }
}

TEST_CASE_FIXTURE(AS, "gc large object allocation") {
  Fixnum* f = 0;
  ARETE_FRAME(state, f);

  f = state.gc.allocate<Fixnum>(ARETE_BLOCK_SIZE * 2);
}

// TEST_CASE_FIXTURE(AS, "gc ")
*/
