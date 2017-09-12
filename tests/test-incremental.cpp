#ifdef ARETE_GC_STRATEGY
# undef ARETE_GC_STRATEGY
#endif 

#define ARETE_GC_STRATEGY ARETE_GC_INCREMENTAL

#include "test-begin.cpp"

///// (GC) GARBAGE COLLECTOR TESTS

TEST_CASE_FIXTURE(AS, "gc allocation correctly allocates from a second block") {
  CHECK(state.gc.blocks.size() == 1);
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
  CHECK(!state.gc.marked(f2.heap));
}

TEST_CASE_FIXTURE(AS, "gc marking recursive values") {
  Value p, f1, f2;
  AR_FRAME(state, p, f1, f2);

  f1 = state.make_flonum(0.0);
  f2 = state.make_flonum(0.0);

  p = state.make_pair(f1, f2);

  state.gc.collect();

  CHECK(state.gc.marked(p.heap));
  CHECK(state.gc.marked(f1.heap));
  CHECK(state.gc.marked(f2.heap));
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

TEST_CASE_FIXTURE(AS, "gc reallocating dead objects works correctly") {
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

// TODO test large object allocation

#include "test-runtime.cpp"
