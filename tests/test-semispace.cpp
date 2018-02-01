#define AR_GC_DEBUG 1

//#define ARETE_LOG_TAGS (ARETE_LOG_TAG_GC)

#include "test-begin.cpp"

TEST_CASE_FIXTURE(AS, "gc collection & live") {
  Value f = state.make_flonum(0.0);
  CHECK(state.gc.live(f));
  state.gc.collect(0, false);
  CHECK(!state.gc.live(f));
}

TEST_CASE_FIXTURE(AS, "gc collection correctly updates frames and handles") {
  Value f = state.make_flonum(0.0);
  AR_FRAME(state, f);
  Handle* h = new Handle(state);
  h->ref = state.make_flonum(0.0);
  CHECK(state.gc.live(f));
  CHECK(state.gc.live(h->ref));
  state.gc.collect(0, false);
  CHECK(state.gc.live(f));
  CHECK(state.gc.live(h->ref));
  delete h;
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
      CHECK(state.gc.live(vars[i].heap));
      CHECK_MESSAGE(((Flonum*)vars[i].heap)->number == i, "live objects were not overwritten");
    } 
  }

  for(size_t i = 0; i != FLONUMS_PER_BLOCK / 2; i++) {
    CHECK_MESSAGE((((Flonum*)vars2[i].heap)->number == -i), "new objects were correctly allocated over dead objects");
  }
}

#include "test-runtime.cpp"
