// test.cpp - arete runtime tests with doctest

// TODO: These are overly verbose in a lot of places and should be simplified

TEST_CASE_FIXTURE(AS, "gc allocation") {
  Value f = state.make_flonum(0.0);
  CHECK(f.bits);
  CHECK(!f.immediatep());
  CHECK(f.type_unsafe() == FLONUM);
#if ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE
  CHECK(state.gc.live(f));
#endif
  CHECK(arete::current_state == &state);
  CHECK(f.type() == FLONUM);
}

// #define FLONUMS_PER_BLOCK (ARETE_BLOCK_SIZE / sizeof(Flonum))


// TEST_CASE_FIXTURE(ASB, "state.boot") {}

///// VALUES

TEST_CASE_FIXTURE(AS, "strings") {
  Value s;
  AR_FRAME(state, s);
  s = state.make_string("hello world");
  CHECK(s.type() == STRING);
  CHECK(s.string_equals("hello world"));

  state.gc.collect();
}

TEST_CASE_FIXTURE(AS, "symbol interning") {
  Value x, y, z;
  AR_FRAME(state, x, y, z);
  x = state.get_symbol("hello-world");
  // CHECK(state.gc.live(x));
  state.gc.collect();
  y = state.get_symbol("hello-world");
  z = state.get_symbol("hello-world2");
  CHECK_MESSAGE(x.bits == y.bits, "symbols intern");
  CHECK(x.type_unsafe() == SYMBOL);
  CHECK(x.bits != z.bits);

  CHECK(x.identifierp());
  CHECK(y.identifierp());
}

TEST_CASE_FIXTURE(AS, "rename creation") {
  Value x, y, z;
  AR_FRAME(state, x, y, z);
  x = state.get_symbol("renamable");
  y = state.make_rename(x, C_FALSE);

  CHECK(y.identifierp());
  CHECK(y.type() == RENAME);
  // CHECK(state.identifier_equal(x, y));
  state.gc.collect();
}

TEST_CASE_FIXTURE(AS, "vectors") {
  Value v;
  {
    AR_FRAME(state, v);
    v = state.make_vector();
    CHECK(v.type() == VECTOR);
    for(size_t i = 0; i != 100; i++) {
      state.vector_append(v, Value::make_fixnum(i));
    }
    for(size_t i = 0; i != 100; i++) {
      CHECK(v.vector_ref(i) == Value::make_fixnum(i));
    }
    CHECK(v.vector_length() == 100);
  }
}

TEST_CASE_FIXTURE(AS, "character literals") {
  Value s;
  AR_FRAME(state, s);
  s = state.make_char('a');
  CHECK(s.type() == CHARACTER);
  CHECK(s.character() == 'a');
}

TEST_CASE_FIXTURE(AS, "exceptions") {
  Value exc, msg;
  AR_FRAME(state, exc);
  exc = state.make_exception("fake-error", "my message");
  msg = exc.exception_message();

  CHECK(exc.is_active_exception());
  CHECK(msg.type() == STRING);

  CHECK(msg.string_equals("my message"));
}

TEST_CASE_FIXTURE(AS, "tables") {
  Value table, key, tmp;
  AR_FRAME(state, table, key, tmp);
  table = state.make_table();
  for(size_t i = 0; i != 100; i++) {
    std::ostringstream os;
    os << i;
    key = state.make_string(os.str());
    state.table_insert(table, key, Value::make_fixnum(i));
    bool found;
    tmp = state.table_get(table, key, found);
    CHECK_MESSAGE(found, "table insert successful");
    CHECK(tmp == Value::make_fixnum(i));
    //CHECK(state.table_get(table, key, found))
  }

  // state.print_table_verbose(table);

  for(ptrdiff_t i = 0; i != 100; i++) {
    Value fx = Value::make_fixnum(0 - i);
    std::ostringstream os;
    os << i;
    key = state.make_string(os.str());
    state.table_set(table, key, fx);
    bool found;
    Value res = state.table_get(table, key, found);
    CHECK(found);
    CHECK(res.type() == FIXNUM);
    CHECK(res.bits == fx.bits);
  }

  // state.print_table_verbose(table);
}

