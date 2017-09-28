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
}

TEST_CASE_FIXTURE(AS, "rename creation") {
  Value x, y, z;
  AR_FRAME(state, x, y, z);
  x = state.get_symbol("renamable");
  y = state.make_rename(x, C_FALSE);
  CHECK(y.type() == RENAME);
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

///// (READ) READER

TEST_SUITE_BEGIN("reader");

TEST_CASE_FIXTURE(AS, "read fixnum") {
  AR_STRING_READER(reader, state, "12345");
  Value x = reader->read();
  CHECK(x.type() == FIXNUM);
  CHECK(x.fixnum_value() == 12345);
  Value e = reader->read();
  CHECK(e == C_EOF);
}

TEST_CASE_FIXTURE(AS, "read string escapes") {
  Value s;
  AR_FRAME(state, s);
  AR_STRING_READER(reader, state, "\"hey\\n\"");
  s = reader->read();
  CHECK(s.type() == STRING);
  CHECK(s.string_equals("hey\n"));
}

TEST_CASE_FIXTURE(AS, "read boolean constants") {
  AR_STRING_READER(reader, state, "#f#t");

  Value f, t;
  AR_FRAME(state, f, t);
  f = reader->read();
  t = reader->read();

  CHECK(t.type() == CONSTANT);
  CHECK(f.type() == CONSTANT);

  CHECK(t == C_TRUE);
  CHECK(f == C_FALSE);
}


TEST_CASE_FIXTURE(AS, "read a symbol") {
  AR_STRING_READER(reader, state, "hello");

  Value sym;
  AR_FRAME(state, sym);
  sym = reader->read();

  std::string check("hello");
  CHECK(sym.type() == BOX);
  CHECK(check.compare(sym.unbox().symbol_name_bytes()) == 0);
}


TEST_CASE_FIXTURE(AS, "read a list") {
  AR_STRING_READER(reader, state, "(1 2 3)");

  Value lst;
  AR_FRAME(state, lst);
  lst = reader->read();

  CHECK(lst.type() == PAIR);
}

TEST_CASE_FIXTURE(AS, "read a dotted list") {
  AR_STRING_READER(reader, state, "(1 . 2)");

  Value lst;
  AR_FRAME(state, lst);
  lst = reader->read();

  CHECK(lst.car() == Value::make_fixnum(1));
  CHECK(lst.cdr() == Value::make_fixnum(2));
}

TEST_CASE_FIXTURE(ASB, "read a quoted expression") {
  std::stringstream ss("'hello");
  Reader reader(state, ss);

  Value x;
  AR_FRAME(state, x)
  x = reader.read();
}

TEST_CASE_FIXTURE(ASB, "StringReader & source code info") {
  AR_STRING_READER(reader, state, "(hello)");
  Value x = reader->read();
  CHECK(x.pair_has_source());
}

TEST_CASE_FIXTURE(ASB, "reader expression comments") {
  AR_STRING_READER(reader, state, "#;(mary had a little lamb)");
  Value x = reader->read();
  CHECK(x == C_EOF);
}

TEST_CASE_FIXTURE(ASB, "EOF in dotted list") {
  AR_STRING_READER(reader, state, "(.\n");

  Value x = reader->read();
  CHECK(x.type() == EXCEPTION);
}

TEST_CASE_FIXTURE(ASB, "EOF in list") {
  AR_STRING_READER(reader, state, "\n\n(\n\n");
  Value x = reader->read();
  CHECK(x.type() == EXCEPTION);
}

TEST_CASE_FIXTURE(ASB, "EOF in list") {
  AR_STRING_READER(reader, state, ")");

  Value x = reader->read();
  CHECK(x.type() == EXCEPTION);
}

TEST_CASE_FIXTURE(ASB, "reader dot at toplevel") {
  AR_STRING_READER(reader, state, ".");
  CHECK(reader->read().type() == EXCEPTION);
}

TEST_CASE_FIXTURE(ASB, "reader nil") {
  AR_STRING_READER(reader, state, "()");
  CHECK(reader->read() == C_NIL);
  AR_STRING_READER(reader2, state, "(                       )");
  CHECK(reader2->read() == C_NIL);
  AR_STRING_READER(reader3, state, "[]");
  CHECK(reader3->read() == C_NIL);
}


TEST_CASE_FIXTURE(ASB, "reader mismatched brackets") {
  AR_STRING_READER(reader, state, "(]");
  CHECK(reader->read().type() == EXCEPTION);
}

TEST_CASE_FIXTURE(ASB, "reader define") {
  AR_STRING_READER(reader, state, "(define x #t)");
  // Value x = reader->read();

}

TEST_CASE_FIXTURE(ASB, "reader comment EOF") {
  AR_STRING_READER(reader, state, ";; hello world");
  CHECK(reader->read() == C_EOF);
}

TEST_CASE_FIXTURE(ASB, "reader comment lines") {
  AR_STRING_READER(reader, state, ";;1\n;;2\n;;3\n");
  CHECK(reader->read() == C_EOF);
  CHECK(reader->line == 4);
}

TEST_CASE_FIXTURE(ASB, "reader string") {
  AR_STRING_READER(reader, state, "\"hello world\"");
  Value x = reader->read();
  CHECK(x.type() == STRING);
  CHECK(x.string_equals("hello world"));
}

TEST_CASE_FIXTURE(ASB, "reader character literals") {
  AR_STRING_READER(reader, state, "#\\a");
  Value x = reader->read();
  CHECK(x.type() == CHARACTER);
  CHECK(x.character() == 'a');

  AR_STRING_READER(reader2, state, "#\\newline");
  x = reader2->read();
  CHECK(x.type() == CHARACTER);
  CHECK(x.character() == '\n');

  AR_STRING_READER(reader3, state, "#\\unknown");
  x = reader3->read();
  CHECK(x.type() == EXCEPTION);

  // eof in character literal
  AR_STRING_READER(reader4, state, "#\\")
  x = reader4->read();
  CHECK(x.type() == EXCEPTION);
}

TEST_CASE_FIXTURE(ASB, "reader vectors") {
  AR_STRING_READER(reader, state, "#()");
  Value x = reader->read();
  CHECK(x.type() == VECTOR);
  CHECK(x.vector_length() == 0);


  AR_STRING_READER(reader2, state, "#(0 1 2 3 4)");
  x = reader2->read();
  CHECK(x.type() == VECTOR);
  CHECK(x.vector_length() == 5);

  for(size_t i = 0; i != x.vector_length(); i++) {
    CHECK(x.vector_ref(i) == Value::make_fixnum(i));
  }

  // EOF in vector
  AR_STRING_READER(reader3, state, "#(1 2");
  x = reader3->read();
  CHECK(x.is_active_exception());
}

TEST_CASE_FIXTURE(ASB, "reader flonums") {
  AR_STRING_READER(reader, state, "123.456");
  Value x = reader->read();
  CHECK(x.type() == FLONUM);
}

TEST_SUITE_END();

///// INTERPRETER TESTS

TEST_CASE_FIXTURE(ASB, "eval immediate") {
  Value x = Value::make_fixnum(12345);
  AR_FRAME(state, x);

  x = state.eval(C_FALSE, x);
  CHECK(x == Value::make_fixnum(12345));

  x = state.eval_toplevel(C_TRUE);
  CHECK(x == C_TRUE);
}

TEST_CASE_FIXTURE(ASB, "eval undefined variable") {
  Value x = state.get_symbol("hello"), res;
  AR_FRAME(state, x, res);
  res = state.eval(C_FALSE, x);
  CHECK(res.type() == EXCEPTION);
}

#if ARETE_GC_STRATEGY == ARETE_GC_INCREMENTAL
TEST_CASE_FIXTURE(ASB, "eval syntactic forms") {
  AR_STRING_READER(reader, state, "(define x 1234) x");
  Value x;
  AR_FRAME(state, x);

  state.eval_toplevel(reader->read());
  x = state.eval_toplevel(reader->read());
  CHECK(x == Value::make_fixnum(1234));

  AR_STRING_READER(reader2, state, "((lambda () 4443))");

  x = state.eval_toplevel(reader2->read());
  CHECK(x == Value::make_fixnum(4443));
}


TEST_CASE_FIXTURE(ASB, "eval undefined variable") {
  // Value x = state.get_symbol("hello");
  // x.car();
  // res = state.eval(C_FALSE, x);
}

#endif 
