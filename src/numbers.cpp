// numbers.cpp - Numeric functionality

#include <math.h>

#include "arete.hpp"

namespace arete {

DefunGroup numbers("numbers");

Value State::make_flonum(double number) {
  Flonum* heap = (Flonum*) gc.allocate(FLONUM, sizeof(Flonum));
  heap->number = number;
  Value v(heap);
  return v;
}

///// FUNCTIONS  

// Casting arithmetic

// This is actually pretty tricky due to the need to cast from fixnum from flonum, the desire to
// avoid code duplication, and the need to avoid allocation because there's no good way currently
// to track variadic arguments

#define OPV(name, cname, operator, sub, nozero, ret0) \
  Value name(State& state, size_t argc, Value* argv, void* v) { \
    if(argc == 0) return Value::make_fixnum(ret0);  \
    static const char* fn_name = cname; (void) v; \
    ptrdiff_t fxresult = 0; \
    size_t i = 0; \
    if(argc == 1) { \
      AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].numeric()); \
      if(sub) return argv[0].type() == FLONUM ? state.make_flonum(0 - argv[0].flonum_value()) : Value::make_fixnum(0 - argv[0].fixnum_value()); \
      return argv[0]; \
    } \
    if(!nozero && argc == 2 && argv[0].fixnump() && argv[1].fixnump()) { \
      return Value::make_fixnum(argv[0].fixnum_value() operator argv[1].fixnum_value()); \
    } \
    for(i = 0; i != argc; i++) { \
      if(argv[i].type() == FLONUM) break; \
      AR_FN_ASSERT_ARG(state, i, "to be a number", argv[i].type() == FIXNUM); \
      if(nozero && argv[i].fixnum_value() == 0) return state.type_error("divide by zero"); \
      if(i == 0) { fxresult = argv[i].fixnum_value(); } else { fxresult = fxresult operator argv[i].fixnum_value(); } \
    } \
    if(i == argc) return Value::make_fixnum(fxresult); \
    double flresult = (double) fxresult; \
    for(; i != argc; i++) { \
      AR_FN_ASSERT_ARG(state, i, "to be a number", argv[i].type() == FIXNUM || argv[i].type() == FLONUM); \
      if(i == 0) { flresult = argv[i].flonum_value(); continue; } \
      if(argv[i].type() == FIXNUM) { \
        if(nozero && argv[i].fixnum_value() == 0) return state.type_error("divide by zero"); \
        flresult = flresult operator (double) argv[i].fixnum_value(); \
      } else if(argv[i].type() == FLONUM) { \
        if(nozero && argv[i].flonum_value() == 0.0) return state.type_error("divide by zero"); \
        flresult = flresult operator argv[i].flonum_value(); \
      } \
    } \
    return state.make_flonum(flresult); \
  }

OPV(fn_add, "+", +, false, false, 0)
OPV(fn_sub, "-", -, true, false, 0)
OPV(fn_mul, "*", *, false, false, 1)
OPV(fn_div, "/", /, false, true, 0)

AR_DEFUN("+", fn_add, 0, 0, true);
AR_DEFUN("-", fn_sub, 1, 1, true);
AR_DEFUN("*", fn_mul, 0, 0, true);
AR_DEFUN("/", fn_div, 2, 2, true);

#undef OPV

#define OPV_BOOL(name, cname, operator) \
  Value name(State& state, size_t argc, Value* argv, void* fn) { \
    static const char* fn_name = cname; (void) fn; \
    AR_FN_ARGC_GTE(state, argc, 2); \
    AR_FN_ASSERT_ARG(state, 0, "to be numeric", argv[0].numeric()); \
    if(argc == 2 && argv[0].fixnump() && argv[1].fixnump()) { \
      return Value::make_boolean(argv[0].fixnum_value() operator argv[1].fixnum_value()); \
    } \
    for(size_t i = 0; i != argc - 1; i++) { \
      AR_FN_ASSERT_ARG(state, (i+1), "to be numeric", argv[i+1].numeric()); \
      if(argv[i].type() == FIXNUM) { \
        if(argv[i+1].type() == FIXNUM) { \
          if(!(argv[i].bits operator argv[i+1].bits)) return C_FALSE; \
        } else { \
          if(!(argv[i].fixnum_value() operator argv[i+1].flonum_value())) return C_FALSE; \
        } \
      } else if(argv[i].type() == FLONUM) { \
        if(argv[i+1].type() == FLONUM) { \
          if(!(argv[i].flonum_value() operator argv[i+1].flonum_value())) return C_FALSE; \
        } else { \
          if(!(argv[i].flonum_value() operator (double) argv[i+1].fixnum_value())) return C_FALSE; \
        } \
      } \
    } \
    Value lhs = argv[argc-2], rhs = argv[argc-1]; \
    if(lhs.type() == FLONUM && rhs.type() == FLONUM) \
      return Value::make_boolean(lhs.flonum_value() operator rhs.flonum_value()); \
    else if(lhs.type() == FLONUM && rhs.type() == FIXNUM) \
      return Value::make_boolean(lhs.flonum_value() operator rhs.fixnum_value()); \
    else if(lhs.type() == FIXNUM && rhs.type() == FLONUM) \
      return Value::make_boolean(lhs.fixnum_value() operator rhs.flonum_value()); \
    else if(lhs.type() == FLONUM && rhs.type() == FIXNUM) \
      return Value::make_boolean(lhs.flonum_value() operator rhs.fixnum_value()); \
    return C_TRUE; \
  } \
  AR_DEFUN(cname, name, 2, 2, true);

// After writing all this out I see why the Lua/JS guys gave up and used floating point math only.
// What fun.

OPV_BOOL(fn_num_equal, "=", ==)
OPV_BOOL(fn_lt, "<", <)
OPV_BOOL(fn_lte, "<=", <=)
OPV_BOOL(fn_gte, ">=", >=)
OPV_BOOL(fn_gt, ">", >)

#undef OPV_BOOL
// Fixnum arithmetic

Value fn_fx_sub(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "fx-";

  AR_FN_ARGC_GTE(state, argc, 1);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  if(argc == 1) {
    return Value::make_fixnum(0 - argv[0].fixnum_value());
  }

  ptrdiff_t result = argv[0].fixnum_value();

  for(size_t i = 1; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    result -= argv[i].fixnum_value();
  }

  return Value::make_fixnum(result);
}
AR_DEFUN("fx-", fn_fx_sub, 1, 1, true);

Value fn_fx_add(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "fx+";
  ptrdiff_t result = 0;

  AR_FN_ARGC_EQ(state, argc, 2);

  for(size_t i = 0; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    result += argv[i].fixnum_value();
  }

  return Value::make_fixnum(result);
}

AR_DEFUN("fx+", fn_fx_add, 2, 2, true);

#define FX_COMPARISON(cname, name, op) \
  Value name (State& state, size_t argc, Value* argv, void* fn) { \
    static const char* fn_name = cname; (void) fn; \
    AR_FN_ARGC_EQ(state, argc, 2); \
    AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM); \
    AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM); \
    return Value::make_boolean(argv[0].fixnum_value() op argv[1].fixnum_value()); \
  } \
  AR_DEFUN(cname, name, 2, 2);

FX_COMPARISON("fx<=", fn_fx_lte, <=)
FX_COMPARISON("fx<", fn_fx_lt, <)
FX_COMPARISON("fx>", fn_fx_gt, >)
FX_COMPARISON("fx>=", fn_fx_gte, >=)

Value fn_fx_equals(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "fx=";
  AR_FN_ARGC_EQ(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].bits == argv[1].bits);
}
AR_DEFUN("fx=", fn_fx_equals, 2, 2);

Value fn_fx_div(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "fx/";
  AR_FN_ARGC_EQ(state, argc, 2);

  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  
  if(argv[1].fixnum_value() == 0) return state.type_error("divide by zero"); \
  
  return Value::make_fixnum(argv[0].fixnum_value() / argv[1].fixnum_value());
}
AR_DEFUN("fx/", fn_fx_div, 2, 2);

Value fn_expt(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "expt";
  AR_FN_ARGC_EQ(state, argc, 2);
  AR_FN_ASSERT_ARG(state, 0, "to be numeric", argv[0].numeric());
  AR_FN_ASSERT_ARG(state, 1, "to be numeric", argv[1].numeric());

  bool exact = argv[0].fixnump() && argv[1].fixnump();

  double result = pow(argv[0].fixnump() ? (double)argv[0].fixnum_value() : argv[0].flonum_value(),
    argv[1].fixnump() ? (double) argv[1].fixnum_value() : argv[1].flonum_value());

  return exact ? Value::make_fixnum((ptrdiff_t) result) : state.make_flonum(result);
}
AR_DEFUN("expt", fn_expt, 2, 2);

Value fn_floor(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "floor";
  AR_FN_ARGC_EQ(state, argc, 1);
  if(argv[0].fixnump()) return argv[0];
  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].heap_type_equals(FLONUM));

  return state.make_flonum(floor(argv[0].flonum_value()));
}
AR_DEFUN("floor", fn_floor, 1);

Value fn_round(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "round";
  AR_FN_ARGC_EQ(state, argc, 1);
  if(argv[0].fixnump()) return argv[0];
  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].heap_type_equals(FLONUM));
  return state.make_flonum(trunc(argv[0].flonum_value()));
}
AR_DEFUN("round", fn_round, 1);


Value fn_truncate(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "truncate";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_NUMBER(state, argv, 0);
  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].heap_type_equals(FLONUM));
  return state.make_flonum(trunc(argv[0].inexact_number()));
}
AR_DEFUN("truncate", fn_truncate, 1);

Value fn_ceiling(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "ceiling";
  AR_FN_ARGC_EQ(state, argc, 1);

  if(argv[0].fixnump()) return argv[0];

  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].heap_type_equals(FLONUM));
  return state.make_flonum(ceil(argv[0].flonum_value()));
}
AR_DEFUN("ceiling", fn_ceiling, 1);

Value fn_modulo(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "modulo";
  AR_FN_ARGC_EQ(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_fixnum((argv[0].fixnum_value() % argv[1].fixnum_value() + argv[1].fixnum_value()) % argv[1].fixnum_value());
}
AR_DEFUN("modulo", fn_modulo, 2);

Value fn_quotient(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "quotient";
  AR_FN_ARGC_EQ(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  ldiv_t div = ldiv((long)argv[0].fixnum_value(), (long)argv[1].fixnum_value());
  return Value::make_fixnum(div.quot);
}
AR_DEFUN("quotient", fn_quotient, 2);

Value fn_remainder(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "remainder";
  AR_FN_ARGC_EQ(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  ldiv_t div = ldiv((long)argv[0].fixnum_value(), (long)argv[1].fixnum_value());
  return Value::make_fixnum(div.rem);
}
AR_DEFUN("remainder", fn_remainder, 2);

Value fn_atan(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "atan";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].numeric());

  double y = argv[0].inexact_number();

  if(argc == 1) {
    return state.make_flonum(atan(y));
  }

  AR_FN_ASSERT_ARG(state, 1, "to be a number", argv[1].numeric());
  double x = argv[1].fixnump() ? (double) argv[1].fixnum_value() : argv[1].fixnum_value();

  return state.make_flonum(atan2(y, x));
}
AR_DEFUN("atan", fn_atan, 1, 2);

// Define trigonometric builtins that take one fixnum or flonum as argument, always return a flonum
// and have the same name as their math.h equivalent

#define DEFUN_TRIG(name) \
  Value fn_ ## name (State& state, size_t argc, Value* argv, void* v) { \
    static const char* fn_name = #name; \
    AR_FN_ARGC_EQ(state, argc, 1); \
    AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].numeric()); \
    return state.make_flonum(name (argv[0].fixnump() ? (double) argv[0].fixnum_value() : argv[0].flonum_value())); \
  }  \
  AR_DEFUN(#name, fn_ ##name, 1);

DEFUN_TRIG(exp);
DEFUN_TRIG(cos);
DEFUN_TRIG(sin);
DEFUN_TRIG(log);
DEFUN_TRIG(tan);
DEFUN_TRIG(asin);
DEFUN_TRIG(acos);

// TODO: Radix argument

Value fn_number_to_string(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "number->string";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].numeric());
  std::ostringstream os;
  os << argv[0];
  return state.make_string(os.str());
}
AR_DEFUN("number->string", fn_number_to_string, 1);

Value fn_minmax(State& state, size_t argc, Value* argv, bool min) {
  const char* fn_name = min ? "min" : "max";
  AR_FN_ARGC_GTE(state, argc, 1);
  ptrdiff_t fxacc = 0;
  bool inexact = false;
  bool set = false;
  size_t i = 0;
  for(; i != argc; i++) {
    if(argv[i].fixnump()) {
      if(!set) {
        fxacc = argv[i].fixnum_value();
        set = true;
      } else {
        fxacc = min ? (fxacc > argv[i].fixnum_value() ? argv[i].fixnum_value() : fxacc) :
          (fxacc > argv[i].fixnum_value() ? fxacc : argv[i].fixnum_value());
      }
    } else if(argv[i].heap_type_equals(FLONUM)) {
      inexact = true;
      break;
    } else {
      AR_FN_ASSERT_ARG(state, i, "to be numeric", argv[i].numeric());
    }
  }
  if(!inexact) return Value::make_fixnum(fxacc);
  double flacc = (double) fxacc;
  for(; i != argc; i++) {
    if(argv[i].fixnump()) {
      if(!set) {
        flacc = (double) argv[i].fixnum_value();
        set = true;
      } else {
        flacc = min ? (flacc > argv[i].fixnum_value() ? argv[i].fixnum_value() : flacc) :
          (flacc > argv[i].fixnum_value() ? flacc : argv[i].fixnum_value());
      }
    } else if(argv[i].heap_type_equals(FLONUM)) {
      if(!set) {
        flacc = argv[i].flonum_value();
        set = true;
      } else {
        flacc = min ? (flacc > argv[i].flonum_value() ? argv[i].flonum_value() : flacc) :
          (flacc > argv[i].flonum_value() ? flacc : argv[i].flonum_value());
      }
    } else {
      AR_FN_ASSERT_ARG(state, i, "to be numeric", argv[i].numeric());
    }
  }
  return state.make_flonum(flacc);
}

Value fn_min(State& state, size_t argc, Value* argv, void* v) {
  return fn_minmax(state, argc, argv, true);
}
AR_DEFUN("min", fn_min, 1, 1, true);

Value fn_max(State& state, size_t argc, Value* argv, void* v) {
  return fn_minmax(state, argc, argv, false);
}
AR_DEFUN("max", fn_max, 1, 1, true);

Value fn_sqrt(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "sqrt";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_NUMBER(state, argv, 0);

  if(argv[0].fixnump()) {
    return state.make_flonum(sqrt((double)argv[0].fixnum_value()));
  } else {
    return state.make_flonum(sqrt(argv[0].flonum_value()));
  }
}
AR_DEFUN("sqrt", fn_sqrt, 1, 1);

Value fn_string_to_number(State& state, size_t argc, Value* argv, void* _) {
  static const char* fn_name = "string->number";

  AR_FN_ARGC_BETWEEN(state, argc, 1, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  if(argc == 2) {
    AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  }

  const char* data = argv[0].string_data();

  std::string string(data);
  NumberReader reader(state, string);

  if(argc == 2) {
    if(!reader.set_radix_param((unsigned)argv[1].fixnum_value())) {
      return state.make_exception("read", reader.error_desc);
    }
  }


  Value v = reader.read();
  if(v == C_FALSE) {
    return state.make_exception("read", reader.error_desc);
  }
  return v;
}
AR_DEFUN("string->number", fn_string_to_number, 1, 2);

Value fn_inexact_to_exact(State& state, size_t argc, Value* argv, void* _) {
  static const char* fn_name = "inexact->exact";

  AR_FN_ARGC_EQ(state, argc, 1);
  if(argv[0].fixnump()) return argv[0];
  AR_FN_EXPECT_TYPE(state, argv, 0, FLONUM);

  double flnum = argv[0].flonum_value();
  if((ptrdiff_t) flnum != flnum) {
    return state.eval_error("inexact number cannot be represented exactly");
  } else {
    return Value::make_fixnum((ptrdiff_t) flnum);
  }
}
AR_DEFUN("inexact->exact", fn_inexact_to_exact, 1);

Value fn_exact_to_inexact(State& state, size_t argc, Value* argv, void* _) {
  static const char* fn_name = "exact->inexact";

  AR_FN_ARGC_EQ(state, argc, 1);
  if(argv[0].heap_type_equals(FLONUM)) return argv[0];
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  return state.make_flonum((double)argv[0].fixnum_value());
}
AR_DEFUN("exact->inexact", fn_exact_to_inexact, 1);

///// SRFI 151: Bitwise operations

Value fn_bitwise_and(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "bitwise-and";
  AR_FN_ARGC_GTE(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  ptrdiff_t fx = argv[0].fixnum_value();
  for(size_t i = 1; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    fx &= argv[i].fixnum_value();
  }

  return Value::make_fixnum(fx);
}
AR_DEFUN("bitwise-and", fn_bitwise_and, 2, 2, true);

Value fn_bitwise_or(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "bitwise-or";
  AR_FN_ARGC_GTE(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  ptrdiff_t fx = argv[0].fixnum_value();
  for(size_t i = 1; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    fx |= argv[1].fixnum_value();
  }

  return Value::make_fixnum(fx);
}
AR_DEFUN("bitwise-ior", fn_bitwise_or, 2, 2, true);

Value fn_bitwise_xor(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "bitwise-xor";
  AR_FN_ARGC_GTE(state, argc, 2);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  ptrdiff_t fx = argv[0].fixnum_value();
  for(size_t i = 1; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    fx ^= argv[1].fixnum_value();
  }

  return Value::make_fixnum(fx);
}
AR_DEFUN("bitwise-xor", fn_bitwise_xor, 2, 2, true);

Value fn_bitwise_not(State& state, size_t argc, Value* argv, void* v) {
  static const char* fn_name = "bitwise-not";
  AR_FN_ARGC_EQ(state, argc, 1);
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);

  return Value::make_fixnum(~argv[0].fixnum_value());
}
AR_DEFUN("bitwise-not", fn_bitwise_not, 1);

void State::load_numeric_functions() {
  // Numbers
  numbers.install(*this);
}

}
