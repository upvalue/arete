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

#define OPV(name, cname, operator, sub, nozero) \
  Value name(State& state, size_t argc, Value* argv) { \
    static const char* fn_name = cname; \
    ptrdiff_t fxresult = 0; \
    size_t i = 0; \
    if(argc == 1) { \
      AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].numeric()); \
      if(sub) return argv[0].type() == FLONUM ? state.make_flonum(0 - argv[0].flonum_value()) : Value::make_fixnum(0 - argv[0].fixnum_value()); \
      return argv[0]; \
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

OPV(fn_add, "+", +, false, false)
OPV(fn_sub, "-", -, true, false)
OPV(fn_mul, "*", *, false, false)
OPV(fn_div, "/", /, false, true)

AR_DEFUN("+", fn_add, 1, 1, true);
AR_DEFUN("-", fn_sub, 1, 1, true);
AR_DEFUN("*", fn_mul, 1, 1, true);
AR_DEFUN("/", fn_div, 2, 2, true);

#undef OPV

#define OPV_BOOL(name, cname, operator) \
  Value name(State& state, size_t argc, Value* argv) { \
    static const char* fn_name = cname; \
    AR_FN_ASSERT_ARG(state, 0, "to be numeric", argv[0].numeric()); \
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

Value fn_fx_sub(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx-";

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

Value fn_fx_add(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx+";
  ptrdiff_t result = 0;

  for(size_t i = 0; i != argc; i++) {
    AR_FN_EXPECT_TYPE(state, argv, i, FIXNUM);
    result += argv[i].fixnum_value();
  }

  return Value::make_fixnum(result);
}

AR_DEFUN("fx+", fn_fx_add, 2, 2, true);

Value fn_fx_equals(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx=";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].bits == argv[1].bits);
}
AR_DEFUN("fx=", fn_fx_equals, 2, 2);


Value fn_fx_lt(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx<";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].fixnum_value() < argv[1].fixnum_value());
}
AR_DEFUN("fx<", fn_fx_lt, 2, 2);

Value fn_fx_gt(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx>";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_boolean(argv[0].fixnum_value() > argv[1].fixnum_value());
}
AR_DEFUN("fx>", fn_fx_gt, 2, 2);

Value fn_fx_div(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "fx/";

  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  
  if(argv[1].fixnum_value() == 0) return state.type_error("divide by zero"); \
  
  return Value::make_fixnum(argv[0].fixnum_value() / argv[1].fixnum_value());
}
AR_DEFUN("fx/", fn_fx_div, 2, 2);

Value fn_floor(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "floor";

  if(argv[0].fixnump()) return argv[0];

  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].heap_type_equals(FLONUM));

  return state.make_flonum(floor(argv[0].flonum_value()));
}
AR_DEFUN("floor", fn_floor, 1);

Value fn_ceiling(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "ceiling";

  if(argv[0].fixnump()) return argv[0];

  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].heap_type_equals(FLONUM));
  return state.make_flonum(ceil(argv[0].flonum_value()));
}
AR_DEFUN("ceiling", fn_ceiling, 1);

Value fn_modulo(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "modulo";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  return Value::make_fixnum(argv[0].fixnum_value() % argv[1].fixnum_value());
}
AR_DEFUN("modulo", fn_modulo, 2);

Value fn_quotient(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "quotient";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  ldiv_t div = ldiv(argv[0].fixnum_value(), argv[1].fixnum_value());
  return Value::make_fixnum(div.quot);
}
AR_DEFUN("quotient", fn_quotient, 2);

Value fn_remainder(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "remainder";
  AR_FN_EXPECT_TYPE(state, argv, 0, FIXNUM);
  AR_FN_EXPECT_TYPE(state, argv, 1, FIXNUM);
  ldiv_t div = ldiv(argv[0].fixnum_value(), argv[1].fixnum_value());
  return Value::make_fixnum(div.rem);
}
AR_DEFUN("remainder", fn_remainder, 2);

Value fn_number_to_string(State& state, size_t argc, Value* argv) {
  static const char* fn_name;
  AR_FN_ASSERT_ARG(state, 0, "to be a number", argv[0].numeric());
  std::ostringstream os;
  os << argv[0];
  return state.make_string(os.str());
}
AR_DEFUN("number->string", fn_number_to_string, 1);

void State::load_numeric_functions() {
  // Numbers
  numbers.install(*this);
}

}
