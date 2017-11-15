// eval2.cpp - interpreter with tail-call optimization and let

// Design of the interpreter

// We use a stack allocated on the heap

// In order to implement TCO, we do everything in one function; when evaluating the tail-expression
// of a "body" (a function body or begin expression) we pop the stack and use a goto

#include "arete.hpp"

namespace arete {

struct State::EvalFrame {
  Value env;
  Value fn_name;
};

// Push function and source information onto the stack trace
#define EVAL2_TRACE(exp) \
  if((exp).type() == PAIR && (exp).pair_has_source()) { \
    std::ostringstream os; \
    os << source_info((exp).pair_src(), (frame.fn_name), true); \
    stack_trace.push_back(os.str()); \
  }
  
// Check if an error has occurred; if it has, trace it and return it
#define EVAL2_CHECK(exp, src, fn_name) \
  if((exp).is_active_exception()) { \
    EVAL_TRACE((src)); \
    return (exp); \
  }

static State::Global get_form(State& state, Value sym) {
  if(sym == state.globals[State::S_BEGIN]) return State::S_BEGIN;

  return (State::Global)-1;
}

Value State::eval2_body(EvalFrame& frame, Value body) {
  Value exp, cell;
tail_call:
  bool tail = false;

  AR_FRAME(this, exp, cell, body, frame.env, frame.fn_name);

  while(body.heap_type_equals(PAIR)) {
    cell = exp;
    exp = body.car();
    body = body.cdr();

    tail = body == C_NIL;

    switch(exp.type()) {
      case SYMBOL: {
        Value res = env_lookup(frame.env, exp);

        if(res == C_UNDEFINED) {
          std::ostringstream os;
          os << "reference to undefined variable " << exp;
          // EVAL_TRACE(car, fn_name);
          Value ret = eval_error(os.str(), exp);
          return ret;
        } else if(res == C_SYNTAX) {
          std::stringstream os;
          os << "attempt to use syntax " << exp << " as value";
          if(exp == globals[S_DEFINE_SYNTAX]) {
            // if this happened, it's probably because the macroexpander has not been loaded
            os << " (did you load the expander?)";
          }
          EVAL2_TRACE(cell);
          return eval_error(os.str(), exp);
        }

        return res;
      }

      case PAIR: {
        size_t length = exp.list_length();

        if(length == 0) {
          EVAL2_TRACE(cell);
          return eval_error("non-list in source code", exp);
        }

        Value kar = exp.car(), res, tmp;

        if(kar.heap_type_equals(RENAME))
          res = env_lookup(kar.rename_env(), kar.rename_expr());

        // Check for syntactic values
        if((kar.heap_type_equals(SYMBOL) && kar.symbol_value() == C_SYNTAX) ||
          (kar.heap_type_equals(RENAME) && res == C_SYNTAX)) {

          // The expander will rename special forms that it introduces into source code with an
          // env of #f; we unwrap those here

          if(kar.heap_type_equals(RENAME) && res == C_SYNTAX) {
            kar = kar.rename_expr();
          }

          switch(get_form(*this, kar)) {
            case S_BEGIN:
              if(tail) {
                body = exp.cdr();
                goto tail_call;
              } else {
                exp = eval2_body(frame, exp.cdr());
              }
              break;
            default:
              break;
          }
        } 
      }

      case RENAME:
        break;

      default: continue;
    }

    // Check for special forms

    // Evaluate function applications
  }

  return exp;
}

Value State::eval2_body(Value exp) {
  EvalFrame frame;
  frame.env = C_FALSE;
  frame.fn_name = C_FALSE;
  return eval2_body(frame, exp);
}


} // namespace arete