// eval2.cpp - interpreter with tail-call optimization and let

// Design of the interpreter

// We use a stack allocated on the heap

// In order to implement TCO, we do everything in one function; when evaluating the tail-expression
// of a "body" (a function body or begin expression) we pop the stack and use a goto

#include "arete.hpp"

namespace arete {

static bool tco_enabled = true;

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
#define EVAL2_CHECK(exp, src) \
  if((exp).is_active_exception()) { \
    EVAL2_TRACE((src)); \
    return (exp); \
  }

static State::Global get_form(State& state, Value sym) {
  if(sym == state.globals[State::S_BEGIN]) return State::S_BEGIN;
  else if(sym == state.globals[State::S_DEFINE]) return State::S_DEFINE;
  else if(sym == state.globals[State::S_LAMBDA]) return State::S_LAMBDA;
  else if(sym == state.globals[State::S_QUOTE]) return State::S_QUOTE;

  return (State::Global)-1;
}

 std::ostream& State::warn(Value src) {
   std::cerr << "arete: Warning: ";
   if(src.heap_type_equals(PAIR) && src.pair_has_source()) {
     std::cerr << source_info(src.pair_src());
   }
   return std::cerr;
 }

Value State::temps_to_list() {
  if(temps.size() == 0) return C_NIL;
  Value ret = C_NIL;
  AR_FRAME(this, ret);
  for(size_t i = temps.size(); i != 0; i--) {
    ret = make_pair(temps[i-1], ret);
  }
  return ret;
}

// Handler for special forms which do not involve tail call optimization
Value State::eval2_form(EvalFrame& frame, Value exp, unsigned type) {
  size_t length = exp.list_length();

  switch(type) {
    case S_DEFINE: {
      if(length != 3)
        return eval_error("define expects exactly three arguments", exp);

      Value name = exp.cadr(), body = exp.caddr(), tmp;
      AR_FRAME(this, exp, name, body, tmp);

      if(!name.heap_type_equals(SYMBOL))
        return eval_error("first argument to define must be a symbol", exp.cdr());

      if(env_defined(frame.env, name)) {
        warn() << name << " shadows existing definition";
      }
      
      tmp = eval2_body(frame, exp.cddr());

      EVAL2_CHECK(tmp, exp);

      // Add to the environment
      if(frame.env == C_FALSE) {
        name.set_symbol_value(tmp);
      } else {
        vector_append(frame.env, name);
        vector_append(frame.env, tmp);
      }

      // Handle a named function
      if(tmp.heap_type_equals(FUNCTION) && tmp.function_name() == C_FALSE) {
        tmp.as_unsafe<Function>()->name = name;
      }

      break;
    }
    case S_LAMBDA: {
      Value fn_env, args, args_head, args_tail, saved_fn;
      AR_FRAME(this, exp, frame.env, frame.fn_name, fn_env, args, args_head, args_tail, saved_fn);
      Function* fn = static_cast<Function*>(gc.allocate(FUNCTION, sizeof(Function)));
      saved_fn = fn;

      fn->parent_env = frame.env;

      if(length < 3)
        return eval_error("lambda must be a list with at least three arguments");

      args = exp.cadr();
      fn->body = exp.cddr();

      // (lambda () ...)
      if(args == C_NIL) {
        fn->arguments = C_NIL;
      } else if(args.identifierp()) {
        // Second case: (lambda rest ...)
        fn->arguments = C_NIL;
        fn->rest_arguments = args;
      } else {
        // Third case: normal arguments list
        // Build a copy of the list and extract the REST arguments
        Value argi = args;

        temps.clear();
        while(argi.heap_type_equals(PAIR)) {
          Value arg = argi.car();

          if(!arg.identifierp()) {
            EVAL2_TRACE(argi);
            return eval_error("lambda arguments must all be symbols", argi);
          }

          temps.push_back(argi.car());          
          argi = argi.cdr();
        }

        if(argi != C_NIL) {
          if(!argi.identifierp()) {
            EVAL2_TRACE(argi);
            return eval_error("lambda arguments must all be symbols", argi);
          }
          fn->rest_arguments = argi;
        }
        
        fn->arguments = temps_to_list();

        fn = saved_fn.as_unsafe<Function>();
      }


      /*std::cout << "fn body: " << fn->body << std::endl;
      std::cout << "fn args: " << fn->arguments << std::endl;
      std::cout << "fn rest: " << fn->rest_arguments << std::endl;*/

      return saved_fn;
    }
  }
  return C_UNSPECIFIED;
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

      case RENAME:
        break;

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
              if(tail && tco_enabled) {
                body = exp.cdr();
                goto tail_call;
              } else {
                exp = eval2_body(frame, exp.cdr());
              }
              break;
            default:
              exp = eval2_form(frame, exp, get_form(*this, kar));
              break;
          }
        } 

        break;
      }

      default: break;
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