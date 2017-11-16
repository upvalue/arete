// eval2.cpp - interpreter with tail-call optimization 

#include "arete.hpp"

namespace arete {

static bool tco_enabled = true;

struct State::EvalFrame {
  EvalFrame(): tco_lost(0) {}

  Value env;
  Value fn_name;
  size_t tco_lost;
};

// Push function and source information onto the stack trace
#define EVAL2_TRACE_FRAME(frame, src) \
  if((src).heap_type_equals(PAIR) && (src).pair_has_source()) { \
    std::ostringstream os; \
    os << source_info((src).pair_src(), (frame.fn_name), true); \
    if(frame.tco_lost) os << std::endl <<  "- " << frame.tco_lost << " frames lost due to tail call optimization"; \
    stack_trace.push_back(os.str()); \
  }

#define EVAL2_TRACE(src) EVAL2_TRACE_FRAME(frame, src)
  
// Check if an error has occurred; if it has, trace it and return it
#define EVAL2_CHECK_FRAME(frame, exp, src) \
  if((exp).is_active_exception()) { \
    EVAL2_TRACE_FRAME(frame, (src)); \
    return (exp); \
  }

#define EVAL2_CHECK(exp, src) EVAL2_CHECK_FRAME(frame, exp, src)

#define EVAL2_BODY_CHECK(exp, src) \
  if((exp).is_active_exception()) { \
    EVAL2_TRACE_FRAME(frame, (src)); \
    continue; \
  }

static State::Global get_form(State& state, Value sym) {

#define GET_FORM(n) else if(sym == state.globals[(n)]) return (n)
  if(0) (void)0;
  GET_FORM(State::S_DEFINE);
  GET_FORM(State::S_SET);
  GET_FORM(State::S_LAMBDA);
  GET_FORM(State::S_QUOTE);
  GET_FORM(State::S_BEGIN);
  GET_FORM(State::S_IF);
  GET_FORM(State::S_AND);
  GET_FORM(State::S_OR);
  GET_FORM(State::S_COND);

#undef GET_FORM

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

  Value tmp;

  AR_FRAME(this, frame.env, frame.fn_name, exp, tmp);

  switch(type) {
    case S_QUOTE: {
      if(length != 2)
        return eval_error("quote only takes one argument", exp);
      return exp.cadr();
    }
    case S_DEFINE: {
      if(length < 3)
        return eval_error("define expects at least two arguments", exp);

      Value name = exp.cadr(), body = exp.caddr(), tmp;
      AR_FRAME(this, name, body, tmp);
    define_lambda:
      // Special case: define lambda, build lambda in place
      if(name.heap_type_equals(PAIR)) {
        Value args = name.cdr(), tmp = name.car();

        name = tmp;

        if(!tmp.heap_type_equals(SYMBOL)) {
          return eval_error("define lambda name must be a symbol", name);
        }

        SourceLocation loc;

        body = make_src_pair(exp.cddr().car(), exp.cddr().cdr(), exp);

        tmp = make_src_pair(args, body, exp);
        tmp = make_src_pair(globals[S_LAMBDA], tmp, exp);

        body = tmp;

        goto define_lambda;
      }

      // std::cout << name << std::endl;
      // std::cout << body << std::endl;

      if(!name.heap_type_equals(SYMBOL))
        return eval_error("first argument to define must be a symbol", exp.cdr());

      if(env_defined(frame.env, name)) {
        warn() << name << " shadows existing definition";
      }

      tmp = eval2_body(frame, body, true);

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
    case S_SET: {
      if(length != 3) 
        return eval_error("set! expects exactly three arguments", exp);

      Value name = exp.cadr(), body = exp.caddr(), env_search = frame.env, rename_key;
      AR_FRAME(this, name, body, env_search, rename_key);

      if(!name.heap_type_equals(SYMBOL)) {
        return eval_error("first argument to set! must be a symbol", exp.cdr());
      }

      bool found;

      tmp = env_lookup_impl(env_search, name, rename_key, found);

      // TODO check immutable
      if(tmp == C_UNDEFINED) {
        std::ostringstream os;
        os << "attempt to set! undefined variable " << name;
        return eval_error(os.str(), exp.cdr());
      }

      tmp = eval2_body(frame, body, true);
      EVAL2_CHECK(tmp, exp.cddr());
      env_set(frame.env, name, tmp);

      return C_UNSPECIFIED;
    }
    case S_AND: {
    case S_OR: 
      bool is_or = type == S_OR;
      exp = exp.cdr();

      // Short case: (and) => #t, (or) => #f
      if(exp == C_NIL) return Value::make_boolean(!is_or);

      while(exp.heap_type_equals(PAIR)) {
        tmp = eval2_body(frame, exp.car(), true);
        EVAL2_CHECK(tmp, exp);

        if((is_or && tmp != C_FALSE) || (!is_or && tmp == C_FALSE)) {
          return tmp;
        }
        
        exp = exp.cdr();
      }

      return tmp;
      break;

    }
    case S_LAMBDA: {
      Value fn_env, args, saved_fn;
      AR_FRAME(this, fn_env, args, saved_fn);
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
        
        args = temps_to_list();

        fn = saved_fn.as_unsafe<Function>();
        fn->arguments = args;
      }

      //std::cout << "fn body: " << fn->body << std::endl;
      //std::cout << "fn args: " << fn->arguments << std::endl;
      //std::cout << "fn rest: " << fn->rest_arguments << std::endl;

      return saved_fn;
    }
  }
  return C_UNSPECIFIED;
}

/** Generic arity check */
static Value eval_check_arity(State& state, Value fn, Value exp,
  size_t argc, size_t min_arity, size_t max_arity, bool var_arity) {

  AR_ASSERT(state.gc.live(fn));
  AR_ASSERT(state.gc.live(exp));

  if(argc < min_arity) {
    std::ostringstream os; 
    os << "function " << fn << " expected at least " << min_arity << " arguments but got only " 
      << argc;
    return state.eval_error(os.str(), exp);
  } else if(argc > max_arity && !var_arity) {
    std::ostringstream os;
    os << "function " << fn << " expected at most " << max_arity << " arguments but got only "
      << argc;
    return state.eval_error(os.str(), exp);
  }

  return C_FALSE;
}

Value State::eval2_body(EvalFrame frame, Value body, bool single) {
  Value exp, cell, tmp;

tail_call:
  bool tail = false;

  AR_ASSERT(single || body.heap_type_equals(PAIR) || body == C_NIL);

  AR_FRAME(this, frame.env, frame.fn_name, body, exp, cell, tmp);

  while(single || body.heap_type_equals(PAIR)) {
    if(single) { 
      exp = cell = body;
      single = false;
      body = C_FALSE;
    } else {
      cell = body;
      exp = body.car();
      body = body.cdr();
    }
    restart_exp:

    tail = tco_enabled && body == C_NIL;

    switch(exp.type()) {
      case SYMBOL: {
        Value res = env_lookup(frame.env, exp);

        if(res == C_UNDEFINED) {
          std::ostringstream os;
          os << "reference to undefined variable " << exp;
          // EVAL_TRACE(car, fn_name);
          Value ret = eval_error(os.str(), cell);
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

      case RENAME: {
        // First we look it up in the env 
        // In case a renamed variable has been introduced as a binding
        // e.g (lambda ((rename 'var )) (rename 'var))
        Value chk = env_lookup(frame.env, exp);

        if(exp.rename_env() != C_FALSE) {
          std::cerr << "interpreter encountered a non-toplevel rename, but this should be gensymed " << exp << std::endl;
          std::cerr << exp.rename_env() << std::endl;
        }

        if(chk != C_UNDEFINED) {
          exp = chk;
          continue;
        }

        // Then we look it up in the rename env
        // e.g. ((r 'lambda) () #t)
        exp = env_lookup(exp.rename_env(), exp.rename_expr());
        continue;
      }

      case PAIR: {
        size_t length = exp.list_length();

        if(length == 0) {
          EVAL2_TRACE(cell);
          return eval_error("dotted list in source code", exp);
        }

        Value kar = exp.car(), res;
        Type kar_type = kar.type();

        if(kar_type == RENAME)
          res = env_lookup(kar.rename_env(), kar.rename_expr());

        // Check for syntactic values
        if((kar_type == SYMBOL && kar.symbol_value() == C_SYNTAX) ||
          (kar_type == RENAME && res == C_SYNTAX)) {

          // The expander will rename special forms that it introduces into source code with an
          // env of #f; we unwrap those here

          if(kar_type == RENAME && res == C_SYNTAX) {
            kar = kar.rename_expr();
          }

          unsigned form = get_form(*this, kar);
          switch(form) {
            case S_IF: {
              if(length < 2) {
                return eval_error("if must have at least two arguments (condition and then branch)",
                  exp);
              }
              
              tmp = eval2_body(frame, exp.cadr(), true);
              EVAL2_CHECK(tmp, exp);

              if(tmp != C_FALSE) {
                exp = exp.list_ref(2);
              } else if(length == 3) {
                // 1-arm if
                exp = C_UNSPECIFIED;
              } else {
                exp = exp.list_ref(3);
              }

              if(tail) {
                single = true;
                body = exp;
                continue;
              } else {
                
                goto restart_exp;
              }

              continue;
            }
            case S_COND: {
              Value pred, then, lst = exp.cdr();
              AR_FRAME(this, pred, then, lst);
              exp = C_UNSPECIFIED;
              while(lst.heap_type_equals(PAIR)) {
                if(lst.car().list_length() < 2) {
                  return eval_error("cond clause must have at least two elements (condition and body)", lst);
                }
                pred = lst.caar();
                then = lst.cdar();

                if(pred == globals[State::S_ELSE]) {
                  // Check for else clause
                  tmp = C_TRUE;
                } else {
                  tmp = eval2_body(frame, pred, true);
                  EVAL2_CHECK(tmp, lst);
                }

                if(tmp != C_FALSE) {
                  if(tail) {
                    body = then;
                    goto tail_call;
                  } else {
                    exp = eval2_body(frame, then);
                    break;
                  }
                }

                lst = lst.cdr();
              }
              continue;
            }
            case S_BEGIN:
              if(tail) {
                body = exp.cdr();
                goto tail_call;
              } else {
                exp = eval2_body(frame, exp.cdr());
              }
              break;
            default:
              exp = eval2_form(frame, exp, form);
              break;
          }
        } else {
          // This is a normal function application 
          tmp = eval2_body(frame, exp.car(), true);
          EVAL2_CHECK(tmp, exp);
          kar_type = tmp.type();

          // Ok. With CFUNCTION and VMFUNCTION, we have no hope of 
          // tail calls so we can do them elsewhere
          // With FUNCTION, we'll have to evaluate the arguments in the given env.
          // If this is a tail call, we'll then modify the EvalFrame and goto. If not, we create
          // a new one. easy peasy.
          switch(kar_type) {
          case CFUNCTION: {
            Value fn = tmp, fn_args, tmp, args = exp.cdr();
            AR_FRAME(this, fn, fn_args, tmp, args);

            size_t argc = args.list_length();

            tmp = eval_check_arity(*this, fn, exp, argc,
              fn.c_function_min_arity(), fn.c_function_max_arity(), fn.c_function_variable_arity());

            EVAL2_CHECK(tmp, exp);

            fn_args = make_vector_storage(argc);

            while(args.heap_type_equals(PAIR)) {
              tmp = eval2_body(frame, args.car(), true);
              EVAL2_CHECK(tmp, exp);
              vector_storage_append(fn_args, tmp);
              args = args.cdr();
            }

            exp = fn.c_function_apply(*this, argc, fn_args.vector_storage_data());
            EVAL2_CHECK(tmp, exp);

            continue;
          }
          case FUNCTION: {
            EvalFrame frame2;
            Value fn = tmp, args = exp.cdr(), fn_args, rest_args_name, new_body;
            frame2.fn_name = tmp.function_name();
            AR_FRAME(this, frame2.fn_name, frame2.env, fn, args, fn_args, rest_args_name, new_body);
            
            fn_args = fn.function_arguments();
            size_t argc = args.list_length();
            size_t arity = fn_args.list_length();
            rest_args_name = fn.function_rest_arguments();

            //std::cout << "tmp: " << tmp << std::endl;

            tmp = eval_check_arity(*this, fn, exp, argc, arity, arity, rest_args_name != C_FALSE);
            EVAL2_CHECK(tmp, exp);

            frame2.env = make_env(fn.function_parent_env(), argc + 1);

            // Evaluate arguemnts, left to right
            while(args.heap_type_equals(PAIR) && fn_args.heap_type_equals(PAIR)) {
              tmp = eval2_body(frame, args.car(), true);
              EVAL2_CHECK(tmp, args);
              vector_append(frame2.env, fn_args.car());
              vector_append(frame2.env, tmp);
              args = args.cdr();
              fn_args = fn_args.cdr();
            }

            if(rest_args_name != C_FALSE) {
              temps.clear();
              while(args.heap_type_equals(PAIR)) {
                tmp = eval2_body(frame, args.car(), true);
                EVAL2_CHECK(tmp, args);
                temps.push_back(tmp);
                args = args.cdr();
              }
              vector_append(frame2.env, rest_args_name);
              vector_append(frame2.env, temps_to_list());
            }

            new_body = fn.function_body();

            if(tail) {
              // tail call
              frame2.tco_lost = frame.tco_lost + 1;
              frame = frame2;
              body = new_body;
              goto tail_call;
            } else {
              // Have to loop here to get accurate source code information out of this
              while(new_body.heap_type_equals(PAIR)) {
                tmp = eval2_body(frame2, new_body.car(), true);
                EVAL2_CHECK_FRAME(frame2, tmp, new_body.car().heap_type_equals(PAIR) ? new_body.car() : new_body);
                new_body = new_body.cdr();
              }

              //tmp = eval2_body(frame2, new_body);
              EVAL2_CHECK_FRAME(frame2, tmp, new_body);
              exp = tmp;
              continue;
            }

            AR_ASSERT(!"should never reach this point");
            return C_FALSE;
          }
          case VMFUNCTION:
            break;
          default: {
            std::ostringstream os;
            os << "attempt to apply non-applicable value of type " << (Type) kar_type << ": " <<
              kar;
            return eval_error(os.str(), exp);
          }
        }
        break;
      }
    } // case PAIR

    default: break;
    }
  }

  return exp;
}

Value State::eval2_exp(Value exp) {
  EvalFrame frame;
  return eval2_body(frame, exp, true);
}

Value State::eval2_list(Value lst) {
  EvalFrame frame;
  return eval2_body(frame, lst);
}

} // namespace arete
