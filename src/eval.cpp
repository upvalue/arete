// eval.cpp - Interpreter & related functionality

// Braindead interpreter

// It implements TCO by putting most of the interpreter in a giant while loop (eval_body) and 
// using goto's and continues when tail calls occur. 

#include "arete.hpp"

// Push function and source information onto the stack trace
#define EVAL_TRACE_FRAME(frame, src) \
  if((src).heap_type_equals(PAIR) && (src).pair_has_source()) { \
    std::ostringstream os; \
    os << source_info((src).pair_src(), (frame.fn_name), true); \
    if(frame.tco_lost) os << std::endl <<  "- " << frame.tco_lost << " frames lost due to tail call optimization"; \
    stack_trace.push_back(os.str()); \
  }

#define EVAL_TRACE(src) EVAL_TRACE_FRAME(frame, src)
  
// Check if an error has occurred; if it has, trace it and return it
#define EVAL_CHECK_FRAME(frame, exp, src) \
  if((exp).is_active_exception()) { \
    EVAL_TRACE_FRAME(frame, (src)); \
    return (exp); \
  }

#define EVAL_CHECK(exp, src) EVAL_CHECK_FRAME(frame, exp, src)

namespace arete {

Value State::make_env(Value parent, size_t size) {
  Value vec;
  AR_FRAME(this, vec, parent);
  vec = make_vector(3 + size);
  vector_append(vec, parent);
  vector_append(vec, C_FALSE);
  return vec;
}

void State::env_set(Value env, Value name, Value val) {
  AR_FRAME(this, env, name, val);
  AR_TYPE_ASSERT(name.identifierp());
  // AR_TYPE_ASSERT(name.type() == SYMBOL);
  // Toplevel set
  while(env != C_FALSE) {
    for(size_t i = env.vector_length() - 1; i > VECTOR_ENV_FIELDS; i -= 2) {
      if(identifier_equal(env.vector_ref(i-1), name)) {
        env.vector_set(i, val);
        return;
      }
    }
    env = env.vector_ref(0);
  }
  if(name.type() == RENAME) {
    name = name.rename_expr();
  }
  name.as<Symbol>()->value = val;
}

bool State::env_defined(Value env, Value name) {
  if(name.type() != SYMBOL) return false;

  if(env.type() == VECTOR) {
    for(size_t i = env.vector_length() - 1; i > VECTOR_ENV_FIELDS; i -= 2) {
      if(identifier_equal(env.vector_ref(i-1), name)) {
        return true;
      }
    }
  } else if(env == C_FALSE) {
    return name.symbol_value() != C_UNDEFINED;
  }
  return false;
}

Value State::env_lookup_impl(Value& env, Value name, bool& reached_toplevel) {
  reached_toplevel = false;

  // First we search through vectors with identifier_equal, which only
  // returns true if symbols are the same or if renames have the same env and expr
  while(env.type() == VECTOR) {
    for(size_t i = env.vector_length() - 1; i > VECTOR_ENV_FIELDS; i -= 2) {
      // Here we check for function-level renames in the environment.
      // This is necessary because the expansion pass will replace them
      // with gensyms before returning the full expression
      if(identifier_equal(env.vector_ref(i-1), name)) {
        return env.vector_ref(i);
      }
    }
    env = env.vector_ref(0); // check parent environment
  }

  // If we've reached here, this is a module or toplevel rename and thus we can resolve it
  // the same as a symbol
  if(name.type() == RENAME) {
    env = name.rename_env();
    name = name.rename_expr();

    AR_ASSERT("rename non-symbol" && name.type() == SYMBOL);
  }

  reached_toplevel = true;

  if(name.type() != SYMBOL) {
    std::cout << name << std::endl;
  }
  AR_ASSERT("reached toplevel on rename" && name.type() == SYMBOL);
  return name.as<Symbol>()->value;
}

Value State::env_lookup(Value env, Value name) {
  bool found;
  return env_lookup_impl(env, name, found);
}

/** Return an eval error with source code information */
Value State::eval_error(const std::string& msg, Value exp) {
  std::ostringstream os;

  if(exp.type() == PAIR && exp.pair_has_source()) {
    os << source_info(exp.pair_src()) << ": ";
  }

  os << msg;
  return make_exception(globals[State::S_EVAL_ERROR], os.str(), exp);
}

struct State::EvalFrame {
  EvalFrame(): tco_lost(0) {}

  Value env;
  Value fn_name;
  /** count of frames lost due to tail call optimization, for stack traces */
  size_t tco_lost;
};

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

Value State::temps_to_list(size_t limit) {
  if(temps.size() == 0) return C_NIL;
  Value ret = C_NIL;
  AR_FRAME(this, ret);
  for(size_t i = temps.size(); i != limit; i--) {
    ret = make_pair(temps[i-1], ret);
  }
  return ret;
}

// Handler for special forms which do not involve tail call optimization
Value State::eval_form(EvalFrame frame, Value exp, unsigned type) {
  size_t length = exp.list_length();

  Value tmp;

  AR_FRAME(this, frame.env, frame.fn_name, exp, tmp);

  switch(type) {
    case S_QUOTE: {
      //if(length != 2)
      //  return eval_error("quote only takes one argument", exp);
      return exp.cadr();
    }
    case S_DEFINE: {
      if(length < 3)
        return eval_error("define expects at least two arguments", exp);

      Value name = exp.cadr(), body = exp.caddr(), args;
      AR_FRAME(this, name, body, args);
    define_lambda:
      // Special case: define lambda, build lambda in place
      if(name.heap_type_equals(PAIR)) {
        args = name.cdr();
        tmp = name.car();

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

      tmp = eval_body(frame, body, true);

      EVAL_CHECK(tmp, exp);

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

      Value name = exp.cadr(), body = exp.caddr(), env_search = frame.env;
      AR_FRAME(this, name, body, env_search);

      if(!name.heap_type_equals(SYMBOL)) {
        return eval_error("first argument to set! must be a symbol", exp.cdr());
      }

      bool found;

      tmp = env_lookup_impl(env_search, name,  found);

      // TODO check immutable
      if(tmp == C_UNDEFINED) {
        std::ostringstream os;
        os << "attempt to set! undefined variable " << name;
        return eval_error(os.str(), exp.cdr());
      }

      tmp = eval_body(frame, body, true);
      EVAL_CHECK(tmp, exp.cddr());
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
        tmp = eval_body(frame, exp.car(), true);
        EVAL_CHECK(tmp, exp);

        if((is_or && tmp != C_FALSE) || (!is_or && tmp == C_FALSE)) {
          return tmp;
        }
        
        exp = exp.cdr();
      }

      return tmp;
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
            EVAL_TRACE(argi);
            return eval_error("lambda arguments must all be symbols", argi);
          }

          temps.push_back(argi.car());          
          argi = argi.cdr();
        }

        if(argi != C_NIL) {
          if(!argi.identifierp()) {
            EVAL_TRACE(argi);
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
    os << "function " << fn << " expected at most " << max_arity << " arguments but got "
      << argc;
    return state.eval_error(os.str(), exp);
  }

  return C_FALSE;
}

// Apply a function. Not used by the interpreter itself but used for generic apply and calls from
// C/VM functions
Value State::eval_apply_function(Value fn, size_t argc, Value* argv) {
  AR_TYPE_ASSERT(fn.heap_type_equals(FUNCTION));
  EvalFrame frame;
  Value fn_args, fn_rest_args, tmp, new_env;

  frame.fn_name = fn.function_name();
  fn_args = fn.function_arguments();
  fn_rest_args = fn.function_rest_arguments();

  AR_FRAME(this, frame.fn_name, frame.env, fn, fn_args, fn_rest_args, tmp, new_env);
  // Check argc against args length
  size_t arity = fn_args.list_length();

  tmp = eval_check_arity(*this, fn, C_FALSE, argc, arity, arity, fn_rest_args != C_FALSE);
  if(tmp.is_active_exception()) return tmp;

  temps.clear();

  for(size_t i = 0; i != argc; i++) {
    temps.push_back(argv[i]);
  }

  size_t actual_args = arity;

  // Handle rest arguments
  if(argc > arity) {
    tmp = temps_to_list(arity);
    temps[arity] = tmp;
    actual_args++;
  } else {
    temps.push_back(C_NIL);
  }

  new_env = make_env(fn.function_parent_env(), actual_args);

  size_t i = 0;
  while(fn_args.heap_type_equals(PAIR)) {
    vector_append(new_env, fn_args.car());
    vector_append(new_env, temps[i++]);
    fn_args = fn_args.cdr();
  }

  if(fn_rest_args != C_FALSE) {
    vector_append(new_env, fn_rest_args);
    vector_append(new_env, temps[i]);
  }

  frame.env = new_env;

  tmp = eval_body(frame, fn.function_body());

  return tmp;

}

Value State::eval_body(EvalFrame frame, Value body, bool single) {
  if(get_global_value(G_FORBID_INTERPRETER) == C_TRUE) {
    std::ostringstream os;
    os << "interpreter has been disabled, but FUNCTION " << frame.fn_name << " was called";

    return eval_error(os.str());
  }
  Value exp, cell, tmp;

tail_call:
  bool tail = false, tco_enabled = get_global_value(G_TCO_ENABLED) != C_FALSE;

  AR_ASSERT(single || body.heap_type_equals(PAIR) || body == C_NIL);

  AR_FRAME(this, frame.env, frame.fn_name, body, exp, cell, tmp);

  while(single || body.heap_type_equals(PAIR)) {
    EVAL_CHECK(exp, exp);
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

    tail = tco_enabled && (body == C_NIL || body == C_FALSE);

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
          EVAL_TRACE(cell);
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
          EVAL_TRACE(cell);
          std::cout << exp << std::endl;
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
              
              tmp = eval_body(frame, exp.cadr(), true);
              EVAL_CHECK(tmp, exp);

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
                  tmp = eval_body(frame, pred, true);
                  EVAL_CHECK(tmp, lst);
                }

                if(tmp != C_FALSE) {
                  if(tail) {
                    body = then;
                    goto tail_call;
                  } else {
                    exp = eval_body(frame, then);
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
                exp = eval_body(frame, exp.cdr());
              }
              break;
            // LET EXPRESSION. 
            // (let loop ((asdf #t)) a)
            // Create a function with the body and arguments list.

            // Simpler case
            // (let ((asdf #t)) asdf)
            default:
              tmp = eval_form(frame, exp, form);
              EVAL_CHECK(tmp, exp);
              exp = tmp;
              break;
          }
        } else {
          // This is a normal function application 
          tmp = eval_body(frame, exp.car(), true);
          EVAL_CHECK(tmp, exp);
          kar_type = tmp.type();

          // Ok. With CFUNCTION and VMFUNCTION, we have no hope of 
          // tail calls so we can do them elsewhere
          // With FUNCTION, we'll have to evaluate the arguments in the given env.
          // If this is a tail call, we'll then modify the EvalFrame and goto. If not, we create
          // a new one. easy peasy.
          switch(kar_type) {
          case CFUNCTION: {
            Value fn = tmp, fn_args, args = exp.cdr();
            AR_FRAME(this, fn, fn_args,  args);

            size_t argc = args.list_length();

            tmp = eval_check_arity(*this, fn, exp, argc,
              fn.c_function_min_arity(), fn.c_function_max_arity(), fn.c_function_variable_arity());
            
            EVAL_CHECK(tmp, exp);

            //std::cout << "frame.env" << frame.env << std::endl;

            fn_args = make_vector_storage(argc);

            while(args.heap_type_equals(PAIR)) {
              tmp = eval_body(frame, args.car(), true);
              EVAL_CHECK(tmp, exp);
              vector_storage_append(fn_args, tmp);
              args = args.cdr();
            }

            tmp = fn.c_function_apply(*this, argc, fn_args.vector_storage_data());
            EVAL_CHECK(tmp, exp);
            exp = tmp;

            continue;
          }
          case FUNCTION: {
            EvalFrame frame2;
            Value fn = tmp, args = exp.cdr(), fn_args, rest_args_name, new_body;
            ListAppender rest;
            frame2.fn_name = tmp.function_name();
            AR_FRAME(this, frame2.fn_name, frame2.env, fn, args, fn_args, rest_args_name, new_body,
              rest.head, rest.tail);
            
            fn_args = fn.function_arguments();
            size_t argc = args.list_length();
            size_t arity = fn_args.list_length();
            rest_args_name = fn.function_rest_arguments();

            //std::cout << "tmp: " << tmp << std::endl;

            tmp = eval_check_arity(*this, fn, exp, argc, arity, arity, rest_args_name != C_FALSE);
            EVAL_CHECK(tmp, exp);

            frame2.env = make_env(fn.function_parent_env(), argc + 1);

            // Evaluate arguemnts, left to right
            while(args.heap_type_equals(PAIR) && fn_args.heap_type_equals(PAIR)) {
              tmp = eval_body(frame, args.car(), true);
              EVAL_CHECK(tmp, args);
              vector_append(frame2.env, fn_args.car());
              vector_append(frame2.env, tmp);
              args = args.cdr();
              fn_args = fn_args.cdr();
            }

            if(rest_args_name != C_FALSE) {
              while(args.heap_type_equals(PAIR)) {
                tmp = eval_body(frame, args.car(), true);
                EVAL_CHECK(tmp, args);

                rest.append(*this, tmp);
                args = args.cdr();
              }

              vector_append(frame2.env, rest_args_name);
              vector_append(frame2.env, rest.head);
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
                tmp = eval_body(frame2, new_body.car(), true);
                EVAL_CHECK_FRAME(frame2, tmp,
                  new_body.car().heap_type_equals(PAIR) ? new_body.car() : new_body);
                new_body = new_body.cdr();
              }

              EVAL_CHECK(tmp, exp);

              exp = tmp;
              continue;
            }

            AR_ASSERT(!"should never reach this point");
            return C_FALSE;
          }
          //case CLOSURE:
          case VMFUNCTION: {
            Value fn = tmp, args = exp.cdr(), argv, varargs_begin, varargs_cur = C_NIL, closure;
            AR_FRAME(this, fn, args, argv, varargs_begin, varargs_cur, closure);

            if(fn.heap_type_equals(CLOSURE)) {
              closure = fn;
              fn = fn.closure_function();
            }

            size_t argc = args.list_length();

            size_t min_arity = fn.vm_function_min_arity(), max_arity = fn.vm_function_max_arity();
            bool var_arity = fn.vm_function_variable_arity();

            tmp = eval_check_arity(*this, fn, exp, argc, min_arity, max_arity, var_arity);
            EVAL_CHECK(tmp, exp);

            argv = make_vector_storage(argc+(var_arity ? 1:0));
            argc = 0;

            while(args.heap_type_equals(PAIR)) {
              if(argc == max_arity) {
                break;
              }
              tmp = eval_body(frame, args.car(), true);
              EVAL_CHECK(tmp, args);

              vector_storage_append(argv, tmp);
              argc++;
              args = args.cdr();
            }

            if(args.heap_type_equals(PAIR) && var_arity) {
              while(args.heap_type_equals(PAIR)) {
                tmp = eval_body(frame, args.car(), true);
                EVAL_CHECK(tmp, args);
                if(varargs_cur == C_NIL) {
                  varargs_begin = varargs_cur = make_pair(tmp, C_NIL);
                } else {
                  tmp = make_pair(tmp, C_NIL);
                  varargs_cur.set_cdr(tmp);
                  varargs_cur = tmp;
                }
                args = args.cdr();
              }
              vector_storage_append(argv, varargs_begin);
              argc++;
            }

            tmp = apply_vm(closure != C_FALSE ? closure : fn, argc, &argv.vector_storage_data()[0]);

            EVAL_CHECK(tmp, exp);
            
            exp = tmp;
            continue;
          }
          default: {
            std::ostringstream os;
            std::cout << get_global_value(G_FORBID_INTERPRETER) << std::endl;
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

Value State::eval_exp(Value exp) {
  EvalFrame frame;
  return eval_body(frame, exp, true);
}

Value State::eval_list(Value lst, bool expand, Value env) {
  Value elt, lst_top, tmp, compiler, vfn;
  EvalFrame frame;
  frame.env = env;
  lst_top = lst;
  compiler = get_global_value(G_COMPILER);

  AR_FRAME(this, lst, elt, lst_top, tmp, compiler, vfn);
  while(lst.heap_type_equals(PAIR)) {
    tmp = lst.car();
    if(expand) {
      tmp = expand_expr(tmp);
      if(tmp.is_active_exception()) return tmp;
    }

    // We evaluate expressions as we go down the list, so that the expander can be installed
    // on the fly and used in the file in which that is done
    if(compiler == C_UNDEFINED) {
      tmp = eval_body(frame, tmp, true);
      if(tmp.is_active_exception() || lst.cdr() == C_NIL) return tmp;
    }

    if(tmp.is_active_exception()) return tmp;
    lst.set_car(tmp);
    lst = lst.cdr();
  }

  lst = lst_top;

  if(compiler != C_UNSPECIFIED && compiler != C_UNDEFINED) {
    Value argv[2] = {lst, get_global_value(G_CURRENT_MODULE)};
    tmp = apply(compiler, 2, argv);
    if(tmp.is_active_exception()) return tmp;
    return apply(tmp, 0, 0);
  }

  return C_UNSPECIFIED;
}

//
///// Generic operations
//

Value State::apply(Value fn, size_t argc, Value* argv) {
  switch(fn.type()) {
    case VMFUNCTION: case CLOSURE:
      return apply_vm(fn, argc, argv);
    case CFUNCTION:
      return fn.c_function_apply(*this, argc, argv);
    case FUNCTION: {
      return eval_apply_function(fn, argc, argv);
    }
    default:
      return eval_error("cannot apply type", fn);
  }

  return C_UNSPECIFIED;
}

Value State::apply_vector_storage(Value fn, Value vector_storage) {
  VectorStorage* store = vector_storage.as<VectorStorage>();
  return apply(fn, store->length, store->data);
}

Value State::expand_expr(Value exp) {
  Value expand = get_global_value(G_EXPANDER);

  // Comment out to disable macroexpansion
  if(expand != C_UNDEFINED) {
    Value sym, mod, saved = C_FALSE;
    AR_FRAME(this, expand, exp, sym, saved);
    Value argv[2] = {exp, get_global_value(G_CURRENT_MODULE)};

    // Save the original expression for source code information
    saved = exp;

    exp = apply(expand, 2, argv);

    if(exp.is_active_exception()) {
      std::ostringstream os1;
      std::ostringstream os2;
      os1 << saved;
      os2 << "Error while expanding expression: " << std::endl << os1.str().substr(0, 120);
      if(os1.str().size() > 120) os2 << " ... ";
      os2 << std::endl;
      //print_src_pair(os2, saved);
      stack_trace.insert(stack_trace.begin(), os2.str());
      return exp;
    }

    if(get_global_value(G_EXPANDER_PRINT) != C_UNDEFINED &&
        get_global_value(G_EXPANDER_PRINT) != C_FALSE) {
      print_src_pair(std::cout, saved, ARETE_COLOR_BLUE);
      std::cout << std::endl;
      std::cout << "Expanded to: " << std::endl;// << exp << std::endl;
      pretty_print(std::cout, exp);
      std::cout << std::endl;
    }
  } 

  return exp;
}


} // namespace arete
