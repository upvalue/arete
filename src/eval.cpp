// eval.cpp - Interpreter & related functionality
#include "arete.hpp"

#define EVAL_TRACE(exp, fn_name) \
  if((exp).type() == PAIR && (exp).pair_has_source()) { \
    std::ostringstream os; \
    os << source_info((exp).pair_src(), (fn_name)); \
    stack_trace.push_back(os.str()); \
  }
  
#define EVAL_CHECK(exp, src, fn_name) \
  if((exp).is_active_exception()) { \
    EVAL_TRACE((src), (fn_name)); \
    return (exp); \
  }


namespace arete {

/** Return an eval error with source code information */
Value State::eval_error(const std::string& msg, Value exp) {
  std::ostringstream os;

  if(exp.type() == PAIR && exp.pair_has_source()) {
    os << source_info(exp.pair_src()) << ": ";
  }

  os << msg;
  return make_exception(globals[State::S_EVAL_ERROR], os.str(), exp);
}

Value State::eval_body(Value env,  Value fn_name, Value calling_fn_name, Value src_exp, Value body) {
  Value tmp;
  AR_FRAME(this, env, fn_name, calling_fn_name, src_exp, body, tmp);

  while(body.type() == PAIR) {
    tmp = eval(env,  body.car(), fn_name);
    if(tmp.is_active_exception()) return tmp;
    if(body.cdr() == C_NIL) {
      return tmp;
    }
    body = body.cdr();
  }

  return C_UNSPECIFIED;
}

Value State::eval_cond(Value env,  Value exp, Value fn_name) {
  Value pred, body, lst = exp.cdr(), tmp;
  AR_FRAME(this, env, exp,  fn_name, pred, body, lst);

  while(lst.cdr() != C_NIL) {
    pred = lst.caar();
    body = lst.cdar();

    tmp = eval(env,  pred, fn_name);
    EVAL_CHECK(tmp, exp, fn_name);

    if(tmp != C_FALSE) {
      tmp = eval_body(env,  fn_name, fn_name, body, body);
      return tmp;
    }

    lst = lst.cdr();
  }

  // Check for else clause
  pred = lst.caar();
  body = lst.cdar();
  if(pred == get_symbol(S_ELSE)) {
    return eval_body(env,  fn_name, fn_name, body, body);
  } else {
    tmp = eval(env,  pred, fn_name);
    EVAL_CHECK(tmp, exp, fn_name);

    if(tmp != C_FALSE) {
      tmp = eval_body(env,  fn_name, fn_name, body, body);
      return tmp;
    }
  }

  return C_UNSPECIFIED;
}

Value State::eval_boolean_op(Value env, Value exp, Value fn_name, bool is_or) {
  Value tmp;
  AR_FRAME(this, env, exp, tmp, fn_name);

  exp = exp.cdr();

  // (and) => #t, (or) => #f
  if(exp == C_NIL) return Value::make_boolean(!is_or);

  while(exp.type() == PAIR) {
    tmp = eval(env, exp.car(), fn_name);
    if(tmp.is_active_exception()) {
      return tmp;
    }
    if((is_or && tmp != C_FALSE) || (!is_or && tmp == C_FALSE)) {
      return tmp;
    }
    exp = exp.cdr();
  }
  
  return tmp;
}

Value State::eval_begin(Value env,  Value exp, Value fn_name) {
  AR_FRAME(this, env,  exp, fn_name);
  return eval_body(env,  fn_name, fn_name, exp, exp.cdr());
}

Value State::eval_lambda(Value env,  Value exp) {
  Value fn_env, args, args_head, args_tail, fn_name;

  AR_FRAME(this, env,  exp, fn_env, args, args_head, args_tail);
  Function* fn = (Function*) gc.allocate(FUNCTION, sizeof(Function));
  fn->name = C_FALSE;
  fn->parent_env = env;

  // Some verification is needed here because eval_lambda may be called by the macroexpander
  if(exp.list_length() < 3) 
    return eval_error("lambda must be a list with at least three elements",  exp);

  args = exp.cadr();
  if(args.identifierp()) {
    fn->arguments = C_NIL;
    fn->rest_arguments = args;
  } else {
    // First case: (lambda rest ...)
    if(args == C_NIL) {
      fn->arguments = C_NIL;
      fn->rest_arguments = C_FALSE;
    } else {
      // Second case
      fn->rest_arguments = C_FALSE;
      Value argi = args;
      while(argi.type() == PAIR) {
        if(!argi.car().identifierp()) {
          return eval_error("lambda argument list all be identifiers", argi);
        }
        argi.set_car(argi.car());
        if(argi.cdr() == C_NIL) {
          break;
        } else if(argi.cdr().type() != PAIR) {
          if(!argi.cdr().identifierp()) {
            return eval_error("lambda argument list must all be identifiers", args);
          }

          fn->rest_arguments = argi.cdr();
          argi.set_cdr(C_NIL);
        }
        argi = argi.cdr();
      }
      // TODO this should not modify the source list.
      fn->arguments = args;

      // std::cout << fn->arguments << std::endl;
      // std::cout << fn->rest_arguments << std::endl;
    }
  }
  fn->body = exp.cddr();
  return fn;
}

Value State::eval_define(Value env,  Value exp, Value fn_name) {
  Value name, body, tmp;
  AR_FRAME(this, env,  exp, name, body, tmp, fn_name);

  if(exp.list_length() != 3) {
    return eval_error("define expects exactly three arguments", exp);
  }

  name = exp.cadr();

  body = exp.caddr();

  if(name.type() != SYMBOL) {
    return eval_error("first argument to define must be a symbol", exp.cdr());
  }

  if(env_defined(env, name)) {
    warn() << source_info(exp.pair_src()) << ' ' <<  name << " shadows existing definition of " << name << std::endl;;
  }

  tmp = eval(env,  body, fn_name);

  EVAL_CHECK(tmp, exp, fn_name);

  if(env == C_FALSE) {
    name.as<Symbol>()->value = tmp;
  } else {
    vector_append(env, name);
    vector_append(env, tmp);
  }

  // TODO how to handle qualified names here? Should we just check the string? Probably.
  if(tmp.type() == FUNCTION && tmp.function_name() == C_FALSE) {
    tmp.as<Function>()->name = name;
  }
  // std::cout << "env_set " << env << ' ' << name << std::endl;

  return C_UNSPECIFIED;
}

Value State::eval_set(Value env, Value exp, Value fn_name) {
  Value tmp, name, body;
  AR_FRAME(this, name, tmp, body, exp, env, fn_name);

  if(exp.list_length() != 3) {
    return eval_error("set! must be a list with exactly three elements");
  }

  name = exp.cadr();

  if(name.type() != SYMBOL) {
    return eval_error("first argument to set! must be a symbol", exp.cdr());
  }

  body = exp.caddr();

  tmp = env_lookup(env, name);
  if(tmp == C_UNDEFINED) {
    std::ostringstream os;
    os << "attempt to set! undefined variable " << name;
    return eval_error(os.str(), exp.cdr());
  }

  tmp = eval(env, body, fn_name);
  EVAL_CHECK(tmp, exp, fn_name);
  env_set(env, name, tmp);

  return C_UNSPECIFIED;
}

Value State::eval_if(Value env, Value exp, bool has_else, Value fn_name) {
  Value cond = exp.list_ref(1);
  Value then_branch = exp.list_ref(2);
  Value else_branch = C_UNSPECIFIED;
  Value res = C_FALSE;

  AR_FRAME(this, env,  exp, fn_name, cond, then_branch, else_branch, res);
  // TODO: protect

  if(has_else) {
    else_branch = exp.list_ref(3);
  }

  cond = eval(env,  cond, fn_name);

  if(cond.is_active_exception()) {
    return cond;
  } else if(cond != C_FALSE) {
    res = eval(env,  then_branch, fn_name);
  } else {
    res = eval(env,  else_branch, fn_name);
  }

  return res;
}

Value State::eval(Value env, Value exp, Value fn_name) {
  AR_ASSERT(env.type() != TABLE);
  // Interpreter should never encounter tables
  Value res, car, tmp;

  AR_FRAME(this, env, exp, res, car, tmp, fn_name);

  if(exp.immediatep())
    return exp;

  switch(exp.type()) {
    case VECTOR: case VECTOR_STORAGE: case FLONUM: case STRING: case CHARACTER:
    case RECORD: case RECORD_TYPE: case FUNCTION: case CFUNCTION:
      return exp;
    case PAIR: {
      size_t length = exp.list_length();
      if(length == 0) {
        return eval_error("non-list in source code", exp);
      }
      car = exp.car();

      // Check for rename in application
      if(car.type() == RENAME) {
        tmp = car.rename_expr();
        res = car.rename_env();

        tmp = env_lookup(car.rename_env(), car.rename_expr());
      }

      if((car.type() == SYMBOL && car.symbol_value() == C_SYNTAX) ||
        (car.type() == RENAME && tmp == C_SYNTAX)) {
        // Renamed syntax is special-cased
        if(car.type() == RENAME && tmp == C_SYNTAX) {
          car = car.rename_expr();
        }
        if(car == get_symbol(S_DEFINE)) {
          return eval_define(env, exp, fn_name);
        } else if(car == get_symbol(S_LAMBDA)) {
          return eval_lambda(env, exp);
        } else if(car == get_symbol(S_SET)) {
          return eval_set(env,  exp, fn_name);
        } else if(car == get_symbol(S_BEGIN)) {
          return eval_begin(env,  exp, fn_name);
        } else if(car == get_symbol(S_COND)) {
          return eval_cond(env,  exp, fn_name);
          // add fn name
        } else if(car == get_symbol(S_AND)) {
          return eval_boolean_op(env, exp, fn_name, false);
        } else if(car == get_symbol(S_OR)) {
          return eval_boolean_op(env, exp, fn_name, true);
        } else if(car == get_symbol(S_IF)) {
          if(length != 3 && length != 4) {
            return eval_error("if requires 2-3 arguments", exp);
          }

          return eval_if(env,  exp, length == 4, fn_name);
        } else if(car == get_symbol(S_QUOTE)) {
          if(length == 1) return eval_error("quote needs at least one argument", exp);
          else if(length > 2) return eval_error("quote takes exactly 1 argument", exp.cddr());
          return exp.cadr();
        }
        // form, let, set!, if, quote
      } 

      // Normal function application
      car = eval(env, exp.car(), fn_name);

      if(car.is_active_exception()) {
        if(exp.car().type() == SYMBOL) {
          return eval_error("attempt to apply undefined function", exp);
        }
        return car;
      }

      if(!car.applicable()) {
        std::ostringstream os;
        if(car == C_UNDEFINED) {
          os << "attempt to apply undefined function " << exp.car() << std::endl;
        } else {
          os << "attempt to apply non-applicable value " << car << std::endl;
        }
        return eval_error(os.str(), exp);
      } else {
        if(car.type() == FUNCTION) {
          return eval_apply_scheme(env, car, exp.cdr(), exp, fn_name);
        } else if(car.type() == CFUNCTION) {
          return eval_apply_c(env,  car, exp.cdr(), exp, fn_name);
        } else if(car.type() == RECORD) {
          return apply_record(env, car, exp.cdr(), exp, fn_name);
        } else if(car.type() == VMFUNCTION) {
          // TODO: Here, we need to gather
          // arguments into a VectorStorage (or whatever)
          Value v = C_FALSE;
          return apply_vm(car, 0, &v);
          // return apply_vm(env, car, exp.cdr(), exp, fn_name);
        }
      }

      return exp;
    }
    case RENAME: {
      // First we look it up in the env 
      // In case a renamed variable has been introduced as a binding
      // e.g (lambda ((rename 'var )) (rename 'var))
      Value chk = env_lookup(env, exp);

      if(exp.rename_env() != C_FALSE) {
        std::cerr << "interpreter encountered a non-toplevel rename, but this should be gensymed " << exp << std::endl;
        std::cerr << exp.rename_env() << std::endl;
      }

      if(chk != C_UNDEFINED) {
        return chk;
      }

      // Then we look it up in the rename env
      // e.g. ((r 'lambda) () #t)
      return env_lookup(exp.rename_env(), exp.rename_expr());
    }
    case SYMBOL: {
      res = env_lookup(env, exp);

      if(res.bits == 0) {
        return C_UNSPECIFIED;
      } 

      if(res == C_UNDEFINED) {
        std::ostringstream os;
        os << "reference to undefined variable " << exp;
        EVAL_TRACE(car, fn_name);
        return eval_error(os.str(), exp);
      }

      if(res == C_SYNTAX) {
        std::stringstream os;
        os << "attempt to use syntax " << exp << " as value";
        if(exp == get_symbol(S_DEFINE_SYNTAX)) {
          // if this happened, it's probably because the macroexpander has not been loaded
          os << " (did you load boot.scm?)";
        }
        EVAL_TRACE(car, fn_name);
        return eval_error(os.str(), exp);
      }

      return res;
    }
    default: {
      if(exp.is_active_exception()) return exp;
      AR_ASSERT(!":(");
    }
  }

  return C_UNSPECIFIED;
}

Value State::eval_apply_scheme(Value env, Value fn, Value args, Value src_exp, Value calling_fn_name, bool eval_args) {
  Value new_env, tmp, rest_args_name, fn_args, rest_args_head = C_NIL, rest_args_tail, body;
  Value fn_name;
  AR_FRAME(this, env, fn, args, new_env, fn_args, tmp, src_exp, rest_args_name, rest_args_head,
    rest_args_tail, body, fn_name, calling_fn_name);

  fn_name = fn.function_name();

  new_env = make_env(fn.function_parent_env());
  // bool eval_args = fn.function_eval_args();

  fn_args = fn.function_arguments();
  size_t arity = fn_args.list_length();
  size_t given_args = args.list_length();

  if(given_args < arity) {
    std::ostringstream os;
    os << "function " << fn << " expected at least " << arity << " arguments but got " << given_args;
    return eval_error(os.str(), src_exp);
  }

  rest_args_name = fn.function_rest_arguments();

  if(rest_args_name == C_FALSE && given_args > arity) {
    std::ostringstream os;
    os << "function " << fn << " expected at most " << arity << " arguments but got " << given_args;
    return eval_error(os.str(), src_exp);
  }

  // Evaluate arguments left to right
  // std::cout << "CALLING FN " << fn_name << " with eval_args " << eval_args << std::endl;
  while(args.type() == PAIR && fn_args.type() == PAIR) {
    if(eval_args) {
      tmp = eval(env,  args.car(), calling_fn_name);
    } else {
      tmp = args.car();
    }
    vector_append(new_env, fn_args.car());
    fn_args = fn_args.cdr();
    vector_append(new_env, tmp);
    EVAL_CHECK(tmp, src_exp, calling_fn_name);
    args = args.cdr();
  }

  // std::cout << "Args " << args << " " << args.type() << std::endl;
  if(fn.function_rest_arguments() != C_FALSE) {
    while(args.type() == PAIR) {
      tmp = eval(env, args.car(), calling_fn_name);
      EVAL_CHECK(tmp, src_exp, calling_fn_name);
      if(rest_args_head == C_NIL) {
        rest_args_head = rest_args_tail = make_pair(tmp, C_NIL);
      } else {
        tmp = make_pair(tmp, C_NIL);
        rest_args_tail.set_cdr(tmp);
        rest_args_tail = tmp;
      }
      args = args.cdr();
    }

    vector_append(new_env, fn.function_rest_arguments());
    vector_append(new_env, rest_args_head);
  }

  // Now eval body left to right
  body = fn.function_body();

  // std::cout << "evaluating function " << fn_name << " in body " << new_env << std::endl;
  tmp =  eval_body(new_env,  fn_name, calling_fn_name, src_exp, body);
  EVAL_CHECK(tmp, src_exp, calling_fn_name);
  return tmp;
}

Value State::eval_apply_c(Value env, Value fn, Value args, Value src_exp, Value fn_name, bool eval_args) {
  Value fn_args, tmp;
  AR_FRAME(this, env,  fn, args, fn_args, src_exp, tmp, fn_name);

  size_t given_args = args.list_length();
  size_t min_arity = fn.as<CFunction>()->min_arity;
  size_t max_arity = fn.as<CFunction>()->max_arity;

  if(given_args < min_arity) {
    std::ostringstream os;
    os << "function " << fn << " expected at least " << min_arity << " arguments but got " << given_args;
    return eval_error(os.str(), src_exp);
  }

  if(!fn.c_function_variable_arity() && given_args > max_arity) {
    std::ostringstream os;
    os << " function " << fn << " expected at most " << max_arity << " arguments but got " << given_args;
    return eval_error(os.str(), src_exp);
  }

  fn_args = make_vector();
  size_t argc = 0;
  while(args.type() == PAIR) {
    if(eval_args) {
      tmp = eval(env,  args.car());
      EVAL_CHECK(tmp, src_exp, fn_name);
    } else {
      tmp = args.car();
    }
    vector_append(fn_args, tmp);
    argc++;
    args = args.cdr();
  }
  
  /*
  std::cout << "OK" << std::endl;
  std::cout << fn_args << std::endl;
  std::cout << fn_args.type() << std::endl;
  std::cout << fn_args.vector_storage().type() << std::endl;
  std::cout << "NOW CRASH" << std::endl;
  */
  tmp = fn.c_function_addr()(*this, argc, fn_args.vector_storage().vector_storage_data());
  EVAL_CHECK(tmp, src_exp, fn_name);
  return tmp;
}

Value State::apply_record(Value env, Value fn, Value args, Value src_exp, Value fn_name) {
  Value apply = fn.record_type().record_type_apply(), args2 = C_FALSE;
  AR_FRAME(this, env, fn, args, src_exp, fn_name, apply, args, args2);

  // Interesting: Reusing the args var here like this causes a GC failure.
  // Perhaps make_pair gets inlined or something here and causes some kind of tracking issue?
  // args = make_pair(fn, args);

  // Prepend record to arguments
  args2 = make_pair(fn, args);

  if(apply.type() == FUNCTION) {
    return eval_apply_scheme(env, apply, args2, src_exp, fn_name, true);
  } else if(apply.type() == CFUNCTION) {
    return eval_apply_c(env, apply, args2, src_exp, fn_name, true);
  } else {
    return type_error("record applicator must be function or cfunction");
  }
  gc.collect();
  // std::cout << "HAH" << std::endl;
  return C_UNSPECIFIED;
}

Value State::eval_apply_generic(Value fn, Value args, bool eval_args) {
  AR_ASSERT(fn.applicable() && "eval_apply_generic called on non-applicable object");
  if(fn.type() == FUNCTION) {
    return eval_apply_scheme(fn.function_parent_env(), fn, args, C_FALSE, C_FALSE, eval_args);
  } else if(fn.type() == CFUNCTION) {
    return eval_apply_c(C_FALSE, fn, args, C_FALSE, C_FALSE, false);
  } 

  std::cerr << "interpreter cannot apply object " << fn << std::endl;
  AR_ASSERT(!"eval_apply_generic failed");
}

} // namespace arete
