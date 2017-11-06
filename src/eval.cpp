// eval.cpp - Interpreter & related functionality

#include "arete.hpp"

#define EVAL_TRACE(exp, fn_name) \
  if((exp).type() == PAIR && (exp).pair_has_source()) { \
    std::ostringstream os; \
    os << source_info((exp).pair_src(), (fn_name), true); \
    stack_trace.push_back(os.str()); \
  }
  
#define EVAL_CHECK(exp, src, fn_name) \
  if((exp).is_active_exception()) { \
    EVAL_TRACE((src), (fn_name)); \
    return (exp); \
  }

namespace arete {

Value State::make_env(Value parent) {
  Value vec;
  AR_FRAME(this, vec, parent);
  vec = make_vector(3);
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
    for(size_t i = env.vector_length() - 1; i >= VECTOR_ENV_FIELDS; i -= 2) {
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
    for(size_t i = env.vector_length() - 1; i >= VECTOR_ENV_FIELDS; i -= 2) {
      if(identifier_equal(env.vector_ref(i-1), name)) {
        return true;
      }
    }
  } else if(env == C_FALSE) {
    return name.symbol_value() != C_UNDEFINED;
  }
  return false;
}

Value State::env_lookup_impl(Value& env, Value name, Value& rename_key, bool& reached_toplevel) {
  rename_key = C_FALSE;
  reached_toplevel = false;

  // First we search through vectors with identifier_equal, which only
  // returns true if symbols are the same or if renames have the same env and expr
  while(env.type() == VECTOR) {
    for(size_t i = env.vector_length() - 1; i >= VECTOR_ENV_FIELDS; i -= 2) {
      // Here we check for function-level renames in the environment.
      // This is necessary because the expansion pass will replace them
      // with gensyms before returning the full expression
      if(identifier_equal(env.vector_ref(i-1), name)) {
        rename_key = env.vector_ref(i-1);
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
  Value rename_key;
  bool found;
  return env_lookup_impl(env, name, rename_key, found);
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

Value State::eval_body(Value env,  Value fn_name, Value calling_fn_name, Value src_exp, Value body) {
  Value tmp;
  AR_FRAME(this, env, fn_name, calling_fn_name, src_exp, body, tmp);

  while(body.type() == PAIR) {
    tmp = eval(env, body.car(), fn_name);
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
  AR_FRAME(this, env, exp,  fn_name, pred, body, lst, tmp);

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
  if(pred == globals[S_ELSE]) {
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
  AR_FRAME(this, env, exp, fn_name, tmp);

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

  AR_FRAME(this, env,  exp, fn_env, args, args_head, args_tail, fn_name);
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
    if(exp.pair_has_source()) {
      warn() << source_info(exp.pair_src()) << ' ' <<  name << " shadows existing definition of " << name << std::endl;;
    } else {
      warn() << name << " shadows existing definition" << std::endl;
    }
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

  return C_UNSPECIFIED;
}

Value State::eval_set(Value env, Value exp, Value fn_name) {
  Value tmp, name, body, env_search = env;
  AR_FRAME(this, name, tmp, body, exp, env, fn_name, env_search);

  if(exp.list_length() != 3) {
    return eval_error("set! must be a list with exactly three elements");
  }

  name = exp.cadr();

  if(name.type() != SYMBOL) {
    return eval_error("first argument to set! must be a symbol", exp.cdr());
  }

  body = exp.caddr();

  Value rename_key;
  bool found;
  tmp = env_lookup_impl(env_search, name, rename_key, found);

  if(env_search == C_FALSE && name.symbol_immutable()) {
    std::ostringstream os;
    os << "attempt to set! immutable builtin " << name << std::endl;
    return eval_error(os.str(), exp.cdr());
  }

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
    case RECORD: case RECORD_TYPE: case FUNCTION: case CFUNCTION: case TABLE:
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
        if(car == globals[S_DEFINE]) {
          return eval_define(env, exp, fn_name);
        } else if(car == globals[S_LAMBDA]) {
          return eval_lambda(env, exp);
        } else if(car == globals[S_SET]) {
          return eval_set(env,  exp, fn_name);
        } else if(car == globals[S_BEGIN]) {
          return eval_begin(env,  exp, fn_name);
        } else if(car == globals[S_COND]) {
          return eval_cond(env,  exp, fn_name);
          // add fn name
        } else if(car == globals[S_AND]) {
          return eval_boolean_op(env, exp, fn_name, false);
        } else if(car == globals[S_OR]) {
          return eval_boolean_op(env, exp, fn_name, true);
        } else if(car == globals[S_IF]) {
          if(length != 3 && length != 4) {
            return eval_error("if requires 2-3 arguments", exp);
          }

          return eval_if(env,  exp, length == 4, fn_name);
        } else if(car == globals[S_QUOTE]) {
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
          return eval_apply_c(env, car, exp.cdr(), exp, fn_name);
        } else if(car.type() == VMFUNCTION || car.type() == CLOSURE) {
          return eval_apply_vm(env, car, exp.cdr(), exp, fn_name);
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
        return C_FALSE;
      } 

      if(res == C_UNDEFINED) {
        std::ostringstream os;
        os << "reference to undefined variable " << exp;
        EVAL_TRACE(car, fn_name);
        Value ret= eval_error(os.str(), exp);
        return ret;
      }

      if(res == C_SYNTAX) {
        std::stringstream os;
        os << "attempt to use syntax " << exp << " as value";
        if(exp == globals[S_DEFINE_SYNTAX]) {
          // if this happened, it's probably because the macroexpander has not been loaded
          os << " (did you load syntax.scm?)";
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

Value State::eval_apply_scheme(Value env, Value fn, Value args, Value src_exp,
    Value calling_fn_name, bool eval_args) {

  Value new_env, tmp, rest_args_name, fn_args, rest_args_head = C_NIL, rest_args_tail, body;
  Value fn_name;

  AR_FRAME(this, env, fn, args, src_exp, calling_fn_name, new_env, tmp, rest_args_name, fn_args,
    rest_args_head, rest_args_tail, body, fn_name);

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

    EVAL_CHECK(tmp, src_exp, calling_fn_name);
    vector_append(new_env, fn_args.car());
    fn_args = fn_args.cdr();
    vector_append(new_env, tmp);
    args = args.cdr();
  }

  // std::cout << "Args " << args << " " << args.type() << std::endl;
  if(fn.function_rest_arguments() != C_FALSE) {
    while(args.type() == PAIR) {
      if(eval_args) {
        tmp = eval(env, args.car(), calling_fn_name);
        EVAL_CHECK(tmp, src_exp, calling_fn_name);
      } else {
        tmp = args.car();
      }
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

Value State::eval_apply_vm(Value env, Value fn, Value args, Value src_exp, Value fn_name, bool eval_args) {
  Value tmp, closure(C_FALSE), vec, varargs_begin, varargs_cur;
  AR_FRAME(this, env, fn, args, src_exp, fn_name, tmp, closure, vec, varargs_begin, varargs_cur);

  if(fn.type() == CLOSURE) {
    closure = fn;
    fn = fn.closure_function();
  }

  size_t given_args = args.list_length();
  size_t min_arity = fn.as<VMFunction>()->min_arity;
  size_t max_arity = fn.as<VMFunction>()->max_arity;

  if(given_args < min_arity) {
    std::ostringstream os;
    os << "function " << fn << " expected at least " << min_arity << " arguments but got " << given_args;
    return eval_error(os.str(), src_exp);
  }

  if(!fn.vm_function_variable_arity() && given_args > max_arity) {
    std::ostringstream os;
    os << " function " << fn << " expected at most " << max_arity << " arguments but got " << given_args;
    return eval_error(os.str(), src_exp);
  }

  size_t argc = 0;
  vec = make_vector();
  while(args.type() == PAIR) {
    if(argc == max_arity) {
      break;
    }
    tmp = args.car();
    if(eval_args) {
      tmp = eval(env, args.car());
      EVAL_CHECK(tmp, src_exp, fn_name);
    } 
    vector_append(vec, tmp);
    // temps.push_back(tmp);
    argc++;
    args = args.cdr();
  }

  if(args.type() == PAIR && fn.vm_function_variable_arity()) {
    varargs_begin = varargs_cur = C_NIL;
    while(args.type() == PAIR) {
      tmp = args.car();
      if(eval_args) {
        tmp = eval(env, tmp);
        EVAL_CHECK(tmp, src_exp, fn_name);
      } 
      if(varargs_cur == C_NIL) {
        varargs_begin = varargs_cur = make_pair(tmp, C_NIL);
      } else {
        tmp = make_pair(tmp, C_NIL);
        varargs_cur.set_cdr(tmp);
        varargs_cur = tmp;
      }
      args = args.cdr();
    }
    vector_append(vec, varargs_begin);
    argc++;
  }

  if(closure != C_FALSE) {
    AR_ASSERT(gc.live(closure));
    AR_ASSERT(closure.type() == CLOSURE);
  }
  AR_ASSERT(gc.live(fn));
  AR_ASSERT(fn.type() == VMFUNCTION);
  tmp = apply_vm(closure != C_FALSE ? closure : fn, argc, &vec.vector_storage().vector_storage_data()[0]);

  EVAL_CHECK(tmp, src_exp, fn_name);
  return tmp;
}


Value State::eval_apply_c(Value env, Value fn, Value args, Value src_exp, Value fn_name, bool eval_args) {
  Value fn_args, tmp;
  AR_FRAME(this, env, fn, args, src_exp, fn_name, fn_args, tmp);

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
      EVAL_CHECK(tmp, src_exp, fn_name);
    }
    vector_append(fn_args, tmp);
    argc++;
    args = args.cdr();
  }
  
  tmp = fn.c_function_addr()(*this, argc, fn_args.vector_storage().vector_storage_data());
  EVAL_CHECK(tmp, src_exp, fn_name);
  return tmp;
}

Value State::eval_apply_function(Value fn, Value args) {
  AR_TYPE_ASSERT(fn.type() == FUNCTION);

  return eval_apply_scheme(fn.function_parent_env(), fn, args, C_FALSE, C_FALSE, false);
}

Value State::apply(Value fn, size_t argc, Value* argv) {
  switch(fn.type()) {
    case VMFUNCTION: case CLOSURE:
      return apply_vm(fn, argc, argv);
    case CFUNCTION:
      return fn.c_function_addr()(*this, argc, argv);
    case FUNCTION: {
      Value lst = C_NIL;
      size_t i;

      temps.clear();

      for(i = 0; i != argc; i++) {
        temps.push_back(argv[i]);
      }

      AR_FRAME(*this, fn, lst);
      while(i--) {
        lst = make_pair(temps[i], lst);
      }

      return eval_apply_function(fn, lst);
    }
      break;
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
    Value argv[2] = {exp, C_FALSE};

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
      print_src_pair(os2, saved);
      stack_trace.insert(stack_trace.begin(), os2.str());
      return exp;
    }

    if(get_global_value(G_EXPANDER_PRINT) != C_UNDEFINED &&
        get_global_value(G_EXPANDER_PRINT) != C_FALSE) {
      print_src_pair(std::cout, saved, ARETE_COLOR_BLUE);
      std::cout << std::endl;
      std::cout << "Expanded to: " << exp << std::endl;
    }
  } 

  return exp;
}

Value State::eval_toplevel(Value exp) {
  // So, this should:
  // expand an expression or list of expressions
  // then compile into a list of expression.
  exp = expand_expr(exp);

  if(exp.is_active_exception()) return exp;

  return eval(C_FALSE, exp);
}

Value State::eval_toplevel_list(Value lst) {
  Value elt, lst_top, tmp, compiler, vfn;
  AR_FRAME(*this, lst, elt, lst_top, tmp, compiler, vfn);
  lst_top = lst;
  compiler = get_global_value(G_COMPILER);
  while(lst.type() == PAIR) {
    tmp = expand_expr(lst.car());

    if(tmp.is_active_exception()) return tmp;

    // We have to eval expressions as we encounter them, if the
    // compiler hasn't been fully installed yet, in order to allow LOADing the expander, compiler
    // and then using them from the file which did that
    if(compiler == C_UNDEFINED) {
      tmp = eval(C_FALSE, tmp);
      if(tmp.is_active_exception() || lst.cdr() == C_NIL) return tmp;
    }

    if(tmp.is_active_exception()) return tmp;
    lst.set_car(tmp);
    lst = lst.cdr();
  }

  lst = lst_top;

  if(compiler != C_UNSPECIFIED && compiler != C_UNDEFINED) {
    Value argv[1] = {lst};
    tmp = apply(compiler, 1, argv);
    if(tmp.is_active_exception()) return tmp;
    tmp = apply(tmp, 0, 0);
    return tmp;
  }

  return C_UNSPECIFIED;
}

} // namespace arete
