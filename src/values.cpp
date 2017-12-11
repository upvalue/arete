// values.cpp - Value constructors, mutators, various methods

#include <algorithm>

#include "arete.hpp"

namespace arete {

std::string Value::type_desc() const {
  Type tipe = type();
  std::ostringstream os;
  if(tipe == RECORD) {
    os << "record " << record_type().record_type_name().string_data();
  } else {
    os << tipe;
  }
  return os.str();
}

Value State::get_symbol(Global sym) {
  Value sym2 = (globals.at((size_t) sym));
  AR_ASSERT(sym2.type() == SYMBOL);
  return sym2;
}

Value State::get_global_value(Global sym) {
  Value s = globals.at((size_t) sym);
  return s.as<Symbol>()->value;
}

void State::set_global_value(Global sym, Value v) {
  globals.at((size_t) sym).as<Symbol>()->value = v;
}

bool State::equals(Value a, Value b) {
  if(a.bits == b.bits) return true;

  Type tipe = a.type();
  if(tipe != b.type()) return false;

  switch(tipe) {
    case VECTOR: {
      if(a.vector_length() != b.vector_length()) return false;
      for(size_t i = 0; i < a.vector_length(); i++) {
        if(!equals(a.vector_ref(i), b.vector_ref(i))) {
          return false;
        }
      }
      return true;
    }
    case PAIR: {
      while(a.heap_type_equals(PAIR) && b.heap_type_equals(PAIR)) {
        if(!equals(a.car(), b.car())) {
          return false;
        }
        a = a.cdr();
        b = b.cdr();
      }

      if(a != C_NIL && b != C_NIL) {
        return equals(a, b);
      }

      if(a != C_NIL || b != C_NIL) return false;

      return true;
    }
    case STRING: {
      if(a.string_bytes() != b.string_bytes()) return false;
      return strncmp(a.string_data(), b.string_data(), a.string_bytes()) == 0;
    }
    case RENAME: {
      return a.rename_env() == b.rename_env() && a.rename_expr() == b.rename_expr() &&
        a.rename_gensym() == b.rename_gensym();
    }
    case CHARACTER:
      return a.character() == b.character();
    case FLONUM:
      return a.flonum_value() == b.flonum_value();
    default:  {
      return a.bits == b.bits;
    }
  }

  return a.bits == b.bits;
}

Value State::get_symbol(const std::string& name) {
  symbol_table_t::const_iterator x = symbol_table->find(name);
  if(x == symbol_table->end()) {
    Symbol* heap = static_cast<Symbol*>(gc.allocate(SYMBOL, sizeof(Symbol)));
    AR_ASSERT(heap->get_type() == SYMBOL);

    Value sym = heap, string;
    AR_FRAME(this, sym, string);


    string = make_string(name);

    sym.as<Symbol>()->value = C_UNDEFINED;
    sym.as<Symbol>()->name = string;

    if(name.size() > 2 && name[0] == '#' && name[1] == '#') {
      sym.heap->set_header_bit(Value::SYMBOL_QUALIFIED_BIT);
    }

    symbol_table->insert(std::make_pair(name, (Symbol*) sym.heap));

    return sym;
  } else {
    AR_ASSERT(symbol_table->size() > 0);
    return x->second;
  }
}

Value State::get_symbol(Value name) {
  AR_TYPE_ASSERT(name.type() == STRING);
  std::string cname(name.string_data());
  Value ret= get_symbol(cname);
  AR_ASSERT(ret.type() == SYMBOL);
  return ret;
}

Value State::gensym(Value sym) {
  std::ostringstream os;
  os << "#:" << sym.symbol_name_data() << gensym_counter;
  gensym_counter++;
  Value sym2 = get_symbol(os.str());
  sym2.heap->set_header_bit(Value::SYMBOL_GENSYM_BIT);
  return sym2;
}

Value State::symbol_dequalify(Value sym) {
  AR_TYPE_ASSERT(sym.heap_type_equals(SYMBOL));

  if(!sym.symbol_qualified()) return sym;

  std::string name;
  const char* data = sym.symbol_name_data();

  size_t length = sym.symbol_name().string_bytes();

  for(size_t i = length; i != 0; i--) {
    if(data[i-1] == '#') break;
    name += data[i-1];
  }

  std::reverse(name.begin(), name.end());

  return get_symbol(name);
}

Value State::make_rename(Value expr, Value env) {
  AR_FRAME(this, expr, env);
  Rename* heap = static_cast<Rename*>(gc.allocate(RENAME, sizeof(Rename)));
  heap->expr = expr;
  heap->env = env;
  heap->gensym = C_FALSE;
  return heap;
}

size_t State::register_record_type(const std::string& cname, unsigned field_count, unsigned data_size,
    Value field_names, Value parent) {
  Value name = C_FALSE, tipe = C_FALSE;

  AR_FRAME(this, tipe, name, field_names, parent);

  tipe = gc.allocate(RECORD_TYPE, sizeof(RecordType));
  name = make_string(cname);

  tipe.as<RecordType>()->name = name;
  tipe.as<RecordType>()->print = C_FALSE;
  tipe.as<RecordType>()->apply = C_FALSE;
  tipe.as<RecordType>()->parent = parent;
  tipe.as<RecordType>()->field_count = field_count;
  tipe.as<RecordType>()->field_names = field_names;
  tipe.as<RecordType>()->data_size = data_size;

  size_t idx = register_global(tipe);
  return idx;
}

void State::record_type_set_finalizer(size_t idx, c_finalizer_t finalizer) {
  RecordType* rtd = globals.at(idx).as<RecordType>();
  rtd->finalizer = finalizer;
}

void State::record_set(Value rec_, unsigned field, Value value) {
  Record* rec = rec_.as<Record>();

  AR_TYPE_ASSERT(rec->type->field_count > field && "out of range error on record");

  rec->fields[field] = value;
}

/** Make an exception from Scheme values */
Value State::make_exception(Value tag, Value message, Value irritants) {
  Value exc;
  AR_FRAME(this, tag, message, irritants, exc);
  Exception* heap = static_cast<Exception*>(gc.allocate(EXCEPTION, sizeof(Exception)));
  exc.heap = heap;
  heap->set_header_bit(Value::EXCEPTION_ACTIVE_BIT);
  heap->set_header_bit(Value::EXCEPTION_TRACE_BIT);
  heap->tag = tag;
  heap->message = message;
  heap->irritants = irritants;
  return heap;
}

/** Make an exception with a C++ std::string message */
Value State::make_exception(Value tag, const std::string& cmessage, Value irritants) {
  Value message;
  AR_FRAME(this, tag, message, irritants);
  AR_ASSERT(gc.live(tag));
  AR_ASSERT(gc.live(irritants));
  message = make_string(cmessage);
  return make_exception(tag, message, irritants);
}

/** Make an exception with a C++ std::string message and tag */
Value State::make_exception(const std::string& ctag, const std::string& cmessage,
    Value irritants) { 
  Value tag;
  AR_FRAME(this, tag, irritants);
  tag = get_symbol(ctag);
  return make_exception(tag, cmessage, irritants);
}

/** Make an exception with a builtin tag */
Value State::make_exception(Global s, const std::string& cmessage, Value irritants) {
  return make_exception(get_symbol(s), cmessage, irritants);
}

Value State::make_record(Value tipe) {
  AR_FRAME(this, tipe);

  unsigned field_count = tipe.as<RecordType>()->field_count;
  unsigned data_size = tipe.as<RecordType>()->data_size;


  Value record = static_cast<Record*>(
    gc.allocate(RECORD, sizeof(Record) + 
      ((field_count * sizeof(Value)) - sizeof(Value))
      + data_size));

  record.as<Record>()->type = tipe.as<RecordType>();
  for(unsigned i = 0; i != field_count; i++) {
    record.as<Record>()->fields[i] = C_FALSE;
  }

  if(tipe.as_unsafe<RecordType>()->finalizer) {
    gc.finalizers.push_back(record);
  }

  return record;
}

Value State::make_record(size_t tag) {
  AR_ASSERT(globals.size() > tag);
  return make_record(globals.at(tag).as<RecordType>());
}

///// FUNCTIONS

// Automatic function registration. This is a little nicer to write than adding functions away
// from where they're defined, but the main point is assigning each function a unique ID for the
// purpose of image serialization.

#define AR_DEFUN_LOG(expr) ARETE_LOG(ARETE_LOG_TAG_DEFUN, "defun", expr)

std::vector<c_function_t>* id_to_function = 0;
std::unordered_map<ptrdiff_t, size_t> *function_to_id = 0;
DefunGroup* defun_group = 0;

c_function_t function_id_to_ptr(size_t id) {
  AR_ASSERT(id < id_to_function->size());
  return id_to_function->at(id);
}

c_function_t function_ptr_to_id(ptrdiff_t addr) {
  auto i = function_to_id->find(addr);
  if(i == function_to_id->end()) {
    std::cerr << "arete: function_ptr_to_id failed to find ID for function " << (ptrdiff_t) addr <<
      std::endl;

    return nullptr;
  }
  return (c_function_t) i->second;
}

static void register_defun(const char* fn_name, c_function_t fn, Defun* push_back) {
  if(!function_to_id) {
    id_to_function = new std::vector<c_function_t>();
    function_to_id = new std::unordered_map<ptrdiff_t, size_t>;
  }

  size_t id = id_to_function->size();
  AR_DEFUN_LOG(defun_group->name << ' ' << fn_name << " " << id << " <=> " << (ptrdiff_t) fn);

  // Add to defun group
  if(push_back != nullptr) {
    defun_group->data.push_back(push_back);
  }

  // Insert to hashtable. We have to check if it's there first because some defuns can point to the
  // same function (close-input-port and close-output-port for example)
  if(function_to_id->find((ptrdiff_t)fn) != function_to_id->end()) {
    AR_DEFUN_LOG("not inserting already-saved function to table");
    return;
  }

  function_to_id->insert(std::make_pair((ptrdiff_t)fn, id));

  id_to_function->push_back(fn);
}

Defun::Defun(const char* fn_name_, c_function_t fn_, size_t min_arity_, size_t max_arity_, bool var_arity_):
    fn_name(fn_name_), fn(fn_), min_arity(min_arity_), max_arity(max_arity_), var_arity(var_arity_) {

  register_defun(fn_name, fn_, this);
  
}

Defun::Defun(void* fn_): fn((c_function_t)fn_) {
  register_defun("finalizer", (c_function_t) fn_, nullptr);
}

void arete_free_function_tables() {
  if(function_to_id) {
    delete id_to_function;
    delete function_to_id;
    function_to_id = 0;
  }
}

void DefunGroup::install(State& state) {
  for(size_t i = 0; i != data.size(); i++) {
    state.defun_core(data[i]->fn_name, data[i]->fn, data[i]->min_arity, data[i]->max_arity,
      data[i]->var_arity);
  }
}

void DefunGroup::install_closure(State& state, Value closure) {
  AR_FRAME(state, closure);
  for(size_t i = 0; i != data.size(); i++) {
    state.defun_core_closure(data[i]->fn_name, closure, (c_closure_t)data[i]->fn,
      data[i]->min_arity, data[i]->max_arity, data[i]->var_arity);
  }
}

Value State::make_module(const std::string& name) {
  Value key, val;
  Value module = make_table();
  
  // Create "module-stage" field and mark module as fully expanded
  key = make_string("module-stage");
  table_set(module, key, Value::make_fixnum(2));

  // Create "module-exports" field
  val = make_table();
  key = make_string("module-exports");
  table_set(module, get_global_value(G_STR_MODULE_EXPORTS), val);

  // Create "module-name" field and store module name
  AR_FRAME(this, key, val, module);
  key = make_string("module-name");
  val = make_string(name);

  // Register module in module table
  table_set(get_global_value(G_MODULE_TABLE), val, module);

  //pretty_print(std::cout, module);
  return module;
}

void State::module_define(Value module, const std::string& ckey, Value value) {
  AR_TYPE_ASSERT(module.heap_type_equals(TABLE));

  Value key, exports;
  AR_FRAME(this, module, key, exports, value);

  key = make_string("module-exports");
  bool hashable;
  exports = table_get(module, key, hashable);
  
  key = make_string(ckey);

  table_set(exports, key, value);
}

void DefunGroup::install_module(State& state, const std::string& cname, Value closure) {
  Value module, cfn, sym, name, exports;
  bool found;
  AR_FRAME(state, module, closure, exports, cfn, sym, name);
  module = state.make_module(cname);

  exports = state.table_get(module, state.get_global_value(State::G_STR_MODULE_EXPORTS), found);
  AR_ASSERT(found);

  for(size_t i = 0; i != data.size(); i++) {
    const Defun* defun = data.at(i);

    std::ostringstream qname;

    name = state.make_string(defun->fn_name);
    AR_ASSERT(defun->fn);
    cfn = state.make_c_function(name, closure, (c_function_t) defun->fn, defun->min_arity,
      defun->max_arity, defun->var_arity);

    name = state.get_symbol(name);

    qname << "##" << cname << "#" << name;

    sym = state.get_symbol(qname.str());
    sym.set_symbol_value(cfn);

    state.table_set(module, name, sym);
    state.table_set(exports, name, name);
  }
  //state.pretty_print(std::cout, module);
}

 Value State::make_c_function(Value name, Value closure, c_function_t addr, size_t min_arity,
    size_t max_arity, bool variable_arity) {
  if(max_arity == 0)
    max_arity = min_arity;
  AR_FRAME(this, name, closure);
  CFunction *cfn = static_cast<CFunction*>(gc.allocate(CFUNCTION, sizeof(CFunction)));
  cfn->name = name;
  if(closure != C_FALSE) {
    cfn->closure = closure;
    cfn->set_header_bit(Value::CFUNCTION_CLOSURE_BIT);
  }
  cfn->addr = addr;
  AR_ASSERT(cfn->addr);
  cfn->min_arity = min_arity;
  cfn->max_arity = max_arity;
  if(variable_arity)
    cfn->set_header_bit(Value::CFUNCTION_VARIABLE_ARITY_BIT);
  return cfn;
}

void State::defun_core_closure(const std::string& cname, Value closure, c_closure_t addr, size_t min_arity, size_t max_arity, bool variable_arity) {
  Value cfn, sym, name;

  AR_FRAME(this, cfn, sym, name, closure);
  name = make_string(cname);
  AR_ASSERT(addr);
  cfn = make_c_function(name, closure, (c_function_t)addr, min_arity, max_arity, variable_arity);

  sym = get_symbol(name);
  sym.set_symbol_value(cfn);
}

void State::defun_core(const std::string& cname, c_function_t addr, size_t min_arity, size_t max_arity, bool variable_arity) {
  Value cfn, sym, name;

  AR_FRAME(this, cfn, sym, name);
  AR_ASSERT(addr);
  name = make_string(cname);
  cfn = make_c_function(name, C_FALSE, addr, min_arity, max_arity, variable_arity);

  sym = get_symbol(name);
  sym.set_symbol_value(cfn);
}

Value Value::c_function_apply(State& state, size_t argc, Value* argv) {
  AR_TYPE_ASSERT(type() == CFUNCTION);
  if(c_function_is_closure()) {
    return c_function_closure_addr()(state, (void*) c_function_closure_data().bits, argc, argv);
  } else {
    return c_function_addr()(state, argc, argv);
  }
}

///// PAIRS

Value State::make_pair(Value car, Value cdr, size_t size) {
  AR_FRAME(this, car, cdr);
  Pair* heap = (Pair*) gc.allocate(PAIR, size);

  heap->data_car = car;
  heap->data_cdr = cdr;
  return heap;
}

Value State::make_src_pair(Value car, Value cdr, SourceLocation& loc) {
  Value pare = C_FALSE;
  AR_FRAME(this, pare, car, cdr);
  pare = make_pair(car, cdr, sizeof(Pair));
  pare.heap->set_header_bit(Value::PAIR_SOURCE_BIT);
  AR_ASSERT(loc.source < source_names.size());
  AR_ASSERT(pare.type() == PAIR);
  AR_ASSERT(pare.pair_has_source());
  pare.set_pair_src(loc);

  return pare;
}

Value State::make_src_pair(Value car, Value cdr, Value src) {
  if(src.heap_type_equals(PAIR) && src.pair_has_source()) {
    SourceLocation loc(src.pair_src());
    AR_ASSERT(loc.source < source_names.size());
    return make_src_pair(car, cdr, loc);
  }
  return make_pair(car, cdr);
}

size_t Value::list_length() {
  Value check(bits);
  if(check == C_NIL || !check.heap_type_equals(PAIR)) return 0;
  size_t len = 0;
  while(true) {
    if(check == C_NIL) break;
    else if(!check.heap_type_equals(PAIR)) return 0;
    else {
      ++len, check = check.cdr();
    }
  }
  return len;
}

bool Value::listp() {
  return bits == C_NIL || list_length() > 0;
}

Value Value::list_ref(size_t n) const {
  Value check(bits);
  size_t i = 0;
  while(check.type() == PAIR && i++ != n) {
    check = check.cdr();
    if(check.type() != PAIR && check != C_NIL) {
      AR_TYPE_ASSERT(!"list-ref in non-list");
      return C_NIL;
    }
  }
  return check.car();
}


Value State::make_char(int c) {
  Char* heap = static_cast<Char*>(gc.allocate(CHARACTER, sizeof(Char)));
  heap->datum = c;
  return heap;
}

///// VECTORS

Value State::make_vector_storage(size_t capacity) {
  // Account for Value[1]
  size_t size = (sizeof(VectorStorage) - sizeof(Value)) + (sizeof(Value) * capacity);
  VectorStorage* storage = static_cast<VectorStorage*>(gc.allocate(VECTOR_STORAGE, size));

  return storage;
}

Value State::make_vector(size_t capacity) {
  Value storage(C_FALSE);

  AR_FRAME(this, storage);
  storage = make_vector_storage(capacity);
  Value vec = gc.allocate(VECTOR, sizeof(Vector));
  vec.as_unsafe<Vector>()->storage = storage;
  vec.as_unsafe<Vector>()->capacity = capacity;

  return vec;
}

Value State::make_vector(Value vector_storage) {
  AR_FRAME(this, vector_storage);

  Value vec = gc.allocate(VECTOR, sizeof(Vector));

  vec.as_unsafe<Vector>()->storage = vector_storage;
  vec.as_unsafe<Vector>()->capacity = vector_storage.as_unsafe<VectorStorage>()->length;

  return vec;
}

void State::vector_storage_append(Value sstore, Value value) {
  VectorStorage* store = sstore.as<VectorStorage>();
  store->data[store->length++] = value;
  AR_ASSERT("attempted to append to vector-storage past capacity" &&
    store->length <= (store->size - sizeof(VectorStorage) + sizeof(Value)) / sizeof(Value*));
}

/** Mutating vector append */
void State::vector_append(Value vector, Value value) {
  AR_TYPE_ASSERT(vector.type() == VECTOR);
  // std::cout << "vector_append" << std::endl;
  VectorStorage* store = static_cast<VectorStorage*>(vector.vector_storage().heap);

  store->data[store->length++] = value;
  
  AR_ASSERT(store->length <= vector.as_unsafe<Vector>()->capacity);
  if(store->length == vector.as_unsafe<Vector>()->capacity) {

    Value storage = store, new_storage;
    AR_FRAME(this, vector, value, storage, new_storage);

    size_t ncap = vector.as_unsafe<Vector>()->capacity * 2;

    new_storage = make_vector_storage(ncap);
    vector.as_unsafe<Vector>()->capacity = ncap;
    store = static_cast<VectorStorage*>(vector.vector_storage().heap);
    static_cast<VectorStorage*>(new_storage.heap)->length = store->length;
    
    memcpy(static_cast<VectorStorage*>(new_storage.heap)->data, store->data, sizeof(Value) * store->length);
    static_cast<Vector*>(vector.heap)->storage = new_storage;
  }
}

} // namespace arete
