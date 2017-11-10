// values.cpp - Value constructors, mutators, various methods

#include "arete.hpp"

namespace arete {

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

  if(a.type() == VECTOR && b.type() == VECTOR) {
    if(a.vector_length() != b.vector_length()) return false;
    for(size_t i = 0; i < a.vector_length(); i++) {
      if(!equals(a.vector_ref(i), b.vector_ref(i))) {
        return false;
      }
    }
    return true;
  } else if(a.type() == PAIR && b.type() == PAIR) {
    while(a.type() == PAIR && b.type() == PAIR) {
      if(!equals(a.car(), b.car())) {
        return false;
      }
      a = a.cdr();
      b = b.cdr();
    }

    if(a != C_NIL || b != C_NIL) {
      return equals(a, b);
    }

    return true;
  } else if(a.type() == STRING && b.type() == STRING) {
    if(a.string_bytes() != b.string_bytes()) return false;
    return strncmp(a.string_data(), b.string_data(), a.string_bytes()) == 0;
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

Value State::make_string(const std::string& body) {
  String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + body.size()));
  heap->bytes = body.size();
  strncpy(heap->data, body.c_str(), body.size());
  AR_ASSERT(heap->data[heap->bytes] == '\0');
  // heap->data[heap->bytes] = '\0';

  return heap;
}

Value State::string_copy(Value x) {
  AR_FRAME(this, x);
  String* heap = static_cast<String*>(gc.allocate(STRING, sizeof(String) + x.string_bytes()));
  strncpy(heap->data, x.string_data(), x.string_bytes());
  heap->bytes = x.string_bytes();
  heap->data[x.string_bytes()] = '\0';
  return heap;
}

/** Make an exception from Scheme values */
Value State::make_exception(Value tag, Value message, Value irritants) {
  Value exc;
  AR_FRAME(this, tag, message, irritants, exc);
  Exception* heap = static_cast<Exception*>(gc.allocate(EXCEPTION, sizeof(Exception)));
  exc.heap = heap;
  heap->set_header_bit(Value::EXCEPTION_ACTIVE_BIT);
  heap->tag = tag;
  heap->message = message;
  heap->irritants = irritants;
  return heap;
}

/** Make an exception with a C++ std::string message */
Value State::make_exception(Value tag, const std::string& cmessage, Value irritants) {
  Value message;
  AR_FRAME(this, tag, message, irritants);
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

 Value State::make_c_function(Value name, c_function_t addr, size_t min_arity, size_t max_arity,
    bool variable_arity) {
  if(max_arity == 0)
    max_arity = min_arity;
  AR_FRAME(this, name);
  CFunction *cfn = static_cast<CFunction*>(gc.allocate(CFUNCTION, sizeof(CFunction)));
  cfn->name = name;
  cfn->addr = addr;
  cfn->min_arity = min_arity;
  cfn->max_arity = max_arity;
  if(variable_arity)
    cfn->set_header_bit(Value::CFUNCTION_VARIABLE_ARITY_BIT);
  return cfn;
}

///// PAIRS

Value State::make_pair(Value car, Value cdr, size_t size) {
  AR_FRAME(this, car, cdr);
  Pair* heap = (Pair*) gc.allocate(PAIR, size);

  heap->data_car = car;
  heap->data_cdr = cdr;
  return heap;
}

/** Generate a pair with source code information */
Value State::make_src_pair(Value car, Value cdr, SourceLocation& loc) {
  Value pare = C_FALSE;
  AR_FRAME(this, pare, car, cdr);
  pare = make_pair(car, cdr, sizeof(Pair));
  pare.heap->set_header_bit(Value::PAIR_SOURCE_BIT);
  AR_ASSERT(pare.type() == PAIR);
  AR_ASSERT(pare.pair_has_source());
  pare.set_pair_src(loc);

  return pare;
}

Value State::make_char(char c) {
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
