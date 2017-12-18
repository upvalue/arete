// table.cpp - hash table functions

#include "arete.hpp"

namespace arete {

bool TableIterator::operator++() {
try_again:
  // If we have a current chain
  if(chain != C_FALSE && chain != C_NIL) {
    cell = chain.car();
    chain = chain.cdr();
    return true;      
  }

  if(i == table.as<Table>()->chains->length) {
    chain = C_FALSE;
    return false;
  }

  // Go to the next chain
  chain = table.as<Table>()->chains->data[i++];
  goto try_again;
}

static const unsigned LOAD_FACTOR = 85;

ptrdiff_t wang_integer_hash(ptrdiff_t key) {
  key = (~key) + (key << 21);
  key = key ^ (key >> 24);
  key = (key + (key << 3)) + (key << 8); 
  key = key ^ (key >> 14);
  key = (key + (key << 2)) + (key << 4); 
  key = key ^ (key >> 28);
  key = key + (key << 31);
  return key;
}

static ptrdiff_t x31_string_hash(const char* s) {
  ptrdiff_t h = *s;
  if(h) for(++s; *s; ++s) h = (h << 5) - h + *s;
  return h;
}

static ptrdiff_t hash_value(Value x, bool& unhashable) {
  unhashable = false;
  switch(x.type()) {
    case STRING:
      return x31_string_hash(x.string_data());
    case SYMBOL:
#if ARETE_GC_STRATEGY == ARETE_GC_SEMISPACE
      // If symbols can be moved (semispace), we need to hash the string, otherwise we can just
      // hash the pointer, so this will fall-through to the below integer hash in the incremental
      // collector
      return x31_string_hash(x.symbol_name_data());
#endif
    case FIXNUM:
    case CONSTANT:
      return wang_integer_hash(x.bits);
    default:
      unhashable = true;
      return 0;
  }
}

void State::table_setup(Value table, size_t size_log2) {
  VectorStorage* chains = 0;

  AR_FRAME(this, table);

  size_t size = (unsigned)(1 << size_log2);
  chains = static_cast<VectorStorage*>(make_vector_storage(size).heap);
  // In case of GC move
  Table *heap = table.as<Table>();
  heap->entries = 0;
  heap->size_log2 = (unsigned char)size_log2;
  // Fill entries with #f
  chains->length = size;
  for(size_t i = 0; i != chains->length; i++) {
    chains->data[i] = C_FALSE;
  } 
  heap->chains = chains;
  // Pre-calculate max entries
  heap->max_entries = (LOAD_FACTOR * size) / 100;
}

ptrdiff_t State::hash_index(Value table, Value key, bool& unhashable) {
  ptrdiff_t hash = hash_value(key, unhashable);
  return hash % (table.as<Table>()->chains->length - 1);
}
  
void State::table_grow(Value table) {
  Value old_chains_ref, chain;
  AR_FRAME(this, table, old_chains_ref, chain);
  old_chains_ref = table.as<Table>()->chains;
  table_setup(table, table.as<Table>()->size_log2 + 1);
  // Insert all old values
  VectorStorage* old_chains = old_chains_ref.as<VectorStorage>();
  for(size_t i = 0; i != old_chains->length; i++) {
    chain = old_chains->data[i];
    while(chain.type() == PAIR) {
      table_insert(table, chain.caar(), chain.cdar());
      old_chains = old_chains_ref.as<VectorStorage>();
      chain = chain.cdr();
    }
  }
}

Value State::unhashable_error(Value irritant) {
  std::ostringstream os;
  os << " value " << irritant << " is unhashable";
  return type_error(os.str());
}


Value State::table_get_cell(Value table, Value key) {
  bool unhashable;
  ptrdiff_t index = hash_index(table, key, unhashable);
  if(unhashable) return unhashable_error(key);
  Value chain = C_FALSE;
  chain = table.as<Table>()->chains->data[index];
  while(chain.heap_type_equals(PAIR)) {
    if(equals(chain.caar(), key)) {
      return chain.car();
    }
    chain = chain.cdr();
  }
  return C_FALSE;
}

Value State::table_set(Value table, Value key, Value value) {
  Value cell = table_get_cell(table, key);
  if(cell.is_active_exception()) return cell;
  if(cell != C_FALSE) {
    cell.set_cdr(value);
    return C_TRUE;
  } else {
    return table_insert(table, key, value);
  }
  return C_FALSE;
}

Value State::table_get(Value table, Value key, bool& found) {
  Value cell = table_get_cell(table, key);
  if(cell != C_FALSE) {
    found = true;
    return cell.cdr();
  } else {
    found = false;
    return C_FALSE;
  }
}

Value State::table_insert(Value table, Value key, Value value) {
  AR_TYPE_ASSERT(table.type() == TABLE);
  Value chain;
  AR_FRAME(this, table, key, value, chain);

  Table* htable = table.as<Table>();

  if(htable->entries >= htable->max_entries) {
    table_grow(table);
  }

  bool unhashable;
  ptrdiff_t index = hash_index(table, key, unhashable);
  if(unhashable) return unhashable_error(value);

  // Build chain
  chain = make_pair(key, value);

  if(key.type() == STRING) {
    key = string_copy(key);
  }

  htable = table.as<Table>();

  // Handle collision
  if(htable->chains->data[index] != C_FALSE) {
    chain = make_pair(chain, htable->chains->data[index]);
  } else {
    chain = make_pair(chain, C_NIL);
  }

  // Insert chain
  htable = table.as<Table>();
  htable->chains->data[index] = chain;
  htable->entries++;

  return C_UNSPECIFIED;
}


Value State::make_table(size_t size_log2 ) {
  Value table = static_cast<Table*>(gc.allocate(TABLE, sizeof(Table)));
  AR_FRAME(this, table);

  table_setup(table, size_log2);

  return table;
}

///// SCHEME FUNCTIONS

Value fn_make_table(State& state, size_t argc, Value* argv) {
  return state.make_table();
}
AR_DEFUN("make-table", fn_make_table, 0);

Value fn_table_ref(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-ref";

  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);
  AR_FN_ASSERT_ARG(state, 1, "to be hashable", argv[1].hashable());

  bool found;
  Value result = state.table_get(argv[0], argv[1], found);
  if(!found) return argc == 3 ? argv[2] : C_FALSE;
  return result;
}
AR_DEFUN("table-ref", fn_table_ref, 2, 3);

Value fn_table_set(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-set!";

  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);
  AR_FN_ASSERT_ARG(state, 1, "to be hashable", argv[1].hashable());

  return state.table_set(argv[0], argv[1], argv[2]);
}
AR_DEFUN("table-set!", fn_table_set, 3);

Value fn_table_delete(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-delete!";

  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);
  AR_FN_ASSERT_ARG(state, 1, "to be hashable", argv[1].hashable());

  Value tbl = argv[0], key = argv[1];
  bool unhashable;

  ptrdiff_t index = state.hash_index(tbl, key, unhashable);
  Value chain = C_FALSE, prev = C_FALSE;

  chain = tbl.as<Table>()->chains->data[index];
  
  while(chain.heap_type_equals(PAIR)) {
    if(state.equals(chain.caar(), key)) {
      if(prev == C_FALSE) {
        tbl.as<Table>()->chains->data[index] = chain.cdr();
      } else {
        prev.set_cdr(chain.cdr());
      }
      break;
    }
    prev = chain;
    chain = chain.cdr();
  }
  
  return C_UNSPECIFIED;
}
AR_DEFUN("table-delete!", fn_table_delete, 2);

Value fn_table_entries(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-entries";
  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);
  return Value::make_fixnum(argv[0].table_entries());
}
AR_DEFUN("table-entries", fn_table_entries, 1);

static Value fn_table_map_impl(const char* fn_name, bool map, State& state, size_t argc, Value* argv) {
  AR_FN_ASSERT_ARG(state, 0, "to be applicable", argv[0].applicable());
  AR_FN_EXPECT_TYPE(state, argv, 1, TABLE);

  Value fn = argv[0], lst = C_NIL, tmp;
  TableIterator ti(argv[1]);
  AR_FRAME(state, tmp, fn, lst, ti.table, ti.chain, ti.cell);

  while(++ti) {
    Value argv[2] = {ti.key(), ti.value()};
    tmp = state.apply(fn, 2, argv);
    if(tmp.is_active_exception()) return tmp;
    if(map) {
      lst = state.make_pair(tmp, lst);
    }
  }
  return map ? lst : C_UNSPECIFIED;
}

Value fn_table_map(State& state, size_t argc, Value* argv) {
  return fn_table_map_impl("table-map", true, state, argc, argv);
}
AR_DEFUN("table-map", fn_table_map, 2);

Value fn_table_for_each(State& state, size_t argc, Value* argv) {
  return fn_table_map_impl("table-for-each", false, state, argc, argv);
}
AR_DEFUN("table-for-each", fn_table_for_each, 2);

Value fn_table_copy(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "table-copy";
  AR_FN_EXPECT_TYPE(state, argv, 0, TABLE);

  Value table = argv[0];
  Value copy, chain;

  AR_FRAME(state, table, copy);

  copy.heap = state.gc.allocate(TABLE, sizeof(Table));

  //for(size_t i = 0; )

  return C_UNSPECIFIED;
}
AR_DEFUN("table-copy", fn_table_copy, 2);

}
