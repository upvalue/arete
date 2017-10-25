// table.cpp - hash table functions

#include "arete.hpp"

namespace arete {

void State::table_setup(Value table, size_t size_log2) {
  VectorStorage* chains = 0;

  AR_FRAME(this, table);

  size_t size = 1 << size_log2;
  chains = static_cast<VectorStorage*>(make_vector_storage(size).heap);
  // In case of GC move
  Table *heap = table.as<Table>();
  heap->entries = 0;
  heap->size_log2 = size_log2;
  // Fill entries with #f
  chains->length = size;
  for(size_t i = 0; i != chains->length; i++) {
    chains->data[i] = C_FALSE;
  } 
  heap->chains = chains;
  // Pre-calculate max entries
  heap->max_entries = (Table::LOAD_FACTOR * size) / 100;
}

ptrdiff_t State::hash_index(Value table, Value key, bool& unhashable) {
  ptrdiff_t hash = hash_value(key, unhashable);
  return hash & (table.as<Table>()->chains->length - 1);
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
  while(chain.type() == PAIR) {
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

}