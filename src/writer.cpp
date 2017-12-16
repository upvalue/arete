// writer.cpp - Output of Scheme objects and source code
// Includes pretty printing and printing of objects with circular references

// TODO: More user control over printing process. Disable pretty or cyclic printing.

// A quick explanation of how the fancy printing works

// Cyclic printing

// Each object has a 16 or 32 bit int in its header that can be used for this purpose. We
// dive through the object tree recursively setting this int to a value that is unique to each
// print. 

// This is conceptually similar to a garbage collector mark, but we have to use the ints to keep
// track of the order in which objects will actually be printed. 

// When actual printing occurs, we use a hash table that relates these unique ints to whether or
// not they've already been printed, as well as an integer that tells the user (or reader) which
// object they are visually.

// A value which has not been printed is printed fully along with this int, i.e. #0=<object text...>
// A value which has already been printed is simply printed as #0#

// Pretty printing

// We try to indent things nicely so that source code and large tables/records are more readable
// for the user. The printer does this by outputting \0 at places that are likely candidates for
// a line break + indent and \b where brackets should be de-dented.

// e.x. (define (a)\0 something-really-long)
// or 
// #<Record\0field1: "value1"\0field2: "value2"\b>

// If an object's contents do not fit on a single line easily, we'll do a line break + indent
// at these points. However, past a certain point (e.g. if we've used up a lot of horizontal space)
// It will give up and print them as a giant blob.

// Both of these combine to mean that pretty-printing is fairly expensive and causes a fair amount
// of heap allocation. They should probably only be used for debugging purposes and for
// serialization of objects with shared structure.

#include <fstream>
#include <math.h>

#include "arete.hpp"

namespace arete {

typedef std::pair<unsigned, bool> print_info_t;
typedef std::unordered_map<unsigned, print_info_t> print_table_t;

struct PrintState {
  PrintState(): try_pretty(true), print_shared(true),
    shared_objects_begin(1), shared_objects_i(1) {}
  ~PrintState() {}

  bool try_pretty;
  bool print_shared;
  unsigned shared_objects_begin, shared_objects_i;
  print_table_t* printed;
  unsigned printed_count;
  size_t row_width;
  size_t indent;

  /** Truncate table entries. If zero, all entries will be printed */
  size_t table_max;
  /** Amount to indent by, default 2. */
  unsigned indent_level;
};

std::ostream& operator<<(std::ostream& os, Type type) {
  switch(type) {
    case FIXNUM: return os << "fixnum";
    case CONSTANT: return os << "constant";
    case RECORD_TYPE: return os << "record-type";
    case RECORD: return os << "record";
    case BLOCK: return os << "block";
    case FLONUM: return os << "flonum";
    case STRING: return os << "string";
    case CHARACTER: return os << "character";
    case SYMBOL: return os << "symbol";
    case RENAME: return os << "rename";
    case VECTOR: return os << "vector";
    case VECTOR_STORAGE: return os << "vector-storage";
    case PAIR: return os << "pair";
    case EXCEPTION: return os << "exception";
    case FUNCTION: return os << "function";
    case BYTEVECTOR: return os << "bytevector";
    case UPVALUE: return os << "upvalue";
    case VMFUNCTION: return os << "vmfunction";
    case CFUNCTION: return os << "cfunction";
    case CLOSURE: return os << "closure";
    case TABLE: return os << "table";
    case FILE_PORT: return os << "file-port";
    default: return os << "unknown";
  }
}

// Simple writer. Will choke on objects with cyclic structure.

std::ostream& operator<<(std::ostream& os, Value v) {
  switch(v.type()) {
    case FIXNUM:  {
      return os << v.fixnum_value();
    }
    case CONSTANT: 
      switch(v.constant_value()) {
        case C_TRUE: return os << "#t";
        case C_NIL: return os << "()";
        case C_FALSE: return os << "#f";
        case C_EOF: return os << "#<eof>";
        case C_UNSPECIFIED: return os << "#<unspecified>";
        case C_SYNTAX: return os << "#<syntax>";
        case C_UNDEFINED: return os << "#<undefined>";
        default: return os << "<unknown constant " << ((ptrdiff_t) v.bits) << '>';
      }
    case RECORD: {
      RecordType* rt = v.as<Record>()->type;
      os << "#<" << rt->name.string_data();
      if(rt->field_count > 0) os << ' ';
      for(unsigned i = 0; i != rt->field_count; i++) {
        os << v.record_ref(i);
        if(i != rt->field_count - 1) os << ' ';
      }
      if(v.record_type().record_type_data_size() > 0) {
        os << " +" << v.record_type().record_type_data_size() << "b";
      }
      return os << '>';
    }

    case RECORD_TYPE: {
      return os << "#<record-type " << v.as<RecordType>()->name.string_data() << ' ' <<
        v.as<RecordType>()->field_count << ' ' << v.as<RecordType>()->data_size << '>';
    }
    case FLONUM: {
      double flo = v.flonum_value();
      // C++ prints 5.0 as 5, we want to print it with the decimal point always
      // so it's clear what type the value has
      if(floor(flo) == flo) {
        os << flo << ".0";
      } else {
        os << flo;
      }
      return os;
    }
    case SYMBOL:
      return os << v.symbol_name_data();
    case STRING: {
      const char* data = v.string_data();
      os << '"';
      for(size_t i = 0; i != v.string_bytes(); i++) {
        switch(data[i]) { 
          case '"': os << "\\\""; break;
          case '\\': os << "\\\\"; break;
          default: os << data[i]; break;
        }
      }
      return os << '"';;
    }
    case PAIR: {
      // if(v.pair_has_source()) os << '&';
      os << '(' << v.car();
      for(v = v.cdr(); v.type() == PAIR; v = v.cdr())  {
        os << ' ' << v.car();
      }
      if(v != C_NIL) {
        os << " . " << v;
      }
      return os << ')';
    }
    case CHARACTER: {
      os << "#\\";
      switch(v.character()) {
        case 27: return os << "backspace";
        case 127: return os << "delete";
        case '\a': return os << "alarm";
        case '\t': return os << "tab";
        case '\r': return os << "return";
        case '\n': return os << "newline";
        case ' ': return os << "space";
      }
      return os << v.character();
    }
    case FUNCTION: {
      // TODO this should include source information
      os << "#<" << (v.function_is_macro() ? "macro" : "function") << ' ';
      Value name = v.function_name();
      if(name == C_FALSE) {
        os << (void*) v.bits;
      } else {
        os << name;
      }
      return os << '>';
    }
    case CLOSURE:
    case VMFUNCTION: {
      if(v.heap_type_equals(CLOSURE)) {
        os << "#<closure ";
        v = v.closure_unbox();
      } else {
        os << "#<vmfunction ";
      }
      os << v.vm_function_name();
      os << ' ' << v.vm_function_min_arity() << '-';
      if(v.vm_function_variable_arity()) {
        os << "rest";
      } else {
        os << v.vm_function_max_arity();
      }
      return os << '>';
    }
    case CFUNCTION: {
      os << "#<cfunction ";
      return os << v.c_function_name().string_data() << '>';
    }
    case VECTOR:
      os << "#(";
      for(size_t i = 0; i != v.vector_length(); i++) {
        os << v.vector_ref(i);
        if(i != v.vector_length() - 1) os << ' ';
      }
      return os << ')';
    case FILE_PORT: {
      Value path = v.file_port_path();
      os << "#<";
      if(v.file_port_readable()) {
        os << "input-file-port";
      } else {
        os << "output-file-port";
      }
      return os << ' ' << path << '>';
    }
    case UPVALUE:
      return os << "#<upvalue " << v.bits << '>';
    case BYTEVECTOR: {
      os << "#u8(";
      for(size_t i = 0; i != v.bv_length(); i++) {
        os << (unsigned) v.bv_ref<unsigned char>(i);
        if(i + 1 != v.bv_length()) os << ' ';
      }
      return os << ")";
    }
      return os << "#< " << v.bits << ">";
    case RENAME: {
      Value env = v.rename_env(),
        sym = v.rename_gensym() == C_FALSE ? v.rename_expr() : v.rename_gensym();

      if(env == C_FALSE) {
        return os << "#R:" << sym;
      } else if(env.type() == TABLE) {
        return os << "#R:module:" << sym;
      } else {
        return os << "#R:local:" << sym;
      }
    }
    case TABLE: {
      os << '{';
      TableIterator ti(v);
      while(++ti) {
        os << ti.key() << ' ' << ti.value() << " . ";
      }
      os << '}';
      return os;
    }
      //return os << "#<table entries: " << v.as<Table>()->entries << '>';
    case EXCEPTION:
      os << "#<exception '" << v.exception_tag() << " " << v.exception_message();
      if(v.exception_irritants() != C_UNSPECIFIED) {
        os << ' ' << v.exception_irritants();
      }
      return os << '>';
    default: 
      return os << "#<unknown value type " << v.type() << " bits: " << v.bits << ">";
  }
  return os;
}

void State::lazy_load_source(size_t src) {
  if(src <= source_names.size()) {
    std::cerr << "arete: attempt to access source id " << src << " but only have " <<
      source_names.size() << "sources" << std::endl;
  }

  while(src <= source_contents.size()) {
    source_contents.push_back("");
  }

  const std::string& contents = source_contents[src];
  if(contents.empty()) {
    std::cout << "attempting to lazy load source code " << source_names[src] << std::endl;


    std::ifstream ifs(source_names[src]);
    if(!ifs) {
      std::cerr << "arete: attempt to access source id " << src << " (" << source_names[src] << ")"
        << " failed as file could not be opened" << std::endl;
    }

    std::ostringstream contents;
    contents << ifs.rdbuf();
    source_contents[src] = contents.str();
  }
}
 
void State::print_src_line(std::ostream& os, const SourceLocation& src, const char* color) {
  unsigned seek_line = 1;
  std::string source_line;
  char c;

  // Inefficient as all get-out, but as this only occurs on errors or during development
  // (e.g. printing the source of expanded stuff) we won't worry about it.
  
  if(src.source > 0 && !source_contents[src.source].empty()) {
    std::stringstream ss;

    ss >> std::noskipws;
    ss << source_contents[src.source];
    os << "At " << source_name(src.source) << " line " << src.line << std::endl;

    while(!ss.eof()) {
      if(seek_line == src.line) {
        unsigned line_position = (unsigned)ss.tellg();
        std::getline(ss, source_line);
        os << color << source_line << ARETE_COLOR_RESET << std::endl;

        unsigned i, j;
        // Print whitespace for each char in source line until the beginning of this element
        for(i = line_position, j = 0; i < src.begin; i++, j++) {
          os << ' ';
        }

        unsigned limit = (unsigned)(j + src.length);
        // If end_position is 0, we'll highlight the whole line this error occurred on.
        // Otherwise, try to highlight specific errors eg.
        // (hello #bad)
        //         ^
        for(; j != source_line.size() && ((src.length == 0) || j != limit); j++) {
          os << '^';
        }
        break;
      }

      ss >> c;
      if(c == '\n') {
        seek_line++;
      }
    }
  }
}

void State::print_exception(std::ostream& os, Value exc) {
  AR_TYPE_ASSERT(exc.type() == EXCEPTION);
  print_stack_trace(os);

  // TODO:
  // Rather than having uniquely formatted errors for each type;
  // we could attach a SourceLocation to Exception. Then, additional information can be pushed onto
  // the call stack and the SourceLocation can be overwritten.

  // The expander would have to be modified to extract source location information.

  // This also makes more sense for e.g. the VM where a source pair is not available at execution.

  // OR: We could have some kind of source-format thing which pulls source information when
  // possible. This makes sense and is easier to use from within Scheme code, and more extensible,
  // not requiring us to special case each type of exception.

  if(exc.exception_tag() == globals[State::S_EVAL_ERROR]) {
    os << "Evaluation error: " << exc.exception_message().string_data() << std::endl;
    if(exc.exception_irritants().type() == PAIR) {
      print_src_pair(os, exc.exception_irritants());
    }
    os << std::endl;
  } else if(exc.exception_tag() == globals[State::S_EXPAND_ERROR]
    || exc.exception_tag() == globals[State::S_SYNTAX_ERROR]) {

    Value irritants = exc.exception_irritants();

    (void) irritants;

    if(exc.exception_tag() == globals[State::S_EXPAND_ERROR]) {
      os << std::endl << "Error during expansion: ";
    } else if(exc.exception_tag() == globals[State::S_SYNTAX_ERROR]) {
      os << "Error in macro syntax: ";
    }

    os << ARETE_COLOR_BLUE << exc.exception_message().string_data() << ARETE_COLOR_RESET << std::endl;
  } else if(exc.exception_tag() == globals[State::S_READ_ERROR]) {
    os << exc.exception_message().string_data() << std::endl;
  } else {
    os << exc << std::endl;
  }
}

void State::print_stack_trace(std::ostream& os, bool clear) {
  if(stack_trace.size() > 0)
    os << "Stack trace: " << std::endl;

  for(size_t i = 0; i != stack_trace.size(); i++) {
    os << stack_trace.at(i).text;
    if(stack_trace.at(i).seen > 0) {
      os << " [x" << stack_trace.at(i).seen << ']';
    }
    os << std::endl;
  }

  if(clear)
    stack_trace.clear();
}

/** Print simple objects, print already-printed shared references, print the opening part of
 * a structure that refers to some shared object */
static bool pretty_print_shared_obj(State& state, std::ostream& os, Value v, PrintState& ps) {
  if(!v.print_recursive()) {
    os << v;
    return true;
  }

  unsigned cyc = v.heap->get_header_int();

  //std::cout << "CHECKING fOR SHARED OBJECT " << cyc << std::endl;

  // std::cout << cyc << ' ' << shared_objects_begin << std::endl;
  print_table_t::iterator it = ps.printed->find(cyc);

  if(it != ps.printed->end()) {
    if(it->second.second) {
      os << "#" << it->second.first << "#";
      return true;
    }
    os << "#" << it->second.first << "=";
    it->second.second = true;
  }
  return false;
}

static Value pretty_print_sub(State& state, std::ostream& os, Value v, PrintState& ps) {
  std::ostringstream os2;

  size_t start_indent1 = ps.indent;
  size_t start_row_width = ps.row_width;
  ps.row_width -= ps.indent;

  if(!pretty_print_shared_obj(state, os2, v, ps)) {
    if(v.type() == PAIR) {
      // LISTS

      os2 << '(';
      Value v2;

      unsigned indent_after = 0, attempt_indent = 0;

      Value kar = v.car();
      if(kar.heap_type_equals(RENAME)) {
        kar = kar.rename_expr();
      }

      if(kar == state.globals[State::S_DEFINE] || kar == state.globals[State::S_LAMBDA] ||
        kar == state.globals[State::S_IF]) {
        attempt_indent = 1;
        indent_after = 2;
      } else {
        attempt_indent = 1;
        indent_after = 1;
      }

      for(v2 = v; v2.type() == PAIR; v2 = v2.cdr()) {
        pretty_print_sub(state, os2, v2.car(), ps);
        if(attempt_indent) {
          if(attempt_indent++ >= indent_after && v2.cdr() != C_NIL) {
            ps.indent = start_indent1 + ps.indent_level;
            os2 << '\0';
          }
        }

        if(v2.cdr().type() == PAIR) os2 << ' ';
      }
      if(v2 != C_NIL) {
        os2 << " . ";
        (void) pretty_print_sub(state, os2, v2, ps);
      }
      os2 << ')';

    } else if(v.heap_type_equals(TABLE)) {
      os2 << '{';
      TableIterator ti(v);
      ps.indent += ps.indent_level;
      size_t entries = 0;
      while(++ti) {
        entries++;
        if(entries == ps.table_max) {
          os2 << '\0';
          os2 << "#+" << v.table_entries() << "... ";
          break;
        } else {
          os2 << '\0';
          //for(size_t i = 0; i != start_indent1; i++) os2 << ' ';
          pretty_print_sub(state, os2, ti.key(), ps);
          os2 << ' ';
          pretty_print_sub(state, os2, ti.value(), ps);
          os2 << " . ";
        }
      }
      if(entries != 0) {
        os2 << '\0';
        os2 << '\b';
      }
      //for(size_t i = 0; i != start_indent1; i++) os2 << ' ';
      os2 << '}';
    } else if(v.type() == RECORD) {
      // RECORDS
      os2 << "#<";

      Value type = v.record_type();

      os2 << type.record_type_name().string_data();

      os2 << '\0';

      //ps.indent = ((pos2 - pos)) + ps.indent_level;
      ps.indent += ps.indent_level;
      // ps.indent += ps.indent_level;
      for(unsigned i = 0; i != v.record_field_count(); i++) {
        os2 << ' ';

        os2 << type.record_type_field_names().list_ref(i) << ": ";
        (void) pretty_print_sub(state, os2, v.record_ref(i), ps);
        os2 << '\0';
      }

      if(type.as_unsafe<RecordType>()->data_size > 0) {
        os2 << " " << type.as_unsafe<RecordType>()->data_size << "b udata";
      }
      os2 << "\b" << '>';
    } else if(v.type() == VECTOR) {
      // VECTORS

      os2 << "#(";
      for(size_t i = 0; i != v.vector_length(); i++) {
        (void) pretty_print_sub(state, os2, v.vector_ref(i), ps);
        if(i != v.vector_length() - 1)
          os2 << ' ';
      }
      os2 << ')';
    }
  }

  std::string str(os2.str());

  if(ps.try_pretty && (str.size() > ps.row_width && ps.row_width > 60)) {
    for(size_t i = 0; i != str.size(); i++) {
      if(str[i] == '\0') {
        os << '\n';
        
        bool dedent = false;

        // Eat whitespace so it doesn't effect our indent.
        while(i != str.size()) {
          if(str[i] == '\0' || isspace(str[i])) {
            i++;
            continue;
          } else if(str[i] == '\b') {
            dedent = true;
            i++;
            continue;
          }
          i--;
          break;
        }

        for(size_t i = 0; i != (dedent ? start_indent1 : ps.indent); i++) {
          os << ' ';
        }
        continue;
      } else {
        os << str[i];
      }
    }

  } else {
    // Give up and just write it out as a big blob!
    for(size_t i = 0; i != str.size(); i++) {
      if(str[i] == '\0' || str[i] == '\b') {
        continue;
      } else {
        os << str[i];
      }
    }
  }

  ps.indent = start_indent1;
  ps.row_width = start_row_width;

  // if os2.size() > ps.row_width OR if row_width has gone down because we're running out of room
  // (say 80 or something)...

  // go through os2's string

  return C_UNSPECIFIED;
}

static Value pretty_print_clear_mark(State& state, Value v, PrintState& ps) {
  if(!v.print_recursive()) return C_UNSPECIFIED;

  if(v.heap->get_header_int() == 0) {
  }

  v.heap->set_header_int(0);

  if(v.type() == PAIR) {
    (void) pretty_print_clear_mark(state, v.car(), ps);
    (void) pretty_print_clear_mark(state, v.cdr(), ps);
  } else if(v.type() == RECORD) {
    for(unsigned i = 0; i != v.record_field_count(); i++) {
      (void) pretty_print_clear_mark(state, v.record_ref(i), ps);
    }
  } else if(v.type() == VECTOR) {
    // std::cout << "marking a vector" << std::endl;
    for(size_t i = 0; i != v.vector_length(); i++) {
      (void) pretty_print_clear_mark(state, v.vector_ref(i), ps);
    }
  } else if(v.type() == TABLE) {
    TableIterator ti(v);
    while(++ti) {
      pretty_print_clear_mark(state, ti.value(), ps);
    }
  }

  return C_UNSPECIFIED;
}

static Value pretty_print_mark(State& state, Value v, PrintState& ps) {
  if(!v.print_recursive()) {
    return C_UNSPECIFIED;
  }

  unsigned cyc = v.heap->get_header_int();
  // std::cout << v << ' ' << v.heap->get_header_int() << std::endl;

  // TODO Check for initial shared object
  if(cyc >= ps.shared_objects_begin) {
    print_table_t::iterator it = ps.printed->find(cyc);

    if(it != ps.printed->end()) {
      // std::cout << "marking " << cyc << "as seen twice" << std::endl;
      // This object has been seen twice and therefore is a cyclic object
      it->second.second = true;
    } else {
      // This isn't quite right. We don't want to print all objects that may be shared e.g.
      // (#<Obj1> #<Obj1> #<Obj1>) shouldn't result in the shared structure stuff
      // (although it is still re-readable like that, so maybe that's fine...)

      // Actually, that seems to be how most Schemes behave, so it's fine.

      // If we wanted to do this differently, we'd have to branch off print tables somehow.
      ps.printed->insert(std::make_pair(cyc, std::make_pair(ps.printed_count++, false)));
    }

    /*
    if(printed->find(cyc) == printed->end()) {
      std::cout << "INSERTING SHARED OBJECT " << cyc << std::endl;
      // This object has already been marked, so it's being printed circularly as well
      printed->insert(std::make_pair(cyc, std::make_pair(printed_count++, false)));
    }
    */
    return C_UNSPECIFIED;
  } else {
    v.heap->set_header_int(ps.shared_objects_i++);
    AR_ASSERT(v.heap->get_header_int() == ps.shared_objects_i - 1);
  }

  if(v.type() == PAIR) {
    (void) pretty_print_mark(state, v.car(), ps);
    (void) pretty_print_mark(state, v.cdr(), ps);
  } else if(v.type() == RECORD) {
    for(unsigned i = 0; i != v.record_field_count(); i++) {
      (void) pretty_print_mark(state, v.record_ref(i), ps);
    }
  } else if(v.type() == VECTOR) {
    // std::cout << "marking a vector" << std::endl;
    for(size_t i = 0; i != v.vector_length(); i++) {
      (void) pretty_print_mark(state, v.vector_ref(i), ps);
    }
  } else if(v.type() == TABLE) {
    TableIterator ti(v);
    while(++ti) {
      pretty_print_mark(state, ti.value(), ps);
    }
  } else {
    std::cerr << "pretty printer doesn't know how to mark object of type " << v.type() << std::endl;
    AR_ASSERT(!"pretty printer doesn't know how to mark object");
  }

  return C_UNSPECIFIED;
}

Value State::pretty_print(std::ostream& os, Value v) {
  std::unordered_map<unsigned, std::pair<unsigned, bool> >* printed = new std::unordered_map<unsigned, std::pair<unsigned, bool> >();

  PrintState ps;

  ps.printed_count = 0;
  ps.indent_level = 2;
  ps.row_width = 120;
  ps.indent = 0;
  ps.table_max = 5;

  ps.printed = printed;
  // Right now printing can't return an exception, but it might if we allow users to extend this
  Value _;

  _ = pretty_print_mark(*this, v, ps);

  // Print a toplevel repeated object object.
  if(!(v.atomic() || v.type() == SYMBOL || v.type() == TABLE)) {
    unsigned cyc = v.heap->get_header_int();

    std::unordered_map<unsigned, std::pair<unsigned, bool> >::iterator it = printed->find(cyc);

    if(it != printed->end() && it->second.second == true) {
      os << "#" << it->second.first << "=";
      it->second.second = true;
    }
  }

  for(print_table_t::iterator i = printed->begin(); i != printed->end(); i++) {
    i->second.second = false;
  }

  _ = pretty_print_sub(*this, os, v, ps);
  _ = pretty_print_clear_mark(*this, v, ps);

  delete printed;
  return _;

}

void State::print_table_verbose(Value tbl) {
  Table* table = tbl.as<Table>();
  std::cout << "#<table size_log2: " << (size_t) table->size_log2 << " entries: " << table->entries << 
    " max_entries: " << table->max_entries << std::endl;

  for(size_t i = 0; i != table->chains->length; i++) {
    Value chain = table->chains->data[i];
    if(chain != C_FALSE) {
      std::cout << "  chain " << i << ": ";
      std::cout << chain << std::endl;
    }
  }

  std::cout << '>' << std::endl;
}

bool State::print_src_pair(std::ostream& os, Value pair, const char* color) {
  if(pair.type() == PAIR && pair.pair_has_source()) {
    SourceLocation src(pair.pair_src());
    print_src_line(os, src, color);
    return true;
  }
  return false;
}

}
