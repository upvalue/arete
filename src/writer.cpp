// writer.cpp - Output of Scheme objects and source code

#include "arete.hpp"

namespace arete {

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
    case VMFUNCTION: return os << "vmfunction";
    case CFUNCTION: return os << "cfunction";
    case TABLE: return os << "table";
    default: return os << "unknown";
  }
}

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
      os << "#<" << rt->name.string_data() << ' ';
      for(unsigned i = 0; i != rt->field_count; i++) {
        os << v.record_ref(i);
        if(i != rt->field_count - 1) os << ' ';
      }
      return os << '>';
    }

    case RECORD_TYPE: {
      return os << "#<record-type " << v.as<RecordType>()->name.string_data() << ' ' <<
        v.as<RecordType>()->field_count << ' ' << v.as<RecordType>()->data_size << '>';
    }
    case FLONUM:
      return os << v.flonum_value(); 
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
    case VMFUNCTION: {
      os << "#<vmfunction " << v.vm_function_name() << ' ' << (void*) v.bits;
      os << ' ' << v.vm_function_min_arity() << '-' << v.vm_function_max_arity();
      os << ' ' << (v.vm_function_variable_arity() ? "#t" : "#f") << '>';
      return os;
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
    case TABLE:
      return os << "#<table entries: " << v.as<Table>()->entries << '>';
    case EXCEPTION:
      os << "#<exception '" << v.exception_tag() << " " << v.exception_message();
      if(v.exception_irritants() != C_UNSPECIFIED) {
        os << ' ' << v.exception_irritants();
      }
      return os << '>';
    default: 
      return os << "<unknown>";
  }
  return os;
}
 
void State::print_src_line(std::ostream& os, const SourceLocation& src) {
  unsigned seek_line = 1;
  std::string source_line;
  char c;
  
  if(src.source > 0 && source_contents[src.source].size() > 0) {
    std::stringstream ss;

    ss >> std::noskipws;
    ss << source_contents[src.source];
    os << "At " << source_name(src.source) << " line " << src.line << std::endl;

    while(!ss.eof()) {
      if(seek_line == src.line) {
        unsigned line_position = ss.tellg();
        std::getline(ss, source_line);
        os << ARETE_COLOR_RED << source_line << ARETE_COLOR_RESET << std::endl;

        size_t i, j;
        // Print whitespace for each char in source line until the beginning of this element
        for(i = line_position, j = 0; i < src.begin; i++, j++) {
          os << ' ';
        }

        unsigned limit = j + src.length;
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

  if(exc.exception_tag() == globals[State::S_EVAL_ERROR]) {
    os << "Evaluation error: " << exc.exception_message().string_data() << std::endl;
    print_src_pair(os, exc.exception_irritants());
    os << std::endl;
  } else if(exc.exception_tag() == globals[State::S_EXPAND_ERROR]
    || exc.exception_tag() == globals[State::S_SYNTAX_ERROR]) {

    Value irritants = exc.exception_irritants();

    if(exc.exception_tag() == globals[State::S_EXPAND_ERROR]) {
      os << "Error during expansion: ";
    } else if(exc.exception_tag() == globals[State::S_SYNTAX_ERROR]) {
      os << "Error in macro syntax: ";
    }

    os << exc.exception_message().string_data() << std::endl;

    bool source_printed = false;

    if(irritants.list_length() == 1) {
      source_printed = print_src_pair(os, irritants.list_ref(0));
    } else if(irritants.list_length() == 2) {
      source_printed = print_src_pair(os, irritants.list_ref(1));
    }

    os << std::endl;

    if(!source_printed) {
      os << "Offending expression: " << irritants.list_ref(0) << std::endl;
    }
  } else {
    os << exc << std::endl;
  }
}

void State::print_gc_stats(std::ostream& os) {
  os << (gc.heap_size / 1024) << "kb in use after " << gc.collections << " collections and "
      << gc.allocations << " allocations " << std::endl;

#ifdef ARETE_BENCH_GC
    std::cout << (gc_collect_timer / 1000) << "ms in collection" << std::endl;
#endif
  }


void State::print_stack_trace(std::ostream& os, bool clear) {
  if(stack_trace.size() > 0)
    os << "Stack trace: " << std::endl;
  for(size_t i = 0; i != stack_trace.size(); i++)
    os << stack_trace.at(i) << std::endl;


  if(clear)
    stack_trace.clear();
}

Value State::pretty_print_sub(std::ostream& os, Value v, unsigned& printed_count, std::vector<int>& printed) {
  // This is the initial call to pretty print
  if(shared_objects_i < shared_objects_begin) {
    shared_objects_i = shared_objects_begin;
  }

  // TODO Detect integer overflows and handle them gracefully.

  // (define x )
  if(v.type() == CONSTANT || v.type() == FIXNUM) {
    os << ARETE_COLOR_BLUE << v << ARETE_COLOR_RESET;
  } else if(v.type() == PAIR) {
    unsigned cyc = v.heap->get_shared_count();

    // std::cout << "encountered object with cycle count: " << v.heap->get_shared_count() << std::endl;
    // This object has already been printed
    if(cyc > shared_objects_begin) {
      for(size_t i = 0; i != printed.size(); i++) {
        if(printed[i] == cyc) {
          os << "#" << i << "#";
          return C_UNSPECIFIED;
        }
      }

      printed.push_back(cyc);
      os << "#" << (printed.size() - 1) << "#";

      // os << "#" << (cyc - 1) << "#";
      return C_UNSPECIFIED;
    } 

    // When an object is first printed,
    // #0=((#0#))

    // (list x #f)
    // (list y x)
    // (set-car! x y)
    // print x
    // shared_count x => 1
    // shared_count y => 2
    // We want to print x as #0=, print y, and print its car as #0# indicating that it is a circular
    // reference.

    // To do that, we'll push back 1 here.
    // When we encounter cyc object 1 above
    // we'll search through the printed vector, and print its indice
    // right?
    // but this prints every object, not just cyclic objects, so that doesn't work.

    // What if we save the size of the printed array here.
    // if we encounter a shared object in a subcall we push to printed array
    // then we print 


    // printed.push_back(shared_objects_i);
    
    std::ostringstream oss;

    v.heap->set_shared_count(++shared_objects_i);
    AR_ASSERT(v.type() == PAIR);

    // os << v.heap->get_shared_count() << "=(";
    oss << '(';
    Value v2, exc = pretty_print_sub(oss, v.car(), printed_count, printed);
    if(exc.is_active_exception()) return exc;
    for(v2 = v.cdr(); v2.type() == PAIR; v2 = v2.cdr()) {
      oss << ' ';
      exc = pretty_print_sub(oss, v2.car(), printed_count, printed);
    }

    if(exc.is_active_exception()) return exc;

    if(v2 != C_NIL) {
      oss << " . ";
      exc = pretty_print_sub(oss, v2, printed_count, printed);
    }
    
    if(exc.is_active_exception()) return exc;

    oss << ')';

    if(printed.size() != printed_count) {
      os << "#" << printed_count << "=";
      printed_count++;
    }

    os << oss.str();
  } else {
    os << v;
  }
  return C_UNSPECIFIED;
}

Value State::pretty_print(std::ostream& os, Value v) {
  std::vector<int> printed;
  unsigned printed_count = 0;
  Value ret =  pretty_print_sub(os, v, printed_count, printed);
  shared_objects_begin = shared_objects_i;
  return ret;

}

}