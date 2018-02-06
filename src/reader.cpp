// reader.cpp - S-Expression reader.

#define AR_LOG_READ(msg) AR_LOG((AR_LOG_TAG_READER), "read", msg)

#include <math.h>

#include "arete.hpp"

// TODO: Record literals
// TODO: Read shared structure

namespace arete {

// Some explication is probably needed on how this works.

// Essentially, it looks ahead one token at a time, and sometimes
// saves the source code position on the stack. This is necessary to attach helpful source code
// information to programs at runtime, which it does by noting the source (file or string) a list
// was read from, and occasionally saving its line and position (in terms of bytes) in the stream
// on the stack

// This allows us to produce lists with the following level of information

// (define 5 bad-value)

// The (define 5 #t) pair will have the entire list noted down in its SourceLocation

// The sub-cell containing 5 will have the number five noted down in its SourceLocation

// Thus we can report errors with a level of granularity generally not provided by Scheme systems.

NumberReader::NumberReader(State& state_, const std::string& string_): state(state_), string(string_),
    radix(10), radix_set(false), exact(true), exact_set(false), flonum_allowed(true),
    error_desc("number reader internal error") {

}

bool NumberReader::set_radix(int radix_) {
  if(radix_set) {
    error_desc = "radix specified more than once";
    return false;
  }
  if(radix_ != 10) flonum_allowed = false;
  radix = radix_;
  radix_set = true;
  return true;
}

bool NumberReader::set_radix_param(int radix_) {
  switch(radix_) {
    case 10: case 8: case 2: case 16: radix = radix_; return true;
    default:  {
      std::ostringstream os;
      os << "unsupported radix " << radix_;
      error_desc = os.str();
      return false;
    }
  }
}

bool NumberReader::set_exact(bool exact_) {
  if(exact_set) {
    error_desc = "number exactitude specified more than once";
    return false;
  }
  exact = exact_;
  exact_set = true;
  return true;
}

bool NumberReader::consume_numeric_directive(size_t& i) {
  if(string.size() > i+1) {
    if(string[i] == '#') {
      char c = string[i+1];
      i += 2;;
      switch(c) {
        case 'e': return set_exact(true); break;
        case 'i': return set_exact(false); break;
        case 'x': return set_radix(16); break;
        case 'b': return set_radix(2); break;
        case 'o': return set_radix(8); break;
        case 'd': return set_radix(10); break;
        default:
          error_desc = "unknown numeric prefix ";
          error_desc += string[i+1];
          return false;
          break;
      }
    }
  }
  return true;
}

bool NumberReader::check_radix_gte(int at_least, char offend) {
  if(radix < at_least) {
    std::ostringstream os;
    os << "found higher number " << offend << " in number of radix " << radix;
    error_desc = os.str();
    return false;
  }
  return true;
}

/** Read a number. Returns C_FALSE on error. */
Value NumberReader::read() {
  size_t i = 0;
  // True if scientific notiation has been seen and an additional
  // number needs to be read
  bool is_exponent = false;
  bool negative = false;

  if(!consume_numeric_directive(i)) return C_FALSE;
  if(!consume_numeric_directive(i)) return C_FALSE;

  if(i == string.size()) {
    std::ostringstream os;
    os << "no number found after numeric directives " << string;
    error_desc = os.str();
    return C_FALSE;
  }

  if(string[i] == '-') {
    negative = true;
    i++;
  } else if(string[i] == '+') {
    i++;
  }

  // True if a decimal has been seen and thus should be read as a floating
  // point number (floats can occur in other scenarios, e.g. #i5 becomes 5.0,
  // but these are not parsed as floats)
  bool is_float = false;

  double flonum = 0.0;
  ptrdiff_t fixnum = 0;

  for(size_t j = i; j != string.size(); j++) {
    if(string[j] == '.') {
      if(!flonum_allowed) {
        error_desc = "floating point numbers must be decimals";
        return C_FALSE;
      } 
      if(exact_set) {
        error_desc = "implementation error: exact fractions not supported";
        return C_FALSE;
      }
      is_float = true;
    }
  }

  if(is_float) {
    std::stringstream ss;
    for(; i != string.size(); i++) {
      if(string[i] == 'e') {
        is_exponent = true;
        i++;
        break;
      }
      if(!isdigit(string[i]) && string[i] != '.') {
        error_desc = "invalid numeric syntax ";
        error_desc += string[i];
        return C_FALSE;
      }
      ss << string[i];
    }
    ss >> flonum;
    //return state.make_flonum(number);
  } else {
    ptrdiff_t place = 0;
    size_t begin = i;
    for(; i != string.size(); i++) {
      char c = string[i];
      switch(c) {
        case '0': place = 0; break;
        case '1': place = 1; break;
        case '2': place = 2; if(!check_radix_gte(8, c)) return C_FALSE; break;
        case '3': place = 3; if(!check_radix_gte(8, c)) return C_FALSE; break; 
        case '4': place = 4; if(!check_radix_gte(8, c)) return C_FALSE; break; 
        case '5': place = 5; if(!check_radix_gte(8, c)) return C_FALSE; break; 
        case '6': place = 6; if(!check_radix_gte(8, c)) return C_FALSE; break; 
        case '7': place = 7; if(!check_radix_gte(8, c)) return C_FALSE; break; 
        case '8': place = 8; if(!check_radix_gte(10, c)) return C_FALSE; break;
        case '9': place = 9; if(!check_radix_gte(10, c)) return C_FALSE; break;
        case 'A': case 'a': place = 10; if(!check_radix_gte(10, c)) return C_FALSE; break;
        case 'B': case 'b': place = 11; if(!check_radix_gte(16, c)) return C_FALSE; break;
        case 'C': case 'c': place = 12; if(!check_radix_gte(16, c)) return C_FALSE; break;
        case 'D': case 'd': place = 13; if(!check_radix_gte(16, c)) return C_FALSE; break;
        case 'E': case 'e': {
          if(radix != 16) {
            // This is an exponent
            i++;
            is_exponent = true;
            goto exponent;
          }
          place = 14;
          if(!check_radix_gte(16, c))
            return C_FALSE;
          break;
        }
        case 'F': case 'f': place = 15; if(!check_radix_gte(16, c)) return C_FALSE; break;
        default: {
          std::ostringstream os;
          os << "number reader encountered unknown character " << c << " while reading number " << string;
          error_desc = os.str();
          return C_FALSE;
        }
      }

      if(i == begin) {
        fixnum = place;
      } else {
        fixnum *= radix;
        fixnum += place;
      }
    }
  }

  exponent:

  // Parse exponent notation
  if(is_exponent) {
    ptrdiff_t exponent_amount = 0;

    bool exp_negative = false;

    if(string[i] == '-') {
      exp_negative = true;
      if(!is_float) {
        flonum = (double) fixnum;
        is_float = true;
      }
      i++;
    }

    // TODO: ADD NEGATIVE
    for(; i < string.size(); i++) {
      bool valid = string[i] >= '0' && string[i] <= '9';
      if(!valid) {
        std::ostringstream os;
        os << "number reader encountered " << string[i] << " after e in exponent notation but only 0-9 allowed";
        error_desc = os.str();
        return C_FALSE;
      }
      exponent_amount *= 10;
      exponent_amount += (string[i] - '0');
    }

    if(is_float) {
      while(exponent_amount--) {
        if(exp_negative) {
          flonum /= 10;
        } else {
          flonum *= 10;
        }
      }
    } else {
      while(exponent_amount--) {
        fixnum *= 10;
      }
    }
  }

  if(!exact || (!is_float && is_exponent && !exact_set)) {
    is_float = true;
    flonum = (double) fixnum;
  } 

  if(is_float) {
    return state.make_flonum(negative ? -flonum : flonum);
  } else {
    return Value::make_fixnum(negative ? -fixnum : fixnum);
  }
}

XReader::XReader(State& state_, std::istream& is_, bool slurp_source, const std::string& desc):
    state(state_), is(is_), source(0), position(0),
    line(1), column(1), active_error(C_FALSE), quasiquote_renaming(false) {
  
  is >> std::noskipws;

  if(desc.compare("anonymous") == 0) {
    source = 0;
  } else {
    source = state.register_source(desc, is);
  }
}


unsigned State::register_source(const std::string& path, std::istream& is) {
  AR_LOG_READ("registering source " << path);
  std::ostringstream contents;
  contents << is.rdbuf();
  is.seekg(0, std::ios::beg);

  source_names.push_back(path);
  source_contents.push_back(contents.str());

  return (unsigned)(source_names.size() - 1);
}

static bool is_symbol_initial(char c) {
  if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) return true;

  switch(c) {
    case '!': case '$': case '%': case '&': case '*': case '/':  case ':':
    case '<': case '=': case '>': case '?': case '^': case '_': case '~':
    case '@':
      return true;
    default: return false;
  }
}

static bool is_symbol(char c) {
  if(is_symbol_initial(c)) return true;
  if(c >= '0' && c <= '9') return true;
  return c == '.' || c == '+' || c == '-';
}

Value XReader::make_src_pair(Value kar, Value kdr, unsigned line, unsigned position, unsigned length) {
  if(source == 0) {
    return state.make_pair(kar, kdr);
  }
  
  SourceLocation src;
  src.source = source;
  src.line = line;
  src.begin = position;
  src.length = length;
  AR_ASSERT(src.source < state.source_names.size());

  return state.make_src_pair(kar, kdr, src);
}

Value XReader::read_error(const std::string& message, unsigned start_line, unsigned start_position,
    unsigned end_position) {

  AR_LOG_READ("read_error " << start_line << ' ' << start_position << ' ' << message << std::endl);
  SourceLocation src;
  src.source = source;
  src.line = start_line;
  src.begin = start_position;
  // If end_position is zero, we'll just underline the whole line
  if(start_position > end_position) {
    src.length = 0;
  } else {
    src.length = end_position - start_position;
  }
  
  std::ostringstream os;
  os << "Reader error: " << message << std::endl;
  state.print_src_line(os, src);
  return active_error = state.make_exception(state.globals[State::S_READ_ERROR], os.str());
}

Value XReader::unexpected_eof(const std::string& message, unsigned start_line,
    unsigned start_position, unsigned end_position = 0) {
  std::ostringstream os;
  os << "unexpected end of input " << message;
  return read_error(os.str(), start_line, start_position, end_position);
}

static bool is_valid_number(char c) {
  return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

XReader::TokenType XReader::tokenize_number(bool sharp, char start) {
  char c = '#';

  // Read numeric directives
  if(sharp) {
    do {
      if(c == '#')  {
        if(!sharp) {
          getc(c);
        } else {
          sharp = false;
        }
        buffer += c;
        if(!peekc(c)) {
          unexpected_eof("after # syntax", token_start_line, position, position+1);
          return TK_ERROR;
        }
        switch(c) {
          case 'e': case 'd': case 'x': case 'o': case 'b': case 'i': {
            getc(c);
            buffer += c;
            break;
          }
          default: {
            read_error("unknown numeric sharp syntax", token_start_line, position - 1, position);
            return TK_ERROR;
          }
        }
      } else if(!is_valid_number(c) && c != '-' && c != '.') {
        read_error("expected digits after numeric syntax", token_start_line, token_start_position, position);
        return TK_ERROR;
      } else {
        break;
      }
    } while(peekc(c));
  }

  if(start != '\0') {
    buffer += start;
  }

  while(peekc(c)) {
    //std::cout << c << std::endl;
    if(is_valid_number(c) || c == '-' || c == '+' || c == '.') {
      getc(c);
      buffer += c;
      // std::cout << "Buffer += " << c << std::endl;
    } else {
      // std::cout << "Tokenized number" << buffer << std::endl;
      return TK_NUMBER;
    }
  }
  // std::cout << "Tokenized number" << buffer << std::endl;

  return TK_NUMBER;
}

void XReader::tokenize_symbol(bool tokenize_sharp) {
  char c;

  while(peekc(c)) {
    if(is_symbol(c) || (tokenize_sharp && c == '#')) {
      getc(c);
      buffer += c;
    } else {
      return;
    }
  }
}

void XReader::tokenize_string() {
  char c;

	// TODO: MSVC error? Why.
  //AR_ASSERT(is.tellg() == token_start_position);
  eatc();

  while(peekc(c)) {
    if(c == '"') {
      eatc();
      return;
    } else if(c == '\\') {
      eatc();
      getc(c);
      if(c == 'n') {
        buffer += '\n';
      } else if(c == 't') {
        buffer += '\t';
      } else if(c == 'r') {
        buffer += '\r';
      } else if(c == '"' || c == '\\') {
        buffer += c;
      } else {
        std::ostringstream os;
        os << "unknown string escape \\" << c;
        read_error(os.str(), token_start_line, position - 2, position);
      }
    } else {
      getc(c);
      buffer += c;
    }
  }

  unexpected_eof("in string", token_start_line, token_start_position);
}

XReader::TokenType XReader::next_token() {
  AR_ASSERT(active_error == C_FALSE);
  char c;
  buffer.clear();
  while(peekc(c)) {
    token_start_line = line;
    token_start_position = position;
    if(c >= '0' && c <= '9') {
      return tokenize_number(false);
    } else if(c == '+' || c == '-') {
      // These can start a number, but they can also start a symbol
      char start = c, c2;
      getc(c);
      if(peekc(c2)) {
        if((c2 >= '0' && c2 <= '9') || c2 == '.') {
          return tokenize_number(false, start);
        } else {
          buffer += c;
          tokenize_symbol();
          return TK_SYMBOL;
        }
      } else {
        // Single + or - at end of file.
        buffer += c;
        tokenize_symbol();
        return TK_SYMBOL;
      }
    } else if(c == ' ' || c == '\r' || c == '\t' || c == '\n' || c == 12) {
      eatc();
    } else if(c == '#') {
      // # Sharp syntax
      eatc();

      char c2;
      if(!peekc(c2)) {
        unexpected_eof("after # read syntax", line, position - 1);
        return TK_ERROR;
      }

      // Numeric syntax
      if(c2 == 'x') return tokenize_number(true);
      else if(c2 == 'o') return tokenize_number(true);
      else if(c2 == 'e') return tokenize_number(true);
      else if(c2 == 'i') return tokenize_number(true);
      else if(c2 == 'b') return tokenize_number(true);
      else if(c2 == 'd') return tokenize_number(true);
      else {
        eatc();
        if(c2 == 't') { return_constant = C_TRUE; return TK_CONSTANT; }
        else if(c2 == 'f') { return_constant = C_FALSE; return TK_CONSTANT; }
        else if(c2 == '`') return TK_QUASIQUOTE_RENAMING;
        else if(c2 == '\'') return TK_RENAME;
        else if(c2 == '(') return TK_VECTOR_OPEN;
        else if(c2 == '\\') {
          // Character literals
          if(!peekc(c2)) {
            unexpected_eof("after #\\ character literal", token_start_line, token_start_position);
            return TK_ERROR;
          }

          if(c2 == ' ')  {
            buffer += "space";
            return TK_CHARACTER;
          }

          getc(c2);
          buffer += c2;
          tokenize_symbol();
          
          return TK_CHARACTER;
        } else if(c2 == '#') {
          // Fully qualified symbols, e.g. ##arete#car
          buffer += "##";
          tokenize_symbol(true);
          return TK_SYMBOL;
        } else if(c2 == '!') {
          // Special symbols: #!optional, #!key, #!keys, #!rest
          tokenize_symbol(true);

          if(buffer.compare("optional") == 0) {
            return_constant = C_OPTIONAL_OBJECT;
          } else if(buffer.compare("key") == 0) {
            return_constant = C_KEY_OBJECT;
          } else if(buffer.compare("keys") == 0) {
            return_constant = C_KEYS_OBJECT;
          } else if(buffer.compare("rest") == 0) {
            return_constant = C_REST_OBJECT;
          } else if(buffer.compare("arete-image") == 0) {
            read_error("reader encountered an image file; images must always be the first argument", token_start_line, token_start_position, position);
            return TK_ERROR;
          } else {
            read_error("unknown #! constant", token_start_line, token_start_position, position);
            return TK_ERROR;
          }
          return TK_CONSTANT;
        } else if(c2 == ';') {
          // Expression comments
          if(!peekc(c2)) {
            unexpected_eof("after #; expression comment", line, position - 2);
            return TK_ERROR;
          }
          return TK_EXPRESSION_COMMENT;
        } else if(c2 == '|') {
          // Multiline comments
          size_t comment_depth = 1;

          while(!is.eof()) {
            getc(c);
            if(c == '#') {
              peekc(c);
              if(c == '|') {
                eatc();
                comment_depth++;
              }
            } else if(c == '|') {
              peekc(c);
              if(c == '#') {
                getc(c);
                if(--comment_depth == 0) break; 
              }
            }
          }
          if(comment_depth > 0) {
            unexpected_eof("in #| multiline comment", token_start_line, token_start_position);
            return TK_ERROR;
          }
        } else if(c2 == '&') {
          // Support for image serialization
          unsigned ts = token_start_line, tp = token_start_position;
          tokenize_symbol(true);
          if(buffer.compare("cfn") == 0) {
            getc(c2);

            if(is.eof()) {
              unexpected_eof("in #&cfn", ts, tp);
              return TK_ERROR;
            } else if(c2 != '(') {
              read_error("#&cfn must be followed by (", ts, tp, position);
              return TK_ERROR;
            }

            buffer.clear();
            tokenize_symbol(false);
            std::cout << buffer << std::endl;

            getc(c2);

            if(is.eof()) {
              unexpected_eof("in #&cfn", ts, tp);
              return TK_ERROR;
            } else if(c2 != ')') {
              read_error("#&cfn must be followed by )", ts, tp, position);
              return TK_ERROR;
            }

            Value sym = state.get_symbol(buffer);
            Value builtins = state.get_global_value(State::G_BUILTIN_TABLE);

            bool found;
            Value res = state.table_get(builtins, sym, found);
            if(!found) {
              read_error("cfn not defined", ts, tp, position);
              return TK_ERROR;
            }

            return_constant = res;
            return TK_CONSTANT;
          } else if(buffer.compare("vfn") == 0) {
            read_error("vfn not supported", ts, tp, position);
            return TK_ERROR;
          } else {
            read_error("unknown builtin type", ts, tp, position);
            return TK_ERROR;
          }
        } else if(c2 >= '0' && c2 <= '9') {
          // Shared object syntax
          // e.g. '(#0=#t #0#) => '(#t #t)
          buffer += c2;
          bool define = false;
          while(true) {
            if(!peekc(c)) {
              unexpected_eof("in shared object syntax", token_start_line, token_start_position);
              return TK_ERROR;
            };

            if(c >= '0' && c <= '9') {
              getc(c);
              buffer += c;
            } else if(c == '=') {
              eatc();
              define = true;
              break;
            } else if(c == '#') {
              eatc();
              break;
            } else {
              read_error("expect either # or = after number indicating a shared object",
                line, position - 1, position);
              return TK_ERROR;
            }
          }

          // Read decimal number
          /*
          std::cout << buffer << std::endl;
          read_error("shared object syntax not supported", token_start_line, token_start_position, position);
          return TK_ERROR;
          */
          return define ? TK_SHARED_DEFINE : TK_SHARED_REF;
          // Check for = definition
        } else {
          (void) read_error("unknown # syntax", token_start_line, token_start_position, position);
          return TK_ERROR;
        }
      }
    } else if(c == '.') {
      eatc();
      char c2;
      if(!peekc(c2)) {
        unexpected_eof("after .", token_start_line, token_start_position);
        return TK_ERROR;
      }

      if(c2 >= '0' && c2 <= '9') {
        tokenize_number(false, '.');
        return TK_NUMBER;
      }

      else if(is_symbol(c2)) {
        buffer += '.';
        tokenize_symbol();
        return TK_SYMBOL;
      }

      return TK_DOT;
    } else if (c == ',') {
      eatc();
      char c2;
      if(!peekc(c2)) {
        // Error will be returned later
        unexpected_eof("after , ", line, position - 1);
        return TK_ERROR;
      }
      if(c2 == '@') {
        eatc();
        return TK_UNQUOTE_SPLICING;
      }

      return TK_UNQUOTE;
    } else if(c == '`') {
      eatc();
      return TK_QUASIQUOTE;
    } else if (c == '\'') {
      eatc();
      return TK_QUOTE;
    } else if(c == ';') {
      while(!is.eof()) {
        peekc(c);
        if(c == '\n') break;
        eatc();
      }
    } else if(c == '"') {
      tokenize_string();
      return TK_STRING;
    } else if(c == '{') {
      eatc();
      return TK_LBRACKET;
    } else if(c == '}') {
      eatc();
      return TK_RBRACKET;
    } else if(c == '(') {
      eatc();
      return TK_LPAREN;
    } else if(c == ')') {
      eatc();
      return TK_RPAREN;
    } else if(is_symbol_initial(c)) {
      tokenize_symbol();
      return TK_SYMBOL;
    } else {
      AR_LOG_READ("unknown character " << c << ' ' << (ptrdiff_t) c);
      std::ostringstream os;
      os << "unknown character '" << c << "' (int = " << (ptrdiff_t) c << ")";
      read_error(os.str(), token_start_line, position, position+1);
      return TK_ERROR;

      // AR_ASSERT(!"unknown character");
    }
  }
  return TK_EOF;
}

Value XReader::read_aux(const std::string& text, unsigned highlight_size, Value symbol,
    bool renaming) {

  unsigned cline = token_start_line, cposition = token_start_position;
  Value x;
  AR_FRAME(state, x, symbol);

  bool qq_saved = quasiquote_renaming;
  quasiquote_renaming = renaming;

  x = read_expr(TK_READ_NEXT);

  quasiquote_renaming = qq_saved;
  if(x == C_EOF) {
    return unexpected_eof(text, line, position - highlight_size, highlight_size);
  } else if(x.is_active_exception()) {
    return x;
  }

  x = state.make_pair(x, C_NIL);
  return make_src_pair(symbol, x, cline, cposition, position - cposition);
}

Value XReader::read_aux2(const std::string& text, unsigned highlight_size, Value symbol,
    Value symbol2) {
  unsigned cline = token_start_line, cposition = token_start_position;

  Value x;
  
  AR_FRAME(state, x, symbol, symbol2);
  bool qq_saved = quasiquote_renaming;
  quasiquote_renaming = false;
  x = read_aux(text, highlight_size, symbol);
  quasiquote_renaming = qq_saved;
  if(x.is_active_exception()) {
    return x;
  }

  x = state.make_pair(x, C_NIL);
  return make_src_pair(symbol2, x, cline, cposition, position - cposition);
}

Value XReader::read_expr(TokenType tk) {
#define NEXT_TOKEN(name) \
    name = next_token(); AR_ASSERT(name <= TK_EOF); if(active_error != C_FALSE) { return active_error; }

  if(tk == TK_READ_NEXT) {
    NEXT_TOKEN(tk);
  }

  if(tk == TK_EOF) return C_EOF;
  
  switch(tk) {
    case TK_RBRACKET: 
      return read_error("unexpected right bracket", line, position - 1, position);
    case TK_RPAREN:
      return read_error("unexpected right paren", line, position - 1, position);
    case TK_CONSTANT: return return_constant;

    case TK_EXPRESSION_COMMENT: {
      Value x = read_expr(TK_READ_NEXT);
      if(x.is_active_exception()) return x;
      // Check next token
      return read_expr(TK_READ_NEXT);
    }

    // Atomic stuff
    case TK_NUMBER: {
      NumberReader nreader(state, buffer); 
      Value v = nreader.read();
      if(v == C_FALSE) {
        return read_error(nreader.error_desc, token_start_line, token_start_position, position);
      }
      return v;
    }

    case TK_STRING: 
      return state.make_string(buffer);
    case TK_SYMBOL: {
      Value sym = state.get_symbol(buffer);
      // Set symbol bits
      if(!sym.symbol_was_read()) {
        sym.heap->set_header_bit(Value::SYMBOL_READ_BIT);
      }
      if(!sym.symbol_keyword()) {
        if(sym.symbol_name_data()[sym.symbol_name().string_bytes()-1] == ':') {
          sym.heap->set_header_bit(Value::SYMBOL_KEYWORD_BIT);
        }

      }
      // #`asdf => (quasiquote (rename 'asdf))
      if(quasiquote_renaming && !sym.symbol_keyword()) {
        AR_FRAME(state, sym);
        sym = state.make_pair(sym, C_NIL);
        sym = state.make_pair(state.globals[State::S_QUOTE], sym);
        sym = state.make_pair(sym, C_NIL);
        sym = state.make_pair(state.globals[State::S_RENAME], sym);
        sym = state.make_pair(sym, C_NIL);
        sym = make_src_pair(state.globals[State::S_UNQUOTE], sym,
          token_start_line, token_start_position, position);
      }
      return sym;
    }
    case TK_CHARACTER:
      if(buffer.compare("space") == 0) {
        return state.make_char(' ');
      } else if(buffer.compare("newline") == 0) {
        return state.make_char('\n');
      } else if(buffer.compare("tab") == 0) {
        return state.make_char('\t');
      } else if(buffer.compare("return") == 0) {
        return state.make_char('\r');
      } else if(buffer.size() > 1) {
        std::ostringstream os;
        os << "unknown character literal " << buffer << std::endl;
        return read_error(os.str(), token_start_line, token_start_position, position);
      }
      return state.make_char(buffer[0]);

    // Auxiliary syntax
    case TK_DOT:
      return read_error("unexpected . at toplevel", token_start_line, token_start_position,
        position);
    case TK_UNQUOTE: return read_aux("after ,", 2, state.globals[State::S_UNQUOTE]);
    case TK_QUASIQUOTE: return read_aux("after `", 1, state.globals[State::S_QUASIQUOTE]);
    case TK_QUASIQUOTE_RENAMING: {
      bool saved = quasiquote_renaming;
      quasiquote_renaming = true;
      Value x = read_aux("after #`", 1, state.globals[State::S_QUASIQUOTE], true);
      quasiquote_renaming = saved;
      return x;

    }
    case TK_QUOTE: return read_aux("after '", 1, state.globals[State::S_QUOTE]);
    case TK_RENAME: return read_aux2("after #'", 1, state.globals[State::S_QUOTE],
      state.globals[State::S_RENAME]);
    case TK_UNQUOTE_SPLICING:
     return read_aux("after ,@", 2, state.globals[State::S_UNQUOTE_SPLICING]);

    case TK_SHARED_DEFINE: {
      ptrdiff_t number = std::stoi(buffer);

      if(shared_objects == C_FALSE) {
        shared_objects = state.make_vector();
      }

      if(number > shared_objects.vector_length()) {
        std::ostringstream os;
        os << "shared objects were enumerated out of order (got " << number << " but only " <<
          shared_objects.vector_length() << " objects have been defined)";
        return read_error(os.str(), token_start_line, token_start_position, position);
      }

      SValue expr = read_expr(TK_READ_NEXT);

      if(expr.is_active_exception()) return expr;

      state.vector_append(shared_objects, expr);

      return expr;
    }

    case TK_SHARED_REF: {
      ptrdiff_t number = std::stoi(buffer);

      if(shared_objects == C_FALSE || number >= shared_objects.vector_length()) {
        std::ostringstream os;
        os << "shared object #" << number << "# has not been defined yet (or this is recursive shared object, not supported yet)";
        return read_error(os.str(), token_start_line, token_start_position, position);
      }

      return shared_objects.vector_ref(number);
    }

    case TK_VECTOR_OPEN: {
      SValue vec, x, tmp;

      unsigned cline = line, cposition = position - 2;

      AR_FRAME(state, vec, x, tmp);

      vec = state.make_vector();

      while(tk != TK_RPAREN) {
        NEXT_TOKEN(tk);
        if(tk == TK_EXPRESSION_COMMENT) {
          tmp = read_expr(TK_READ_NEXT);
          if(tmp.is_active_exception()) return tmp;
          continue;
        } else if(tk == TK_RPAREN) {

        } else if(tk == TK_EOF) {
          return unexpected_eof("in vector", cline, cposition);
        } else {
          x = read_expr(tk);
          AR_ASSERT(x != C_EOF);
          state.vector_append(vec, x);
        }
      }

      return vec;
    }

    // Tables
    case TK_LBRACKET: {
      unsigned cline = line, cposition = position - 1;
      TokenType tk2 = TK_READ_NEXT;
      NEXT_TOKEN(tk2);

      if(tk2 == TK_RBRACKET) {
        return state.make_table();
      } else if(tk2 == TK_DOT) {
        return read_error("unexpected , at beginning of table", line, cposition, position);
      }

      SValue table, key, value;
      AR_FRAME(state, table, key, value)

      table = state.make_table();
      
      // TODO: Should this return some kind of function invocation, like perhaps
      // (table key value ...)? That would allow quasiquote to work on tables.

      while(tk2 != TK_RBRACKET) {
        unsigned eline = line, epos = position;
        key = read_expr(tk2);

        if(key == C_EOF) {
          unexpected_eof("in table literal", cline, cposition);
          return active_error;
        } else if(key.is_active_exception()) {
          return key;
        } else if(!key.hashable()) {
          std::ostringstream os;
          os << "expected key in table literal to be hashable, but got a " << key.type();
          return read_error(os.str(), eline, epos, position);
        } 

        value = read_expr(TK_READ_NEXT);

        if(key == C_EOF) {
          unexpected_eof("in table literal", cline, cposition);
          return active_error;
        } else if(value.is_active_exception()) {
          return value;
        }

        state.table_set(table, key, value);

        NEXT_TOKEN(tk2);

        if(tk2 == TK_EOF) { 
          return unexpected_eof("in table literal", cline, cposition, position);
        } else if(tk2 == TK_RBRACKET) {
          break;
        } else if(tk2 != TK_DOT) {
          return read_error("expected . after key and value in table", token_start_line,
            token_start_position, position);
        }

        NEXT_TOKEN(tk2);
      }
      
      return table;
    }

    // Lists
    case TK_LPAREN: {
      // Read a list
      // These variables track where the list began
      unsigned cline = line, cposition = position - 1;

      TokenType tk2 = TK_READ_NEXT;
      NEXT_TOKEN(tk2);

      if(tk2 == TK_RPAREN) {
        return C_NIL;
      } else if(tk2 == TK_DOT) {
        return read_error("unexpected . at beginning of list", line, cposition, position);
      }

      SValue head = C_NIL, tail = C_FALSE, elt = C_FALSE, swap = C_FALSE, tmp = C_FALSE;
      AR_FRAME(state, head, tail, elt, swap);

      // (a b c)
      // (a)
      // (a b . c)
      // bad cases:
      // (a b . c)
      // (. c)
      while(tk2 != TK_RPAREN && tk2 != TK_DOT) {
        // Special case: Expression comment at the end of a list has to be handled up here
        // because recursion will not be able to handle TK_RPAREN
        if(tk2 == TK_EXPRESSION_COMMENT) {
          tmp = read_expr(TK_READ_NEXT);
          if(tmp.is_active_exception()) return tmp;
          NEXT_TOKEN(tk2);
          continue;
        }

        elt = read_expr(tk2);

        if(elt == C_EOF) {
          unexpected_eof("in list", cline, cposition);
          return active_error;
        } else if(elt.is_active_exception()) {
          return elt;
        }

        // Here we want to find the position of the elt.

        // If an elt is another pair, it already has as much source information as it's going
        // to get
        if(elt.type() == PAIR) {
          swap = state.make_pair(elt, C_NIL);
        } else {
          // std::cout << "ELT " << elt << " from " << token_start_position << " to " << position << std::endl;
          swap = make_src_pair(elt, C_NIL, token_start_line, token_start_position, position - token_start_position); // -1 ?
        }

        if(tail != C_FALSE) {
          tail.set_cdr(swap);
        }

        tail = swap;

        if(head == C_NIL) {
          head = tail;
        }
        
        NEXT_TOKEN(tk2);
      }

      if(tk2 == TK_DOT) {
        NEXT_TOKEN(tk2);
        if(tk2 == TK_RPAREN) {
          read_error("expected expression after dot in list but got a closing paren", cline, cposition, position);
          return active_error;
        }
        unsigned endpos = position;

        swap = read_expr(tk2);
        NEXT_TOKEN(tk2);
        if(tk2 != TK_RPAREN) {
          // TODO EOF
          read_error("expected one expression after dot in list but got multiple", cline, endpos, position);
          return active_error;
        }
        tail.set_cdr(swap);
      }

      if(head.type() == PAIR) {
        // Lists will have the whole thing highlighted.
        return make_src_pair(head.car(), head.cdr(), cline, cposition, position - cposition);
      }

      // It is possible to end up with a nil here: (#;x)
      AR_ASSERT(head == C_NIL);
      return head;

    }

    default: {
      AR_LOG_READ("unknown token " << tk);
      std::cerr << tk << std::endl;
      AR_ASSERT(!"unknown token");
    }
  }

#undef NEXT_TOKEN

  return C_EOF;
}

Value XReader::read() {
  AR_FRAME(state, shared_objects);
  return read_expr(TK_READ_NEXT);
}

}
