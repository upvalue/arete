// reader.cpp - S-Expression reader.
#define ARETE_LOG_READ(msg) ARETE_LOG((ARETE_LOG_TAG_READER), "read", msg)

#include "arete.hpp"

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

unsigned State::register_source(const std::string& path, std::istream& is) {
  ARETE_LOG_READ("registering source " << path);
  std::ostringstream contents;
  contents << is.rdbuf();
  is.seekg(0, std::ios::beg);

  source_names.push_back(path);
  source_contents.push_back(contents.str());

  return source_names.size() - 1;
}

static bool is_symbol_initial(char c) {
  if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) return true;

  switch(c) {
    case '!': case '$': case '%': case '&': case '*': case '/':  case ':':
    case '<': case '=': case '>': case '?': case '^': case '_': case '~':
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

  return state.make_src_pair(kar, kdr, src);
}

Value XReader::read_error(const std::string& message, unsigned start_line, unsigned start_position,
    unsigned end_position) {

  ARETE_LOG_READ("read_error " << start_line << ' ' << start_position << ' ' << message << std::endl);
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

XReader::TokenType XReader::tokenize_number(bool negative) {
  char c;
  if(negative) buffer += '-';

  while(peekc(c)) {
    if(c >= '0' && c <= '9') {
      getc(c);
      buffer += c;
    } else {
      break;
    }
  }

  if(c == '.') {
    eatc();
    buffer += '.';
  } else {
    return TK_FIXNUM;
  }

  while(peekc(c)) {
    if(c >= '0' && c <= '9') {
      getc(c);
      buffer += c;
    } else if(c == '.') {
      read_error("multiple periods in number", token_start_line, position, position+1);
      return TK_ERROR;
    } else {
      break;
    }
  }

  return TK_FLONUM;
}

void XReader::tokenize_symbol() {
  char c;

  while(peekc(c)) {
    if(is_symbol(c)) {
      getc(c);
      buffer += c;
    } else {
      return;
    }
  }
}

void XReader::tokenize_string() {
  char c;

  AR_ASSERT(is.tellg() == token_start_position);
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
      return tokenize_number();
    } else if(c == '+' || c == '-') {
      // These can start a number, but they can also start a symbol
      getc(c);
      AR_ASSERT(c == '+' || c == '-');
      char c2;
      if(peekc(c2)) {
        if(c2 >= '0' && c2 <= '9') {
          return tokenize_number(c == '-');
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
    } else if(c == ' ' || c == '\r' || c == '\t' || c == '\n') {
      eatc();
    } else if(c == '#') {
      // # Sharp syntax
      eatc();

      char c2;
      if(!getc(c2)) {
        unexpected_eof("after # read syntax", line, position - 1);
        return TK_ERROR;
      }

      if(c2 == 't') return TK_TRUE;
      else if(c2 == 'f') return TK_FALSE;
      else if(c2 == '\'') return TK_RENAME;
      else if(c2 == '(') return TK_VECTOR_OPEN;
      else if(c2 == '\\') {
        // Character literals
        if(!peekc(c2)) {
          unexpected_eof("after #\\ character literal", token_start_line, token_start_position);
        }

        if(c2 == ' ')  {
          buffer += "space";
          return TK_CHARACTER;
        }

        getc(c2);
        buffer += c2;
        tokenize_symbol();
        
        return TK_CHARACTER;
      }
      else if(c2 == ';') {
        // Expression comments
        if(!peekc(c2)) {
          unexpected_eof("after #; expression comment", line, position - 2);
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
      } else {
        eatc();
        (void) read_error("unknown # syntax", token_start_line, token_start_position, position);
        return TK_ERROR;
      }
    } else if(c == '.') {
      eatc();
      char c2;
      if(!peekc(c2)) {
        unexpected_eof("after .", token_start_line, token_start_position);
        return TK_ERROR;
    }

    if(c2 >= '0' && c2 <= '9') {
      buffer += '0';
      buffer += '.';
      tokenize_number();
      return TK_FLONUM;
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
      std::cerr << "unknown character " << c << ' ' << (ptrdiff_t) c << std::endl;
      ARETE_LOG_READ("unknown character " << c << ' ' << (ptrdiff_t) c);
      AR_ASSERT(!"unknown character");
    }
  }
  return TK_EOF;
}

Value XReader::read_aux(const std::string& text, unsigned highlight_size, Value symbol) {
  unsigned cline = token_start_line, cposition = token_start_position;
  Value x;
  AR_FRAME(state, x, symbol);

  x = read_expr(TK_READ_NEXT);
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
  x = read_aux(text, highlight_size, symbol);
  if(x.is_active_exception()) return x;

  x = state.make_pair(x, C_NIL);
  return make_src_pair(symbol2, x, cline, cposition, position - cposition);
}

Value XReader::read_expr(TokenType tk) {
#define NEXT_TOKEN(name) \
    name = next_token(); if(active_error != C_FALSE) { return active_error; }

  if(tk == TK_READ_NEXT) {
    NEXT_TOKEN(tk);
  }

  if(tk == TK_EOF) return C_EOF;
  
  switch(tk) {
    case TK_RPAREN:
      return read_error("unexpected right paren", line, position - 1, position);
    case TK_TRUE: return C_TRUE;
    case TK_FALSE: return C_FALSE;

    case TK_EXPRESSION_COMMENT: {
      Value x = read_expr(TK_READ_NEXT);
      if(x.is_active_exception()) return x;
      // Check next token
      return read_expr(TK_READ_NEXT);
    }

    // Atomic stuff
    case TK_FIXNUM: {
      ptrdiff_t n = 0;
      bool negative = buffer[0] == '-';
      for(size_t i = (negative ? 1 : 0); i != buffer.size(); i++) {
        n += buffer[i] - '0';
        if(i == buffer.size() - 1) break;
        n *= 10;
      }
      return Value::make_fixnum(negative ? -n : n);
    }

    case TK_FLONUM: {
      std::stringstream ss;
      ss << buffer;
      double value;
      ss >> value;
      return state.make_flonum(value);
    }

    case TK_STRING: 
      return state.make_string(buffer);
    case TK_SYMBOL:
      return state.get_symbol(buffer);
    case TK_CHARACTER:
      if(buffer.compare("space") == 0) {
        return state.make_char(' ');
      } else if(buffer.compare("newline") == 0) {
        return state.make_char('\n');
      } else if(buffer.size() > 1) {
        std::ostringstream os;
        os << "unknown character literal " << buffer << std::endl;
        return read_error(os.str(), token_start_line, token_start_position, position);
      }
      return state.make_char(buffer[0]);

    // Auxiliary syntax
    case TK_UNQUOTE: return read_aux("after ,", 2, state.globals[State::S_UNQUOTE]);
    case TK_QUASIQUOTE: return read_aux("after `", 1, state.globals[State::S_QUASIQUOTE]);
    case TK_QUOTE: return read_aux("after '", 1, state.globals[State::S_QUOTE]);
    case TK_RENAME: return read_aux2("after #'", 1, state.globals[State::S_QUOTE],
      state.globals[State::S_RENAME]);
    case TK_UNQUOTE_SPLICING:
     return read_aux("after ,@", 2, state.globals[State::S_UNQUOTE_SPLICING]);

    case TK_VECTOR_OPEN: {
      Value vec, x, tmp;

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

      Value head = C_NIL, tail = C_FALSE, elt = C_FALSE, swap = C_FALSE, tmp = C_FALSE;
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
      ARETE_LOG_READ("unknown token " << tk);
      std::cerr << tk << std::endl;
      AR_ASSERT(!"unknown token");
    }
  }

#undef NEXT_TOKEN

  return C_EOF;
}

Value XReader::read() {
  return read_expr(TK_READ_NEXT);
}

}