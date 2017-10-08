// test.cpp - in-progress C++ stuff
#include <assert.h>
#include <iostream>
#include <sstream>

#define ARETE_LOG_TAGS 2

#include "arete.hpp"
#include "arete.cpp"

#include "vendor/linenoise.h"

using namespace arete;

enum TokenType {
  TK_ERROR,
  TK_READ_NEXT,
  TK_LPAREN,
  TK_RPAREN,
  TK_DOT,
  TK_SYMBOL,
  TK_STRING,
  TK_FIXNUM,
  TK_TRUE,
  TK_FALSE,
  TK_EXPRESSION_COMMENT,
  TK_EOF,
};

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

struct XReader {
  State& state;
  std::istream& is;
  unsigned source, position, line, column;
  Value active_error;

  std::string buffer;
  unsigned token_start_position;

  XReader(State& state_, std::istream& is_): state(state_), is(is_), source(0), position(0), line(0), column(0), active_error(C_FALSE)
    {
    is >> std::noskipws;
  }

  ~XReader() {

  }

  /**
   * Read a character while tracking source code location
   * @param c reference to a character to be read.
   * @return Whether the stream has hit EOF
   */
  bool getc(char& c) {
    is >> c;
    if(is.eof()) return false;
    column++;
    position++;
    if(c == '\n') {
      line++; column = 0;
    }
    return !is.eof();
  }

  /**
   * Peek at the next character
   * @return Whether the stream has hit EOF
   */
  bool peekc(char& c) {
    c = is.peek();
    return !is.eof();
  }

  /**
   * Consume a character while tracking source code location
   */
  void eatc() {
    char c;
    getc(c);
  }

  void read_error(const std::string& message, unsigned start_line, unsigned start_position, unsigned end_position) {
    // If end_position is zero, we'll just underline the whole line
    std::string source_line;
    char c;
    unsigned seek_line = 0;

    ARETE_LOG_READ("unexpected_eof " << start_line << ' ' << start_position << ' ' << message << std::endl);
    std::ostringstream os;
    os << state.source_name(source) << ':' << start_line << ": Reader error: " << message << std::endl << std::endl;
    // Print line in question
    is.clear();
    is.seekg(0, std::ios::beg);

    while(!is.eof()) {
      if(seek_line == start_line) {
        unsigned line_position = is.tellg();
        std::getline(is, source_line);
        os << "  " << ARETE_COLOR_RED << source_line << ARETE_COLOR_RESET << std::endl;
        os << "  ";
        size_t i, j;

        // TODO UNICODE
        for(i = line_position, j = 0; i < start_position; i++, j++) {
          os << ' ';
        }

        // If end_position is 0, we'll highlight the whole line this error occurred on. Otherwise,
        // try to highlight specific errors e.g.
        // (hello #bad)
        //         ^
        for(; j != source_line.size() && ((end_position == 0) || j != (end_position - start_position)); j++) {
          os << '^';
        }
        break;
      }

      is >> c;

      if(c == '\n') {
        seek_line++;
      }
    }

    active_error = state.make_exception(state.globals[State::S_READ_ERROR], os.str());
  }

  void unexpected_eof(const std::string& message, unsigned start_line, unsigned start_position) {
    std::ostringstream os;
    os << "unexpected end of file " << message;
    read_error(os.str(), start_line, start_position, 0);
  }

  void tokenize_number() {
    char c;
    token_start_position = position;

    while(peekc(c)) {
      if(c >= '0' && c <= '9') {
        getc(c);
        buffer += c;
      } else {
        break;
      }
    }
  }

  void tokenize_symbol() {
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

  void tokenize_string() {
    char c;
    unsigned cline = line;
    token_start_position = position;
    AR_ASSERT(is.tellg() == token_start_position);
    eatc();

    while(peekc(c)) {
      if(c == '"') {
        return;
      } else {
        getc(c);
        buffer += c;
      }
    }

    unexpected_eof("in string", cline, token_start_position);
  }

  TokenType next_token() {
    AR_ASSERT(active_error == C_FALSE);
    char c;
    buffer.clear();
    while(peekc(c)) {
      if(c >= '0' && c <= '9') {
        tokenize_number();
        return TK_FIXNUM;
      } else if(c == ' ' || c == '\r' || c == '\t' || c == '\n') {
        eatc();
      } else if(c == '#') {
        eatc();

        char c2;
        if(!getc(c2)) {
          unexpected_eof("after # read syntax", line, position - 1);
          return TK_ERROR;
        }

        if(c2 == 't') return TK_TRUE;
        else if(c2 == 'f') return TK_FALSE;
        else if(c2 == ';') {
          if(!peekc(c2)) {
            unexpected_eof("after #; expression comment", line, position - 2);
          }
          return TK_EXPRESSION_COMMENT;
        }
        else {
          read_error("unknown # syntax", line, position - 1, position);
        }
      } else if(c == '.') {
        eatc();
        return TK_DOT;
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
        ARETE_LOG_READ("unknown character " << c << ' ' << (ptrdiff_t) c);
        AR_ASSERT(!"unknown character");
      }
    }
    return TK_EOF;
  }

  Value read_expr(TokenType tk) {

#define LOG_BUFFER(tipe) \
    ARETE_LOG_READ(tipe << ' ' << line << ' ' << buffer)

#define LOG_NOBUFFER(tipe, c) \
    ARETE_LOG_READ(tipe << ' ' << line << ' ' << c)
  
#define NEXT_TOKEN(name) \
    name = next_token(); if(active_error != C_FALSE) { return active_error; }

    if(tk == TK_READ_NEXT) {
      NEXT_TOKEN(tk);
    }

    if(tk == TK_EOF) return C_EOF;

    switch(tk) {
      case TK_RPAREN: {
        std::cout << "RPAREN" << std::endl;
        break;
      }

      case TK_TRUE: return C_TRUE;
      case TK_FALSE: return C_FALSE;

      case TK_EXPRESSION_COMMENT: {
        LOG_NOBUFFER("TK_EXPRESSION_COMMENT", "#;");
        Value x = read_expr(TK_READ_NEXT);
        if(x.is_active_exception()) return x;
        // std::cout << "Ignoring read expression " << x << std::endl;
        return read_expr(TK_READ_NEXT);
      }

      case TK_FIXNUM: {
        LOG_BUFFER("TK_FIXNUM");
        ptrdiff_t n = 0;
        for(size_t i = 0; i != buffer.size(); i++) {
          n += buffer[i] - '0';
          if(i == buffer.size() - 1) break;
          n *= 10;
        }
        return Value::make_fixnum(n);
      }

      case TK_SYMBOL: {
        return state.get_symbol(buffer);
      }

      case TK_LPAREN: {
        // Read a list
        LOG_NOBUFFER("TK_LPAREN", '(');
        unsigned cline = line, cposition = position - 1;

        TokenType tk2;
        NEXT_TOKEN(tk2);

        if(tk2 == TK_RPAREN) {
          return C_NIL;
        } else if(tk2 == TK_DOT) {
          read_error("unexpected . at beginning of list", line, cposition, position);
          return active_error;
        }

        Value head = C_FALSE, tail = C_FALSE, elt, swap = C_FALSE;
        AR_FRAME(state, head, tail, elt, swap);
        bool dotted = false;

        // (a b c)
        // (a)
        // (a b . c)
        // bad cases:
        // (a b . c)
        // (. c)
        while(tk2 != TK_RPAREN && tk2 != TK_DOT) {
          Value elt = read_expr(tk2);

          if(elt == C_EOF) {
            unexpected_eof("in list", cline, cposition);
            return active_error;
          } else if(elt.is_active_exception()) {
            return elt;
          }

          swap = state.make_pair(elt, C_NIL);

          if(tail != C_FALSE) {
            tail.set_cdr(swap);
          }

          tail = swap;

          if(head == C_FALSE) {
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
          if(tk2 != TK_RPAREN) {
            // TODO EOF
            read_error("expected one expression after dot in list but got multiple", cline, endpos, position);
            return active_error;
          }
          tail.set_cdr(swap);
        }

        std::cout << "List 1 goes from " << cposition << " to " << position << std::endl;

        return head;
      }

      default: {
        ARETE_LOG_READ("unknown token " << tk);
        AR_ASSERT(!"unknown token");
      }
    }

    return C_EOF;
  }

  Value read() {
    return read_expr(TK_READ_NEXT);
  }


};

void do_string(State& state, const std::string& str) {
  std::stringstream ss;
  ss >> std::noskipws;
  ss << str;

  XReader xreader(state, ss);
  xreader.source = state.register_file("imaginary-file.scm", ss);
  while(true) {
    Value x = xreader.read();
    if(x == C_EOF) break;
    if(x.is_active_exception()) {
      std::cout << x.exception_message().string_data() << std::endl;
      break;
    }
    std::cout << x << std::endl;
  }
}

int main(void) {
  State state;

  state.boot();

  do_string(state, "(1 2 3)");
  do_string(state, "(1 . )");
  do_string(state, "(.)");

  // do_string(state, "#;(12345)");

  // do_string(state, "(12345 6789");
  // do_string(state, "(12345");
  // do_string(state, "(12345 #)");
  // do_string(state, "(12345)");
  // do_string(state, "\"12345");

  return 0;
}
