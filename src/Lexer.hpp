#pragma once

#include <string>
#include "error.hpp"

using CharT = char;
using StringT = std::basic_string<CharT>;

enum class TokenType {
	Eof,
	
  Ident,
  Int,     // number in ebnf
  Char,
  String,

  Terminal,// ;

  Colon,   // :
  DColon,  // ::

  OBracket,// {
  CBracket,// }

  Eq,      // =

  OParen,  // (
  CParen,  // )
  
  Plus,
  Minus,
  Asterisk,
  Slash,
};

struct Token {
  long p, line, col;
  TokenType tok;
};

struct TokenChar: Token {
  char ch;
};

struct TokenString : Token {
    std::string str;
};

struct TokenIdent: Token {
  StringT ident;
};

struct TokenInt: Token {
  unsigned number;
};

struct Lexer {
  Lexer(const char* file_path);
  ~Lexer();
  Token* next_token();
  private:
   void advance();

  private:
   FILE* file_p = nullptr;
   CharT curr;
   long p = -1;
   long line = 1, col = 0;
};

namespace error {
  struct LexerError : public error::Error {
    public:
     LexerError(Token, const char* format, ...);
     Token meta() const;
    private:
     Token token;
  };
};  // namespace error
