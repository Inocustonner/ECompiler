#pragma once

#include <string>
#include "error.hpp"

enum class TokenType {
  Ident,
  Int,     // number in ebnf

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
  Slash
};

struct Token {
  long p, line, col;
  TokenType tok;
};

struct TokenString: Token {
  std::string str;
};

struct TokenNumber: Token {
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
   char curr;
   long p = -1;
   long line = -1, col = -1;
};

namespace error {
  struct LexerError : public error::Error {
    public:
     LexerError(Token, const char* format, ...);
     Token meta();
    private:
     Token token;
  };
};  // namespace error