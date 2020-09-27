#pragma once

#include <string>

enum class TokenType {
  Ident,
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