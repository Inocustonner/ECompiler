#pragma once

using CharT = char;
using StringT = std::basic_string<CharT>;

enum class TokenType {
  Eof,

  Ident,
  Int, // number in ebnf
  Char,
  String,

  Return, // return

  Terminal, // ;

  Comma, // ,

  Colon,  // :
  DColon, // ::

  OBracket, // [
  CBracket, // ]

  OBracer, // {
  CBracer, // }

  Eq, // =

  OParen, // (
  CParen, // )

  Plus,     // +
  Minus,    // -
  Asterisk, // *
  Slash,    // /
};

struct Token {
  long p, line, col;
  long end_p;
  TokenType tok;
};
typedef Token *PToken;

struct TokenChar : Token {
  char ch;
};

struct TokenString : Token {
  std::string *str;
};

struct TokenIdent : Token {
  std::string *ident;
};

struct TokenInt : Token {
  long number;
};

static const std::tuple<const char *, TokenType> g_key_words[] = {
  {"return", TokenType::Return}
};

void freeToken(Token *tok);
std::string serializeToken(PToken token);

struct Lexer {
  Lexer(const char *file_path);

  ~Lexer();
  Token *next_token();

private:
  void advance();

private:
#ifndef USE_CPP_STREAM
  FILE *file_p = nullptr;
#else
  std::ifstream file_s;
#endif
  CharT curr;
  long p = -1;
  long line = 1, col = 0;
};

namespace error {
struct LexerError : public error::Error {
public:
  LexerError(Token, const char *format, ...);
  Token meta() const;

private:
  Token token;
};
}; // namespace error
