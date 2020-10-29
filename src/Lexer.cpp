#include "Lexer.hpp"
#include "error.hpp"
#include <cstdarg>
#define IN_RANGE_INCL(from, n, to) (from <= n && n <= to)
#define INIT_TOKEN(ident, tok_type_enum)                                       \
  (ident)->p = p;                                                              \
  (ident)->line = line;                                                        \
  (ident)->col = col;                                                          \
  (ident)->tok = tok_type_enum;

Lexer::Lexer(const char *file_path) {
  fopen_s(&file_p, file_path, "rb");
  if (file_p == nullptr) {
    throw error::Error("Unnable to open %s file", file_path);
  }
  advance();
}

Lexer::~Lexer() { fclose(file_p); }

void Lexer::advance() {
  curr = fgetc(file_p);
  if (curr != EOF) {
    p += 1;
    col += 1;
    if (curr == '\n') {
      line += 1;
      col = 0;
    }
  }
}

#define TOKEN_MATCH_RETURN(tokc, toktype)                                      \
  case tokc: {                                                                 \
    Token *token = new Token;                                                  \
    token->p = p, token->line = line, token->col = col;                        \
    token->tok = toktype;                                                      \
    advance();                                                                 \
    return token;                                                              \
  } break;

static bool isVarChar(CharT c, bool include_numbers = false) {
  return IN_RANGE_INCL('a', c, 'z') || IN_RANGE_INCL('A', c, 'Z') ||
         (c == '_') || (include_numbers && IN_RANGE_INCL('0', c, '9'));
}

Token *Lexer::next_token() {
  while (isspace(curr))
    advance();
  switch (curr) {
  case ':': {
    Token *token = new Token;
    token->p = p, token->line = line, token->col = col;
    token->tok = TokenType::Colon;
    advance();
    if (curr == ':') {
      token->tok = TokenType::DColon;
      advance();
    }
    return token;
  }

    TOKEN_MATCH_RETURN('{', TokenType::OBracket);
    TOKEN_MATCH_RETURN('}', TokenType::CBracket);
    TOKEN_MATCH_RETURN('(', TokenType::OParen);
    TOKEN_MATCH_RETURN(')', TokenType::CParen);

    TOKEN_MATCH_RETURN('+', TokenType::Plus);
    TOKEN_MATCH_RETURN('-', TokenType::Minus);
    TOKEN_MATCH_RETURN('*', TokenType::Asterisk);
    TOKEN_MATCH_RETURN('/', TokenType::Slash);
    TOKEN_MATCH_RETURN('=', TokenType::Eq);

    TOKEN_MATCH_RETURN(';', TokenType::Terminal);

  case '\'': {
    TokenChar *token = new TokenChar;
    INIT_TOKEN(token, TokenType::Char);
    advance();
    token->ch = curr;
    advance();
    if (curr == '\'') {
      advance();
      return token;
    } else {
      if (isspace(curr) && curr != ' ') {
        auto space_snd_char = [](char c) {
          switch (c) {
          case '\t':
            return 't';
          case '\n':
            return 'n';
          case '\v':
            return 'v';
          case '\f':
            return 'f';
          case '\r':
            return 'r';
          default:
            return ' ';
          }
        };
        throw error::LexerError(
            *token, "Unexpected character \\'%c'(0x%0.2x) , excpected \"%c\"",
            space_snd_char(curr), (unsigned)curr, '\'');

      } else {
        throw error::LexerError(
            *token, "Unexpected character '%c', excpected '%c'", curr, '\'');
      }
    }
  }

  case '"': {
    TokenString *token = new TokenString;
    INIT_TOKEN(token, TokenType::String);
    advance();

    do {
      token->str.push_back(curr);
      advance();
    } while (curr != '"' && curr != EOF);

    if (curr == '"') {
      advance();
      return token;
    } else
      break;
  }
  default:
    // number
    if (isdigit(curr)) {
#define CHAR_DIGIT_TO_I(c) ((c)-0x30)
      long number = CHAR_DIGIT_TO_I(curr);
      long exp = 10;
      advance();

      bool hex = curr == 'x';
      if (hex) {
        exp = 0x10;
        advance();
      }
      bool octal = curr == 'o';
      if (octal) {
        exp = 8;
        advance();
      }
      bool decimal = isdigit(curr);
      if (decimal)
        number = number * exp + CHAR_DIGIT_TO_I(curr);

      TokenInt *token = new TokenInt;
      INIT_TOKEN(token, TokenType::Int);

      auto hexalphanumber = [](char c) -> long {
        long numbers[] = {0xA, 0xB, 0xC, 0xD, 0xE, 0xF};
        if ('a' <= c && c <= 'f')
          return numbers[c - 'a'];
        else if ('A' <= c && c <= 'F')
          return numbers[c - 'A'];
        else
          return -1;
      };

      for (; isalnum(curr); advance()) {
        if (isdigit(curr)) {
          // check boundaries
          if (octal && CHAR_DIGIT_TO_I(curr) >= 8) {
            throw error::LexerError(
                *token, "Character '%c' doesn't belong to octal digits", curr);
          }
          number = number * exp + CHAR_DIGIT_TO_I(curr);

        } else if (auto hnumber = hexalphanumber(curr); hnumber != -1 && hex) {
          number = number * exp + hnumber;
        } else {
          throw error::LexerError(
              *token, "Character '%c' doesn't belong to %s numbers", curr,
              decimal ? "decimal"
              : hex   ? "hex"
                      : "octal");
        }
      }
      token->number = number;
      return token;

    } else if (isVarChar(curr)) {
      TokenIdent *ident_tok = new TokenIdent;
      INIT_TOKEN(ident_tok, TokenType::Ident);
      do {
        ident_tok->ident.push_back(curr);
        advance();
      } while (isVarChar(curr, true));
      return ident_tok;

    } else if (curr == EOF) {
      Token *eof_token = new Token;
      INIT_TOKEN(eof_token, TokenType::Eof);
      return eof_token;

    } else {
      Token tok;
      INIT_TOKEN(&tok, TokenType::Eof);
      throw error::LexerError(tok, "Unexpected character \"%c\"(0x%x)", curr,
                              (unsigned)curr);
    }
  }
  Token tok;
  INIT_TOKEN(&tok, TokenType::Eof);
  throw error::LexerError(tok, "Unexpected end of file");
}

namespace error {
LexerError::LexerError(Token token, const char *format, ...) : Error() {
  va_list args;
  va_start(args, format);

  this->make_msg(format, args);
  this->token = token;

  va_end(args);
}

Token LexerError::meta() const { return token; }
} // namespace error
