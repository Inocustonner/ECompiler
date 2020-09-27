#include "Lexer.hpp"
#include "error.hpp"
#include <cstdarg>

Lexer::Lexer(const char* file_path) {
  fopen_s(&file_p, file_path, "rb");
  if (file_p == nullptr) {
    throw error::Error("Unnable to open %s file", file_path);
  }
  advance();
}

Lexer::~Lexer() {
  fclose(file_p);
}

void Lexer::advance() {
  curr = fgetc(file_p);
  if (curr != EOF) {
    p += 1;
    if (curr == '\n') {
      line += 1;
      col = 0;
    }
  }
}

#define TOKEN_MATCH_RETURN(tokc, toktype)               \
  case tokc: {                                          \
    Token* token = new Token;                           \
    token->p = p, token->line = line, token->col = col; \
    token->tok = toktype;                               \
    advance();                                          \
    return token;                                       \
  } break;

Token* Lexer::next_token() {
  while (isspace(curr)) advance();
  switch (curr) {
    TOKEN_MATCH_RETURN( ';', TokenType::Terminal )

    case ':': {
      Token* token = new Token;
      token->p = p, token->line = line,
      token->col = col;
      token->tok = TokenType::Colon;
      advance();
      if (curr == ':') {
        token->tok = TokenType::DColon;
        advance();
      }
      return token;
    }

    TOKEN_MATCH_RETURN( '{', TokenType::OBracket )
    TOKEN_MATCH_RETURN( '}', TokenType::CBracket )
    TOKEN_MATCH_RETURN( '(', TokenType::OParen )
    TOKEN_MATCH_RETURN( ')', TokenType::CParen )

    TOKEN_MATCH_RETURN( '+', TokenType::Plus )
    TOKEN_MATCH_RETURN( '-', TokenType::Minus )
    TOKEN_MATCH_RETURN( '*', TokenType::Asterisk )
    TOKEN_MATCH_RETURN( '/', TokenType::Slash )

    default:
    // number
    if (isdigit(curr)) {
#define CHAR_DIGIT_TO_I(c) ((c) - 0x30)
      long number = CHAR_DIGIT_TO_I(curr);
      long exp = 10;
      advance();

      bool hex = curr == 'x';
      if (hex)
        exp = 0x10;
      bool octal = curr == 'o';
      if (octal)
        exp = 8;
      bool decimal = isdigit(curr);
      if (decimal)
        number = number * exp + CHAR_DIGIT_TO_I(curr);
      advance();

      TokenNumber token = {
        .p = p, .line = line, 
        .col = col, .tok = TokenType::Int
      };

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
            throw error::LexerError(token, "Character '%c' doesn't belong to octal digits", curr);
          }
          number = number * exp + CHAR_DIGIT_TO_I(curr);

        } else if (auto hnumber = hexalphanumber(curr); hnumber != -1 && hex) {
          number = number * exp + hnumber;
        } else {
          throw error::LexerError(token,
                                  "Character '%c' doesn't belong to %s numbers", curr,
                                  decimal ? "decimal" : hex ? "hex" : "octal");
        }
      }
    }
    // ident
  }
}

namespace error {
  LexerError::LexerError(Token token, const char* format, ...): Error() {
    va_list args;
    va_start(args, format);

    this->make_msg(format, args);
    this->token = token;

    va_end(args);
  }

  Token LexerError::meta() {
    return token;
  }
}