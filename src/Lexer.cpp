#include "Lexer.hpp"
#include "error.hpp"

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
    // ident
  }
}