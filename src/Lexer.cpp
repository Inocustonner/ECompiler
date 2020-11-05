#include "Compiler.hpp"
#include "error.hpp"
#include <cstdarg>

#define IN_RANGE_INCL(from, n, to) (from <= n && n <= to)
#define INIT_TOKEN(ident, tok_type_enum)                                       \
  (ident)->p = p;                                                              \
  (ident)->line = line;                                                        \
  (ident)->col = col;                                                          \
  (ident)->tok = tok_type_enum;
#define CONST_LEN(arr) (sizeof(arr)/sizeof(*arr))
static bool isVarChar(CharT c, bool include_numbers = false) {
  return IN_RANGE_INCL('a', c, 'z') || IN_RANGE_INCL('A', c, 'Z') ||
         (c == '_') || (include_numbers && IN_RANGE_INCL('0', c, '9'));
}

static bool isKeyWord(std::string *word, TokenType *tok) {
  for (int i = 0; i < CONST_LEN(g_key_words); ++i) {
    if (std::get<0>(g_key_words[i]) == *word) {
      if (tok)
        *tok = std::get<1>(g_key_words[i]);
      return true;
    }
  }
  return false;
}

void freeToken(Token *tok) {
  assert(tok != nullptr);
  switch (tok->tok) {
  case TokenType::String:
    DELETE_NOT_NULL(reinterpret_cast<TokenString *>(tok)->str);
    delete tok;
    break;
  case TokenType::Ident:
    DELETE_NOT_NULL(reinterpret_cast<TokenIdent *>(tok)->ident);
    delete tok;
    break;
  default:
    delete tok;
  }
}

std::string serializeToken(PToken token) {
  std::string seril;
  seril +=
      "Token " + std::string(magic_enum::enum_name(token->tok).data()) + "\n";
  switch (token->tok) {
  case TokenType::Ident: {
    seril += "\tident: " + *static_cast<TokenIdent *>(token)->ident + "\n";
  } break;
  case TokenType::String: {
    seril += "\tstr: " + *static_cast<TokenString *>(token)->str + "\n";
  } break;
  case TokenType::Char: {
    seril +=
        "\tch: " + std::to_string(static_cast<TokenChar *>(token)->ch) + "\n";
  } break;
  case TokenType::Int: {
    seril +=
        "\tnumber: " + std::to_string(static_cast<TokenInt *>(token)->number) +
        "\n";
  } break;
  }
  seril += "\tline: " + std::to_string(token->line) + "\n";
  seril += "\tcol: " + std::to_string(token->col) + "\n";
  seril += "\tp: " + std::to_string(token->p) + "\n";
  seril += "\tend_p: " + std::to_string(token->end_p) + "\n";
  return seril;
}
#ifndef USE_CPP_STREAM
Lexer::Lexer(const char *file_path) {
  fopen_s(&file_p, file_path, "rb");
  if (file_p == nullptr) {
    throw error::Error("Unnable to open %s file", file_path);
  }
  advance();
}
Lexer::~Lexer() { fclose(file_p); }
#else
Lexer::Lexer(const char *file_path) {
  file_s.open(file_path, std::ifstream::in);
  if (!file_s.is_open()) {
    throw error::Error("Unnable to open %s file", file_path);
  }
  advance();
}
Lexer::~Lexer() { file_s.close(); }
#endif

void Lexer::advance() {
#ifndef USE_CPP_STREAM
  curr = fgetc(file_p);
#else
  curr = file_s.get();
#endif
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
    token->p = p, token->line = line, token->col = col, token->end_p = p + 1;  \
    token->tok = toktype;                                                      \
    advance();                                                                 \
    return token;                                                              \
  } break;

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
    token->end_p = p;
    return token;
  }
    TOKEN_MATCH_RETURN(';', TokenType::Terminal);

    TOKEN_MATCH_RETURN(',', TokenType::Comma);

    TOKEN_MATCH_RETURN('[', TokenType::OBracket);
    TOKEN_MATCH_RETURN(']', TokenType::OBracket);
    TOKEN_MATCH_RETURN('{', TokenType::OBracer);
    TOKEN_MATCH_RETURN('}', TokenType::CBracer);
    TOKEN_MATCH_RETURN('(', TokenType::OParen);
    TOKEN_MATCH_RETURN(')', TokenType::CParen);

    TOKEN_MATCH_RETURN('+', TokenType::Plus);
    TOKEN_MATCH_RETURN('-', TokenType::Minus);
    TOKEN_MATCH_RETURN('*', TokenType::Asterisk);
    TOKEN_MATCH_RETURN('/', TokenType::Slash);
    TOKEN_MATCH_RETURN('=', TokenType::Eq);

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
    token->str = new std::string;

    advance();

    do {
      token->str->push_back(curr);
      advance();
    } while (curr != '"' && curr != EOF);

    if (curr == '"') {
      advance();
      token->end_p = p;
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
      token->end_p = p;
      token->number = number;
      return token;

    } else if (isVarChar(curr)) {
      std::string *word = new std::string;
      auto start_p = p;
      do {
        // ident_tok->ident->push_back(curr);
        word->push_back(curr);
        advance();
      } while (isVarChar(curr, true));

      TokenType keyWord;
      if (isKeyWord(word, &keyWord)) {
        Token *tok = new Token{};
        INIT_TOKEN(tok, keyWord);
        tok->p = start_p;
        tok->end_p = p;
        return tok;
      } else {
        TokenIdent *ident_tok = new TokenIdent;
        INIT_TOKEN(ident_tok, TokenType::Ident);
        ident_tok->ident = word;
        ident_tok->p = start_p;
        ident_tok->end_p = p;
        return ident_tok;
      }
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
