#include <assert.h>
#define TEST_LEXER_PATH1 "D:\\Projects\\C++\\ECompiler\\test\\LexerTest1.e"

#define LEXER_NEXT_TOKEN lex.next_token()
#define ASSERT assert

#define ASSERT_TOKEN(token_type)                                               \
  token = LEXER_NEXT_TOKEN;                                                    \
  ASSERT(token->tok == token_type)

#define ASSERT_STRING(str_str)                                                 \
  tok_str = (TokenString *)LEXER_NEXT_TOKEN;                                   \
  ASSERT(tok_str->tok == TokenType::String);                                   \
  ASSERT(tok_str->str == str_str)

#define ASSERT_IDENT(ident_str)                                                \
  tok_ident = (TokenIdent *)LEXER_NEXT_TOKEN;                                  \
  ASSERT(tok_ident->tok == TokenType::Ident);                                  \
  ASSERT(tok_ident->ident == ident_str)

#define ASSERT_INT(num)                                                        \
  tok_int = (TokenInt *)LEXER_NEXT_TOKEN;                                      \
  ASSERT(tok_int->tok == TokenType::Int);                                      \
  ASSERT(tok_int->number == num)

#define ASSERT_CHAR(c)                                                         \
  tok_char = (TokenChar *)LEXER_NEXT_TOKEN;                                    \
  ASSERT(tok_char->tok == TokenType::Char);                                    \
  ASSERT(tok_char->ch == c)

bool test1() {
  try {
    Token *token;
    TokenIdent *tok_ident;
    TokenString *tok_str;
    TokenInt *tok_int;
    TokenChar *tok_char;
    Lexer lex = Lexer{TEST_LEXER_PATH1};
    ASSERT_TOKEN(TokenType::OBracket);

    ASSERT_IDENT("string_var");
    ASSERT_TOKEN(TokenType::Eq);
    ASSERT_STRING("string ???");
    ASSERT_TOKEN(TokenType::Terminal);

    ASSERT_IDENT("char_var");
    ASSERT_TOKEN(TokenType::Eq);
    ASSERT_CHAR('c');
    ASSERT_TOKEN(TokenType::Terminal);

    ASSERT_IDENT("identificator");

    ASSERT_TOKEN(TokenType::Colon);
    ASSERT_IDENT("type_ident");

    ASSERT_TOKEN(TokenType::Eq);
    ASSERT_TOKEN(TokenType::OParen);

    ASSERT_INT(1);
    ASSERT_TOKEN(TokenType::Plus);
    ASSERT_INT(2);
    ASSERT_TOKEN(TokenType::Asterisk);
    ASSERT_INT(3);
    ASSERT_TOKEN(TokenType::Minus);
    ASSERT_INT(4);
    ASSERT_TOKEN(TokenType::Slash);
    ASSERT_INT(5);

    ASSERT_TOKEN(TokenType::CParen);
    ASSERT_TOKEN(TokenType::Terminal);

    ASSERT_TOKEN(TokenType::CBracket);

    ASSERT_TOKEN(TokenType::Eof);
    ASSERT_TOKEN(TokenType::Eof);
    return true;
  } catch (const error::LexerError &e) {
    Token tok = e.meta();
    printf("(%d, %d): %s\n", tok.line, tok.col, e.what());
  } catch (const error::Error &e) {
    printf("%s\n", e.what());
  }
  return false;
}
