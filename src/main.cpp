#include <iostream>
#include "Compiler.hpp"

#define LEXER_TEST
#ifdef LEXER_TEST
#include "../test/LexerTest.cpp"
#endif

int main() {
  #ifdef LEXER_TEST
  test1();
  #endif
  return 0;
}
