#pragma once
// егор глупый циплёнок
#include "types.hpp"

#define USE_CPP_STREAM
#include "Lexer.hpp"
#include "Parser.hpp"


#define DELETE_NOT_NULL(p)                                                     \
  if ((p) != nullptr)                                                          \
  delete (p)

