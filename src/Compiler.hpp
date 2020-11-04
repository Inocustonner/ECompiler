#pragma once
#define USE_CPP_STREAM

#include "types.hpp"
#include "Lexer.hpp"
#include "Parser.hpp"

#define DELETE_NOT_NULL(p)                                                     \
  if ((p) != nullptr)                                                          \
  delete (p)
