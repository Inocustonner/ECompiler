#pragma once
// егор глупый циплёнок
#include "types.hpp"

#define USE_CPP_STREAM
#include <magic_enum.hpp>

#include <string>
#include <tuple>
#include <vector>

#include "error.hpp"

#include "Lexer.hpp"
#include "Parser.hpp"


#define DELETE_NOT_NULL(p)                                                     \
  if ((p) != nullptr)                                                          \
  delete (p)

