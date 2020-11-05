#pragma once
#include <string_view>

struct TypeBase {
  const std::string_view type_ident; // MUST only reference a location, and never contain a freeable pointer
  size_t size; // size of current type in bytes
};

struct TypeBool: TypeBase {
  
};


