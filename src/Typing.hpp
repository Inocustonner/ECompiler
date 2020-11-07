#pragma once
#include <string_view>

enum class TypeEnum {
  Int,
  
  Bool,
  
  String,
  Char,
  
  // programmer defined like struct in C
  Complex,
};


struct Type {
  TypeEnum type;
  std::string_view alias;
};

static std::tuple<const char*, Type> g_ident_type_map[] = {
  {"int", { TypeEnum::Int, "int"} },
  {"bool", { TypeEnum::Bool, "bool" }},
  {"char", { TypeEnum::Char, "char" }}
};
