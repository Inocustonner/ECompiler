#pragma once
#include "types.hpp"

#include <string_view>
#include <memory>

namespace SymbolTable {
template <typename SymbolT> class SymbolTableIntrl {
public:
  std::unordered_map<std::string_view, SymbolT> hash_map;
  SymbolTableIntrl<SymbolT> *enclosing = nullptr;

  SymbolTableIntrl() = default;
  SymbolTableIntrl(SymbolTableIntrl<SymbolT>* enclosing_table)
    : enclosing{enclosing_table} {}


  SymbolT *find(const std::string_view name) {
    auto result = hash_map.find(name);
    if (result != hash_map.end())
      return &result->second;
    else if (enclosing)
      return enclosing->find(name);
    else
      return nullptr; // not found
  }

  SymbolT* find_local(const std::string_view name) {
    auto result = hash_map.find(name);
    if (result != hash_map.end())
      return &result->second;
    else
      return nullptr; // not found
  }

  SymbolTableIntrl<SymbolT>* createEnclosed() {
    return new SymbolTableIntrl(this);
  }

  bool insert(std::string_view ident, SymbolT sym) {
    return hash_map.insert(std::make_pair(ident, sym)).second;
  }
};
} // namespace SymbolTable

enum class SymbolType {
  Var,
  Func,
  Arg,
  Type,
};
#define SymbolCONS(type) \
  IRPSymbol ## type (): IRPSymbol{SymbolType::type} {}

struct IRPSymbol {
  SymbolType sym_type;
  std::unique_ptr<std::string> ident;

  ulong p = 0;
  ulong end_p = 0;
};

struct IRPSymbolVar: public IRPSymbol {
  SymbolCONS(Var);
  std::unique_ptr<Type::Meta> var_type;
};

struct IRPSymbolArg: public IRPSymbol {
  SymbolCONS(Arg);
  std::unique_ptr<Type::Meta> var_type;
};

struct IRPSymbolType: public IRPSymbol {
  SymbolCONS(Type);
  std::unique_ptr<Type::Meta> var_type;
};

struct IRPascalNodeFunc;
struct IRPSymbolFunc: public IRPSymbol {
  SymbolCONS(Func);
  ptr_view<IRPascalNodeFunc> irp_func = nullptr;
  std::unique_ptr<Type::Meta> ret_type;
};

using PasSymTable = SymbolTable::SymbolTableIntrl<IRPSymbol*>;

#undef SymbolCONS
