#pragma once
#include "types.hpp"
#include <string>
#include <vector>

namespace Type {
  using idtype = size_t;
  enum BuiltIn: idtype {
    BuiltIn_Void = 0, // no-type

    BuiltIn_Int,
    BuiltIn_Char,


    
    BuiltIn_Ptr,

    BuiltIns
  };

  struct Meta {
    std::string name;
    idtype type_id;

    // for ptr, array or else
    ptr_opt_own<Meta> subtype = nullptr;

    // includes subtypes
    std::string getFullTypeName() const noexcept;
    size_t getTypeSize() const noexcept;
    Meta *deep_copy() const noexcept;
    bool equal(const Meta& meta) const noexcept;
  };

  template<typename ...Metas>
  bool isBuiltIns(Metas&&... metas) {
    std::array<const Meta*, sizeof...(Metas)> ms = {metas...};
    for (const Meta* meta : ms) {
      if (!magic_enum::enum_contains((BuiltIn)meta->type_id)) return false;
    }
    return true;
  }

  template<typename ...Metas>
  bool isIntegerBuiltins(Metas&&... metas) {
    std::array<const Meta*, sizeof...(Metas)> ms = { metas... };
    for (const Meta* meta : ms) {
      switch (meta->type_id) {
      case BuiltIn_Int:
      case BuiltIn_Char:
        continue;
      default: return false;
      }
    }
    return true;
  }

  template<typename ...Metas>
  bool isPointer(Metas&&... metas) {
    std::array<const Meta*, sizeof...(Metas)> ms = { metas... };
    for (const Meta*meta : ms) {
      if (meta->type_id != BuiltIn_Ptr) return false;
    }
    return true;
  }

  constexpr auto default_flat_types_cnt = BuiltIn_Char + 1;
  std::array<Meta*, default_flat_types_cnt>& getDefaultFlatTypes();
} // namespace Type
