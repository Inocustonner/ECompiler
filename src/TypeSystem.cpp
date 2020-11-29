#include "TypeSystem.hpp"
#include "Compiler.hpp"
#include <cassert>
#include <magic_enum>

#define TO_BITS(byte) byte * 8

#define SET_TYPE_SIZE(builtin_type_name, size) size,

constexpr size_t builtin_types_cnt = Type::BuiltIn::BuiltIns;
constexpr size_t type_size_table[builtin_types_cnt] = {
    // keep the order
    SET_TYPE_SIZE(Type::BuiltIn_Int, TO_BITS(4))
        SET_TYPE_SIZE(Type::BuiltIn_Char, TO_BITS(1))};

namespace Type {
  std::string Meta::getFullTypeName() const noexcept {
    return name + (subtype ? subtype->getFullTypeName() : "");
  }

  size_t Meta::getTypeSize() const noexcept {
    if (magic_enum::enum_contains<BuiltIn>(static_cast<BuiltIn>(type_id))) {
      if (type_id == BuiltIn_Ptr) {
        return g_target_arch == Arch::x86 ? TO_BITS(4) : TO_BITS(8);
      } else {
        return type_size_table[type_id];
      }
    } else {
      // user-defined
      assert(false);
    }
  }

  Meta *Meta::deep_copy() const noexcept {
    Meta *meta_copy = new Meta{};
    meta_copy->name = name;
    meta_copy->type_id = type_id;

    if (subtype) {
      if (subtype.is_owned()) {
        meta_copy->subtype = ptr_opt_own{subtype->deep_copy(), true};
      } else {
        meta_copy->subtype = subtype;
      }
    }
    return meta_copy;
  }

  bool Meta::equal(const Meta &meta) const noexcept {
    Meta* m1 = this;
    Meta* m2 = const_cast<Meta*>(&meta);
    while (m1 && m2) {
      // if types equeal go to subtypes
      if (m1->type_id == m2->type_id) {
        m1 = m1->subtype.get();
        m2 = m2->subtype.get();
      } else {
        return false;
      }
      // if they have different subtypes depth
      if (!m1 && m2 || m1 && !m2) {
        return false;
      }
    }
    return true;
  }

  std::array<Meta *, default_flat_types_cnt> &getDefaultFlatTypes() {
    std::array<Meta *, default_flat_types_cnt> flat_types;
    for (size_t i = 0; i < default_flat_types_cnt; ++i) {
      flat_types[i] = new Meta;
      flat_types[i]->type_id = i;
      switch (flat_types[i]->type_id) {
      case BuiltIn_Void:
        flat_types[i]->name = "void";
        break;
      case BuiltIn_Int:
        flat_types[i]->name = "int";
        break;
      case BuiltIn_Char:
        flat_types[i]->name = "char";
        break;
      default:
        assert(0);
      }
    }
    return flat_types;
  }
} // namespace Type
