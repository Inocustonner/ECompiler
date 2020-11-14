#pragma once
#include <memory>

using ulong = unsigned long;
using uint = unsigned int;
// using byte = unsigned char;

// Unfreeable pointer
// - pointer view
// - non owning
template <typename T> class ptr_view {
  T *m_ptr = nullptr;

public:
  ptr_view() = default;
  ptr_view(T *ptr) : m_ptr{ptr} {}
  T *get() const noexcept { return m_ptr; }
  operator bool() const noexcept { return m_ptr != nullptr; }

  template <typename U> operator U *() const noexcept {
    return static_cast<U *>(m_ptr);
  }

  template <typename PtrT> bool operator==(PtrT ptr) const noexcept {
    return m_ptr == ptr;
  }

  ptr_view<T>& operator=(T* ptr) noexcept { m_ptr = ptr; return *this; }
  T operator*() const noexcept { return *m_ptr; }
  T *operator->() const noexcept { return m_ptr; }
};

namespace types_type_traits {

template <class U, typename... Args> struct is_unique_ptr : std::false_type {};

template <typename... Args>
struct is_unique_ptr<std::unique_ptr<Args...>> : std::true_type {};

template <class U> constexpr bool is_unique_ptr_v = is_unique_ptr<U>::value;

template <typename T, class... Args> struct unique_ptr_underlying_type {
  using type = void;
};

template <typename T> struct unique_ptr_underlying_type<std::unique_ptr<T>> {
  using type = T;
};

template <class Uptr>
using unique_ptr_underlying_type_t =
    typename unique_ptr_underlying_type<Uptr>::type;
} // namespace types_type_traits

template <typename PtrTOut, typename T>
PtrTOut uptr_cast(std::unique_ptr<T> &uptr) {
  return static_cast<PtrTOut>(uptr.get());
}

template <typename CastT, typename T>
std::unique_ptr<CastT> uptr_deep_copy(std::unique_ptr<T>& src) {
  if (src)
    return std::unique_ptr<T>(new T{ *static_cast<CastT*>(src) });
  else
    return nullptr;
}

template <typename T>
std::unique_ptr<T> uptr_deep_copy(std::unique_ptr<T> &src) {
  return uptr_deep_copy<T>(src);
}
