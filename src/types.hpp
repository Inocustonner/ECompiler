#pragma once
#include <memory>

using ulong = unsigned long;
using uint = unsigned int;
// using byte = unsigned char;

// Non-owning ptr
// - pointer view
template <typename T> class ptr_view {
  T *m_ptr = nullptr;

public:
  ptr_view() = default;
  ptr_view(std::nullptr_t) {}
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
  T& operator*() const noexcept { return *m_ptr; }
  T *operator->() const noexcept { return m_ptr; }
};

// Optionaly freed pointer
template <typename T> class ptr_opt_own {
  T* m_ptr = nullptr;
  bool owned = true;

public:
  ptr_opt_own() = default;
  ptr_opt_own(ptr_view<T> ptr_v) { owned = false; m_ptr = ptr_v.get(); }
  ptr_opt_own(const ptr_opt_own<T>& src_ptr) { owned = false; m_ptr = src_ptr.get(); }
  ptr_opt_own(ptr_opt_own<T>&& src_ptr) { owned = src_ptr.is_owned(); m_ptr = src_ptr.unown(); }
  ptr_opt_own(std::nullptr_t) {}
  ptr_opt_own(T* ptr, bool owned) : m_ptr{ ptr }, owned{ owned } {}
  ~ptr_opt_own() { if (owned && m_ptr != nullptr) delete m_ptr; }
  T* get() const noexcept { return m_ptr; }
  T* unown() noexcept { owned = false; T* p = m_ptr; m_ptr = nullptr; return p; }
  void own(T* ptr) const noexcept { owned = true, m_ptr = ptr; }
  bool is_owned() const noexcept { return owned; }
  operator bool() const noexcept { return m_ptr != nullptr; }

  template <typename U> operator U* () const noexcept {
    return static_cast<U*>(m_ptr);
  }

  template <typename PtrT> bool operator==(PtrT ptr) const noexcept {
    return m_ptr == ptr;
  }

  ptr_opt_own<T>& operator=(const ptr_opt_own<T>& ptr) noexcept {
    if (owned && m_ptr) delete m_ptr;

    m_ptr = ptr.get();
    owned = false;
    return *this;
  }

  ptr_opt_own<T>& operator=(ptr_opt_own<T>&& ptr) noexcept {
    if (owned && m_ptr) delete m_ptr;

    m_ptr = ptr.unown();
    owned = ptr.is_owned();
    return *this;
  }

  T& operator*() const noexcept { return *m_ptr; }
  T* operator->() const noexcept { return m_ptr; }
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

template <class PtrView> struct is_ptr_view : std::false_type {};
template <typename UnderlT> struct is_ptr_view<ptr_view<UnderlT>> : std::true_type {};
template <class U> constexpr bool is_ptr_view_v = is_ptr_view<U>::value;

template<class PtrOptOwn> struct is_ptr_opt_own : std::false_type {};
template <typename UnderlT> struct is_ptr_opt_own<ptr_opt_own<UnderlT>> : std::true_type {};
template <class U> constexpr bool is_ptr_opt_own_v = is_ptr_opt_own<U>::value;
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
