#pragma once
// егор глупый циплёнок
#include "types.hpp"

// #define USE_IRPASCAL

#define USE_CPP_STREAM

#ifdef USE_CPP_STREAM
#include <fstream>
#endif

#include <magic_enum.hpp>

#include <string>
#include <tuple>
#include <vector>
#include <unordered_map>

#include "error.hpp"

#include "Lexer.hpp"
#include "Parser.hpp"

#define USE_IRPASCAL
#include "IRPascal.hpp"

#define DELETE_NOT_NULL(p)                                                     \
  if ((p) != nullptr)                                                          \
  delete (p)

enum class Arch {
  x86,
  x86_64
};
extern Arch g_target_arch;

template<typename T>
void own(std::unique_ptr<T> &dst, T* &p) {
  dst.reset(p);
  p = nullptr;
}

template<typename T>
void own(T* &dst, T* &p) {
  dst = p;
  p = nullptr;
}

template<typename T>
inline T safe_deref(T* ptr) {
  if (ptr) return *ptr;
  else return nullptr;
}
