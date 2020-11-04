#pragma once
#include <array>
#include <cassert>

template <typename Type, ulong size>
struct RingArray {
  Type& get(size_t i) {
    assert(i < size);
    return array[(i + p) % size];
  }

  void push(Type t) {
    array[p % size] = t;
    p = (p + 1) % size;
  }
  std::array<Type, size> array = {};
  ulong p = 0; // current point
};
