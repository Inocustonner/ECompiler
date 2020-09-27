#include <iostream>
#define FUNC(...) __VA_ARGS__

enum E {
  A, B
};

template<typename E>
void d() {
	std::cout << __FUNCDNAME__ << __FUNCSIG__ << __FUNCTION__ << __func__ << std::endl;
}

int main() {
  std::cout << __FUNCDNAME__ << __FUNCSIG__ << __FUNCTION__ << __func__ << std::endl;
  return 0;
}