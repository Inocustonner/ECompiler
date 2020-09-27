#pragma once
#include <exception>

namespace error {
  struct Error: std::exception {
    Error(const char* format, ...);
    ~Error();

    const char* what() const noexcept;

    private:
    char* msg;
  };
}
