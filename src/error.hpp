#pragma once
#include <exception>

namespace error {
  struct Error: public std::exception {
    Error(const char* format, ...);
    Error();
    virtual ~Error();

    const char* what() const noexcept;

    protected:
     void make_msg(const char* format, va_list args);
     char* msg;
  };
};  // namespace error
