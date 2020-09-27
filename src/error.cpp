#include "error.hpp"
#include <cstdio>
#include <cstdarg>

#define RESERVED_BUFFER_SIZE 1024

static char reserved_buffer[RESERVED_BUFFER_SIZE] = {};

using namespace error;

Error::Error(const char* format, ...) {
  va_list args;
  va_start(args, format);
  int len = vsnprintf(msg, 0, format, args) + 1;
  if (len <= RESERVED_BUFFER_SIZE)
    msg = reserved_buffer;
  else
    msg = reinterpret_cast<char*>(malloc(len)); // handle returned nullptr
  vsnprintf(msg, len, format, args);
  va_end(args);
}

Error::~Error() {
  if (msg != reserved_buffer && msg != nullptr)
    free(msg);
}

const char *Error::what() const noexcept {
  return msg;
}