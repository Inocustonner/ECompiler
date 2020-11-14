#include <fstream>
#include "types.hpp"
#include "error.hpp"

#include <cstdio>
#include <cstdarg>
#include <cassert>
#include <string>

#define RESERVED_BUFFER_SIZE 1024

static char reserved_buffer[RESERVED_BUFFER_SIZE] = {};

using namespace error;
Error::Error(const char* format, ...): std::exception() {
  va_list args;
  va_start(args, format);
  make_msg(format, args);
  va_end(args);
}

Error::Error(): std::exception() {}

Error::~Error() {
  if (msg != reserved_buffer && msg != nullptr)
    free(msg);
}

const char *Error::what() const noexcept {
  return msg;
}

void Error::make_msg(const char* format, va_list args) {
  int len = vsnprintf(msg, 0, format, args) + 1;
  if (len <= RESERVED_BUFFER_SIZE)
    msg = reserved_buffer;
  else
    msg = reinterpret_cast<char*>(malloc(len)); // handle returned nullptr
  vsnprintf(msg, len, format, args);
}

Reporter::Reporter(const char* file_name) {
  file = file_name;
  is.open(file_name);
  assert(is.is_open()); // assert it is open
}

void Reporter::report_error(ReportMeta meta, const char *format, ...) const {
  errors += 1;

  printf("%s\n", file.c_str());
  printf("error C%0.4x: ", static_cast<int>(meta.error));

  va_list args;
  va_start(args, format);
  vprintf_s(format, args);
  va_end(args);
  
  putchar('\n');
#define UNDERLINE_STR(str) "\33[4m" str "\33[0m"
  if (meta.p < meta.end_p) {
    std::string region = print_hl(meta.p, meta.end_p);
    printf("..." UNDERLINE_STR("%.*s") "...\n", (int)region.length(), region.c_str());
  }
}

std::string Reporter::print_hl(ulong p, ulong end_p) const {
  std::string line;
  size_t len = end_p - p + 1;

  line.resize(len);
  is.seekg(p, is.beg);
  is.read(line.data(), len);
  
  return line;
}
