#include <fstream>
#include <sstream>
#include <string>
#include <cassert>
#include <cstdarg>
#include <cstdio>
#include "types.hpp"
#include "error.hpp"

#define RESERVED_BUFFER_SIZE 1024

static char reserved_buffer[RESERVED_BUFFER_SIZE] = {};

using namespace error;
Error::Error(const char *format, ...) : std::exception() {
  va_list args;
  va_start(args, format);
  make_msg(format, args);
  va_end(args);
}

Error::Error() : std::exception() {}

Error::~Error() {
  if (msg != reserved_buffer && msg != nullptr)
    free(msg);
}

const char *Error::what() const noexcept { return msg; }

void Error::make_msg(const char *format, va_list args) {
  int len = vsnprintf(msg, 0, format, args) + 1;
  if (len <= RESERVED_BUFFER_SIZE)
    msg = reserved_buffer;
  else
    msg = reinterpret_cast<char *>(malloc(len)); // handle returned nullptr
  vsnprintf(msg, len, format, args);
}

Reporter::Reporter(const char *file_name) {
  file = file_name;
  is.open(file_name, std::ifstream::binary | std::ifstream::in);
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
  if (meta.p < meta.end_p) {
    std::string region = format_error(meta.p, meta.end_p);
    printf("%s\n\n", region.c_str());
  }
}

size_t Reporter::seekToLineBeg(long skip_lines) const {
  size_t p = is.tellg();

  skip_lines += 1;
  while (skip_lines && is.tellg() > 0) {
    char c;
    c = is.get();
    if (c == '\n') skip_lines--;
    is.unget();
    is.seekg(-1, is.cur);
  }

  if (is.tellg() != 0) {
    is.seekg(2, is.cur);
  }
  return p - is.tellg();
}

std::string Reporter::readBackwards(ulong p) const {
  is.clear();

  std::string line = "";
  is.seekg(p, is.beg);
  size_t len = seekToLineBeg(additional_lines_above);
  if (len > 0) {
    line.resize(len);
    is.read(line.data(), len);
  }
  return line;
}

std::string Reporter::readForward(ulong p) const {
  is.clear();

  std::string line = "";
  size_t lines_cnt = additional_lines_below + 1;
  is.seekg(p, is.beg);
  while (lines_cnt-- && !is.eof()) {
    std::string tmp;
    std::getline(is, tmp);
    line += tmp + '\n';
  }
  return line;
}

size_t Reporter::getLineFromPos(size_t p) const {
  size_t curr_pos = is.tellg();
  is.seekg(0, is.beg);
  size_t line = 0;
  while (is.tellg() < p) {
    line += 1;
    is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }
  return line;
}

std::string Reporter::format_error(ulong p, ulong end_p) const {
  std::string line;
  size_t len = end_p - p;

  line.resize(len);
  is.seekg(p, is.beg);
  is.read(line.data(), len);

#define UNDERLINE_STR(str) "\33[4m" str "\33[0m"
#define _TO_STR(x) # x
#define TO_STR(x) _TO_STR(x)
#define ESC(seq) "\33[" _TO_STR(seq) "m"
#define RGB(r,g,b) r;g;b
#define UNDERLINE_STRING(str) ESC(48;2;RGB(170,45,27)) ESC(4) + str + ESC(0)
#define SET_FOREGROUND_COLOR(str) /*ESC(38;2;RGB(150,150,150)) + */str/* + ESC(0) */
  std::string precc_lines= readBackwards(p);

  const char end_line_char = '\n';
  size_t lines_up = std::count(precc_lines.begin(), precc_lines.end(), end_line_char);
  size_t curr_line = getLineFromPos(p) - lines_up;
  
  std::string colored = SET_FOREGROUND_COLOR(precc_lines) + SET_FOREGROUND_COLOR(UNDERLINE_STRING(line)) + SET_FOREGROUND_COLOR(readForward(end_p));
  std::string result = "";
  size_t prefix_len = (size_t)std::log10(curr_line) + 3;
  std::istringstream input;
  input.str(colored);
  for (std::string line; std::getline(input, line);) {
    auto line_n = std::to_string(curr_line++);
    result += line_n + std::string(prefix_len - std::size(line_n), ' ') + line + "\n";
  }
  return result;
}
