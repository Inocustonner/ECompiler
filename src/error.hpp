#pragma once
#include <exception>

namespace error {
enum class ErrorEnum {
  Invalid_Symbol = 0,
  Unexpected_Token,

  Undeclared_Ident,

  ReDecl,
  Undefined_Type,
  Is_Not_A_Type,

  Assignment_To_Const,
  Invalid_Cast
};

struct Error : public std::exception {
  Error(const char *format, ...);
  Error();
  virtual ~Error();

  const char *what() const noexcept;

protected:
  void make_msg(const char *format, va_list args);
  char *msg;
};

struct ReportMeta {
  union {
    ErrorEnum error;
  };
  ulong p, end_p;
  ulong p1, end_p1; // some related info
};

struct Reporter {
  std::string file;
  mutable std::ifstream is;
  mutable ulong errors = 0;
  Reporter(const char *file_name);
  void report_error(ReportMeta meta, const char *format, ...) const;

private:
  std::string print_hl(ulong p, ulong end_p) const;
  // void report_warning(ReportMeta meta, const char* format, ...);
  // void report_info(ReportMeta meta, );
};
}; // namespace error
