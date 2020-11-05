#include "Compiler.hpp"
#include <cmdparser.hpp>
#include <iostream>

#define LEXER_FILE_SUFFIX ".elex"
#define PARSER_FILE_SUFFIX ".eparse.xml"

bool g_verbose = false;

void set_parser_args(cli::Parser &parser) {
#define SET_OPT(type, ...) parser.set_optional<type>(__VA_ARGS__)
#define SET_OPT_NA(type, name, ...)                                            \
  SET_OPT(type, name, name, __VA_ARGS__) // no alternative name
#define SET_REQ(type, ...) parser.set_required<type>(__VA_ARGS__)

  SET_REQ(std::vector<std::string>, "i", "input",
          "Input files for compilation");
  SET_OPT_NA(bool, "out-lex", false,
             "Run lexer and produce token output to "
             "<CompilationUnit>." LEXER_FILE_SUFFIX " assosiated files");
  SET_OPT_NA(bool, "out-parse", false,
             "Run parser and produce ast output to "
             "<CompilationUnit>." PARSER_FILE_SUFFIX " assosiated files");
  SET_OPT(bool, "v", "verbose", false, "Show verbose output");
#undef SET_OPT_NA
#undef SET_REQ
#undef SET_OPT
}

template <typename... Args> void printfv(Args &&...args) {
  if (g_verbose)
    printf(args...);
}

void runLexer(const std::vector<std::string> &input) {
  for (const auto &file : input) {
    std::string elex_path = file + LEXER_FILE_SUFFIX;

    FILE *f = fopen(elex_path.c_str(), "wb");
    if (f == NULL) {
      printf("Couldn't create/open file %s for writing\n", elex_path.c_str());
      continue;
    }

    printfv("Running lexer for %s...\n", file.c_str());
    Lexer lex{file.c_str()};

    Token *tok = nullptr;
    TokenType prev_type;
    do {
      tok = lex.next_token();
      prev_type = tok->tok;
      std::string token_info = serializeToken(tok);
      fwrite(token_info.c_str(), token_info.length(), 1, f);
      freeToken(tok);
    } while (prev_type != TokenType::Eof);
    printfv("[Done]\n");
    fclose(f);
  }
}

void runParser(const std::vector<std::string> &input) {
  for (const auto &file : input) {
    std::string out_path = file + PARSER_FILE_SUFFIX;

    FILE *f = fopen(out_path.c_str(), "wb");
    if (f == NULL) {
      printf("Couldn't create/open file %s for writing\n", out_path.c_str());
      continue;
    }
    
    printfv("Running parser for %s...\n", file.c_str());
    Parser parser{file.c_str()};

    Ast* ast = parser.parse();
    const std::string output = serializeAstXml(ast);
    freeAst(ast);
    
    fwrite(output.c_str(), output.length(), 1, f);
    
    printfv("[Done]\n");
    fclose(f);
  }
}

int main(int argc, char **argv) {
#define GET_FLAG(flag_name) parser.get<bool>(flag_name)
  cli::Parser parser(argc, argv);
  set_parser_args(parser);
  parser.run_and_exit_if_error();

  g_verbose = GET_FLAG("v");

  try {
    std::vector<std::string> input = parser.get<std::vector<std::string>>("i");

    if (GET_FLAG("out-lex")) {
      runLexer(input);
      return 0;
    } else if (GET_FLAG("out-parse")) {
      runParser(input);
      return 0;
    }
  }  catch (error::ParserError &e) {
    printf("(from:%d, to:%d) %s\n", e.meta().p, e.meta().end_p, e.what());
  } catch (error::LexerError &e) {
    printf("(line: %d, col: %d) %s\n", e.meta().line, e.meta().col, e.what());
  } catch (std::exception &e) {
    printf("%s\n", e.what());
  }
  return 0;
}
