#pragma once
#include "RingArray.hpp"

#define LL_N 2

enum class AstType {
  StmtBlock,

  Expr,

  VarDecl,
  FuncDecl,
  FuncArg,

  Return
};

enum class ExprType {
  Add,
  Sub,
  Mult,
  Div,

  Assign,

  Ident,
  Number,
  Char,
  String,
};

enum class NumberClass {
  Integer
};

struct Ast {
  AstType ast;
  // ulong line;
  // ulong col;
  ulong p;
  ulong end_p;
};

struct AstExpr : public Ast {
  ExprType expr_type;
  union {
    struct {
      AstExpr *fst; // used in return, if, for, 
      AstExpr *snd; // optional
    };
    struct {
      NumberClass num_class; // ignored for now
      long integer;      
    };
    std::string* ident;
    std::string* str;
    AstExpr *ret_expr; // used in AstReturn
    char c;
  };
};

using AstReturn = AstExpr;

struct AstVarDecl : public Ast {
  std::string *ident;
  std::string *type_ident;
  AstExpr *init_expr;      // optional
  ulong type_ident_p, type_ident_end_p;
};

struct AstFuncArg: public Ast {
  std::string *ident;
  std::string *type_ident;
  ulong type_ident_p, type_ident_end_p;
};

struct AstStmtBlock;

struct AstFuncDecl : public Ast {
  std::vector<AstFuncArg*> args;
  std::string *ident;
  std::string *ret_type;
  AstStmtBlock *stmt_block;
  ulong header_end_p;
  ulong type_ident_p, type_ident_end_p;
};

struct AstStmtBlock : public Ast {
  std::vector<Ast *> stmt_vec;
};

void freeAst(Ast* ast);
std::string serializeAstXml(const Ast* ast);

struct Parser {
  Parser(const char* file_path);
  ~Parser();

  Ast *parse();
  AstStmtBlock *stmtBlock(bool braced = true);
  Ast *varDecl();
  Ast *funcDecl();
  // expr asts
  Ast *returnStmt();
  AstExpr *expr();       // assignment
  AstExpr *expr_arith(); // + -
  AstExpr *term();       // * /
  AstExpr *factor();     // ( ), number, ident
  // expr asts
private:
  void advance();
  PToken &lookUp(ulong i);
  PToken *plookUp(ulong i);
  
  TokenType peek(ulong i);
  void match(TokenType type);

private:
  Lexer m_lex;
  RingArray<PToken, LL_N> tok_buffer = {};
};

namespace error {
struct ParserError : public error::Error {
public:
  ParserError(Ast, const char *format, ...);
  ParserError(Ast *, const char *format, ...); // will copy and free
  Ast meta() const;

private:
  Ast ast;
};
} // namespace error
