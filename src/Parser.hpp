#pragma once
#define PARSER
#include "RingArray.hpp"
#include <string>
#include <vector>

#define LL_N 2

enum class AstType {
  StmtBlock,

  Expr,

  VarType, // t_ident

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

  AddrOf,
  DeRef,
  SeqAccess,

  Ident,
  Number,
  Char,
  String,
};

enum class NumberClass { Integer };

enum class TypeModEnum { Ptr, Array };

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
      union {
        AstExpr *fst; // used in return, if, for,
        AstExpr *expr;
      };
      AstExpr *snd; // optional
    };
    struct {
      NumberClass num_class; // ignored for now
      long integer;
    };
    std::string *ident;
    std::string *str;
    AstExpr *ret_expr; // used in AstReturn
    char c;
  };
};

struct AstVarType : public Ast {
  struct TypeMod {
    TypeModEnum mod;
    AstExpr *expr; // optional, only for array mod
  };
  std::string *type_ident;
  std::vector<TypeMod> mods;
};

using AstReturn = AstExpr;

struct AstVarDecl : public Ast {
  std::string *ident;
  AstVarType *var_type;
  AstExpr *init_expr; // optional
};

struct AstFuncArg : public Ast {
  std::string *ident;
  AstVarType *var_type;
};

struct AstStmtBlock;

struct AstFuncDecl : public Ast {
  std::vector<AstFuncArg *> args;
  std::string *ident;
  AstVarType *ret_type;
  AstStmtBlock *stmt_block;
  ulong header_end_p;
};

struct AstStmtBlock : public Ast {
  std::vector<Ast *> stmt_vec;
};

void freeAst(Ast *ast);
std::string serializeAstXml(const Ast *ast);

#ifndef LEXER
enum class TokenType;
struct Token;
using PToken = Token*;
struct Lexer;
#endif

#ifndef ERROR_REPORTER 
namespace error {
  struct Reporter;
}
#endif

struct Parser {
  Parser(Lexer& lex, error::Reporter& reporter);
  ~Parser();

  Ast *parse();
  AstStmtBlock *stmtBlock(bool braced = true);
  Ast *varDecl();
  Ast *funcDecl();
  AstVarType *varType();
  // expr asts
  Ast *returnStmt();
  AstExpr *expr();       // assignment
  AstExpr *expr_arith(); // + -
  AstExpr *term();       // * /
  AstExpr *factor();     // ( ), number, ident, deref, addr_of, seq_access
  // expr asts
private:
  void advance();
  PToken &lookUp(ulong i);
  PToken *plookUp(ulong i);

  TokenType peek(ulong i);
  void match(TokenType type);

private:
  Lexer& m_lex;
  RingArray<PToken, LL_N> tok_buffer = {};
  error::Reporter& reporter;
};
//
//namespace error {
//struct ParserError : public error::Error {
//public:
//  ParserError(Ast, const char *format, ...);
//  ParserError(Ast *, const char *format, ...); // will copy and free
//  Ast meta() const;
//
//private:
//  Ast ast;
//};
//} // namespace error
