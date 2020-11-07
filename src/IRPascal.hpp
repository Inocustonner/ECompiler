#pragma once
// #include "Typing.hpp"

#define IRPascalNodeCONS(type) \
  IRPascalNode ## type (): IRPascalNode{IRPascalNodeType::type} {}

enum class IRPascalNodeType {
  Block,
  
  Func,
  ArgDecl,
  VarDecl,
  Const,

  Mutate,
  Return
};

enum class IRMutation {
  Add,
  Sub,
  Mult,
  Div,
  
  Assign
};

enum class IRConst {
  Int,
  String,
  Char
};

struct IRPascalNode {
  IRPascalNodeType node_type;
  // ulong p, end_p;
};

struct IRPascalNodeTemp: public IRPascalNode {
  ulong id;
  Type var_type;
};

struct IRPascalNodeConst : public IRPascalNode {
  IRPascalNodeCONS(Const);
  Type const_type;
  union {
    std::string* str;
    long integer;
    char ch;
  };
};

struct IRPascalNodeMutate: public IRPascalNode {
  IRPascalNodeCONS(Mutate);
  IRMutation mutation;
  IRPascalNode *dst = nullptr; // IRPascalVar or IRPascalTemp
  IRPascalNode *src = nullptr;
};

// struct IRPascalNodeArgDecl: public IRPascalNode {
//   IRPascalNodeCONS(ArgDecl);
//   std::string_view ident;
//   Type var_type;
// };

struct IRPascalNodeBlock: public IRPascalNode {
  IRPascalNodeCONS(Block);
  std::vector<IRPascalNode*> stmts;
  PasSymTable* sym_table = nullptr;
};

struct IRPascalNodeFunc: public IRPascalNode {
  IRPascalNodeCONS(Func);
  std::vector<std::string_view*> arg_ident_vec; // args will be in sym table of the block
  IRPascalNodeBlock* block;
};
#undef IRPascalNodeCONS

struct IRPascalProducer {
  PasSymTable* curr_sym_table = nullptr;
  IRPascalNodeBlock* current_block = nullptr;
  const error::Reporter& reporter;

  IRPascalProducer(const error::Reporter& reporter);
  
  IRPascalNode* produce(Ast* ast);
  IRPascalNodeBlock* produceStmtBlock(AstStmtBlock* stmt_block);
  void produceVarDecl(AstVarDecl* var_decl);
  void produceFuncDecl(AstFuncDecl* func_decl);

private:
  bool ensureSymbolType(std::string_view type_ident, IRPSymbolType** p_type_sym, Ast* ast_assoc);
  bool ensureUndeclared(std::string_view ident, Ast* ast_assoc, bool local = true);
};

namespace error {
  struct IRPError: error::Error {
    IRPError(IRPascalNode irp, const char* format, ...);
    IRPError(IRPascalNode* irp, const char* format, ...);
    IRPascalNode meta();
    IRPascalNode m_irp;
  };
}
