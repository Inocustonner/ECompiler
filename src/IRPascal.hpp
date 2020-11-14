#pragma once
// #include "Typing.hpp"

#define IRPascalNodeCONS(type)                                                 \
  IRPascalNode##type() : IRPascalNode{IRPascalNodeType::type} {}

enum class IRPascalNodeType {
  Block,

  Func,
  Var,
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

enum class IRConst { Int, String, Char };

struct IRPascalNode {
  IRPascalNodeType node_type;
  ulong p, end_p;
};

// struct IRPascalNodeTemp: public IRPascalNode {
//   ulong id;
//   Type var_type;
// };

struct IRPascalNodeVar : public IRPascalNode {
  IRPascalNodeCONS(Var);
  long id = -1;
  ptr_view<IRPSymbol> sym = nullptr; // must not be freed
  Type var_type;
};

struct IRPascalNodeConst : public IRPascalNode {
  IRPascalNodeCONS(Const);
  Type const_type;
  union {
    std::string *str;
    long integer;
    char ch;
  };
};

// maybe somehow generalize NodeMutate to put return in??
struct IRPascalNodeReturn : public IRPascalNode {
  IRPascalNodeCONS(Return);
  std::unique_ptr<IRPascalNodeVar> ret = nullptr;
};

struct IRPascalNodeMutate : public IRPascalNode {
  IRPascalNodeCONS(Mutate);
  IRMutation mutation;
  std::unique_ptr<IRPascalNode> dst = nullptr; // IRPascalVar or IRPascalTemp
  std::unique_ptr<IRPascalNode> src = nullptr;
  Type var_type;
};

struct IRPascalNodeBlock : public IRPascalNode {
  IRPascalNodeCONS(Block);
  std::vector<std::unique_ptr<IRPascalNode>> stmts = {};
  PasSymTable *sym_table = nullptr;
};

struct IRPascalNodeFunc : public IRPascalNode {
  IRPascalNodeCONS(Func);
  std::string_view ident;
  std::vector<std::string_view>
      arg_ident_vec; // args will be in sym table of the block
  std::unique_ptr<IRPascalNodeBlock> block = nullptr;
};
#undef IRPascalNodeCONS

std::string serializeIRP(IRPascalNode *node);
void freeIRP(IRPascalNode *node);

struct IRPascalProducer {
  struct Env {
    ptr_view<IRPSymbolFunc> curr_func_sym = nullptr;
    ptr_view<PasSymTable> curr_sym_table = nullptr;
    ptr_view<IRPascalNodeBlock> current_block = nullptr;
  };
  ptr_view<IRPSymbolFunc> curr_func_sym = nullptr;
  ptr_view<PasSymTable> curr_sym_table = nullptr;
  ptr_view<IRPascalNodeBlock> current_block = nullptr;
  std::vector<Env> env_stack;
  
  const error::Reporter &reporter;
  std::vector<ptr_view<IRPascalNodeVar>> temp_stack;

  IRPascalProducer(const error::Reporter &reporter);

  std::unique_ptr<IRPascalNodeBlock> produce(Ast *&ast);
  std::unique_ptr<IRPascalNodeBlock>
  produceStmtBlock(Ast /*StmtBlock*/ *&ast_stmt_block);
  void produceVarDecl(Ast /*VarDecl*/ *&ast_var_decl);
  void produceFuncDecl(Ast /*FuncDecl*/ *&ast_func_decl);
  void correctBasicNodeMutate(IRPascalNodeMutate *&mut, AstExpr* expr);
  _NODISCARD
  std::unique_ptr<IRPascalNodeMutate>
  createArithmeticMutation(AstExpr *expr, IRMutation mutation);
  _NODISCARD
  std::unique_ptr<IRPascalNodeMutate> createAssignMutation(
      std::unique_ptr<IRPascalNode> &&dst, std::unique_ptr<IRPascalNode> &&src,
      Ast *ast_with_assignment); /* All unique_ptrs will be std::move'd, create
                                    a copy of the resource if you want to keep
                                    it valid */
  void produceExpr(Ast /*Expr*/ *&ast_expr,
                   std::unique_ptr<IRPascalNode> *dst = nullptr);
  void produceReturn(Ast /*Return*/ *&ast_ret);

private:
  void pushEnv();
  void popEnv();
  
  IRPSymbol *findSymbol(std::string_view ident);
  IRPSymbol *findSymbolLocal(std::string_view ident);
  bool insertSymbol(IRPSymbol *symbol);

  bool ensureSymbolType(std::string_view type_ident,
                        ptr_view<IRPSymbolType> &p_type_sym, Ast *ast_assoc);
  bool ensureUndeclared(std::string_view ident, Ast *ast_assoc,
                        bool local = true);

  IRPascalNodeVar *pushTemp(Type temp_type);
  void popTemp();

  // analysis errors
  void errorInvalidSymbol(ulong region_start, ulong region_end);

  void errorUndeclaredIdent(ulong region_start, ulong region_end);

  void errorRedecl(ulong redecl_region_start, ulong redecl_region_end,
                   ulong decl_region_start, ulong decl_region_end,
                   std::string_view ident);

  void errorUndefinedType(ulong region_start, ulong region_end,
                          ulong type_ident_location_p,
                          ulong type_ident_location_end_p,
                          std::string_view type_alias);

  void errorNotAType(ulong region_start, ulong region_end,
                     ulong type_ident_location_p,
                     ulong type_ident_location_end_p, std::string_view ident,
                     Type orginal_var_type);

  void errorAssignmentToConst(ulong region_start, ulong region_end);

  void errorInvalidCast(ulong region_start, ulong region_end, Type dst_type,
                        Type src_type);
};

namespace error {
struct IRPError : error::Error {
  IRPError(IRPascalNode irp, const char *format, ...);
  IRPError(IRPascalNode *irp, const char *format, ...);
  IRPascalNode meta();
  IRPascalNode m_irp;
};
} // namespace error
