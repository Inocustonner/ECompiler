#include "Compiler.hpp"
#include <cassert>
#include <cstdarg>

// #define PRODUCING_METHOD

IRPascalProducer::IRPascalProducer(const error::Reporter &reporter)
    : reporter{reporter} {}

IRPascalNode *IRPascalProducer::produce(Ast *ast) {
  assert(ast->ast == AstType::StmtBlock);
  curr_sym_table = new PasSymTable;

  // Fill types in the symbol_table
  for (auto [ident, type] : g_ident_type_map) {
    IRPSymbolType *sym_type = new IRPSymbolType{};
    sym_type->ident = std::make_unique<std::string>(new std::string(ident));
    sym_type->var_type = type;

    curr_sym_table->insert(ident, sym_type);
  }

  return produceStmtBlock((AstStmtBlock *)ast);
}

IRPascalNodeBlock *
IRPascalProducer::produceStmtBlock(AstStmtBlock *stmt_block) {
  IRPascalNodeBlock *block_node = new IRPascalNodeBlock{};
  block_node->sym_table = curr_sym_table;
  current_block = block_node;

  // all stmt_block asts will be freed with stmt_block
  for (Ast *ast : stmt_block->stmt_vec) {
    switch (ast->ast) {
    case AstType::VarDecl: {
      produceVarDecl(static_cast<AstVarDecl *>(ast));
    } break;

    case AstType::FuncDecl: {
      produceFuncDecl(static_cast<AstFuncDecl *>(ast));
    } break;
    }
  }

  freeAst(stmt_block);
  return nullptr;
}

bool IRPascalProducer::ensureSymbolType(std::string_view type_ident,
                                        IRPSymbolType **p_type_sym,
                                        Ast *ast_assoc) {
  *p_type_sym = static_cast<IRPSymbolType *>(*curr_sym_table->find(type_ident));
  if (*p_type_sym && (*p_type_sym)->sym_type == SymbolType::Type) {
    return true;
  } else {
    ulong p = 0, end_p = 0, type_ident_p = 0, type_ident_end_p = 0;
    const char *type_ident = nullptr;
    if (ast_assoc->ast == AstType::VarDecl) {
      AstVarDecl *var_decl = static_cast<AstVarDecl *>(ast_assoc);
      p = var_decl->p;
      end_p = var_decl->end_p;
      type_ident_p = var_decl->type_ident_p;
      type_ident_end_p = var_decl->type_ident_end_p;
      type_ident = var_decl->type_ident->c_str();
    } else if (ast_assoc->ast == AstType::FuncArg) {
      AstFuncArg *func_arg = static_cast<AstFuncArg *>(ast_assoc);
      p = func_arg->p;
      end_p = func_arg->end_p;
      type_ident_p = func_arg->type_ident_p;
      type_ident_end_p = func_arg->type_ident_end_p;
      type_ident = func_arg->type_ident->c_str();
    } else {
      throw error::Error("Invalid ast type %s(%#x) in " __FUNCTION__,
                         magic_enum::enum_contains(ast_assoc->ast)
                             ? magic_enum::enum_name(ast_assoc->ast)
                             : "",
                         static_cast<int>(ast_assoc->ast));
    }

    if (*p_type_sym == nullptr) {
      // some how point to the location of the type
      reporter.report_error({error::ErrorEnum::Undefined_Type, p, end_p,
                             type_ident_p, type_ident_end_p},
                            "Undefined type %s", type_ident);
    } else /* sym_type is not a type symbol */ {
      reporter.report_error({error::ErrorEnum::Is_Not_A_Type, p, end_p,
                             type_ident_p, type_ident_end_p},
                            "%s is not a type", type_ident);
    }
  }
}

bool IRPascalProducer::ensureUndeclared(std::string_view ident, Ast *ast_assoc,
                                        bool local) {
  IRPSymbol *sym = nullptr;
  if (local) {
    sym = *curr_sym_table->find_local(ident);
  } else {
    sym = *curr_sym_table->find(ident);
  }
  if (sym == nullptr)
    return true;
  else {
    const char *declared_sym_type_name = nullptr;
    if (sym->sym_type == SymbolType::Var || sym->sym_type == SymbolType::Arg)
      declared_sym_type_name = "Variable";
    else if (sym->sym_type == SymbolType::Func)
      declared_sym_type_name = "Function";
    else if (sym->sym_type == SymbolType::Type)
      declared_sym_type_name = "Type";
    else
      throw error::Error("Unexpected symbol type %s(%#x) in " __FUNCTION__,
                         magic_enum::enum_contains(sym->sym_type)
                             ? magic_enum::enum_name(sym->sym_type)
                             : "");

    reporter.report_error({error::ErrorEnum::Var_ReDecl, ast_assoc->p,
                           ast_assoc->end_p, sym->p, sym->end_p},
                          "%s %s already was declared", declared_sym_type_name,
                          ident.data());
  }
}

void IRPascalProducer::produceVarDecl(AstVarDecl *var_decl) {
  // is var already in the scope
  if (!curr_sym_table->find_local(*var_decl->ident)) {
    IRPSymbolVar *sym_var = new IRPSymbolVar;
    own(sym_var->ident, var_decl->ident);

    sym_var->p = var_decl->p;
    sym_var->end_p = var_decl->end_p;

    IRPSymbolType *sym_type = nullptr;
    if (ensureSymbolType(*var_decl->type_ident, &sym_type)) {
      sym_var->var_type = sym_type->var_type;
      static_assert(
          0, "init_expr, maybe init_expr convert to AstAssign in parser?");
    }
  } else {
    IRPSymbol *sym = *curr_sym_table->find_local(*var_decl->ident);
    const char *declared_sym_type_name = nullptr;
    if (sym->sym_type == SymbolType::Var || sym->sym_type == SymbolType::Arg)
      declared_sym_type_name = "Variable";
    else if (sym->sym_type == SymbolType::Func)
      declared_sym_type_name = "Function";
    else if (sym->sym_type == SymbolType::Type)
      declared_sym_type_name = "Type";
    else
      throw error::IRPError(nullptr, "Unexpected symbol type %s(%#x)",
                            magic_enum::enum_contains(sym->sym_type)
                                ? magic_enum::enum_name(sym->sym_type)
                                : "");

    reporter.report_error({error::ErrorEnum::Var_ReDecl, var_decl->p,
                           var_decl->end_p, sym->p, sym->end_p},
                          "%s %s already was declared", declared_sym_type_name,
                          var_decl->ident->c_str());
  }
  freeAst(var_decl);
}

void IRPascalProducer::produceFuncDecl(AstFuncDecl *func_decl) {
  if (ensureUndeclared(*func_decl->ident, func_decl)) {
    IRPSymbolFunc* sym_func = new IRPSymbolFunc;
    own(sym_func->ident, func_decl->ident);
    sym_func->p = func_decl->p;
    sym_func->end_p = func_decl->end_p;

    for (AstFuncArg* ast_arg : func_decl->args) {
      static_assert(0, "DO THINGS");
    }
  }

  freeAst(func_decl);
}

namespace error {
IRPError::IRPError(IRPascalNode irp, const char *format, ...) {
  va_list args;
  va_start(args, format);

  this->make_msg(format, args);
  this->m_irp = irp;

  va_end(args);
}

IRPError::IRPError(IRPascalNode *irp, const char *format, ...) {
  va_list args;
  va_start(args, format);

  this->make_msg(format, args);
  if (irp) {
    this->m_irp = *irp;
    delete irp;
  }

  va_end(args);
}

IRPascalNode IRPError::meta() { return m_irp; }
} // namespace error
