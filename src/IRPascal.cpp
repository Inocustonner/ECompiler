#include "Compiler.hpp"
#ifdef USE_IRPASCAL
#include <cassert>
#include <cstdarg>

#define ENUM_STR(e) magic_enum::enum_name(e)

template <typename PositionDst, typename PositionSrc>
void inheritPosition(PositionDst& node, PositionSrc&& ast) {
  using PosSrc = std::remove_reference_t<PositionSrc>;
  using PosDst = std::remove_reference_t<PositionDst>;
  static_assert(std::is_pointer_v<PosDst> 
    || types_type_traits::is_unique_ptr_v<PosDst> 
    || types_type_traits::is_ptr_view_v<PosDst>
    || types_type_traits::is_ptr_opt_own_v<PosSrc>, __FUNCSIG__);
  static_assert(std::is_pointer_v<PosSrc> 
    || types_type_traits::is_unique_ptr_v<PosSrc> 
    || types_type_traits::is_ptr_view_v<PosSrc> 
    || types_type_traits::is_ptr_opt_own_v<PosSrc>, __FUNCSIG__);
  node->p = ast->p;
  node->end_p = ast->end_p;
}

template <typename T> IRPascalNode *deep_copy_irp_node(T &&arg_src) {
  IRPascalNode *src;

  if constexpr (std::is_base_of_v<IRPascalNode, std::remove_pointer_t<T>>) {
    src = arg_src;
  } else if constexpr (std::is_base_of_v<IRPascalNode,
                                         types_type_traits::
                                             unique_ptr_underlying_type_t<std::remove_reference_t<T>>>) {
    src = arg_src.get();
  } else {
    static_assert(0, "Cannot make copy for " __FUNCSIG__);
  }

  switch (src->node_type) {
  case IRPascalNodeType::Var: {
    IRPascalNodeVar *var_node = new IRPascalNodeVar{};
    IRPascalNodeVar *src_node = static_cast<IRPascalNodeVar *>(src);
    inheritPosition(var_node, src_node);
    var_node->id = src_node->id;
    var_node->sym = src_node->sym;
    var_node->var_type = ptr_opt_own{ src_node->var_type->deep_copy(), true };
    return var_node;
  }
  case IRPascalNodeType::Const: {
    IRPascalNodeConst *const_node = new IRPascalNodeConst{};
    IRPascalNodeConst *src_node = static_cast<IRPascalNodeConst *>(src);
    inheritPosition(const_node, src_node);
    const_node->const_type = ptr_opt_own{ src_node->const_type->deep_copy(), true };
    const_node->un_value = src_node->un_value;
    return const_node;
  }
  case IRPascalNodeType::Cast: {
    IRPascalNodeCast *cast_node = new IRPascalNodeCast{};
    IRPascalNodeCast *src_node = static_cast<IRPascalNodeCast *>(src);
    inheritPosition(cast_node, src_node);
    cast_node->src =
        std::unique_ptr<IRPascalNode>(deep_copy_irp_node(src_node->src));
    cast_node->to_type = ptr_opt_own{ src_node->to_type->deep_copy(), true };
    return cast_node;
  }
  case IRPascalNodeType::Block:
  case IRPascalNodeType::Func:
  case IRPascalNodeType::Mutate:
  case IRPascalNodeType::Return: {
    throw error::Error(
        "uptr_deep_copy_irp_node must not be able to copy IRPascalNode%s",
        ENUM_STR(src->node_type));
  }
  default:
    throw error::Error(
        "uptr_deep_copy_irp_node is not implemented for IRPascalNode%s",
        ENUM_STR(src->node_type));
  }
#undef RETURN_COPY_PTR_NODE_TYPE
#undef DEFAULT_CASE_NODE_TYPE
}

template <typename UptrT, typename T>
std::unique_ptr<UptrT> uptr_deep_copy_irp_node(T &arg_src) {
  return std::unique_ptr<UptrT>(
      static_cast<UptrT *>(deep_copy_irp_node(arg_src)));
}

#pragma warning(push)
#pragma warning(disable : 4715)
Type::Meta &getNodeVarType(IRPascalNode *node) {
  assert(node != nullptr); // for now
  switch (node->node_type) {
  case IRPascalNodeType::Var: {
    IRPascalNodeVar *var = static_cast<IRPascalNodeVar *>(node);
    return *var->var_type;
  };
  case IRPascalNodeType::Const: {
    IRPascalNodeConst *cnst = static_cast<IRPascalNodeConst *>(node);
    return *cnst->const_type;
  };
  case IRPascalNodeType::Mutate: {
    IRPascalNodeMutate *mut = static_cast<IRPascalNodeMutate *>(node);
    return *mut->var_type;
  }
  case IRPascalNodeType::Cast: {
    return *static_cast<IRPascalNodeCast *>(node)->to_type;
  }
  default:
    assert(0);
  }
}

ptr_view<Type::Meta> getNodeVarTypeView(IRPascalNode *node) {
  return ptr_view{&getNodeVarType(node)};
}

ptr_view<Type::Meta>
getNodeVarTypeView(std::unique_ptr<IRPascalNode> &up_node) {
  return getNodeVarTypeView(up_node.get());
}
#pragma warning(pop)
template <typename T1, typename T2> bool equalVarTypes(T1 &&fst, T2 &&snd) {
  ptr_view<Type::Meta> t1, t2;
  using DT1 = std::decay_t<T1>;
  using DT2 = std::decay_t<T2>;

  if constexpr (std::is_same_v<DT1, Type::Meta> ||
                std::is_same_v<T1, decltype(t1)>) {
    t1 = fst;
  } else if constexpr (std::is_base_of_v<IRPascalNode, DT1>) {
    t1 = getNodeVarTypeView(fst);
  } else if constexpr (std::is_base_of_v<
                           IRPascalNode,
                           types_type_traits::unique_ptr_underlying_type_t<
                               DT1>>) {
    t1 = getNodeVarTypeView(fst.get());
  } else {
    static_assert(0, "T1 dont have var info in " __FUNCTION__);
    throw error::Error("Type %s has no variable type info", typeid(T1).name());
  }

  if constexpr (std::is_same_v<std::decay_t<DT2>, Type::Meta> ||
                std::is_same_v<T2, decltype(t2)>) {
    t2 = snd;
  } else if constexpr (std::is_base_of_v<IRPascalNode, DT2>) {
    t2 = getNodeVarTypeView(snd);
  } else if constexpr (std::is_base_of_v<
                           IRPascalNode,
                           types_type_traits::unique_ptr_underlying_type_t<
                               DT2>>) {
    t2 = getNodeVarTypeView(snd.get());
  } else {
    static_assert(0, "T1 dont have var info in " __FUNCTION__);
    throw error::Error("Type %s has no variable type info", typeid(T2).name());
  }
  return t1->equal(*t2);
}

ptr_view<Type::Meta> getSymbolVarType(IRPSymbol *sym) {
  // what to do if symbol have no type notation???
  switch (sym->sym_type) {
  case SymbolType::Var: {
    return static_cast<IRPSymbolVar *>(sym)->var_type.get();
  }
  case SymbolType::Type: {
    return static_cast<IRPSymbolType *>(sym)->var_type.get();
  }
  case SymbolType::Func: {
    return static_cast<IRPSymbolFunc *>(sym)->ret_type.get();
  }
  case SymbolType::Arg: {
    return static_cast<IRPSymbolArg *>(sym)->var_type.get();
  }
  default:
    throw error::Error("Unexpected symbol type %s(%#x) in " __FUNCTION__,
                       magic_enum::enum_contains(sym->sym_type)
                           ? magic_enum::enum_name(sym->sym_type).data()
                           : "",
                       static_cast<int>(sym->sym_type));
  }
}

static std::string serializeIRPSymbolForVars(IRPSymbol *sym) {
#define CAST_INIT_SYM(name, type)                                              \
  IRPSymbol##type *name = static_cast<IRPSymbol##type *>(sym)
  std::string str = "";
  switch (sym->sym_type) {
  case SymbolType::Arg: {
    CAST_INIT_SYM(arg, Arg);
    str += *arg->ident + " :" + arg->var_type->getFullTypeName();
    return str;
  }
  case SymbolType::Var: {
    CAST_INIT_SYM(var, Var);
    str += *var->ident + " :" + var->var_type->getFullTypeName();
    return str;
  }
  case SymbolType::Type: {
    CAST_INIT_SYM(type, Type);
    str += "typedef " + *type->ident + " :" + type->var_type->getFullTypeName();
    return str;
  }
  case SymbolType::Func: {
    CAST_INIT_SYM(func, Func);
    return "\n" + serializeIRP(func->irp_func.get());
  }
  default:
    assert(0);
  }
  return "";
#undef CAST_INIT_SYM
}

std::string _serializeIRP(const ptr_view<IRPascalNode> node, size_t indent) {
  std::string indent_s(indent, '\t');
  std::string str = "";
#define INDENT indent_s +
#define PUT(s) str = str + s + "\n"
#define PUT_I(s) PUT(INDENT s)
#define CAST_INIT_NODE(name, type)                                             \
  const IRPascalNode##type *name = static_cast<const IRPascalNode##type *>(node)
  switch (node->node_type) {
  case IRPascalNodeType::Block: {
    CAST_INIT_NODE(block, Block);
    for (auto [ident, sym] : block->sym_table->hash_map) {
      if (sym->sym_type != SymbolType::Arg) {
        PUT_I(serializeIRPSymbolForVars(sym));
      }
    }
    PUT_I("begin");
    indent += 1;
    indent_s = std::string(indent, '\t');

    for (const std::unique_ptr<IRPascalNode> &stmt : block->stmts) {
      PUT(_serializeIRP(stmt.get(), indent));
    }

    indent -= 1;
    indent_s = std::string(indent, '\t');

    PUT_I("end");
  } break;
  case IRPascalNodeType::Func: {
    CAST_INIT_NODE(func, Func);
    PUT_I("func " + func->ident.data());
    PUT_I("args");

    indent_s = std::string(indent + 1, '\t');
    for (std::string_view arg_ident : func->arg_ident_vec) {
      PUT_I(
          serializeIRPSymbolForVars(*func->block->sym_table->find(arg_ident)));
    }
    PUT(_serializeIRP(func->block.get(), indent));
  } break;
  case IRPascalNodeType::Var: {
    CAST_INIT_NODE(var, Var);
    if (var->id > -1) {
      str = "TEMP" + std::to_string(var->id) + " :" +
            var->var_type->getFullTypeName();
    } else if (var->sym) {
      return serializeIRPSymbolForVars(var->sym);
    } else {
      assert(0);
    }
  } break;
  case IRPascalNodeType::Const: {
    CAST_INIT_NODE(cnst, Const);
    switch (cnst->const_type->type_id) {
    case Type::BuiltIn_Int: {
      str = std::to_string(cnst->un_value.integer);
    } break;
    case Type::BuiltIn_Char: {
      str = std::to_string(cnst->un_value.ch);
    } break;
    default:
      assert(0);
    }
  } break;
  case IRPascalNodeType::Mutate: {
    CAST_INIT_NODE(mut, Mutate);
    std::string oper;
    switch (mut->mutation) {
    case IRMutation::Assign:
      oper = " = ";
      break;
    case IRMutation::Add:
      oper = " += ";
      break;
    case IRMutation::Sub:
      oper = " -= ";
      break;
    case IRMutation::Mult:
      oper = " *= ";
      break;
    case IRMutation::Div:
      oper = " /= ";
      break;
    default:
      assert(0);
    }
    PUT_I(_serializeIRP(mut->dst.get(), 0) + oper +
          _serializeIRP(mut->src.get(), 0));
    // pic'ka
  } break;
  case IRPascalNodeType::Cast: {
    CAST_INIT_NODE(cast, Cast);
    PUT_I("cast<" + cast->to_type->getFullTypeName() + ">( " +
          serializeIRP(cast->src.get()) + " )");
  } break;
  case IRPascalNodeType::Return: {
    CAST_INIT_NODE(ret, Return);
    if (ret->ret)
      PUT_I("ret " + _serializeIRP(ret->ret.get(), 0));
    else
      PUT_I("ret");
  } break;
  default:
    assert(0);
  }
  return str;
#undef INDENT
#undef PUT
#undef PUT_I
#undef CAST_INIT_NODE
}

std::string serializeIRP(IRPascalNode *node) { return _serializeIRP(node, 0); }

// #define PRODUCING_METHOD
IRPascalProducer::IRPascalProducer(const error::Reporter &reporter)
    : reporter{reporter} {}

#define ERROR_REPORT_METHOD void IRPascalProducer::

ERROR_REPORT_METHOD errorUndeclaredIdent(ulong region_start, ulong region_end) {
}

ERROR_REPORT_METHOD errorRedecl(ulong redecl_region_start,
                                ulong redecl_region_end,
                                ulong decl_region_start, ulong decl_region_end,
                                std::string_view ident) {
  // add to message what it was in the first decloration???
  reporter.report_error({error::ErrorEnum::ReDecl, redecl_region_start,
                         redecl_region_end, decl_region_start, decl_region_end},
                        "'%s' was already declared in the current scope",
                        ident.data());
}

ERROR_REPORT_METHOD errorUndefinedType(ulong region_start, ulong region_end,
                                       ulong type_ident_location_p,
                                       ulong type_ident_location_end_p,
                                       std::string_view type_alias) {
  reporter.report_error({error::ErrorEnum::Undefined_Type, region_start,
                         region_end, type_ident_location_p,
                         type_ident_location_end_p},
                        "Undefined type %s", type_alias);
}

ERROR_REPORT_METHOD errorNotAType(ulong region_start, ulong region_end,
                                  ulong type_ident_location_p,
                                  ulong type_ident_location_end_p,
                                  std::string_view ident,
                                  ptr_view<Type::Meta> orginal_var_type) {
  reporter.report_error({error::ErrorEnum::Is_Not_A_Type, region_start,
                         region_end, type_ident_location_p,
                         type_ident_location_end_p},
                        "'%s' is not a type identifier", ident.data());
}

ERROR_REPORT_METHOD errorAssignmentToConst(ulong region_start,
                                           ulong region_end) {
  reporter.report_error(
      {error::ErrorEnum::Assignment_To_Const, region_start, region_end},
      "Cannot assign to constant");
}

ERROR_REPORT_METHOD errorInvalidCast(ulong region_start, ulong region_end,
                                     ptr_view<Type::Meta> dst_type,
                                     ptr_view<Type::Meta> src_type) {
  reporter.report_error(
      {error::ErrorEnum::Invalid_Cast, region_start, region_end},
      "Invalid cast type '%s' to type '%s'", src_type->getFullTypeName(),
      dst_type->getFullTypeName());
}

#undef ERROR_REPORT_METHOD
std::unique_ptr<IRPascalNodeBlock> IRPascalProducer::produce(Ast *&ast) {
  assert(ast->ast == AstType::StmtBlock);
  curr_sym_table = new PasSymTable;

  // Fill types in the symbol_table
  for (Type::Meta *&meta : Type::getDefaultFlatTypes()) {
    IRPSymbolType *sym_type = new IRPSymbolType{};
    sym_type->ident.reset(new std::string(std::move(meta->name)));
    own(sym_type->var_type, meta);

    curr_sym_table->insert(*sym_type->ident, sym_type);
  }

  return produceStmtBlock(ast);
}

std::unique_ptr<IRPascalNodeBlock>
IRPascalProducer::produceStmtBlock(Ast /*StmtBlock*/ *&ast_stmt_block) {
  assert(ast_stmt_block->ast == AstType::StmtBlock);

  AstStmtBlock *stmt_block = static_cast<AstStmtBlock *>(ast_stmt_block);

  auto block_node = std::unique_ptr<IRPascalNodeBlock>(new IRPascalNodeBlock{});
  inheritPosition(block_node, stmt_block);

  block_node->sym_table = curr_sym_table;
  current_block = block_node.get();

  // all stmt_block asts will be freed with stmt_block
  for (Ast *&ast : stmt_block->stmt_vec) {
    switch (ast->ast) {
    case AstType::VarDecl: {
      produceVarDecl(ast);
    } break;

    case AstType::FuncDecl: {
      produceFuncDecl(ast);
    } break;

    case AstType::Expr: {
      // ignore no discard
      produceExpr(ast);
    } break;
    case AstType::Return: {
      produceReturn(ast);
    } break;
    }
  }
  freeAst(ast_stmt_block);
  ast_stmt_block = nullptr;
  return block_node;
}

void IRPascalProducer::pushEnv() {
  Env curr{};
  curr.curr_func_sym = curr_func_sym;
  curr.curr_sym_table = curr_sym_table;
  curr.current_block = current_block;

  env_stack.push_back(curr);
}

void IRPascalProducer::popEnv() {
  Env curr = env_stack.back();
  curr_func_sym = curr.curr_func_sym;
  curr_sym_table = curr.curr_sym_table;
  current_block = curr.current_block;

  env_stack.pop_back();
}

IRPSymbol *IRPascalProducer::findSymbol(std::string_view ident) {
  return safe_deref(curr_sym_table->find(ident));
}

IRPSymbol *IRPascalProducer::findSymbolLocal(std::string_view ident) {
  return safe_deref(curr_sym_table->find_local(ident));
}

bool IRPascalProducer::insertSymbol(IRPSymbol *symbol) {
  return curr_sym_table->insert(*symbol->ident, symbol);
}

bool IRPascalProducer::ensureSymbolType(std::string_view type_ident,
                                        ptr_view<IRPSymbolType> &p_type_sym,
                                        Ast *ast_assoc) {
  p_type_sym = static_cast<IRPSymbolType *>(
      safe_deref(curr_sym_table->find(type_ident)));
  if (!(p_type_sym && p_type_sym->sym_type == SymbolType::Type)) {
    ulong p = 0, end_p = 0, type_ident_p = 0, type_ident_end_p = 0;
    const char *type_ident = nullptr;
    if (ast_assoc->ast == AstType::VarDecl) {
      AstVarDecl *var_decl = static_cast<AstVarDecl *>(ast_assoc);
      p = var_decl->p;
      end_p = var_decl->end_p;
      type_ident_p = var_decl->var_type->p;
      type_ident_end_p = var_decl->var_type->end_p;
      type_ident = var_decl->var_type->type_ident->c_str();
    } else if (ast_assoc->ast == AstType::FuncArg) {
      AstFuncArg *func_arg = static_cast<AstFuncArg *>(ast_assoc);
      p = func_arg->p;
      end_p = func_arg->end_p;
      type_ident_p = func_arg->var_type->p;
      type_ident_end_p = func_arg->var_type->end_p;
      type_ident = func_arg->var_type->type_ident->c_str();
    } else {
      throw error::Error("Invalid ast type %s(%#x) in " __FUNCTION__,
                         ENUM_STR(ast_assoc->ast),
                         static_cast<int>(ast_assoc->ast));
    }

    if (p_type_sym == nullptr) {
      // some how point to the location of the type
      errorUndefinedType(p, end_p, type_ident_p, type_ident_end_p, type_ident);
    } else /* sym_type is not a type symbol */ {
      errorNotAType(p, end_p, type_ident_p, type_ident_end_p, type_ident,
                    getSymbolVarType(p_type_sym));
    }
    return false;
  }
  return true;
}

bool IRPascalProducer::ensureUndeclared(std::string_view ident, Ast *ast_assoc,
                                        bool local) {
  IRPSymbol *sym = nullptr;
  if (local) {
    sym = safe_deref(curr_sym_table->find_local(ident));
  } else {
    sym = safe_deref(curr_sym_table->find(ident));
  }
  if (sym == nullptr)
    return true;
  else {
    errorRedecl(ast_assoc->p, ast_assoc->end_p, sym->p, sym->end_p, ident);
    return false;
  }
}

IRPascalNodeVar *IRPascalProducer::pushTemp(TypePtr temp_type) {
  IRPascalNodeVar *temp = new IRPascalNodeVar{};
  assert(Type::isBuiltIns(temp_type.get())); // for now
  temp->id = temp_stack.size();
  temp->var_type = temp_type;
  temp_stack.push_back(temp);
  return temp;
}

void IRPascalProducer::popTemp() { temp_stack.pop_back(); }

void IRPascalProducer::pushStmt(std::unique_ptr<IRPascalNode> &&node) {
  current_block->stmts.push_back(
      std::forward<std::unique_ptr<IRPascalNode> &&>(node));
}

void IRPascalProducer::produceVarDecl(Ast /*VarDecl*/ *&ast_var_decl) {
  // is var already in the scope
  assert(ast_var_decl->ast == AstType::VarDecl);

  AstVarDecl *var_decl = static_cast<AstVarDecl *>(ast_var_decl);
  if (!findSymbolLocal(*var_decl->ident)) {
    IRPSymbolVar *sym_var = new IRPSymbolVar;
    own(sym_var->ident, var_decl->ident);

    inheritPosition(sym_var, var_decl);
    // sym_var->p = var_decl->p;
    // sym_var->end_p = var_decl->end_p;

    ptr_view<IRPSymbolType> sym_type = nullptr;
    if (ensureSymbolType(*var_decl->var_type->type_ident, sym_type, var_decl)) {
      sym_var->var_type =
          std::unique_ptr<Type::Meta>(sym_type->var_type->deep_copy());
      // put in the symbol table
      curr_sym_table->insert(*sym_var->ident, sym_var);

      // INIT EXPR
      if (var_decl->init_expr) {
        std::unique_ptr<IRPascalNode> src;
        produceExpr(reinterpret_cast<Ast *&>(var_decl->init_expr), &src);

        std::unique_ptr<IRPascalNodeVar> var_node{new IRPascalNodeVar{}};
        var_node->sym = sym_var;
        var_node->var_type =
            ptr_opt_own{sym_var->var_type.get(), false}; // non-owning

        current_block->stmts.push_back(produceAssignMutation(
            std::move(var_node), std::move(src), var_decl));
      }
    }
  } else {
    IRPSymbol *sym_redeclared = findSymbolLocal(*var_decl->ident);
    errorRedecl(var_decl->p, var_decl->end_p, sym_redeclared->p,
                sym_redeclared->end_p, *var_decl->ident);
  }
  freeAst(ast_var_decl);
  ast_var_decl = nullptr;
}

void IRPascalProducer::produceFuncDecl(Ast /*FuncDecl*/ *&ast_func_decl) {
  assert(ast_func_decl->ast == AstType::FuncDecl);
  AstFuncDecl *func_decl = static_cast<AstFuncDecl *>(ast_func_decl);

  if (ensureUndeclared(*func_decl->ident, func_decl)) {
    IRPSymbolFunc *sym_func = new IRPSymbolFunc{};
    own(sym_func->ident, func_decl->ident);
    inheritPosition(sym_func, func_decl);

    sym_func->irp_func = new IRPascalNodeFunc{};
    inheritPosition(sym_func->irp_func, func_decl);

    sym_func->irp_func->ident = *sym_func->ident;
    insertSymbol(sym_func);

    pushEnv();
    // create scope for the function
    curr_sym_table = curr_sym_table->createEnclosed();
    curr_func_sym = sym_func;

    // put args in the scope
    for (AstFuncArg *ast_arg : func_decl->args) {
      ptr_view<IRPSymbolType> type_sym = nullptr;
      if (ensureSymbolType(*ast_arg->var_type->type_ident, type_sym, ast_arg)) {
        IRPSymbolArg *sym_arg = new IRPSymbolArg{};
        own(sym_arg->ident, ast_arg->ident);
        sym_arg->var_type =
            std::unique_ptr<Type::Meta>(type_sym->var_type->deep_copy());

        inheritPosition(sym_arg, ast_arg);
        if (!insertSymbol(sym_arg)) {
          // only occur if two arguments have the same name
          IRPSymbol *sym_redeclared = findSymbolLocal(*sym_arg->ident);
          errorRedecl(sym_arg->p, sym_arg->end_p, sym_redeclared->p,
                      sym_redeclared->end_p, *sym_arg->ident);
        } else {
          // if all went fine
          // put arg in the symbol table for the scope
          sym_func->irp_func->arg_ident_vec.push_back(*sym_arg->ident);
        }
      }
    }
    // set the ret type
    if (func_decl->ret_type) {
      ptr_view<IRPSymbolType> ret_type_sym = nullptr;
      if (func_decl->ret_type &&
          ensureSymbolType(*func_decl->ret_type->type_ident, ret_type_sym,
                           func_decl)) {
        sym_func->ret_type =
            std::unique_ptr<Type::Meta>(ret_type_sym->var_type->deep_copy());
      } else {
        /*sym_func->ret_type =
            static_cast<IRPSymbolType *>(findSymbol("void"))->var_type;*/
        errorUndefinedType(func_decl->p, func_decl->header_end_p,
                           func_decl->ret_type->p, func_decl->ret_type->end_p,
                           *func_decl->ret_type->type_ident);
      }
    } else {
      sym_func->ret_type = std::unique_ptr<Type::Meta>(
          static_cast<IRPSymbolType *>(findSymbol("void"))
              ->var_type->deep_copy());
    }
    sym_func->irp_func->block =
        produceStmtBlock(reinterpret_cast<Ast *&>(func_decl->stmt_block));

    popEnv();
  } else {
    IRPSymbol *sym_redeclared = findSymbolLocal(*func_decl->ident);
    errorRedecl(func_decl->p, func_decl->end_p, sym_redeclared->p,
                sym_redeclared->end_p, *func_decl->ident);
  }
  freeAst(ast_func_decl);
  ast_func_decl = nullptr;
}

void IRPascalProducer::correctBasicNodeMutate(IRPascalNodeMutate *&mut,
                                              AstExpr *expr) {
  assert(mut->dst && mut->src);
  assert(mut->mutation != IRMutation::Assign);
  if (mut->dst->node_type == IRPascalNodeType::Const &&
      mut->src->node_type == IRPascalNodeType::Const) {
    // assign first const to temp, and set temp as dst
    std::unique_ptr<IRPascalNodeVar> temp = std::unique_ptr<IRPascalNodeVar>(
        pushTemp(getNodeVarTypeView(mut->dst)));

    auto uptr_temp_copy = uptr_deep_copy_irp_node<IRPascalNodeVar>(temp);
    current_block->stmts.push_back(
        produceAssignMutation(std::move(temp), std::move(mut->dst), expr));

    mut->dst = std::move(uptr_temp_copy); // create a copy
  } else if (mut->dst->node_type == IRPascalNodeType::Const &&
             mut->src->node_type == IRPascalNodeType::Var) {
    std::swap(mut->dst, mut->src);
  } /*else if (mut->src->node_type == IRPascalNodeType::Var &&
             static_cast<IRPascalNodeVar *>(mut->src.get())->id > -1) {
    popTemp();
  }*/
}

std::unique_ptr<IRPascalNodeMutate>
IRPascalProducer::produceDefaultExprMutation(AstExpr *expr,
                                             IRMutation mutation) {
  IRPascalNodeMutate *mut_node = new IRPascalNodeMutate{};
  inheritPosition(mut_node, expr);
  mut_node->mutation = mutation;
  mut_node->dst = produceOperand(expr->fst);
  mut_node->src = produceOperand(expr->snd);

  mut_node->src =
      produceCast(std::move(mut_node->src), getNodeVarTypeView(mut_node->dst));
  if (mut_node->src == nullptr) {
    errorInvalidCast(expr->p, expr->end_p, getNodeVarTypeView(mut_node->dst),
                     getNodeVarTypeView(mut_node->src));
    delete mut_node;
    mut_node = nullptr;
  } else {
    fixMutationOperands(*mut_node, expr);  
  }
  return std::unique_ptr<IRPascalNodeMutate>(mut_node);
}

std::unique_ptr<IRPascalNode>
IRPascalProducer::produceCast(std::unique_ptr<IRPascalNode> &&casted,
                              ptr_view<Type::Meta> to_type,
                              CastType cast_type) {
  ptr_view<Type::Meta> casted_type = getNodeVarTypeView(casted);
  if (casted_type->equal(*to_type)) {
    return casted; // nothing to cast
  }
  if (Type::isBuiltIns(casted_type.get(), to_type.get())) {
    // cast between two integer types
    if (Type::isIntegerBuiltins(casted_type.get(), to_type.get())) {
      if (casted_type->getTypeSize() > to_type->getTypeSize()) {
#pragma message("Report warning that info might be lost")
      }

      IRPascalNodeCast *cast_node = new IRPascalNodeCast{};
      cast_node->p = casted->p;
      cast_node->end_p = casted->end_p;
      cast_node->to_type = ptr_opt_own{to_type->deep_copy(), true};
      cast_node->src = std::move(casted);

      Ast ast_position = Ast{.p = casted->p, .end_p = casted->end_p};
      inheritPosition(cast_node, &ast_position);

      std::unique_ptr<IRPascalNode> mut = produceAssignMutation(
          nullptr, std::unique_ptr<IRPascalNode>(cast_node), &ast_position);
      std::unique_ptr<IRPascalNode> dst{deep_copy_irp_node(mut)};

      if (casted->node_type == IRPascalNodeType::Const) {
        return dst;
      } else if (casted->node_type == IRPascalNodeType::Var) {
        return dst;
      } else {
        assert(0);
      }
    }

    // pointers with different subtypes
    if (Type::isPointer(casted_type.get(), to_type.get())) {
      if (cast_type == CastType::Implicit)
        return nullptr;
      else {
        Type::Meta &var_type = getNodeVarType(casted.get());
        var_type.subtype = to_type->subtype;
        return casted;
      }
    }

    return nullptr;
  } else {
    assert(false);
  }
}

std::unique_ptr<IRPascalNodeMutate>
IRPascalProducer::produceAssignMutation(std::unique_ptr<IRPascalNode> &&dst,
                                        std::unique_ptr<IRPascalNode> &&src,
                                        Ast *ast_with_assignment) {
  if (dst == nullptr) {
    dst = std::unique_ptr<IRPascalNode>(pushTemp(getNodeVarTypeView(src)));
  } else if (dst->node_type == IRPascalNodeType::Const) {
    errorAssignmentToConst(ast_with_assignment->p, ast_with_assignment->end_p);
    return nullptr;
  }

  src = produceCast(std::move(src), getNodeVarTypeView(dst));
  if (!src) {
    errorInvalidCast(ast_with_assignment->p, ast_with_assignment->end_p,
                     getNodeVarTypeView(dst), getNodeVarTypeView(src));
    return nullptr;
  }
  IRPascalNodeMutate *mut = new IRPascalNodeMutate{};
  mut->mutation = IRMutation::Assign;
  mut->dst = std::move(dst);
  mut->src = std::move(src);

  // if (mut->src->node_type == IRPascalNodeType::Var &&
  //    uptr_cast<IRPascalNodeVar *>(mut->src)->id > -1) {
  //  popTemp();
  //}
  return std::unique_ptr<IRPascalNodeMutate>(mut);
}

// called after types are set
void IRPascalProducer::fixMutationOperands(IRPascalNodeMutate &mut_node,
                                           Ast *ast) {
  if (mut_node.dst && mut_node.src)
    assert(equalVarTypes(mut_node.dst, mut_node.src));

  assert(Type::isBuiltIns(getNodeVarTypeView(mut_node.dst).get(),
                          getNodeVarTypeView(mut_node.src).get()));

  switch (mut_node.mutation) {
  case IRMutation::Add:
  case IRMutation::Sub:
  case IRMutation::Div:
  case IRMutation::Mult: {
    if ((mut_node.dst->node_type == IRPascalNodeType::Const &&
         mut_node.src->node_type == IRPascalNodeType::Const) ||
        (mut_node.dst->node_type == IRPascalNodeType::Const &&
         mut_node.src->node_type == IRPascalNodeType::Var)) {
      // to preserve the order, assign "dst const" to tmp
      std::unique_ptr<IRPascalNode> assign_mut =
          produceAssignMutation(nullptr, std::move(mut_node.dst), ast);
      mut_node.dst = std::unique_ptr<IRPascalNode>(
          deep_copy_irp_node(uptr_cast<IRPascalNodeMutate *>(assign_mut)->dst));
      pushStmt(std::move(assign_mut));
    }
  } break;
  default:
    assert(false);
  }
}

std::unique_ptr<IRPascalNode>
IRPascalProducer::produceOperand(AstExpr *&ast_oper) {
  std::unique_ptr<IRPascalNode> oper_node = nullptr;
  produceExpr(reinterpret_cast<Ast *&>(ast_oper), &oper_node);
  return oper_node;
}

// returns the destonation of the expr
void IRPascalProducer::produceExpr(Ast /*Expr*/ *&ast_expr,
                                   std::unique_ptr<IRPascalNode> *dst) {
  assert(ast_expr->ast == AstType::Expr);
  AstExpr *expr = static_cast<AstExpr *>(ast_expr);

  switch (expr->expr_type) {
  case ExprType::Assign: {
    std::unique_ptr<IRPascalNode> dst_ = produceOperand(expr->fst);
    std::unique_ptr<IRPascalNode> src = produceOperand(expr->snd);
    if (src && dst_) {
      if (dst)
        *dst = uptr_deep_copy_irp_node<IRPascalNode>(dst_);

      current_block->stmts.emplace_back(
          produceAssignMutation(std::move(dst_), std::move(src), expr));
    } else
      break;
  } break;

  case ExprType::Add: {
    // std::unique_ptr<IRPascalNode> mut =
    //    produceDefaultExprMutation(expr, IRMutation::Add);
    IRPascalNodeMutate *mut_node = new IRPascalNodeMutate{};
    inheritPosition(mut_node, expr);
    mut_node->mutation = IRMutation::Add;
    mut_node->dst = produceOperand(expr->fst);
    mut_node->src = produceOperand(expr->snd);

    mut_node->src = produceCast(std::move(mut_node->src),
                                getNodeVarTypeView(mut_node->dst));
    if (mut_node->src == nullptr) {
      errorInvalidCast(expr->p, expr->end_p, getNodeVarTypeView(mut_node->dst),
                       getNodeVarTypeView(mut_node->src));
      break;
    }

    fixMutationOperands(*mut_node, expr);

    if (dst)
      *dst = uptr_deep_copy_irp_node<IRPascalNode>(mut_node->dst);
    current_block->stmts.push_back(std::unique_ptr<IRPascalNode>(mut_node));
  } break;

  case ExprType::Sub: {
    std::unique_ptr<IRPascalNode> mut =
        produceDefaultExprMutation(expr, IRMutation::Sub);
    if (dst)
      *dst = uptr_deep_copy_irp_node<IRPascalNode>(
          uptr_cast<IRPascalNodeMutate *>(mut)->dst);
    current_block->stmts.push_back(std::move(mut));
  } break;

  case ExprType::Mult: {
    std::unique_ptr<IRPascalNode> mut =
        produceDefaultExprMutation(expr, IRMutation::Mult);
    if (dst)
      *dst = uptr_deep_copy_irp_node<IRPascalNode>(
          uptr_cast<IRPascalNodeMutate *>(mut)->dst);
    current_block->stmts.push_back(std::move(mut));
  } break;

  case ExprType::Div: {
    std::unique_ptr<IRPascalNode> mut =
        produceDefaultExprMutation(expr, IRMutation::Div);
    if (dst)
      *dst = uptr_deep_copy_irp_node<IRPascalNode>(
          uptr_cast<IRPascalNodeMutate *>(mut)->dst);
    // zero division?
    current_block->stmts.push_back(std::move(mut));
  } break;

  case ExprType::Ident: {
    if (!dst) // will not be used anyway
      break;

    std::unique_ptr<IRPascalNodeVar> var_node{new IRPascalNodeVar{}};
    inheritPosition(var_node, expr);

    IRPSymbol *sym = findSymbol(*expr->ident);
    if (sym) {
      var_node->var_type = getSymbolVarType(var_node->sym);
      *dst = uptr_deep_copy_irp_node<IRPascalNode>(var_node);
    } else
      errorUndeclaredIdent(expr->p, expr->end_p);
  } break;
  case ExprType::Number: {
    if (!dst) // will not be used anyway
      break;

    std::unique_ptr<IRPascalNodeConst> const_node{new IRPascalNodeConst{}};
    inheritPosition(const_node, expr);
    assert(expr->num_class == NumberClass::Integer);
    // we're sure that int type is in the table
    const_node->un_value.integer = expr->integer;
    const_node->const_type = ptr_opt_own{
        static_cast<IRPSymbolType *>(findSymbol("int"))->var_type.get(), false};

    *dst = uptr_deep_copy_irp_node<IRPascalNode>(const_node);
  } break;

  case ExprType::Char: {
    if (!dst) // will not be used anyway
      break;

    std::unique_ptr<IRPascalNodeConst> const_node{new IRPascalNodeConst{}};
    inheritPosition(const_node, expr);

    const_node->un_value.ch = expr->c;
    const_node->const_type = ptr_opt_own{
        static_cast<IRPSymbolType *>(findSymbol("char"))->var_type.get(),
        false};
    *dst = uptr_deep_copy_irp_node<IRPascalNode>(const_node);
  } break;

  case ExprType::String:
    assert(0);
  default:
    throw error::Error("Unexpected expr type %s(%#x)in " __FUNCTION__,
                       ENUM_STR(expr->expr_type),
                       static_cast<int>(expr->expr_type));
  }
  freeAst(ast_expr);
  ast_expr = nullptr;
}

void IRPascalProducer::produceReturn(Ast /*Return*/ *&ast_ret) {
  assert(ast_ret->ast == AstType::Return);
  AstReturn *ret = static_cast<AstReturn *>(ast_ret);
  auto ret_node = std::unique_ptr<IRPascalNodeReturn>{new IRPascalNodeReturn{}};
  inheritPosition(ret_node, ret);

  if (ret->ret_expr) {
    std::unique_ptr<IRPascalNode> node_expr_ret;
    produceExpr(reinterpret_cast<Ast *&>(ret->ret_expr), &node_expr_ret);

    std::unique_ptr<IRPascalNodeMutate> uptr_assign_mut =
        produceAssignMutation(nullptr, std::move(node_expr_ret), ret);

    ret_node->ret =
        uptr_deep_copy_irp_node<IRPascalNodeVar>(uptr_assign_mut->dst);
    current_block->stmts.push_back(std::move(uptr_assign_mut));
    popTemp(); // pop the temp we assignet

  } else {
    ret_node->ret = nullptr; // no return
  }
#pragma message("Function return type check!!!")
  freeAst(ast_ret);
  ast_ret = nullptr;
  current_block->stmts.push_back(std::move(ret_node));
}
//
//namespace error {
//  IRPError::IRPError(IRPascalNode irp, const char *format, ...) {
//    va_list args;
//    va_start(args, format);
//
//    this->make_msg(format, args);
//    this->m_irp = irp;
//
//    va_end(args);
//  }
//
//  IRPError::IRPError(IRPascalNode *irp, const char *format, ...) {
//    va_list args;
//    va_start(args, format);
//
//    this->make_msg(format, args);
//    this->m_irp = IRPascalNode{};
//    if (irp) {
//      this->m_irp = *irp;
//      delete irp;
//    }
//
//    va_end(args);
//  }
//
//  IRPascalNode IRPError::meta() { return m_irp; }
//} // namespace error
#endif
