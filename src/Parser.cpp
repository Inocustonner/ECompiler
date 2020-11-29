#include "Compiler.hpp"
#include <cstdarg>

#define PARSING_METHOD Ast *Parser::
#define PARSING_METHOD_EXPR AstExpr *Parser::

#define INIT_AST(var, ast_type_name)                                           \
  auto *var = new Ast##ast_type_name{};                                        \
  var->ast = AstType::ast_type_name;                                           \
  var->p = lookUp(0)->p

#define INIT_AST_VAR_DECL(var) INIT_AST(var, VarDecl);

#define INIT_AST_FUNC_DECL(var)                                                \
  INIT_AST(var, FuncDecl);                                                     \
  var->stmt_block = new AstStmtBlock {}

#define INIT_AST_EXPR(var, Expr_type)                                          \
  INIT_AST(var, Expr);                                                         \
  var->expr_type = ExprType::Expr_type

#define CAST_INIT_CONST_AST(name, type)                                              \
  const Ast##type *name = static_cast<const Ast##type *>(ast);

#define CAST_INIT_AST(name, type)                                              \
  Ast##type *name = static_cast<Ast##type *>(ast);

extern bool g_verbose;

void freeAst(Ast* ast) {
  switch (ast->ast) {
  case AstType::StmtBlock: {
    CAST_INIT_AST(stmt_block, StmtBlock);
    for (Ast* child_ast : stmt_block->stmt_vec)
      if (child_ast)
        freeAst(child_ast);
  }break;
  case AstType::Expr: {
    CAST_INIT_AST(expr, Expr);
    switch (expr->expr_type) {
    case ExprType::Add:
    case ExprType::Sub:
    case ExprType::Mult:
    case ExprType::Div:
    case ExprType::Assign: {
      if (expr->fst)
        freeAst(expr->fst);
      if (expr->snd)
        freeAst(expr->snd);
      
      expr->fst = nullptr;
      expr->snd = nullptr;
    }break;
    case ExprType::Ident: {
      if (expr->ident)
        delete expr->ident;
      expr->ident = nullptr;
    }break;
    case ExprType::Number: break;
    case ExprType::Char: break;
    case ExprType::String: {
      if (expr->str)
        delete expr->str;
      expr->str = nullptr;
    }break;
    }
  }break;
  case AstType::VarType: {
    CAST_INIT_AST(var_type, VarType);
    DELETE_NOT_NULL(var_type->type_ident);
  }break;

  case AstType::VarDecl: {
    CAST_INIT_AST(var_decl, VarDecl);
    if (var_decl->init_expr)
      freeAst(var_decl->init_expr);
    
    DELETE_NOT_NULL(var_decl->ident);
    var_decl->ident = nullptr;
    
    freeAst(var_decl->var_type);
    var_decl->var_type = nullptr;
  }break;
    
  case AstType::FuncDecl: {
    CAST_INIT_AST(func_decl, FuncDecl);
    for (AstFuncArg* &func_arg: func_decl->args) {
      freeAst(func_arg);
      func_arg = nullptr;
    }
    if (func_decl->ident)
      delete func_decl->ident;
    func_decl->ident = nullptr;
    
    DELETE_NOT_NULL(func_decl->ret_type);
    if (func_decl->stmt_block)
      freeAst(func_decl->stmt_block);
    func_decl->stmt_block = nullptr;
  }break;

  case AstType::FuncArg: {
    CAST_INIT_AST(func_arg, FuncArg);
    delete func_arg->ident;
    freeAst(func_arg->var_type);
    func_arg->ident = nullptr;
    func_arg->var_type = nullptr;
  }break;
    
  case AstType::Return: {
    CAST_INIT_AST(ret, Return);
    if (ret->ret_expr) {
      freeAst(ret->ret_expr);
      ret->ret_expr = nullptr;
    }
  }break;
  
  }
  delete ast;
}

std::string serializeAstXml(const Ast *ast) {
#define XML_TAG(name) (std::string("<") + name + ">\n")
  std::string block = XML_TAG(magic_enum::enum_name(ast->ast).data());
  block += XML_TAG("p") + std::to_string(ast->p) + XML_TAG("/p");
  block += XML_TAG("end_p") + std::to_string(ast->end_p) + XML_TAG("/end_p");

  switch (ast->ast) {
  case AstType::StmtBlock: {
    CAST_INIT_CONST_AST(ast_block, StmtBlock);
    for (const Ast *stmt_ast : ast_block->stmt_vec) {
      block += serializeAstXml(stmt_ast);
    }
  } break;

  case AstType::FuncDecl: {
    CAST_INIT_CONST_AST(func_decl, FuncDecl);
    block += XML_TAG("header_end_p") + std::to_string(func_decl->header_end_p) + XML_TAG("/header_end_p");
    if (func_decl->ret_type) {
      block += XML_TAG("ret_type") + serializeAstXml(func_decl->ret_type) + XML_TAG("/ret_type");
    }
    block += XML_TAG("Arguments");
    for (const AstFuncArg *arg : func_decl->args) {
      block += serializeAstXml(static_cast<const Ast *>(arg));
    }
    block += XML_TAG("/Arguments");
    block += serializeAstXml(func_decl->stmt_block);
  } break;

  case AstType::FuncArg: {
    CAST_INIT_CONST_AST(func_arg, FuncArg);
    block += XML_TAG("ident") + *func_arg->ident + XML_TAG("/ident");
    block += serializeAstXml(func_arg->var_type);
        
  } break;
  case AstType::VarType: {
    CAST_INIT_CONST_AST(var_type, VarType);
    for (const auto& mod : var_type->mods) {
      switch (mod.mod) {
      case TypeModEnum::Ptr: {
        block += XML_TAG("Ptr/");
      }break;
      case TypeModEnum::Array: {
        block += XML_TAG("Brackets") + (mod.expr ? serializeAstXml(mod.expr) : "") + XML_TAG("/Brackets");
      }break;
      default:
        throw error::Error("Unexpected TypeModEnum::%s(%d)", magic_enum::enum_name(mod.mod), static_cast<int>(mod.mod));
      }
    }
    block += XML_TAG("type_ident") + *var_type->type_ident + XML_TAG("/type_ident");
  }break;

  case AstType::VarDecl: {
    CAST_INIT_CONST_AST(var_decl, VarDecl);
    block += XML_TAG("ident") + *var_decl->ident + XML_TAG("/ident");
    block +=
        serializeAstXml(var_decl->var_type);
    if (var_decl->init_expr) {
      block += XML_TAG("init_expr");
      block += serializeAstXml(var_decl->init_expr);
      block += XML_TAG("/init_expr");
    }
  } break;

  case AstType::Expr: {
    CAST_INIT_CONST_AST(expr, Expr);
    block += XML_TAG("expr_type") +
             magic_enum::enum_name(expr->expr_type).data() +
             XML_TAG("/expr_type");
    if (expr->expr_type == ExprType::Ident) {
      block += XML_TAG("ident") + *expr->ident + XML_TAG("/ident");
    } else if (expr->expr_type == ExprType::Number) {
      block += XML_TAG("integer") + std::to_string(expr->integer) +
               XML_TAG("/integer");
    } else if (expr->expr_type == ExprType::String) {
      block += XML_TAG("str") + *expr->str + XML_TAG("/str");
    } else if (expr->expr_type == ExprType::Char) {
      block +=
          XML_TAG("integer") + std::to_string(expr->c) + XML_TAG("/integer");
    } else {
      block += serializeAstXml(expr->fst);
      if (expr->snd)
        block += serializeAstXml(expr->snd);
    }
  } break;
  }
  block += XML_TAG("/" + magic_enum::enum_name(ast->ast).data());
  return block;
#undef CAST_INIT_CONST_AST
#undef XML_TAG
}

Parser::Parser(Lexer& lex, error::Reporter& reporter) : m_lex{ lex }, reporter{reporter} {
  for (ulong i = 0; i < LL_N; i++) {
    tok_buffer.array[i] = m_lex.next_token();
  }
}

Parser::~Parser() {
  for (PToken &p_tok : tok_buffer.array) {
    if (p_tok != nullptr) {
      delete p_tok;
      p_tok = nullptr;
    }
  }
}

void Parser::advance() {
  freeToken(tok_buffer.array[tok_buffer.p]);
  tok_buffer.array[tok_buffer.p] = nullptr;

  tok_buffer.push(m_lex.next_token());
}

PToken &Parser::lookUp(ulong i) {
  assert(i < LL_N);
  return tok_buffer.get(i);
}

PToken *Parser::plookUp(ulong i) {
  assert(i < LL_N);
  return &tok_buffer.get(i);
}

TokenType Parser::peek(ulong i) { return lookUp(i)->tok; }

void Parser::match(TokenType type) {
  if (type == peek(0)) {
    advance();
  } else {
    reporter.report_error({error::ErrorEnum::Unexpected_Token, (ulong)lookUp(0)->p, (ulong)lookUp(0)->end_p }, "Unexpected token %s, expected %s", magic_enum::enum_name(peek(0)).data(),
    magic_enum::enum_name(type).data());
    throw error::Error("Unexpected token %s",
                             magic_enum::enum_name(peek(0)).data());
  }
}

PARSING_METHOD parse() {
  // maybe do some initialization
  return stmtBlock(false);
}

AstStmtBlock *Parser::stmtBlock(bool braced) {
  if (braced)
    match(TokenType::OBracer);

  INIT_AST(stmt_block, StmtBlock);
  TokenType end_of_block = braced ? TokenType::CBracer : TokenType::Eof;

  while (peek(0) != end_of_block) {
    if (peek(0) == TokenType::Ident && peek(1) == TokenType::Colon) {
      stmt_block->stmt_vec.push_back(varDecl());
    } else if (peek(0) == TokenType::Ident && peek(1) == TokenType::DColon) {
      stmt_block->stmt_vec.push_back(funcDecl());
    } else if (peek(0) == TokenType::Return) {
      stmt_block->stmt_vec.push_back(returnStmt());
    } else {
      stmt_block->stmt_vec.push_back(expr());
    }
  }
  if (braced) {
    stmt_block->end_p = lookUp(0)->end_p;
    match(TokenType::CBracer);
  }
  else {
    stmt_block->end_p = stmt_block->stmt_vec.back()->end_p;
  }
  return stmt_block;
}

PARSING_METHOD varDecl() {
  INIT_AST_VAR_DECL(var_decl);

  TokenIdent *tok_ident = static_cast<TokenIdent *>(*plookUp(0));

  var_decl->ident = tok_ident->ident;
  tok_ident->ident = nullptr;

  match(TokenType::Ident);
  match(TokenType::Colon);

  tok_ident = static_cast<TokenIdent *>(*plookUp(0));

  var_decl->var_type = varType();

  if (peek(0) == TokenType::Eq) {
    match(TokenType::Eq);
    var_decl->init_expr = expr();
  }
  var_decl->end_p = lookUp(0)->p;
  match(TokenType::Terminal);
  return var_decl;
}

PARSING_METHOD funcDecl() {
  INIT_AST_FUNC_DECL(func_decl);
  TokenIdent *tok_ident = static_cast<TokenIdent *>(*plookUp(0));
  func_decl->ident = tok_ident->ident;
  tok_ident->ident = nullptr;

  match(TokenType::Ident);
  match(TokenType::DColon);
  match(TokenType::OParen);
  // func args
  TokenType next_arg_exist_type_check = TokenType::Ident;
  if (peek(0) == TokenType::Ident) {
    INIT_AST(func_arg, FuncArg);

    tok_ident = static_cast<TokenIdent *>(*plookUp(0));
    func_arg->ident = tok_ident->ident;
    tok_ident->ident = nullptr;

    match(TokenType::Ident);
    match(TokenType::Colon);

    tok_ident = static_cast<TokenIdent *>(*plookUp(0));
    func_arg->var_type = varType();
    func_arg->end_p = func_arg->var_type->end_p;

    func_decl->args.push_back(func_arg);
    
    while (peek(0) == TokenType::Comma) {
      match(TokenType::Comma);
      INIT_AST(func_arg, FuncArg);

      tok_ident = static_cast<TokenIdent *>(*plookUp(0));
      func_arg->ident = tok_ident->ident;
      tok_ident->ident = nullptr;

      match(TokenType::Ident);
      match(TokenType::Colon);

      tok_ident = static_cast<TokenIdent *>(*plookUp(0));
      func_arg->var_type = varType();
      func_arg->end_p = func_arg->var_type->end_p;

      func_decl->args.push_back(func_arg);
    }
  }
  func_decl->header_end_p = tok_ident->end_p;
  match(TokenType::CParen);

  if (peek(0) != TokenType::OBracer) {
    tok_ident = static_cast<TokenIdent *>(*plookUp(0));
    func_decl->ret_type = varType();
    func_decl->header_end_p = func_decl->ret_type->end_p;
  }
  
  func_decl->stmt_block = stmtBlock();
  func_decl->end_p = func_decl->stmt_block->end_p;
  return func_decl;
}


AstVarType* Parser::varType() {
  INIT_AST(type, VarType);
  while (peek(0) != TokenType::Ident) {
    if (peek(0) == TokenType::Asterisk) {
      match(TokenType::Asterisk);
      AstVarType::TypeMod mod = { .mod = TypeModEnum::Ptr };
      type->mods.push_back(mod);
    } else if (peek(0) == TokenType::OBracket) {
      AstVarType::TypeMod mod = { TypeModEnum::Array};
      if (peek(1) == TokenType::CBracket) {
        match(TokenType::OBracket);
        match(TokenType::CBracket);
      } else { // if expr beetween bracktest
        mod.expr = factor(); // factor knows how to handle it
      }
      type->mods.push_back(mod);
    } else {
      reporter.report_error({ error::ErrorEnum::Invalid_Type_Modifier, static_cast<ulong>(lookUp(0)->p), static_cast<ulong>(lookUp(0)->end_p) }, "Invalid type modifier %s", magic_enum::enum_name(peek(0)));
      break; // will cause error on the following ident match
    }
  }
  own(type->type_ident, static_cast<TokenIdent*>(lookUp(0))->ident);
  type->end_p = lookUp(0)->end_p;
  match(TokenType::Ident);
  return type;
}

PARSING_METHOD returnStmt() {
  match(TokenType::Return);
  INIT_AST(ret, Return);

  if (peek(0) != TokenType::Terminal)
    ret->ret_expr = expr();
  else ret->ret_expr = nullptr;
  
  ret->end_p = lookUp(0)->end_p;
  match(TokenType::Terminal);
  return ret;
}

PARSING_METHOD_EXPR expr() {
  AstExpr *curr_expr = expr_arith();
  while (peek(0) == TokenType::Eq) {
    match(TokenType::Eq);

    INIT_AST_EXPR(asn_expr, Assign);

    asn_expr->fst = curr_expr;
    asn_expr->p = asn_expr->fst->p;

    asn_expr->snd = static_cast<AstExpr *>(expr_arith());
    asn_expr->end_p = asn_expr->snd->end_p;

    curr_expr = asn_expr;
  }
  return curr_expr;
}

PARSING_METHOD_EXPR expr_arith() {
  AstExpr *arith_expr = term();
  while (true) {
    if (peek(0) == TokenType::Plus) {
      match(TokenType::Plus);
      INIT_AST_EXPR(add_expr, Add);
      add_expr->fst = arith_expr;
      add_expr->p = add_expr->fst->p;

      add_expr->snd = term();
      add_expr->end_p = add_expr->snd->end_p;

       arith_expr = add_expr;
    } else if (peek(0) == TokenType::Minus) {
      match(TokenType::Minus);
      INIT_AST_EXPR(minus_expr, Sub);
      minus_expr->fst = arith_expr;
      minus_expr->p = minus_expr->fst->p;

      minus_expr->snd = term();
      minus_expr->end_p = minus_expr->snd->end_p;

      arith_expr = minus_expr;
    } else
      break;
  }
  return arith_expr;
}

PARSING_METHOD_EXPR term() {
  AstExpr *term_expr = factor();
  while (true) {
    if (peek(0) == TokenType::Asterisk) {
      match(TokenType::Asterisk);
      INIT_AST_EXPR(mult_expr, Mult);
      mult_expr->fst = term_expr;
      mult_expr->p = mult_expr->fst->p;

      mult_expr->snd = factor();
      mult_expr->end_p = mult_expr->snd->end_p;

      term_expr = mult_expr;
    } else if (peek(0) == TokenType::Slash) {
      match(TokenType::Slash);

      INIT_AST_EXPR(div_expr, Div);
      div_expr->fst = term_expr;
      div_expr->p = div_expr->fst->p;

      div_expr->snd = factor();
      div_expr->end_p = div_expr->snd->end_p;

      term_expr = div_expr;
    } else
      break;
  }
  return term_expr;
}

PARSING_METHOD_EXPR factor() {
  switch (peek(0)) {
  case TokenType::OParen: {
    match(TokenType::OParen);
    AstExpr *factor_expr = expr();
    match(TokenType::CParen);
    return factor_expr;
  }
  case TokenType::Ident: {
    INIT_AST_EXPR(factor_ident, Ident);

    // move ident pointer to factor_ident ast
    TokenIdent *tok_ident = static_cast<TokenIdent *>(*plookUp(0));
    factor_ident->ident = tok_ident->ident;
    tok_ident->ident = nullptr;
    match(TokenType::Ident);

    factor_ident->end_p = factor_ident->p + factor_ident->ident->length();

    return factor_ident;
  }
  case TokenType::Int: {
    INIT_AST_EXPR(factor_int, Number);
    factor_int->num_class = NumberClass::Integer;
    factor_int->integer = reinterpret_cast<TokenInt *>(lookUp(0))->number;
    factor_int->end_p = lookUp(0)->end_p;
    match(TokenType::Int);
    return factor_int;
  }
  case TokenType::Char: {
    INIT_AST_EXPR(factor_char, Char);
    factor_char->c = reinterpret_cast<TokenChar *>(lookUp(0))->ch;
    factor_char->end_p = lookUp(0)->end_p;

    match(TokenType::Char);
    return factor_char;
  }
  case TokenType::String: {
    INIT_AST_EXPR(factor_string, String);

    TokenString* tok_str = reinterpret_cast<TokenString*>(*plookUp(0));
    factor_string->str = tok_str->str;
    factor_string->end_p = lookUp(0)->end_p;

    tok_str->str = nullptr;
    match(TokenType::String);
    return factor_string;
  }
  case TokenType::Ampersand: {
    INIT_AST_EXPR(factor_addrof, AddrOf);
    match(TokenType::Ampersand);
    factor_addrof->expr = expr();
    return factor_addrof;
  }
  case TokenType::Asterisk: {
    INIT_AST_EXPR(factor_deref, DeRef);
    match(TokenType::Asterisk);
    factor_deref->expr = expr();
    return factor_deref;
  }
  case TokenType::OBracket: {
    INIT_AST_EXPR(factor_seq_acc, SeqAccess);
    match(TokenType::OBracket);
    factor_seq_acc->expr = expr();
    match(TokenType::CBracket);
    return factor_seq_acc;
  }
  default:
    reporter.report_error({ error::ErrorEnum::Unexpected_Token, (ulong)lookUp(0)->p, (ulong)lookUp(0)->end_p }, "Unexcpected token %s", magic_enum::enum_name(peek(0)).data());
    throw error::Error("Unexpected token %s",
                             magic_enum::enum_name(peek(0)).data());
  }
}
//
//namespace error {
//ParserError::ParserError(Ast ast, const char *format, ...) {
//  va_list args;
//  va_start(args, format);
//
//  this->make_msg(format, args);
//  this->ast = ast;
//
//  va_end(args);
//}
//
//ParserError::ParserError(Ast *ast_, const char *format, ...) {
//  va_list args;
//  va_start(args, format);
//
//  this->make_msg(format, args);
//  this->ast = *ast_;
//  delete ast_;
//
//  va_end(args);
//}
//
//Ast ParserError::meta() const { return ast; }
//} // namespace error
