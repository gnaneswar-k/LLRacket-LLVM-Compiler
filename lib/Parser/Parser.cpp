#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <vector>

using namespace llracket;
using llvm::StringRef;
using std::vector;
using tok::TokenKind;

AST *Parser::parse() {
  Program *P = new Program(parseExpr());
  AST *Res = llvm::dyn_cast<AST>(P);
  expect(TokenKind::eof);
  return Res;
}

Expr *Parser::parseExpr() {
  auto ErrorHandler = [this]() {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };

  if (Tok.is(TokenKind::unknown)) {
    return ErrorHandler();
  }

  if (Tok.is(TokenKind::integer_literal)) {
    Int *Ret = new Int(Tok.getText());
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::identifier)) {
    Var *Ret = new Var(Tok.getText());
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::boolean)) {
    Bool *Ret = new Bool(Tok.getText());
    advance();
    return Ret;
  }

  if (!consume(TokenKind::l_paren))
    return ErrorHandler();

  if (Tok.is(TokenKind::read)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::read);
  }

  if (Tok.is(TokenKind::kw_let)) {
    advance();
    // Consume l_paren and l_square.
    if (!consume(TokenKind::l_paren))
      return ErrorHandler();
    if (!consume(TokenKind::l_square))
      return ErrorHandler();

    // Expect a variable.
    if (!expect(TokenKind::identifier))
      return ErrorHandler();
    // The variable.
    StringRef Ident = Tok.getText();
    advance();
    // Parse the expression for the variable.
    Expr *EV = parseExpr();

    // Consume r_square and r_paren.
    if (!consume(TokenKind::r_square))
      return ErrorHandler();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();

    // Parse the body expression.
    Expr *EB = parseExpr();

    // Consume closing r_paren.
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    // Return the let.
    return new Let(Ident, EV, EB);
  }

  // All operators that require two expressions.
  if (Tok.is(TokenKind::plus) || Tok.is(TokenKind::op_and) ||
      Tok.is(TokenKind::op_or) || Tok.is(TokenKind::cmp_eq) ||
      Tok.is(TokenKind::cmp_less) || Tok.is(TokenKind::cmp_less_eq) ||
      Tok.is(TokenKind::cmp_great) || Tok.is(TokenKind::cmp_great_eq)) {
    // Save op token.
    TokenKind OpKind = Tok.getKind();
    advance();
    // Parse expressions.
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(OpKind, E1, E2);
  }

  if (Tok.is(TokenKind::minus)) {
    advance();

    Expr *E1 = parseExpr();

    if (Tok.is(TokenKind::r_paren)) {
      advance();
      return new Prim(TokenKind::minus, E1);
    }

    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::minus, E1, E2);
  }

  if (Tok.is(TokenKind::op_not)) {
    advance();
    Expr *E = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::op_not, E);
  }

  if (Tok.is(TokenKind::kw_if_cond)) {
    advance();
    Expr *EC = parseExpr();
    Expr *ET = parseExpr();
    Expr *EE = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new If(EC, ET, EE);
  }

  if (Tok.is(TokenKind::kw_begin)) {
    advance();
    vector<Expr *> EList;
    while (!Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
      Expr *E = parseExpr();
      EList.push_back(E);
    }

    // Throw error if we reached EOF before closing parenthesis of begin.
    if (Tok.is(TokenKind::eof))
      return ErrorHandler();
    consume(TokenKind::r_paren);

    // Throw an error if there isn't at least one subexpression
    // along with one body expression.
    if (EList.size() < 1)
      return ErrorHandler();
    Expr *EB = EList.back();
    EList.pop_back();
    return new Begin(EList, EB);
  }

  if (Tok.is(TokenKind::kw_set_bang)) {
    advance();
    // Expect an identifier.
    if (!expect(TokenKind::identifier))
      return ErrorHandler();
    // The identifier.
    StringRef Ident = Tok.getText();
    advance();
    // Parse the expression for the identifier.
    Expr *E = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new SetBang(Ident, E);
  }

  if (Tok.is(TokenKind::kw_while_loop)) {
    advance();
    Expr *EC = parseExpr();
    Expr *EB = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new WhileLoop(EC, EB);
  }

  if (Tok.is(TokenKind::kw_vector)) {
    advance();
    vector<Expr *> EList;
    // Get list of expressions in the vector.
    while (!Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
      Expr *E = parseExpr();
      EList.push_back(E);
    }

    // Throw error if we reached EOF before closing parenthesis of begin.
    if (Tok.is(TokenKind::eof))
      return ErrorHandler();
    consume(TokenKind::r_paren);

    return new Vect(EList);
  }

  if (Tok.is(TokenKind::kw_vector_length)) {
    advance();
    // The vector.
    Expr *V = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::kw_vector_length, V);
  }

  if (Tok.is(TokenKind::kw_vector_ref)) {
    advance();
    // The vector.
    Expr *V = parseExpr();
    // The index.
    if (!expect(TokenKind::integer_literal))
      return ErrorHandler();
    Int *Index = (Int *)parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::kw_vector_ref, V, Index);
  }

  if (Tok.is(TokenKind::kw_vector_set_bang)) {
    advance();
    // The vector.
    Expr *EV = parseExpr();
    // The index.
    if (!expect(TokenKind::integer_literal))
      return ErrorHandler();
    Int *Index = (Int *)parseExpr();
    // The new expression element.
    Expr *EE = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::kw_vector_set_bang, EV, Index, EE);
  }

  return ErrorHandler();
}
