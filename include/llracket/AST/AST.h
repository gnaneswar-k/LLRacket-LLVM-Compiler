#ifndef LLRACKET_AST_AST_H
#define LLRACKET_AST_AST_H

#include "llracket/Lexer/Token.h"
#include <any>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <string>
#include <vector>

class AST;
class Program;
typedef llvm::StringMap<std::any> ProgramInfo;
using std::string;
using std::vector;
class Expr;
class Prim;
class Int;
class Bool;
class Var;
class Let;
class If;
class Begin;
class SetBang;
class WhileLoop;
class Vect;

enum RetType { Integer, Boolean, Void, Vector, Read, Unknown };
struct RetVal {
  RetType Type = RetType::Unknown;
  vector<RetVal> TypeList;

  RetVal(RetType Type) : Type(Type) {}
  RetVal(RetType Type, vector<RetVal> TypeList)
      : Type(Type), TypeList(TypeList) {}
};

class ASTVisitor {
public:
  virtual ~ASTVisitor() {}
  virtual void visit(Program &) {}
  virtual void visit(Expr &) {}
  virtual void visit(Prim &) {}
  virtual void visit(Int &) = 0;
  virtual void visit(Var &) = 0;
  virtual void visit(Bool &) = 0;
  virtual void visit(Let &) {}
  virtual void visit(If &) {}
  virtual void visit(Begin &) {}
  virtual void visit(SetBang &) {}
  virtual void visit(WhileLoop &) {}
  virtual void visit(Vect &) {}
};

class AST {
public:
  virtual ~AST() {}
  virtual void accept(ASTVisitor &V) = 0;
};

class Program : public AST {
  Expr *E;
  ProgramInfo Info;

public:
  Program(Expr *E) : E(E) {}
  Program(Expr *E, ProgramInfo Info) : E(E), Info(Info) {}

  Expr *getExpr() const { return E; };
  ProgramInfo getInfo() const { return Info; }

  void setExpr(Expr *NewExpr) { E = NewExpr; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

class Expr : public AST {
public:
  enum ExprKind {
    ExprPrim,
    ExprInt,
    ExprVar,
    ExprLet,
    ExprBool,
    ExprIf,
    ExprBegin,
    ExprSetBang,
    ExprWhile,
    ExprVector
  };

private:
  const ExprKind Kind;
  RetType ExpectedType = RetType::Unknown; // Unknown by default.
  RetVal ReturnValue;                      // Unknown by default.

public:
  Expr(ExprKind Kind) : Kind(Kind), ReturnValue(RetType::Unknown) {}

  ExprKind getKind() const { return Kind; };
  RetType getExpectedType() const { return ExpectedType; }
  RetVal getReturnValue() const { return ReturnValue; }

  void setExpectedType(RetType NewType) { ExpectedType = NewType; }
  void setReturnValue(RetVal newReturnValue) { ReturnValue = newReturnValue; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

class Prim : public Expr {
  TokenKind Op;
  Expr *E1 = NULL;
  Expr *E2 = NULL;
  Expr *E3 = NULL;

public:
  Prim(TokenKind Op) : Expr(ExprPrim), Op(Op) {}
  Prim(TokenKind Op, Expr *E1) : Expr(ExprPrim), Op(Op), E1(E1) {}
  Prim(TokenKind Op, Expr *E1, Expr *E2)
      : Expr(ExprPrim), Op(Op), E1(E1), E2(E2) {}
  Prim(TokenKind Op, Expr *E1, Expr *E2, Expr *E3)
      : Expr(ExprPrim), Op(Op), E1(E1), E2(E2), E3(E3) {}

  TokenKind getOp() const { return Op; }
  Expr *getE1() const { return E1; }
  Expr *getE2() const { return E2; }
  Expr *getE3() const { return E3; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprPrim; }
};

class Int : public Expr {
  StringRef Value;

public:
  Int(StringRef Value) : Expr(ExprInt), Value(Value) {}

  StringRef getValue() const { return Value; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return true; }
};

class Var : public Expr {
  string Value;

public:
  Var(StringRef Value) : Expr(ExprVar), Value(Value.str()) {}

  string getValue() const { return Value; }

  void setValue(StringRef NewValue) { Value = NewValue.str(); }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return true; }
};

class Let : public Expr {
  string Ident;
  Expr *EV;
  Expr *EB;

public:
  Let(StringRef Ident, Expr *EV, Expr *EB)
      : Expr(ExprLet), Ident(Ident.str()), EV(EV), EB(EB) {}

  string getIdent() const { return Ident; }
  Expr *getVarExpr() const { return EV; }
  Expr *getBodyExpr() const { return EB; }

  void setIdent(string NewIdent) { Ident = NewIdent; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprLet; }
};

class Bool : public Expr {
  StringRef Value;

public:
  Bool(StringRef Value) : Expr(ExprBool), Value(Value) {}

  StringRef getValue() const { return Value; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return true; }
};

class If : public Expr {
  Expr *EC;
  Expr *ET;
  Expr *EE;

public:
  If(Expr *EC, Expr *ET, Expr *EE) : Expr(ExprIf), EC(EC), ET(ET), EE(EE) {}

  Expr *getCondExpr() const { return EC; }
  Expr *getThenExpr() const { return ET; }
  Expr *getElseExpr() const { return EE; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprIf; }
};

class Begin : public Expr {
  vector<Expr *> EList;
  Expr *EB;

public:
  Begin(vector<Expr *> EList, Expr *EB)
      : Expr(ExprBegin), EList(EList), EB(EB) {}

  const vector<Expr *> &getSubExprList() const { return EList; }
  Expr *getBodyExpr() const { return EB; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprBegin; }
};

class SetBang : public Expr {
  string Ident;
  Expr *E;

public:
  SetBang(StringRef Ident, Expr *E)
      : Expr(ExprSetBang), Ident(Ident.str()), E(E) {}

  string getIdent() const { return Ident; }
  Expr *getExpr() const { return E; }

  void setIdent(StringRef NewValue) { Ident = NewValue.str(); }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprSetBang; }
};

class WhileLoop : public Expr {
  Expr *EC;
  Expr *EB;

public:
  WhileLoop(Expr *EC, Expr *EB) : Expr(ExprWhile), EC(EC), EB(EB) {}

  Expr *getCondExpr() const { return EC; }
  Expr *getBodyExpr() const { return EB; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprWhile; }
};

class Vect : public Expr {
  vector<Expr *> EList;

public:
  Vect(vector<Expr *> EList) : Expr(ExprVector), EList(EList) {}

  const vector<Expr *> &getExprList() const { return EList; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVector; }
};

#endif
