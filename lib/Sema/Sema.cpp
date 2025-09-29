#include "llracket/Sema/Sema.h"
#include "llracket/Lexer/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"
#include <string>
#include <vector>

using llvm::StringRef;
using std::string;
using std::vector;

namespace {
/**
 * @brief Visitor to uniquify variables in the AST.
 */
class ProgramUniquify : public ASTVisitor {
  llvm::StringMap<string> Env;
  int Counter;

public:
  ProgramUniquify() : Counter(0) {}

  virtual void visit(Program &Node) override {
    if (Node.getExpr())
      Node.getExpr()->accept(*this);
  }

  virtual void visit(Int &Node) override {
    // No update needed for int.
    return;
  }

  virtual void visit(Bool &Node) override {
    // No update needed for bool.
    return;
  }

  virtual void visit(Var &Node) override {
    string Ident = Node.getValue();
    // Update if identifier exists in environment.
    if (Env.contains(Ident))
      Node.setValue(Env[Ident]);
    else
      return; // Missing variable is handled in ProgramCheck.
  }

  virtual void visit(Let &Node) override {
    // Update the assigned expression first.
    Node.getVarExpr()->accept(*this);

    // Generate new identifier.
    string Ident = Node.getIdent();
    string NewIdent = (Ident + "." + std::to_string(Counter++));

    // Store old identifier in case of nesting.
    string PrevIdent;
    bool hadPrevIdent = false;
    if (Env.contains(Ident)) {
      PrevIdent = Env[Ident];
      hadPrevIdent = true;
    }

    // Store new identifier in the environment and in the Let node.
    Env[Ident] = NewIdent;
    Node.setIdent(NewIdent);

    // Update the body expression.
    Node.getBodyExpr()->accept(*this);

    // Restore previous identifier if it exists.
    if (hadPrevIdent)
      Env[Ident] = PrevIdent;
    // Delete identifier from environment if it did not exist previously.
    else
      Env.erase(Ident);
  }

  virtual void visit(Prim &Node) override {
    // Update expressions if they exist.
    if (Node.getE1())
      Node.getE1()->accept(*this);
    if (Node.getE2())
      Node.getE2()->accept(*this);
    if (Node.getE3())
      Node.getE3()->accept(*this);
  }

  virtual void visit(If &Node) override {
    // Update expressions.
    Node.getCondExpr()->accept(*this);
    Node.getThenExpr()->accept(*this);
    Node.getElseExpr()->accept(*this);
  }

  virtual void visit(Begin &Node) override {
    const vector<Expr *> &EList = Node.getSubExprList();
    // Update expressions.
    for (Expr *E : EList)
      E->accept(*this);
    Node.getBodyExpr()->accept(*this);
  }

  virtual void visit(SetBang &Node) override {
    string Ident = Node.getIdent();
    // Update if identifier exists in environment.
    if (Env.contains(Ident))
      Node.setIdent(Env[Ident]);
    else
      return; // Missing variable is handled in ProgramCheck.
    // Update expression.
    Node.getExpr()->accept(*this);
  }

  virtual void visit(WhileLoop &Node) override {
    // Update expressions.
    Node.getCondExpr()->accept(*this);
    Node.getBodyExpr()->accept(*this);
  }

  virtual void visit(Vect &Node) override {
    const vector<Expr *> &EList = Node.getExprList();
    // Update expressions.
    for (Expr *E : EList)
      E->accept(*this);
  }
};

/**
 * @brief Visitor to perform semantic and type checking in the AST.
 */
class ProgramCheck : public ASTVisitor {
  bool HasError;
  llvm::StringMap<RetVal *> Scope;

public:
  ProgramCheck() : HasError(false) {}

  bool hasError() { return HasError; }

  virtual void visit(Program &Node) override {
    if (Node.getExpr()) {
      Node.getExpr()->accept(*this);
      RetType ReturnType = Node.getExpr()->getReturnValue().Type;
      // Throw error if program does not return an integer or boolean.
      if ((ReturnType != RetType::Integer) &&
          (ReturnType != RetType::Boolean) && (ReturnType != RetType::Void))
        HasError = true;
    }
    // Throw error if program doesn't have an expression.
    else
      HasError = true;
  };

  virtual void visit(Expr &Node) override {
    // Check the expressions based on type.
    if (llvm::isa<Prim>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Int>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Bool>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Var>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Let>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<If>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<WhileLoop>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Begin>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<SetBang>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Vect>(Node)) {
      Node.accept(*this);
      return;
    }
    // Throw error if not an expression type.
    HasError = true;
  }

  virtual void visit(Prim &Node) override {
    auto &PrimNode = llvm::cast<Prim>(Node);

    // No further checking needed for read.
    if (PrimNode.getOp() == tok::read) {
      if (PrimNode.getExpectedType() == RetType::Unknown)
        PrimNode.setReturnValue(RetVal(RetType::Read));
      else
        PrimNode.setReturnValue(RetVal(PrimNode.getExpectedType()));
      return;
    }

    // Check expression for not.
    if (PrimNode.getOp() == tok::op_not) {
      // Handle expression.
      Expr *E1 = PrimNode.getE1();
      E1->setExpectedType(RetType::Boolean);
      E1->accept(*this);
      // Throw error if parsed expression is not a boolean and not a read.
      if (E1->getExpectedType() != E1->getReturnValue().Type)
        HasError = true;
      // Else update return type.
      else
        PrimNode.setReturnValue(RetVal(RetType::Boolean));
      return;
    }

    // Check expression for negation.
    if (PrimNode.getOp() == tok::minus) {
      if (PrimNode.getE1() and !PrimNode.getE2()) {
        Expr *E1 = PrimNode.getE1();
        E1->setExpectedType(RetType::Integer);
        E1->accept(*this);
        // Throw error if parsed expression is not an integer and not a read.
        if (E1->getExpectedType() != E1->getReturnValue().Type)
          HasError = true;
        // Else update return type.
        else
          PrimNode.setReturnValue(RetVal(RetType::Integer));
        return;
      }
    }

    // Check expression for eq?.
    if (PrimNode.getOp() == tok::cmp_eq) {
      Expr *E1 = PrimNode.getE1();
      E1->accept(*this);
      RetVal TypeE1 = E1->getReturnValue();

      Expr *E2 = PrimNode.getE2();
      E2->accept(*this);
      RetVal TypeE2 = E2->getReturnValue();

      // Throw error if E1 or E2 is of unknown type.
      if (TypeE1.Type == RetType::Unknown || TypeE2.Type == RetType::Unknown) {
        HasError = true;
        return;
      }
      // Throw error if E1 and E2 are read.
      if (TypeE1.Type == RetType::Read && TypeE2.Type == RetType::Read) {
        HasError = true;
        return;
      }
      // Set E1 type if it is read and E2 is not read.
      if (TypeE1.Type == RetType::Read && TypeE2.Type != RetType::Read) {
        E1->setExpectedType(TypeE2.Type);
        E1->accept(*this);
      }
      // Set E2 type if it is read and E1 is not read.
      else if (TypeE1.Type != RetType::Read && TypeE2.Type == RetType::Read) {
        E2->setExpectedType(TypeE1.Type);
        E2->accept(*this);
      }

      // Throw error if E1 and E2 are not the same type.
      if (E1->getReturnValue().Type != E2->getReturnValue().Type)
        HasError = true;
      // Else update return type.
      else
        PrimNode.setReturnValue(RetVal(RetType::Boolean));
      return;
    }

    // Check expressions for addition and subtraction.
    if (PrimNode.getOp() == tok::plus || PrimNode.getOp() == tok::minus) {
      Expr *E1 = PrimNode.getE1();
      E1->setExpectedType(RetType::Integer);
      E1->accept(*this);
      RetVal TypeE1 = E1->getReturnValue();

      Expr *E2 = PrimNode.getE2();
      E2->setExpectedType(RetType::Integer);
      E2->accept(*this);
      RetVal TypeE2 = E2->getReturnValue();

      // Throw error if E1 and E2 do not return integers.
      if (TypeE1.Type != RetType::Integer || TypeE2.Type != RetType::Integer)
        HasError = true;
      // Else update return type.
      else
        PrimNode.setReturnValue(RetVal(RetType::Integer));
      return;
    }

    // Check expressions for >=, >, <= ,<.
    if (PrimNode.getOp() == tok::cmp_great_eq ||
        PrimNode.getOp() == tok::cmp_great ||
        PrimNode.getOp() == tok::cmp_less_eq ||
        PrimNode.getOp() == tok::cmp_less) {
      Expr *E1 = PrimNode.getE1();
      E1->setExpectedType(RetType::Integer);
      E1->accept(*this);
      RetVal TypeE1 = E1->getReturnValue();

      Expr *E2 = PrimNode.getE2();
      E2->setExpectedType(RetType::Integer);
      E2->accept(*this);
      RetVal TypeE2 = E2->getReturnValue();

      // Throw error if E1 and E2 do not return integers.
      if (TypeE1.Type != RetType::Integer || TypeE2.Type != RetType::Integer)
        HasError = true;
      // Else update return type.
      else
        PrimNode.setReturnValue(RetVal(RetType::Boolean));
      return;
    }

    // Check expressions for and, or operations.
    if (PrimNode.getOp() == tok::op_and || PrimNode.getOp() == tok::op_or) {
      Expr *E1 = PrimNode.getE1();
      E1->setExpectedType(RetType::Boolean);
      E1->accept(*this);
      RetVal TypeE1 = E1->getReturnValue();

      Expr *E2 = PrimNode.getE2();
      E2->setExpectedType(RetType::Boolean);
      E2->accept(*this);
      RetVal TypeE2 = E2->getReturnValue();

      // Throw error if E1 or E2 does not return a boolean.
      if (TypeE1.Type != RetType::Boolean || TypeE2.Type != RetType::Boolean)
        HasError = true;
      // Else update return type.
      else
        PrimNode.setReturnValue(RetVal(RetType::Boolean));
      return;
    }

    // Check expression for vector-length.
    if (PrimNode.getOp() == tok::kw_vector_length) {
      Expr *E1 = PrimNode.getE1();
      E1->setExpectedType(RetType::Vector);
      E1->accept(*this);
      RetVal TypeE1 = E1->getReturnValue();

      // Throw error if E1 does not return a vector.
      if (TypeE1.Type != RetType::Vector)
        HasError = true;
      // Else update return type.
      else
        PrimNode.setReturnValue(RetVal(RetType::Integer));
    }

    // Check expression for vector-ref.
    if (PrimNode.getOp() == tok::kw_vector_ref) {
      Expr *E1 = PrimNode.getE1();
      E1->setExpectedType(RetType::Vector);
      E1->accept(*this);
      RetVal TypeE1 = E1->getReturnValue();

      // Throw error if E1 does not return a vector.
      if (TypeE1.Type != RetType::Vector) {
        HasError = true;
        return;
      }

      Expr *E2 = PrimNode.getE2();
      // Throw error if E2 does not return an Integer.
      if (E2->getKind() != Expr::ExprInt) {
        HasError = true;
        return;
      }

      E2->setExpectedType(RetType::Integer);
      E2->accept(*this);

      // Update return type.
      Int *I = (Int *)E2;
      int index = std::stoi(I->getValue().str());
      PrimNode.setReturnValue(TypeE1.TypeList.at(index));
    }

    // Check expression for vector-set!.
    if (PrimNode.getOp() == tok::kw_vector_set_bang) {
      Expr *E1 = PrimNode.getE1();
      E1->setExpectedType(RetType::Vector);
      E1->accept(*this);
      RetVal TypeE1 = E1->getReturnValue();

      // Throw error if E1 does not return a vector.
      if (TypeE1.Type != RetType::Vector) {
        HasError = true;
        return;
      }

      Expr *E2 = PrimNode.getE2();
      // Throw error if E2 does not return an Int expression.
      if (E2->getKind() != Expr::ExprInt) {
        HasError = true;
        return;
      }
      E2->setExpectedType(RetType::Integer);
      E2->accept(*this);
      Int *I = (Int *)E2;
      int index = std::stoi(I->getValue().str());

      // Get return type of element to be updated.
      RetVal TypeEI = TypeE1.TypeList.at(index);

      Expr *E3 = PrimNode.getE3();
      E3->setExpectedType(TypeEI.Type);
      E3->accept(*this);
      RetVal TypeE3 = E3->getReturnValue();

      // Throw error if element to be updated and E3 have different types.
      if (TypeEI.Type != TypeE3.Type)
        HasError = true;
      // Else update return type.
      else
        PrimNode.setReturnValue(RetVal(RetType::Void));
    }
  }

  virtual void visit(Let &Node) override {
    auto &LetNode = llvm::cast<Let>(Node);

    // Check expression being assigned to variable.
    Expr *VarExpr = LetNode.getVarExpr();
    VarExpr->accept(*this);
    RetVal VarType = VarExpr->getReturnValue();

    // Throw error if VarExpr returns unknown.
    if (VarType.Type == RetType::Unknown) {
      HasError = true;
      return;
    }
    // If VarExpr returns read, set identifier to be integer.
    if (VarType.Type == RetType::Read) {
      VarExpr->setExpectedType(RetType::Integer);
      VarExpr->accept(*this);
      RetVal RV(RetType::Integer);
      Scope[LetNode.getIdent()] = &RV;
    }
    // Else store the identifier and its type in scope.
    else
      Scope[LetNode.getIdent()] = &VarType;

    // Check body expression.
    Expr *BodyExpr = LetNode.getBodyExpr();
    BodyExpr->setExpectedType(LetNode.getExpectedType());
    BodyExpr->accept(*this);
    // Remove identifier from scope.
    Scope.erase(LetNode.getIdent());

    // Throw error if BodyExpr returns unknown.
    if (BodyExpr->getReturnValue().Type == RetType::Unknown)
      HasError = true;
    // Update return type.
    else
      LetNode.setReturnValue(BodyExpr->getReturnValue());

    return;
  }

  virtual void visit(Var &Node) override {
    // Error if scope doesn't contain the identifier.
    if (Scope.contains(Node.getValue())) {
      // Get the identifier from the variable and
      // update return type as the identifier's type.
      Node.setReturnValue(*Scope[Node.getValue()]);
      return;
    }
    HasError = true;
  }

  virtual void visit(Int &Node) override {
    Node.setReturnValue(RetVal(RetType::Integer));
    return;
  }

  virtual void visit(Bool &Node) override {
    Node.setReturnValue(RetVal(RetType::Boolean));
    return;
  }

  virtual void visit(If &Node) override {
    auto &IfNode = llvm::cast<If>(Node);

    // Check if condition returns boolean.
    Expr *CondExpr = IfNode.getCondExpr();
    CondExpr->setExpectedType(RetType::Boolean);
    CondExpr->accept(*this);
    if (CondExpr->getReturnValue().Type != CondExpr->getExpectedType()) {
      HasError = true;
      return;
    }

    // Handle Then.
    Expr *ThenExpr = IfNode.getThenExpr();
    ThenExpr->setExpectedType(IfNode.getExpectedType());
    ThenExpr->accept(*this);
    RetVal ThenType = ThenExpr->getReturnValue();

    // Handle Else.
    Expr *ElseExpr = IfNode.getElseExpr();
    ElseExpr->setExpectedType(IfNode.getExpectedType());
    ElseExpr->accept(*this);
    RetVal ElseType = ElseExpr->getReturnValue();

    // Throw error if Then or Else are unknown or have different types.
    if (ThenType.Type == RetType::Unknown ||
        ElseType.Type == RetType::Unknown || ThenType.Type != ElseType.Type)
      HasError = true;
    // Else update return type.
    else
      IfNode.setReturnValue(ThenType);

    return;
  }

  virtual void visit(Begin &Node) override {
    auto &BeginNode = llvm::cast<Begin>(Node);

    // Handle subexpressions.
    for (Expr *E : BeginNode.getSubExprList()) {
      E->accept(*this);
    }

    // Handle Body.
    Expr *BodyExpr = BeginNode.getBodyExpr();
    BodyExpr->setExpectedType(BeginNode.getExpectedType());
    BodyExpr->accept(*this);

    // Update return type.
    BeginNode.setReturnValue(BodyExpr->getReturnValue());
    return;
  }

  virtual void visit(SetBang &Node) override {
    auto &SetBangNode = llvm::cast<SetBang>(Node);

    // Error if Scope doesn't contain the identifier.
    string Ident = SetBangNode.getIdent();
    if (Scope.contains(Ident)) {
      // Check expression being assigned to variable.
      Expr *E = SetBangNode.getExpr();
      E->accept(*this);

      // Throw error if initial type and new type are not the same.
      if (Scope[SetBangNode.getIdent()]->Type != E->getReturnValue().Type)
        HasError = true;

      // Update return type.
      SetBangNode.setReturnValue(RetVal(RetType::Void));
      return;
    }
    HasError = true;
  }

  virtual void visit(WhileLoop &Node) override {
    auto &WhileLoopNode = llvm::cast<WhileLoop>(Node);

    // Check if condition returns boolean.
    Expr *CondExpr = WhileLoopNode.getCondExpr();
    CondExpr->accept(*this);
    if (CondExpr->getReturnValue().Type != RetType::Boolean) {
      return;
    }

    // Check body expression.
    WhileLoopNode.getBodyExpr()->accept(*this);

    // Update return type.
    WhileLoopNode.setReturnValue(RetVal(RetType::Void));
    return;
  }

  virtual void visit(Vect &Node) override {
    auto &VectorNode = llvm::cast<Vect>(Node);

    vector<RetVal> ExprTypeList;
    // Handle subexpressions.
    for (Expr *E : VectorNode.getExprList()) {
      E->accept(*this);
      RetVal TypeE = E->getReturnValue();
      if (TypeE.Type == RetType::Read || TypeE.Type == RetType::Void ||
          TypeE.Type == RetType::Unknown) {
        HasError = true;
        return;
      }
      ExprTypeList.push_back(TypeE);
    }

    // Update return type.
    VectorNode.setReturnValue(RetVal(RetType::Vector, ExprTypeList));
    return;
  }
};
} // namespace

bool Sema::semantic(AST *Tree) {
  if (!Tree)
    return false;

  // Uniquify identifiers.
  ProgramUniquify Uniquify;
  Tree->accept(Uniquify);

  // Perform semantic check.
  ProgramCheck Check;
  Tree->accept(Check);

  // Check if semantic errors exist.
  return !Check.hasError();
}
