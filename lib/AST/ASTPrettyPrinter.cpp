#include "llracket/AST/ASTPrettyPrinter.h"
#include <iostream>
#include "llvm/Support/raw_ostream.h"

std::string retTypeToString(RetType type) {
  switch (type) {
    case RetType::Integer: return "Integer";
    case RetType::Boolean: return "Boolean";
    case RetType::Void: return "Void";
    case RetType::Vector: return "Vector";
    case RetType::Func: return "Func";
    case RetType::FuncTypeDec: return "FuncTypeDec";
    case RetType::Read: return "Read";
    case RetType::Unknown: return "Unknown";
    default: return "Other";
  }
}

class ASTPrettyPrinter : public ASTVisitor {
  int indent = 0;
  void printIndent() const;
public:
  void visit(Program &Node) override;
  void visit(Def &Node) override;
  void visit(Int &Node) override;
  void visit(Bool &Node) override;
  void visit(Var &Node) override;
  void visit(Let &Node) override;
  void visit(Prim &Node) override;
  void visit(If &Node) override;
  void visit(Begin &Node) override;
  void visit(SetBang &Node) override;
  void visit(WhileLoop &Node) override;
  void visit(Vect &Node) override;
  void visit(Apply &Node) override;
};

void ASTPrettyPrinter::printIndent() const {
  for (int i = 0; i < indent; ++i) llvm::errs() << "  ";
}

void ASTPrettyPrinter::visit(Program &Node) {
  printIndent(); llvm::errs() << "Program\n";
  indent++;
  for (auto *def : Node.getFuncList()) def->accept(*this);
  if (Node.getExpr()) Node.getExpr()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(Def &Node) {
  printIndent(); llvm::errs() << "Def: " << Node.getFunctionID() << " : ";
  llvm::errs() << retTypeToString(Node.getFunctionType().Type) << "\n";
  indent++;
  for (auto *arg : Node.getArgList()) arg->accept(*this);
  Node.getFunctionBody()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(Int &Node) {
  printIndent(); llvm::errs() << "Int: " << Node.getValue().str();
  llvm::errs() << " : " << retTypeToString(Node.getReturnValue().Type) << "\n";
}
void ASTPrettyPrinter::visit(Bool &Node) {
  printIndent(); llvm::errs() << "Bool: " << Node.getValue().str();
  llvm::errs() << " : " << retTypeToString(Node.getReturnValue().Type) << "\n";
}
void ASTPrettyPrinter::visit(Var &Node) {
  printIndent(); llvm::errs() << "Var: " << Node.getValue();
  llvm::errs() << " : " << retTypeToString(Node.getReturnValue().Type) << "\n";
}
void ASTPrettyPrinter::visit(Let &Node) {
  printIndent(); llvm::errs() << "Let: " << Node.getIdent() << "\n";
  indent++;
  Node.getVarExpr()->accept(*this);
  Node.getBodyExpr()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(Prim &Node) {
  printIndent(); llvm::errs() << "Prim: op=" << Node.getOp() << " : ";
  llvm::errs() << retTypeToString(Node.getReturnValue().Type) << "\n";
  indent++;
  if (Node.getE1()) Node.getE1()->accept(*this);
  if (Node.getE2()) Node.getE2()->accept(*this);
  if (Node.getE3()) Node.getE3()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(If &Node) {
  printIndent(); llvm::errs() << "If : " << retTypeToString(Node.getReturnValue().Type) << "\n";
  indent++;
  Node.getCondExpr()->accept(*this);
  Node.getThenExpr()->accept(*this);
  Node.getElseExpr()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(Begin &Node) {
  printIndent(); llvm::errs() << "Begin\n";
  indent++;
  for (auto *e : Node.getSubExprList()) e->accept(*this);
  Node.getBodyExpr()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(SetBang &Node) {
  printIndent(); llvm::errs() << "SetBang: " << Node.getIdent() << "\n";
  indent++;
  Node.getExpr()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(WhileLoop &Node) {
  printIndent(); llvm::errs() << "WhileLoop\n";
  indent++;
  Node.getCondExpr()->accept(*this);
  Node.getBodyExpr()->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(Vect &Node) {
  printIndent(); llvm::errs() << "Vect : " << retTypeToString(Node.getReturnValue().Type) << "\n";
  indent++;
  for (auto *e : Node.getExprList()) e->accept(*this);
  indent--;
}
void ASTPrettyPrinter::visit(Apply &Node) {
  printIndent(); llvm::errs() << "Apply : " << retTypeToString(Node.getReturnValue().Type) << "\n";
  indent++;
  Node.getFunctionID()->accept(*this);
  for (auto *arg : Node.getArgList()) arg->accept(*this);
  indent--;
}

bool ASTPrint::print(AST *Tree) {
  if (!Tree) return false;
  ASTPrettyPrinter Printer;
  Tree->accept(Printer);
  return true;
}
