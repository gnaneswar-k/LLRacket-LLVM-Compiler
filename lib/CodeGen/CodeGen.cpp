#include "llracket/CodeGen/CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include <map>

using namespace llvm;
using std::string;

namespace {
/**
 * @brief Visitor to convert AST expressions to monadic form.
 */
class ProgramToMonadic : public ASTVisitor {
  int Counter;

public:
  ProgramToMonadic() : Counter(0) {}

  // Function to create unique temp identifier string.
  string newIdent() { return "tmp." + std::to_string(Counter++); }

  /**
   * @brief Function to check if expression is atomic.
   *
   * @param E Expression which is to be checked for atomicity.
   *
   * @return `true` if the expression is atomic, else `false`.
   *
   * @note Var is not considered atomic here even though it is according to the
   * grammar.
   *
   * This is to allow duplication of variables so that they won't be affected by
   * later set! procedures.
   */
  bool isAtomic(Expr *E) {
    Expr::ExprKind ExpType = E->getKind();
    return ExpType == Expr::ExprInt || ExpType == Expr::ExprBool;
  }

  virtual void visit(Program &Node) override {
    Expr *ExprNode = Node.getExpr();
    if (ExprNode) {
      // Save modified monadic expression.
      Node.setExpr(modifyExpr(ExprNode));
    }
  }

  virtual void visit(Int &Node) override {
    // No need to modify integer literals.
    return;
  }

  virtual void visit(Bool &Node) override {
    // No need to modify booleans.
    return;
  }

  virtual void visit(Var &Node) override {
    // No need to modify variables.
    return;
  }

  Expr *modifyExpr(Expr *Node) {
    if (llvm::isa<Prim>(Node)) {
      // Return modified Prim expression.
      return modifyPrim(*llvm::cast<Prim>(Node));
    }
    // No change for Int, Bool, Var, If, Begin, SetBang, WhileLoop, Vector.
    return Node;
  }

  virtual void visit(Let &Node) override {
    // No need to modify Let expression.
    return;
  }

  Expr *modifyPrim(Prim &Node) {
    // If read, return as is.
    if (Node.getOp() == tok::read) {
      return &Node;
    }

    Expr *E1 = Node.getE1();
    Expr *NewE1;
    string Ident1;
    Var *NewVarE1;
    bool atomicE1 = false;
    if (E1 != NULL)
      atomicE1 = isAtomic(E1);
    if (E1 != NULL && !atomicE1) {
      // Update E1.
      NewE1 = modifyExpr(E1);
      // Create new temp identifier for E1.
      Ident1 = newIdent();
      // Create new Var expression for E1.
      NewVarE1 = new Var(Ident1);
      NewVarE1->setReturnValue(NewE1->getReturnValue());
    }

    Expr *E2 = Node.getE2();
    Expr *NewE2;
    string Ident2;
    Var *NewVarE2;
    bool atomicE2 = false;
    if (E2 != NULL)
      atomicE2 = isAtomic(E2);
    if (E2 != NULL && !atomicE2) {
      // Update E2.
      NewE2 = modifyExpr(E2);
      // Create new temp identifier for E2.
      Ident2 = newIdent();
      // Create new Var expression for E2.
      NewVarE2 = new Var(Ident2);
      NewVarE2->setReturnValue(NewE2->getReturnValue());
    }

    Expr *E3 = Node.getE3();
    Expr *NewE3;
    string Ident3;
    Var *NewVarE3;
    bool atomicE3 = false;
    if (E3 != NULL)
      atomicE3 = isAtomic(E3);
    if (E3 != NULL && !atomicE3) {
      // Update E3.
      NewE3 = modifyExpr(E3);
      // Create new temp identifier for E3.
      Ident3 = newIdent();
      // Create new Var expression for E1.
      NewVarE3 = new Var(Ident3);
      NewVarE3->setReturnValue(NewE3->getReturnValue());
    }

    TokenKind Op = Node.getOp();

    // Unary op.
    if (E2 == NULL && E3 == NULL) {
      // Return as is if unop has atomic expression.
      if (atomicE1)
        return &Node;

      // Initialise new Prim expression.
      Prim *NewPrim = new Prim(Node.getOp(), NewVarE1);
      // Set return type for NewPrim.
      NewPrim->setReturnValue(Node.getReturnValue());

      // Return new Let expression with modified E1.
      Let *NewLet = new Let(Ident1, NewE1, NewPrim);
      NewLet->setReturnValue(NewPrim->getReturnValue());
      return NewLet;
    }
    // Binary op.
    else if (E3 == NULL) {
      // If E1 is atomic.
      if (atomicE1) {
        // Return as is if binop has only atomic expressions.
        if (atomicE2)
          return &Node;

        // Initialise new Prim expression.
        Prim *NewPrim = new Prim(Op, E1, NewVarE2);
        // Set return type for NewPrim.
        NewPrim->setReturnValue(Node.getReturnValue());

        // Return new Let expression with atomic E1 and modified E2.
        Let *NewLet = new Let(Ident2, NewE2, NewPrim);
        NewLet->setReturnValue(NewPrim->getReturnValue());
        return NewLet;
      }
      // If E1 is not atomic.
      else {
        // If E2 is atomic.
        if (atomicE2) {
          // Initialise new Prim expression.
          Prim *NewPrim = new Prim(Op, NewVarE1, E2);
          // Set return type for NewPrim.
          NewPrim->setReturnValue(Node.getReturnValue());

          // Return new Let expression with modified E1 and atomic E2.
          Let *NewLet = new Let(Ident1, NewE1, NewPrim);
          NewLet->setReturnValue(NewPrim->getReturnValue());
          return NewLet;
        }

        // Initialise new Prim expression.
        Prim *NewPrim = new Prim(Op, NewVarE1, NewVarE2);
        // Set return type for NewPrim.
        NewPrim->setReturnValue(Node.getReturnValue());

        // Create Let for E2.
        Let *LetE2 = new Let(Ident2, NewE2, NewPrim);
        LetE2->setReturnValue(NewPrim->getReturnValue());

        // Return new Let expression with modified E1 and E2.
        Let *NewLet = new Let(Ident1, NewE1, LetE2);
        NewLet->setReturnValue(LetE2->getReturnValue());
        return NewLet;
      }
    }
    // Ternary op.
    else {
      // If E1 and E2 are atomic.
      if (atomicE1 && atomicE2) {
        // If E3 is atomic.
        if (atomicE3)
          return &Node;

        // Initialise new Prim expression.
        Prim *NewPrim = new Prim(Op, E1, E2, NewVarE3);
        // Set return type for NewPrim.
        NewPrim->setReturnValue(Node.getReturnValue());

        // Return new Let expression with atomic E1, atomic E2 and modified E3.
        Let *NewLet = new Let(Ident3, NewE3, NewPrim);
        NewLet->setReturnValue(NewPrim->getReturnValue());
        return NewLet;
      }
      // If E1 is atomic but not E2.
      else if (atomicE1 && !atomicE2) {
        // If E3 is atomic.
        if (atomicE3) {
          // Initialise new Prim expression.
          Prim *NewPrim = new Prim(Op, E1, NewVarE2, E3);
          // Set return type for NewPrim.
          NewPrim->setReturnValue(Node.getReturnValue());

          // Return new Let expression with
          // atomic E1, modified E2 and atomic E3.
          Let *NewLet = new Let(Ident2, NewE2, NewPrim);
          NewLet->setReturnValue(NewPrim->getReturnValue());
          return NewLet;
        }

        // Initialise new Prim expression.
        Prim *NewPrim = new Prim(Op, E1, NewVarE2, NewVarE3);
        // Set return type for NewPrim.
        NewPrim->setReturnValue(Node.getReturnValue());

        // Create Let for E3.
        Let *LetE3 = new Let(Ident3, NewE3, NewPrim);
        LetE3->setReturnValue(NewPrim->getReturnValue());

        // Return new Let expression with atomic E1, modified E2 and E3.
        Let *NewLet = new Let(Ident2, NewE2, LetE3);
        NewLet->setReturnValue(LetE3->getReturnValue());
        return NewLet;
      }
      // If E2 is atomic but not E1.
      else if (!atomicE1 && atomicE2) {
        // If E3 is atomic.
        if (atomicE3) {
          // Initialise new Prim expression.
          Prim *NewPrim = new Prim(Op, NewVarE1, E2, E3);
          // Set return type for NewPrim.
          NewPrim->setReturnValue(Node.getReturnValue());

          // Return new Let expression with
          // atomic E1, modified E2, and atomic E3.
          Let *NewLet = new Let(Ident1, NewE1, NewPrim);
          NewLet->setReturnValue(NewPrim->getReturnValue());
          return NewLet;
        }

        // Initialise new Prim expression.
        Prim *NewPrim = new Prim(Op, NewVarE1, E2, NewVarE3);
        // Set return type for NewPrim.
        NewPrim->setReturnValue(Node.getReturnValue());

        // Create Let for E3.
        Let *LetE3 = new Let(Ident3, NewE3, NewPrim);
        LetE3->setReturnValue(NewPrim->getReturnValue());

        // Return new Let expression with
        // modified E1, atomic E2, and modified E3.
        Let *NewLet = new Let(Ident1, NewE1, LetE3);
        NewLet->setReturnValue(LetE3->getReturnValue());
        return NewLet;
      }
      // If E1 and E2 are not atomic.
      else {
        // If E3 is atomic.
        if (atomicE3) {
          // Initialise new Prim expression.
          Prim *NewPrim = new Prim(Op, NewVarE1, NewVarE2, E3);
          // Set return type for NewPrim.
          NewPrim->setReturnValue(Node.getReturnValue());

          // Create Let for E2.
          Let *LetE2 = new Let(Ident2, NewE2, NewPrim);
          LetE2->setReturnValue(NewPrim->getReturnValue());

          // Return new Let expression with
          // modified E1 and E2, and atomic E3.
          Let *NewLet = new Let(Ident1, NewE1, LetE2);
          NewLet->setReturnValue(NewPrim->getReturnValue());
          return NewLet;
        }

        // Initialise new Prim expression.
        Prim *NewPrim = new Prim(Op, NewVarE1, NewVarE2, NewVarE3);
        // Set return type for NewPrim.
        NewPrim->setReturnValue(Node.getReturnValue());

        // Create Let for E3.
        Let *LetE3 = new Let(Ident3, NewE3, NewPrim);
        LetE3->setReturnValue(NewPrim->getReturnValue());

        // Create Let for E3.
        Let *LetE2 = new Let(Ident2, NewE2, LetE3);
        LetE2->setReturnValue(LetE3->getReturnValue());

        // Return new Let expression with modified E1, E2 and E3.
        Let *NewLet = new Let(Ident1, NewE1, LetE2);
        NewLet->setReturnValue(LetE2->getReturnValue());
        return NewLet;
      }
    }
  }

  virtual void visit(If &Node) override {
    // No need to modify If expression.
    return;
  }

  virtual void visit(Begin &Node) override {
    // No need to modify Begin expression.
    return;
  }

  virtual void visit(SetBang &Node) override {
    // No need to modify SetBang expression.
    return;
  }

  virtual void visit(WhileLoop &Node) override {
    // No need to modify WhileLoop expression.
    return;
  }

  virtual void visit(Vect &Node) override {
    // No need to modify Vect expression.
    return;
  }
};

/**
 * @brief Visitor to convert AST to LLVM IR.
 */
class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int32Ty;
  Type *Int1Ty;
  PointerType *PtrTy;
  Constant *Int32Zero;
  Value *V;
  StringMap<AllocaInst *> NameMap;
  std::unordered_map<AllocaInst *, StructType *> TupleAllocaStructMap;
  std::map<std::pair<AllocaInst *, unsigned>, AllocaInst *>
      TupleElementAllocaMap;
  RetType RetValType;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    Int1Ty = Type::getInt1Ty(M->getContext());
    PtrTy = PointerType::getUnqual(M->getContext());
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  void run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
    Builder.SetInsertPoint(BB);
    Tree->accept(*this);

    // Call the appropriate write function.
    FunctionType *WriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    if (RetValType == RetType::Integer) {
      // Function to write integer.
      Function *WriteIntFn = M->getFunction("write_int");
      if (!WriteIntFn) {
        WriteIntFn = Function::Create(WriteFnTy, GlobalValue::ExternalLinkage,
                                      "write_int", M);
      }
      Builder.CreateCall(WriteIntFn, {V});
    } else if (RetValType == RetType::Boolean) {
      // Function to write boolean.
      Function *WriteBoolFn = M->getFunction("write_bool");
      if (!WriteBoolFn) {
        WriteBoolFn = Function::Create(WriteFnTy, GlobalValue::ExternalLinkage,
                                       "write_bool", M);
      }
      Builder.CreateCall(WriteBoolFn, {V});
    }

    Builder.CreateRet(Int32Zero);
  }

  virtual void visit(Program &Node) override {
    Node.getExpr()->accept(*this);
    RetValType = Node.getExpr()->getReturnValue().Type;
  };

  virtual void visit(Expr &Node) override {
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
    if (llvm::isa<Prim>(Node)) {
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
    if (llvm::isa<Begin>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<WhileLoop>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<SetBang>(Node)) {
      Node.accept(*this);
      return;
    }
  };

  virtual void visit(Int &Node) override {
    int Intval;
    Node.getValue().getAsInteger(10, Intval);
    V = ConstantInt::get(Int32Ty, Intval, true);
  };

  virtual void visit(Bool &Node) override {
    bool Boolval;
    string Val = Node.getValue().str();
    if (Val == "#t")
      Boolval = true;
    else
      Boolval = false;
    V = ConstantInt::get(Int1Ty, Boolval, false);
  };

  virtual void visit(Var &Node) override {
    string Ident = Node.getValue();
    // Get allocated space.
    AllocaInst *VarAlloca = NameMap[Ident];
    // Load the value of the variable.
    if (Node.getReturnValue().Type == RetType::Vector)
      V = VarAlloca;
    else
      V = Builder.CreateLoad(VarAlloca->getAllocatedType(), VarAlloca, Ident);
  };

  virtual void visit(Prim &Node) override {
    if (Node.getOp() == tok::read) {
      Function *ReadFn;
      if ((ReadFn = M->getFunction("read_value")) == nullptr) {
        FunctionType *ReadFty = FunctionType::get(Int32Ty, {Int32Ty}, false);
        ReadFn = Function::Create(ReadFty, GlobalValue::ExternalLinkage,
                                  "read_value", M);
      }
      // 0 for int, 1 for bool.
      int ReadType = (Node.getReturnValue().Type == RetType::Boolean) ? 1 : 0;
      Value *ReadTypeConst = ConstantInt::get(Int32Ty, ReadType);
      Value *ReadVal = Builder.CreateCall(ReadFn, {ReadTypeConst}, "int_val");

      // If input is boolean.
      if (ReadType == 1) {
        V = Builder.CreateTrunc(ReadVal, Type::getInt1Ty(M->getContext()),
                                "bool_val");
      }
      // If input is an integer.
      else
        V = ReadVal;
      return;
    }

    if (Node.getOp() == tok::minus) {
      if (Node.getE1() and !Node.getE2()) {
        Node.getE1()->accept(*this);
        V = Builder.CreateNSWNeg(V);
        return;
      }
    }

    if (Node.getOp() == tok::op_not) {
      Node.getE1()->accept(*this);
      V = Builder.CreateNot(V);
      return;
    }

    if (Node.getOp() == tok::kw_vector_length) {
      Node.getE1()->accept(*this);
      AllocaInst *TupleAlloca = llvm::cast<AllocaInst>(V);
      StructType *TupleTy = TupleAllocaStructMap[TupleAlloca];
      int Length = TupleTy->getNumElements();
      V = ConstantInt::get(Int32Ty, Length, true);
    }

    if (Node.getOp() == tok::kw_vector_ref) {
      // Get the tuple.
      Expr *E1 = Node.getE1();
      E1->accept(*this);
      Value *Tuple = V;

      // Get pointer to allocated space.
      AllocaInst *TupleAlloca = llvm::cast<AllocaInst>(Tuple);
      // Get the type of elements in the tuple.
      StructType *TupleTy = TupleAllocaStructMap[TupleAlloca];

      // Get the index.
      Node.getE2()->accept(*this);
      Value *Index = V;
      unsigned ConstantIndex = llvm::cast<ConstantInt>(Index)->getZExtValue();

      // Get pointer to the element being read.
      Value *RefPtr = Builder.CreateStructGEP(TupleTy, TupleAlloca,
                                              ConstantIndex, "tpl_elem_ptr");
      // Get element type.
      Type *ElemTy = TupleTy->getElementType(ConstantIndex);

      if (ElemTy->isPointerTy()) {
        // Return the AllocaInst of the stored tuple.
        V = TupleElementAllocaMap[{TupleAlloca, ConstantIndex}];
      } else {
        // Load the element.
        V = Builder.CreateLoad(ElemTy, RefPtr, "tpl_elem");
      }
    }

    if (Node.getOp() == tok::kw_vector_set_bang) {
      // Get the tuple.
      Expr *E1 = Node.getE1();
      E1->accept(*this);
      Value *Tuple = V;

      // Get pointer to allocated space.
      AllocaInst *TupleAlloca = llvm::cast<AllocaInst>(Tuple);
      // Get the type of elements in the tuple.
      StructType *TupleTy = TupleAllocaStructMap[TupleAlloca];

      // Get the index.
      Node.getE2()->accept(*this);
      Value *Index = V;
      unsigned ConstantIndex = llvm::cast<ConstantInt>(Index)->getZExtValue();

      // Get pointer to the element being replaced.
      Value *RefPtr = Builder.CreateStructGEP(TupleTy, TupleAlloca,
                                              ConstantIndex, "tpl_elem_ptr");

      // Get the new element to be stored.
      Node.getE3()->accept(*this);
      Value *Elem = V;

      // Store the new element.
      Builder.CreateStore(Elem, RefPtr);
      // If the element is a tuple, update the map.
      if (llvm::isa<AllocaInst>(Elem)) {
        TupleElementAllocaMap[{TupleAlloca, ConstantIndex}] =
            llvm::cast<AllocaInst>(Elem);
      }

      V = nullptr;
    }

    if (Node.getOp() == tok::plus || Node.getOp() == tok::minus ||
        Node.getOp() == tok::op_and || Node.getOp() == tok::op_or ||
        Node.getOp() == tok::cmp_eq || Node.getOp() == tok::cmp_less ||
        Node.getOp() == tok::cmp_less_eq || Node.getOp() == tok::cmp_great ||
        Node.getOp() == tok::cmp_great_eq) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      switch (Node.getOp()) {
#define CASE(op, builder_val)                                                  \
  case op:                                                                     \
    V = builder_val;                                                           \
    break;

        CASE(tok::plus, Builder.CreateNSWAdd(E1, E2));
        CASE(tok::minus, Builder.CreateNSWSub(E1, E2));
        CASE(tok::op_and, Builder.CreateAnd(E1, E2));
        CASE(tok::op_or, Builder.CreateOr(E1, E2));
        CASE(tok::cmp_eq, Builder.CreateICmpEQ(E1, E2));
        CASE(tok::cmp_less, Builder.CreateICmpSLT(E1, E2));
        CASE(tok::cmp_less_eq, Builder.CreateICmpSLE(E1, E2));
        CASE(tok::cmp_great, Builder.CreateICmpSGT(E1, E2));
        CASE(tok::cmp_great_eq, Builder.CreateICmpSGE(E1, E2));

      default:
        break;
#undef CASE
      }
      return;
    }
  };

  virtual void visit(Let &Node) override {
    // Evaluate the expression to be assigned.
    Expr *VarExpr = Node.getVarExpr();
    VarExpr->accept(*this);
    Value *EV = V;
    RetVal RV = VarExpr->getReturnValue();

    string Ident = Node.getIdent();

    // For vector.
    if (RV.Type == RetType::Vector) {
      // Get the pointer to the allocated space.
      AllocaInst *TupleAlloca = llvm::cast<AllocaInst>(EV);
      // Save the allocated space.
      NameMap[Ident] = TupleAlloca;
    }
    // For Integer and Boolean.
    else if (RV.Type != RetType::Void) {
      Type *VarType = (RV.Type == RetType::Boolean) ? Int1Ty : Int32Ty;
      // Allocate space.
      AllocaInst *VarAlloca = Builder.CreateAlloca(VarType, nullptr, Ident);
      // Create store instruction.
      Builder.CreateStore(EV, VarAlloca);
      // Store the allocated space for the variable.
      NameMap[Ident] = VarAlloca;
    }

    // Evaluate the body expression.
    Node.getBodyExpr()->accept(*this);

    // Remove the variable from scope.
    if (RV.Type != RetType::Void)
      NameMap.erase(Ident);
    return;
  };

  virtual void visit(If &Node) override {
    // Get current function.
    Function *CurrentFn = Builder.GetInsertBlock()->getParent();
    // Define Basic Blocks.
    BasicBlock *IfThenBB =
        BasicBlock::Create(M->getContext(), "if.then", CurrentFn);
    BasicBlock *IfElseBB =
        BasicBlock::Create(M->getContext(), "if.else", CurrentFn);
    BasicBlock *IfDoneBB =
        BasicBlock::Create(M->getContext(), "if.done", CurrentFn);

    // Evaluate Condition.
    Node.getCondExpr()->accept(*this);
    Value *Cond = V;
    // Create Branch.
    Builder.CreateCondBr(Cond, IfThenBB, IfElseBB);

    // Set entry point for the Then Basic Block.
    Builder.SetInsertPoint(IfThenBB);
    // Evaluate the Then expression and save the result.
    Node.getThenExpr()->accept(*this);
    Value *ThenValue = V;
    // Create branch to after the Then Basic Block.
    Builder.CreateBr(IfDoneBB);
    // Update Basic Block in case it changed during nested evaluation.
    IfThenBB = Builder.GetInsertBlock();

    // Set entry point for the Else Basic Block.
    Builder.SetInsertPoint(IfElseBB);
    // Evaluate the Else expression and save the result.
    Node.getElseExpr()->accept(*this);
    Value *ElseValue = V;
    // Create branch to after the Else Basic Block.
    Builder.CreateBr(IfDoneBB);
    // Update Basic Block in case it changed during nested evaluation.
    IfElseBB = Builder.GetInsertBlock();

    // Set entry point for the Basic Block after the Then or Else Basic Blocks.
    Builder.SetInsertPoint(IfDoneBB);
    // Create Phi Node if the Then and Else Basic Blocks
    // return an integer or a boolean.
    if (Node.getReturnValue().Type == RetType::Boolean ||
        Node.getReturnValue().Type == RetType::Integer) {
      // Set up Phi Node.
      PHINode *Phi;
      if (Node.getReturnValue().Type == RetType::Boolean)
        Phi = Builder.CreatePHI(Int1Ty, 2, "if.merge_bool");
      else // if (Node.getReturnType() == RetType::Integer)
        Phi = Builder.CreatePHI(Int32Ty, 2, "if.merge_int");
      Phi->addIncoming(ThenValue, IfThenBB);
      Phi->addIncoming(ElseValue, IfElseBB);

      V = Phi;
    }
    // If Then and Else Basic Blocks return Void.
    else
      V = nullptr;
  };

  virtual void visit(Begin &Node) override {
    for (Expr *E : Node.getSubExprList())
      E->accept(*this);

    Node.getBodyExpr()->accept(*this);
    return;
  }

  virtual void visit(SetBang &Node) override {
    // Evaluate expression to be assigned.
    Node.getExpr()->accept(*this);
    Value *EV = V;

    string Ident = Node.getIdent();
    // Get existing allocated space.
    AllocaInst *VarAlloca = NameMap[Ident];
    // Store the new value.
    Builder.CreateStore(EV, VarAlloca);

    V = nullptr;
  }

  virtual void visit(WhileLoop &Node) override {
    // Get current function.
    Function *CurrentFn = Builder.GetInsertBlock()->getParent();
    // Define Basic Blocks.
    BasicBlock *WhileCondBB =
        BasicBlock::Create(M->getContext(), "while.cond", CurrentFn);
    BasicBlock *WhileBodyBB =
        BasicBlock::Create(M->getContext(), "while.body", CurrentFn);
    BasicBlock *WhileDoneBB =
        BasicBlock::Create(M->getContext(), "while.done", CurrentFn);

    // Create branch to the Cond Basic Block.
    Builder.CreateBr(WhileCondBB);

    // Set entry point for the Cond Basic Block.
    Builder.SetInsertPoint(WhileCondBB);
    // Evaluate the Cond expression and save the result.
    Node.getCondExpr()->accept(*this);
    Value *CondValue = V;
    // Create branch to after the Cond Basic Block.
    Builder.CreateCondBr(CondValue, WhileBodyBB, WhileDoneBB);
    // Update Basic Block in case it changed during nested evaluation.
    WhileCondBB = Builder.GetInsertBlock();

    // Set entry point for the Body Basic Block.
    Builder.SetInsertPoint(WhileBodyBB);
    // Evaluate the Body expression.
    Node.getBodyExpr()->accept(*this);
    // Create branch to the Cond Basic Block.
    Builder.CreateBr(WhileCondBB);
    // Update Basic Block in case it changed during nested evaluation.
    WhileBodyBB = Builder.GetInsertBlock();

    // Set entry point for the Basic Block after exiting WhileLoop.
    Builder.SetInsertPoint(WhileDoneBB);

    V = nullptr;
  }

  virtual void visit(Vect &Node) override {
    vector<Value *> TupleElementValues;
    vector<Type *> TupleElementTypes;

    for (Expr *E : Node.getExprList()) {
      // Evaluate the expression.
      E->accept(*this);
      // Store the value and its type.
      TupleElementValues.push_back(V);
      TupleElementTypes.push_back(V->getType());
    }

    // Create struct to hold the types of elements in the tuple.
    StructType *TupleType =
        StructType::get(M->getContext(), TupleElementTypes, false);

    // Allocate space for the tuple using the struct containing the types.
    AllocaInst *TupleAlloca = Builder.CreateAlloca(TupleType, nullptr, "tuple");
    // Save for global access.
    TupleAllocaStructMap[TupleAlloca] = TupleType;

    for (unsigned i = 0; i < TupleElementValues.size(); i++) {
      // Get the allocated space corresponding to the element.
      Value *GEP = Builder.CreateStructGEP(TupleType, TupleAlloca, i);
      // Store the value of the element into the allocated space.
      Builder.CreateStore(TupleElementValues[i], GEP);
      // Store the alloca if the element is a tuple.
      if (llvm::isa<AllocaInst>(TupleElementValues[i])) {
        TupleElementAllocaMap[{TupleAlloca, i}] =
            llvm::cast<AllocaInst>(TupleElementValues[i]);
      }
    }

    V = TupleAlloca;
  }
};
}; // namespace

void CodeGen::compile(AST *Tree) {
  // Convert Tree to Monadic Tree.
  ProgramToMonadic ToMonadic;
  Tree->accept(ToMonadic);

  // Convert to IR.
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}
