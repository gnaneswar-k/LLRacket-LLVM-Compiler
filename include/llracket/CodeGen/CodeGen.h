#ifndef LLRACKET_CODEGEN_CODEGEN_H
#define LLRACKET_CODEGEN_CODEGEN_H

#include "llracket/AST/AST.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

class CodeGen {
  llvm::Module *M;
  llvm::LLVMContext *Ctx;

public:
  CodeGen(llvm::Module *M, llvm::LLVMContext *Ctx) : M(M), Ctx(Ctx) {}
  void compile(AST *Tree);
};

#endif
