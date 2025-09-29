#ifndef RACKETLLVM_H
#define RACKETLLVM_H

#include "llracket/Basic/Diagnostic.h"
#include "llracket/CodeGen/CodeGen.h"
#include "llracket/Lexer/Lexer.h"
#include "llracket/Parser/Parser.h"
#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <system_error>

class LLRacket {
  SourceMgr *SrcMgr;
  DiagnosticsEngine Diags;

public:
  LLRacket(SourceMgr *SrcMgr, DiagnosticsEngine &Diags)
      : SrcMgr(SrcMgr), Diags(Diags) {
    moduleInit();
  };

  SourceMgr *getSourceMgr() { return SrcMgr; }

  int exec();

private:
  std::unique_ptr<llvm::LLVMContext> Ctx;
  std::unique_ptr<llvm::Module> Module;
  std::unique_ptr<llvm::IRBuilder<>> Builder;

  void moduleInit() {
    Ctx = std::make_unique<llvm::LLVMContext>();
    Module = std::make_unique<llvm::Module>("RacketLLVM", *Ctx);
  }

  void saveModuleToFile(llvm::StringRef FileName) {
    std::error_code ErrorCode;
    llvm::raw_fd_ostream OutLl(FileName, ErrorCode);
    Module->print(OutLl, nullptr);
  }
};
#endif
