#include "LLRacket.h"
#include "llracket/Basic/Diagnostic.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/SourceMgr.h>

static llvm::cl::opt<std::string> Input(llvm::cl::Positional,
                                        llvm::cl::desc("<input file>"),
                                        llvm::cl::Required);
static llvm::cl::opt<std::string> Output("o", llvm::cl::desc("Output file"),
                                         llvm::cl::value_desc("filename"),
                                         llvm::cl::init("a.ll"));

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::cl::ParseCommandLineOptions(argc_, argv_, "LLRacket compiler\n");

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
      llvm::MemoryBuffer::getFile(Input);

  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::errs() << "Error reading " << Input << ": " << BufferError.message()
                 << "\n";
    return 1;
  }

  // Source manager class to manage source buffers
  llvm::SourceMgr *SrcMgr = new llvm::SourceMgr();
  DiagnosticsEngine Diags(*SrcMgr);

  LLRacket Compiler(SrcMgr, Diags);
  Compiler.getSourceMgr()->AddNewSourceBuffer(std::move(*FileOrErr),
                                              llvm::SMLoc());

  return Compiler.exec();
}

int LLRacket::exec() {
  // Parse the program to AST
  Lexer Lex(*SrcMgr, Diags);
  Parser P(Lex, Diags);
  AST *Tree = P.parse();
  if (!Tree || Diags.numErrors()) {
    llvm::errs() << "Syntax error\n";
    return 1;
  }

  // Semantic analysis
  Sema S;
  if (!S.semantic(Tree)) {
    llvm::errs() << "Semantic error\n";
    return 2;
  }

  // Compile to LLVM IR
  CodeGen CG(Module.get(), Ctx.get());
  CG.compile(Tree);

  Module->print(llvm::outs(), nullptr);
  // save to a .ll file
  saveModuleToFile(Output);
  return 0;
}
