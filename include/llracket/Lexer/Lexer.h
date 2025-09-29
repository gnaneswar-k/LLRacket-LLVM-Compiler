#ifndef LLRACKET_LEXER_LEXER_H
#define LLRACKET_LEXER_LEXER_H

#include "llracket/Basic/Diagnostic.h"
#include "llracket/Lexer/Token.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

using llvm::StringRef;
using namespace llracket;

class Lexer {
  unsigned CurrBuffer = 0;
  StringRef Buffer;
  const char *BufferPtr;

  llvm::SourceMgr &SrcMgr;

  // Might want it for future use
  // KeywordFilter kwFilter;

  DiagnosticsEngine Diags;

public:
  Lexer(llvm::SourceMgr &SrcMgr, DiagnosticsEngine &Diags)
      : SrcMgr(SrcMgr), Diags(Diags) {
    CurrBuffer = SrcMgr.getMainFileID();
    Buffer = SrcMgr.getMemoryBuffer(CurrBuffer)->getBuffer();
    BufferPtr = Buffer.begin();
  }

  DiagnosticsEngine &getDiagnostics() { return Diags; }

  void next(Token &Tok);

  // Might be useful
  Token peek(unsigned N = 0);

private:
  void formToken(Token &Result, const char *TokEnd, TokenKind Kind);

  SMLoc getLoc() { return SMLoc::getFromPointer(BufferPtr); }
};

#endif
