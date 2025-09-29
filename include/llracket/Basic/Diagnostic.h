#ifndef LLRACKET_BASIC_DIAGNOSTIC_H
#define LLRACKET_BASIC_DIAGNOSTIC_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SourceMgr.h"

using llvm::formatv;
using llvm::SMLoc;
using llvm::SourceMgr;
using llvm::StringRef;

namespace llracket {

namespace diag {
enum {
#define DIAG(ID, Level, Msg) ID,
#include "llracket/Basic/Diagnostic.def"
};
} // namespace diag

class DiagnosticsEngine {
  static const char *getDiagnosticText(unsigned DiagID);
  static SourceMgr::DiagKind getDiagnosticKind(unsigned DiagID);
  SourceMgr &SrcMgr;
  unsigned NumErrors;

public:
  DiagnosticsEngine(SourceMgr &SrcMgr) : SrcMgr(SrcMgr), NumErrors(0) {}

  unsigned numErrors() { return NumErrors; }
  template <typename... Args>
  void report(SMLoc Loc, unsigned DiagID, Args &&...Arguments) {
    std::string Msg = formatv(getDiagnosticText(DiagID), Arguments...).str();
    SourceMgr::DiagKind Kind = getDiagnosticKind(DiagID);
    SrcMgr.PrintMessage(Loc, Kind, Msg);
    NumErrors += (Kind == SourceMgr::DK_Error);
  }
};
} // namespace llracket

#endif
