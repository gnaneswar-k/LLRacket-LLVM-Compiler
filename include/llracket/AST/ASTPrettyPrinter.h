#ifndef LLRACKET_AST_ASTPRETTYPRINTER_H
#define LLRACKET_AST_ASTPRETTYPRINTER_H

#include "llracket/AST/AST.h"
#include <string>

std::string retTypeToString(RetType type);

class ASTPrint {
public:
  bool print(AST *Tree);
};

#endif
