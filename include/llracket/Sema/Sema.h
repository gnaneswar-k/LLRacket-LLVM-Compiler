#ifndef LLRACKET_SEMA_SEMA_H
#define LLRACKET_SEMA_SEMA_H

#include "llracket/AST/AST.h"
#include "llracket/Lexer/Lexer.h"

class Sema {
public:
  bool semantic(AST *Tree);
};

#endif
