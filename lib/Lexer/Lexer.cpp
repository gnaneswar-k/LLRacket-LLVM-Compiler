#include "llracket/Lexer/Lexer.h"

namespace charinfo {
LLVM_READNONE inline static bool isWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r' ||
         c == '\n';
}
LLVM_READNONE inline static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}
LLVM_READNONE inline static bool isLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

LLVM_READNONE inline static bool isAlphanumeric(char c) {
  return isLetter(c) || isDigit(c);
}

LLVM_READNONE inline static bool isAlphanumeric_(char c) {
  return isAlphanumeric(c) || c == '_';
}

LLVM_READNONE inline static bool isComp(char c) {
  return c == '<' || c == '=' || c == '>';
}
} // namespace charinfo

void Lexer::next(Token &token) {
  while (*BufferPtr && charinfo::isWhitespace(*BufferPtr))
    ++BufferPtr;

  if (!*BufferPtr) {
    token.Kind = TokenKind::eof;
    return;
  }

  if (charinfo::isDigit(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isDigit(*End))
      ++End;
    formToken(token, End, TokenKind::integer_literal);
    return;
  }

  if (*BufferPtr == '#') {
    const char *End = BufferPtr + 1;
    while (charinfo::isLetter(*End))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    if ((Text == "#t") || (Text == "#f")) {
      formToken(token, End, TokenKind::boolean);
      return;
    }
    formToken(token, End, TokenKind::unknown);
    return;
  }

  if (charinfo::isComp(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isComp(*End))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    if (Text == "<") {
      formToken(token, End, TokenKind::cmp_less);
      return;
    }
    if (Text == "<=") {
      formToken(token, End, TokenKind::cmp_less_eq);
      return;
    }
    if (Text == ">") {
      formToken(token, End, TokenKind::cmp_great);
      return;
    }
    if (Text == ">=") {
      formToken(token, End, TokenKind::cmp_great_eq);
      return;
    }
    formToken(token, End, TokenKind::unknown);
    return;
  }

  if (*BufferPtr == 'e') {
    const char *End = BufferPtr + 1;
    while (charinfo::isAlphanumeric(*End) || (*End == '?'))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    if (Text == "eq?") {
      formToken(token, End, TokenKind::cmp_eq);
      return;
    }
  }

  if (*BufferPtr == 's') {
    const char *End = BufferPtr + 1;
    while (charinfo::isAlphanumeric(*End) || (*End == '!'))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    if (Text == "set!") {
      formToken(token, End, TokenKind::kw_set_bang);
      return;
    }
  }

  if (*BufferPtr == 'v') {
    const char *End = BufferPtr + 1;
    while (charinfo::isAlphanumeric(*End) || (*End == '!') || (*End == '-'))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    if (Text == "vector") {
      formToken(token, End, TokenKind::kw_vector);
      return;
    }
    if (Text == "vector-length") {
      formToken(token, End, TokenKind::kw_vector_length);
      return;
    }
    if (Text == "vector-ref") {
      formToken(token, End, TokenKind::kw_vector_ref);
      return;
    }
    if (Text == "vector-set!") {
      formToken(token, End, TokenKind::kw_vector_set_bang);
      return;
    }
  }

  if (*BufferPtr == '_') {
    formToken(token, BufferPtr + 1, TokenKind::identifier);
    return;
  }

  if (charinfo::isLetter(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isAlphanumeric_(*End))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    if (Text == "read") {
      formToken(token, End, TokenKind::read);
      return;
    }
    if (Text == "let") {
      formToken(token, End, TokenKind::kw_let);
      return;
    }
    if (Text == "if") {
      formToken(token, End, TokenKind::kw_if_cond);
      return;
    }
    if (Text == "and") {
      formToken(token, End, TokenKind::op_and);
      return;
    }
    if (Text == "or") {
      formToken(token, End, TokenKind::op_or);
      return;
    }
    if (Text == "not") {
      formToken(token, End, TokenKind::op_not);
      return;
    }
    if (Text == "begin") {
      formToken(token, End, TokenKind::kw_begin);
      return;
    }
    if (Text == "while") {
      formToken(token, End, TokenKind::kw_while_loop);
      return;
    }
    formToken(token, End, TokenKind::identifier);
    return;
  }

  switch (*BufferPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(token, BufferPtr + 1, TokenKind::tok);                           \
    break;

    CASE('+', plus);
    CASE('-', minus);
    CASE('(', l_paren);
    CASE(')', r_paren);
    CASE('[', l_square);
    CASE(']', r_square);
#undef CASE

  default:
    Diags.report(getLoc(), diag::err_unknown_token, *BufferPtr);
    formToken(token, BufferPtr + 1, TokenKind::unknown);
    break;
  }
  return;
}

void Lexer::formToken(Token &Tok, const char *TokEnd, TokenKind Kind) {
  Tok.Kind = Kind;
  Tok.Text = StringRef(BufferPtr, TokEnd - BufferPtr);
  BufferPtr = TokEnd;
}
