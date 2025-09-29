add_library(llracketLexer Lexer.cpp)

target_link_libraries(llracketLexer PRIVATE LLVMCore LLVMSupport)

target_include_directories(llracketLexer PRIVATE ${LLVM_INCLUDE_DIRS})
