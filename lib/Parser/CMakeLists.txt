add_library(llracketParser Parser.cpp)

target_link_libraries(llracketParser PRIVATE LLVMCore LLVMSupport llracketBasic llracketLexer)

target_include_directories(llracketParser PRIVATE ${LLVM_INCLUDE_DIRS})
