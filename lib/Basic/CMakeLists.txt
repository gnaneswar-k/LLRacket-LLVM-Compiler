add_library(llracketBasic Diagnostic.cpp TokenKinds.cpp)

target_link_libraries(llracketBasic PRIVATE LLVMCore LLVMSupport)

target_include_directories(llracketBasic PRIVATE ${LLVM_INCLUDE_DIRS})
