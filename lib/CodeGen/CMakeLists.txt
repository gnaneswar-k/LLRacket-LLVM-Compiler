add_library(llracketCodeGen CodeGen.cpp)

target_link_libraries(llracketCodeGen PRIVATE LLVMCore LLVMSupport)

target_include_directories(llracketCodeGen PRIVATE ${LLVM_INCLUDE_DIRS})
