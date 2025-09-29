add_library(llracketSema Sema.cpp)

target_link_libraries(llracketSema PRIVATE LLVMCore LLVMSupport)

target_include_directories(llracketSema PRIVATE ${LLVM_INCLUDE_DIRS})
