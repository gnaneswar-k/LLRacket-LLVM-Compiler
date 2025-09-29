set(LLVM_LINK_COMPONENTS Support)

add_llvm_executable(llracket LLRacket.cpp)

set_target_properties(llracket PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                          ${CMAKE_BINARY_DIR}/bin)

target_link_libraries(
  llracket PRIVATE llracketBasic llracketLexer llracketParser llracketSema
                   llracketCodeGen)
