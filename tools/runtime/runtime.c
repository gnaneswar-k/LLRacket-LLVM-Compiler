#include "runtime.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>


/**
 * @brief Writes an integer value to standard output (stdout).
 * 
 * @param v     The integer value to write.
 * @return      void
 *
 */
void write_int(int v) { 
  printf("%d\n", v);
}



/**
 * @brief Writes a boolean value to standard output (stdout).
 * 
 * @param v     If 0, writes a "#f". If non-zero, writes a "#t".
 * @return      void 
 *
 */
void write_bool(int v) { 
  if (v == 0){
    printf("#f\n");
  } else {
    printf("#t\n");
  }
}

/**
 * @brief Reads a boolean or integer token from standard input (stdin).
 * 
 * @param type  If 0, reads an integer. If 1, reads a boolean (`#t` or `#f`).
 * @return      On success:
 * 
 *              - For type 0: The integer value read.
 * 
 *              - For type 1: 1 for `#t` (true), 0 for `#f` (false).
 *              On failure: -1 (invalid input, wrong type, or EOF).
 * 
 * @note Input tokens must be separated by whitespace (e.g., "#t 123 #f").
 * 
 */
int read_value(int type) {
  char token[11];

  if (scanf("%10s", token) != 1) {
    printf("Error: read failed\n");
    exit(1);
  }

  if (type == 0){
    int val;
    if(sscanf(token, "%d", &val) != 1) {
      printf("Error: read_int failed\n");
      exit(1);
    }
    return val;
  }
  if (type == 1){
    if (strncmp(token, "#t", 2) == 0) {
      return 1;
    } 
    if (strncmp(token, "#f", 2) == 0) {
      return 0;
    } 
    printf("Error: read_bool failed\n");
    exit(1);
  }
  else {
    printf("Error: read_int called with invalid type\n");
    exit(1);
  }
}
