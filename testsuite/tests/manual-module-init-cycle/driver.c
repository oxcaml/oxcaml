#define CAML_INTERNALS
#define NATIVE_CODE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/globroots.h>

int main(int argc, char **argv)
{
  printf("=== Manual Module Init Cycle Detection Test ===\n");
  fflush(stdout);

  printf("Step 1: caml_startup (no modules should initialize)\n");
  fflush(stdout);
  caml_startup(argv);
  printf("Step 1: done\n\n");
  fflush(stdout);

  printf("Step 2: Initialize Cycle_module (should detect cycle and abort)\n");
  fflush(stdout);
  caml_init_module("Cycle_module");

  /* Should not reach here */
  printf("ERROR: should have aborted due to cycle detection!\n");
  return 1;
}
