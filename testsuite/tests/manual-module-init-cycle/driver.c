#define CAML_INTERNALS
#define NATIVE_CODE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/startup.h>

int main(int argc, char **argv)
{
  printf("=== Manual Module Init Cycle Detection Test ===\n");
  fflush(stdout);

  printf("Step 1: caml_startup (no modules should initialize)\n");
  fflush(stdout);
  caml_startup(argv);
  printf("Step 1: done\n\n");
  fflush(stdout);

  printf("Step 2: Initialize Cycle_module (cycle detected and caught in OCaml)\n");
  fflush(stdout);
  caml_init_module("Cycle_module");
  printf("Step 2: done\n\n");
  fflush(stdout);

  printf("=== All tests passed ===\n");
  return 0;
}
