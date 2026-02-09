#define CAML_INTERNALS
#define NATIVE_CODE
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/printexc.h>
#include <caml/startup.h>

int main(int argc, char **argv)
{
  const value *test_gc_compact_fn;

  printf("=== Manual Module Init Test ===\n");
  fflush(stdout);

  printf("Step 1: caml_startup (no modules should initialize)\n");
  fflush(stdout);
  caml_startup(argv);
  printf("Step 1: done\n\n");
  fflush(stdout);

  printf("Step 2: Initialize Gc_compact_test module (should init all dependencies)\n");
  fflush(stdout);
  caml_init_module("Gc_compact_test");
  printf("Step 2: done\n\n");
  fflush(stdout);

  printf("Step 3: Initialize Gc_compact_test again (should be no-op)\n");
  fflush(stdout);
  caml_init_module("Gc_compact_test");
  printf("Step 3: done\n\n");
  fflush(stdout);

  printf("Step 4: Initialize Base directly (should be no-op since already initialized)\n");
  fflush(stdout);
  caml_init_module("Base");
  printf("Step 4: done\n\n");
  fflush(stdout);

  printf("Step 5: Run GC test (outside of init callback)\n");
  fflush(stdout);
  test_gc_compact_fn = caml_named_value("test_gc");
  if (test_gc_compact_fn == NULL) {
    printf("ERROR: test_gc callback not found\n");
    return 1;
  }
  caml_callback(*test_gc_compact_fn, Val_unit);
  printf("Step 5: done\n\n");
  fflush(stdout);

  printf("Step 6: Test re-entrant caml_init_module\n");
  fflush(stdout);
  caml_init_module("Reentrant_b");
  printf("Step 6: done\n\n");
  fflush(stdout);

  printf("Step 7: Verify Reentrant_a is now initialized (should be no-op)\n");
  fflush(stdout);
  caml_init_module("Reentrant_a");
  printf("Step 7: done\n\n");
  fflush(stdout);

  printf("Step 8: Test init_module called from OCaml (Ocaml_init uses caml_init_module_from_ocaml)\n");
  fflush(stdout);
  caml_init_module("Ocaml_init");
  printf("Step 8: done\n\n");
  fflush(stdout);

  printf("Step 9: Test module that raises during initialization\n");
  fflush(stdout);
  {
    value result = caml_init_module_exn("Raises");
    if (Is_exception_result(result)) {
      value exn = Extract_exception(result);
      char *msg = caml_format_exception(exn);
      printf("Step 9: caught exception: %s\n", msg);
      fflush(stdout);
      caml_stat_free(msg);
    } else {
      printf("ERROR: expected exception from Raises module\n");
      return 1;
    }
  }
  printf("Step 9: done\n\n");
  fflush(stdout);

  printf("=== All tests passed ===\n");
  return 0;
}
