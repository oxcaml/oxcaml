/* Raises SIGABRT so that AFL registers a crash: AFL only counts signals as
   crashes, and an uncaught OCaml exception merely exits with a nonzero code.
   The [long] parameter and result stand for [value] (an OCaml [unit] here),
   avoiding a dependency on the in-tree runtime headers. */

#include <stdlib.h>

long afl_cfg_fuzzer_abort(long unit)
{
  abort();
  return unit;
}
