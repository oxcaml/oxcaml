#include <stdio.h>
#include <sys/resource.h>
#include <unistd.h>
#include <caml/mlvalues.h>

/* Cap the address space at its current size plus [headroom_mib], so that
   allocating fiber stacks (which reserve several megabytes each when
   guarded) fails after a bounded number of fibers, whatever the kernel's
   mapping limit. Returns false if the limit could not be read or set. */

/* Restore the address-space limit to the hard limit, undoing the cap. */

CAMLprim value test_uncap_address_space(value unit)
{
  struct rlimit rl;
  (void)unit;
  if (getrlimit(RLIMIT_AS, &rl) != 0) return Val_false;
  rl.rlim_cur = rl.rlim_max;
  if (setrlimit(RLIMIT_AS, &rl) != 0) return Val_false;
  return Val_true;
}

CAMLprim value test_cap_address_space(value headroom_mib)
{
  struct rlimit rl;
  unsigned long pages = 0;
  rlim_t want;

  FILE* statm = fopen("/proc/self/statm", "r");
  if (statm == NULL) return Val_false;
  if (fscanf(statm, "%lu", &pages) != 1) {
    fclose(statm);
    return Val_false;
  }
  fclose(statm);

  if (getrlimit(RLIMIT_AS, &rl) != 0) return Val_false;
  want = (rlim_t)pages * (rlim_t)sysconf(_SC_PAGESIZE)
       + (rlim_t)Long_val(headroom_mib) * 1024 * 1024;
  if (rl.rlim_max != RLIM_INFINITY && want > rl.rlim_max) return Val_false;
  rl.rlim_cur = want;
  if (setrlimit(RLIMIT_AS, &rl) != 0) return Val_false;
  return Val_true;
}
