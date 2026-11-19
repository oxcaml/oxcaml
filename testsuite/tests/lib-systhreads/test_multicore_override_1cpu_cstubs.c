/* Pin the process to a single CPU before the OCaml runtime starts, so
   that the OCaml code under test initializes under single-CPU
   conditions.  An ELF constructor is fine here: this stub is only
   built for the linux-gated test alongside it. */

#define _GNU_SOURCE
#include <sched.h>

static void restrict_to_one_cpu(void) __attribute__((constructor));

static void restrict_to_one_cpu(void)
{
  cpu_set_t set;
  CPU_ZERO(&set);
  if (sched_getaffinity(0, sizeof set, &set) != 0) return;
  /* Keep only the first CPU of the current affinity mask. */
  int found = 0;
  for (int i = 0; i < CPU_SETSIZE; i++) {
    if (CPU_ISSET(i, &set)) {
      if (found) CPU_CLR(i, &set);
      found = 1;
    }
  }
  (void)sched_setaffinity(0, sizeof set, &set);
}
