#define CAML_INTERNALS
#include <stdio.h>
#include <caml/misc.h>
#include <caml/cpu.h>

int main(void)
{
  caml_detect_cpu_features();
  puts("a function of the runtime library was called");
  return 0;
}
