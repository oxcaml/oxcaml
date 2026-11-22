#include <caml/m.h>
#ifdef STACK_CHECKS_ENABLED
let stack_checks_in_c = true
#else
let stack_checks_in_c = false
#endif
