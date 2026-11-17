/* Coverage runtimes other than compiler-rt's set the gate themselves. */

#include <stdint.h>

int64_t __sancov_should_track = 1;
