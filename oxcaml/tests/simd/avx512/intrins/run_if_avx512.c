/* Runs the given test executable if the host supports AVX512, and exits 0
   silently otherwise, so the runtest rules skip cleanly on hosts without
   AVX512. (__builtin_cpu_supports also validates OS XSAVE state.) */
#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "usage: %s <test.exe> [args...]\n", argv[0]);
        return 2;
    }
    if (!(__builtin_cpu_supports("avx512f")
          && __builtin_cpu_supports("avx512dq")
          && __builtin_cpu_supports("avx512bw")
          && __builtin_cpu_supports("avx512vl")
          && __builtin_cpu_supports("avx512cd")))
        return 0;
    execv(argv[1], argv + 1);
    perror(argv[1]);
    return 1;
}
