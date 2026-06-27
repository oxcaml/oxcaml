# Compiler Perf CI Plan

- [x] Add build timing for base source built by the base compiler.
- [x] Add build timing for base source built by the head compiler.
- [x] Use the head-built base install for file benchmarks.
- [x] Compare the same base source files for both compilers.
- [x] Switch file benchmarks to paired suite repetitions.
- [x] Report build ratios, corpus ratios, and per-file diagnostics.
- [x] Keep failure rules conservative until calibrated.
- [x] Validate locally with existing installed compilers.
- [x] Push the implementation branch.
- [ ] Update the synthetic #6113 test branches and check CI.
