# About
oxfuzzer generates random OxCaml programs, compiles and runs them under different compiler configurations, and reports compilation failures, execution failures, and output mismatches.

# Evaluation order

For generated programs, oxfuzzer assumes right-to-left evaluation of operands
whose order OCaml leaves unspecified: function and operator arguments, tuple
components, constructor payloads, record-construction fields, and array
elements. Observable differences caused by a compiler departing from this order
are treated as failures. This is a fuzzer assumption, not a guarantee of the
OCaml language.

Record updates are different: the tested compilers evaluate the base record
before replacement field expressions. These expressions are also left unbound.

Language-defined sequencing is unchanged: sequential let-bindings and statements
run in order, `&&` and `||` evaluate their left operand first and short-circuit,
and conditionals evaluate only the selected branch after their condition.

Operands are left unbound, including in `May_raise` mode, so differences in
mutation or exception order remain observable. Bindings are retained where
needed for single evaluation or sharing, such as integer division/remainder
operands, comparison results, concatenation results, and array initializers.
When such bindings sequence multiple operands, they follow right-to-left order.

# How to use
To use you first need to build OxCaml (more in the repostitory's root README), which will also automatically build oxfuzzer. To run the script you need to have a Python version 3.11 or higher.

```
make compiler
python3 oxfuzzer.py -o output_directory --iterations 100000
```

At least one of `--iterations N` and `--time-limit-minutes M` is required. If
both are given, the run stops at whichever limit is reached first.

Use `--max-failures N` (a positive integer) to stop starting new cases after N
failing cases have been saved. Each generated case counts once, regardless of
how many compiler configurations fail. In-flight cases are allowed to finish
and save their results rather than being cancelled, so the final failure count
may exceed N. Omitting this option leaves the number of failures unlimited;
`--iterations` and `--time-limit-minutes` still limit the run. The exit status
remains 1 if any case failed.

Use `--time-limit-minutes M` to stop starting new cases after M minutes, counted
from startup. As with `--max-failures`, in-flight cases finish, so the run may
last longer than M minutes. When the run ends, oxfuzzer prints which limit
stopped it.

By default the compilers, stdlib, runtime and `oxfuzzer.exe` are taken from the
`_build` directory of the repository containing `oxfuzzer.py`. Use
`--build-dir DIR` to point at the `_build` directory of another checkout, or
`--install-dir DIR` to use an installed compiler instead, e.g. the result of
`make install` or `nix build .#oxcaml`. A build directory is snapshotted into the
output directory before fuzzing, while an installation is used in place.

Failures will be saved in `output_directory/seed-<seed>/` with the following structure:
```
program.ml   the generated program
reason.txt   why the case failed
meta.txt     seed, time and the run-level metadata (see run_metadata)
<config>/    one directory per configuration
    run.sh (a standalone reproducer)
    compile.{status,stdout,stderr}
    run.{status,stdout,stderr} (appears if the program was run)
```

Learn more with `python oxfuzzer.py -h`.
