# About
oxfuzzer generates random OxCaml programs, compiles and runs them under different compiler configurations, and reports compilation failures, execution failures, and output mismatches.

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

On a terminal, oxfuzzer shows the counts and the rate in a status line that it
updates in place. Otherwise, as in a log, it prints a line for each failure it
saves and the totals when the run ends, and nothing while cases pass.

SIGINT (Ctrl-C), SIGTERM or SIGHUP (the terminal was closed) also stops the
run, but cases in flight are killed rather than allowed to finish. Killed cases
are dropped: they are neither counted nor saved. The run then finishes as if a
limit had been reached: oxfuzzer prints the final statistics and `oxfuzzer:
finished: interrupted (SIGINT)` (or `SIGTERM`, or `SIGHUP`), and exits with
status 1 if any case failed and 0 otherwise. A signal during startup takes
effect once the startup step in progress (such as snapshotting the build
directory) is done. While oxfuzzer is stopping, a second SIGINT kills it
immediately (use it if shutdown gets stuck); further SIGTERMs and SIGHUPs are
ignored. Under `nohup`, which starts a program with SIGHUP ignored, oxfuzzer
leaves it ignored and keeps running when the terminal is closed.

oxfuzzer also keeps running when it can no longer write to stderr, say because
the terminal is gone or the program it is piped into has exited: it stops
printing, and still saves failures, writes the summary and sets its exit
status.

Use `--summary-file PATH` to have oxfuzzer write a short Markdown summary to
PATH when the run ends, whether it finished, was stopped by a signal, or ended
with an error (such as a failed self-check, a toolchain that cannot be run, an
output directory that cannot be written, or an unexpected error in oxfuzzer
itself): why it ended, the passed/discarded/failed counts, the rate (cases
completed per hour of fuzzing), the wall time, the base seed and the saved
failures. It has no top-level heading. After such an error the exit status is
1.

Under GitHub Actions, oxfuzzer also appends that summary to the step summary
(the file `$GITHUB_STEP_SUMMARY` names), with or without `--summary-file`, and
reports each saved failure as an error annotation, which GitHub lists on the
page of the run (the first ten of a step). In the log it folds its settings and
the failures into groups, so that the final lines stand out.

By default the compilers, stdlib, runtime and `oxfuzzer.exe` are taken from the
`_build` directory of the repository containing `oxfuzzer.py`. Use
`--build-dir DIR` to point at the `_build` directory of another checkout, or
`--install-dir DIR` to use an installed compiler instead, e.g. the result of
`make install` or `nix build .#oxcaml`. A build directory is snapshotted into the
output directory before fuzzing, while an installation is used in place.

The log and each failure's `meta.txt` give a revision. For a build directory it
is the `source revision`: that of the checkout the build directory is in. An
installation does not record the revision it was built from, so for one it is
the `oxfuzzer.py revision`: that of the checkout `oxfuzzer.py` is in. In CI
that is the commit being fuzzed; elsewhere, check that the installation was
built from that checkout.

Failures will be saved in `output_directory/<seed>/` with the following structure:
```
program.ml   the generated program
reason.txt   why the case failed
meta.txt     seed, time and the run-level metadata (see run_metadata)
<config>/    one directory per configuration
    run.bash (a standalone reproducer: `bash run.bash`)
    compile.{status,stdout,stderr}
    run.{status,stdout,stderr} (appears if the program was run)
```
Each failure is written to a hidden `.<seed>.<random>/` directory and renamed
into place when complete, so a failure's directory is never partial. A leftover
hidden directory is from a run that was killed while saving.

To run the case of a saved failure again, say with another compiler, pass its
seed (the name of its directory) as `--seed`, with `--iterations 1`. A `--seed`
that is a number below 2^62 is the seed of the first case. The cases after it,
and all the cases of any other `--seed`, come from a random generator seeded
with the `--seed` string.

Learn more with `python oxfuzzer.py -h`.
