# About
oxfuzzer generates random OxCaml programs, compiles and runs them under different compiler configurations, and reports compilation failures, execution failures, and output mismatches.

# How to use
To use you first need to build OxCaml (more in the repostitory's root README), which will also automatically build oxfuzzer. To run the script you need to have a Python version 3.11 or higher.

```
make compiler
python3 oxfuzzer.py -o output_directory
```

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
