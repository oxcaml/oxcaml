# quickcheck-allocation

Property-based tester for the frontend `Allocation` mode axis, using the backend
`zero_alloc` analysis as an oracle.

## Build & run

Requires a built compiler. From the repo root:

```sh
COMP=$PWD/_build/install/main/bin/ocamlopt.opt

dune exec oxcaml/tests/quickcheck-allocation/main.exe -- $COMP \
  -count 200 -seed 0 -mode soundness
```

Flags:

- `-count N` -- number of programs to try (default 200)
- `-seed S` -- starting PRNG seed (default 0); round `k` uses seed `S + k`, so
  runs are reproducible and a single case can be re-run with
  `-count 1 -seed <seed>`
- `-mode soundness|completeness` -- generation bias (default `soundness`):
  `soundness` favors programs the frontend should accept (immediates,
  `exclave_`), hunting FE-accept & BE-reject bugs; `completeness` favors
  non-allocating computation the frontend may conservatively reject

Pass the compiler as an absolute path (it is invoked via the shell). The fuzz
loop can also be run via its alias, which uses the just-built `ocamlopt.opt`
and the default flags:

```sh
dune build @oxcaml/tests/quickcheck-allocation/quickcheck --force
```

(`--force` because dune caches the successful run; without it a second build
does nothing.) The alias is not part of `make test`.

## Output

A tally line, e.g.:

```
agree(noalloc)=22 suspects=0 fe_rejects=38 gen_errors=0
```

- `agree(noalloc)` -- FE accepted and the backend `zero_alloc` check passed
- `suspects` -- FE accepted but the backend rejected: potential soundness bugs.
  Each is printed in full below the tally (seed, program, backend error)
- `fe_rejects` -- FE rejected: completeness candidates, printed as a
  frequency-ranked table of rejection causes (the frontend-improvement
  backlog). Since the program carries both annotations, a rejected program
  does not compile, so the backend cannot confirm these
- `gen_errors` -- compile failures unrelated to either check, i.e. generator
  bugs; should be 0

## Editor / ocamllsp tip

If ocamllsp shows spurious errors (e.g. `Unbound module`), rebuild in the
`default` context and restart the language server:

```sh
dune build _build/default/oxcaml/tests/quickcheck-allocation/main.exe
```
