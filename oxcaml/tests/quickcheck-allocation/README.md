# quickcheck-allocation

Property-based tester for the frontend `Allocation` mode axis, using the backend
`zero_alloc` analysis as an oracle.

## Build & run

Requires a built compiler. From the repo root:

```sh
dune build @oxcaml/tests/quickcheck-allocation/quickcheck
```

This builds `main.exe` and runs the fuzz loop against the just-built
`ocamlopt.opt`. It is behind its own alias, so `make test` never runs it.

Run directly for more control (`<ocamlopt>` = path to the built `ocamlopt.opt`):

```sh
dune exec oxcaml/tests/quickcheck-allocation/main.exe -- <ocamlopt> -count 200 -seed 0
```

Flags: `-count N` (programs to try), `-seed S` (starting PRNG seed).

## Output

A per-quadrant tally, a list of soundness suspects (FE-accept & BE-reject), and
a frequency-ranked table of precision-gap causes (FE-reject & BE-pass).

## Editor / ocamllsp tip

If ocamllsp shows spurious errors (e.g. `Unbound module`), rebuild in the
`default` context and restart the language server:

```sh
dune build _build/default/oxcaml/tests/quickcheck-allocation/main.exe
```
