# avx512gen — AVX512 intrinsic generator & coverage checker

Data-driven tooling that exposes Intel AVX512 (F/DQ/CD/BW/VL) intrinsics in
OxCaml. The authoritative target is the vendored Intel Intrinsics Guide XML; the
available instructions come from the simdgen DB
(`../simdgen/amd64_simd_instrs.ml`). Each Intel intrinsic is mapped onto an
existing instruction binding, and the descriptive name, the OCaml external, the
instruction selection, and a C-counterpart test all fall out of that one map.

## Files

- `intel_intrinsics.xml` — vendored, pinned Intel Intrinsics Guide dataset
  (version 3.6.9, 2024-07-12). Source of the target surface area. Re-download
  from the URL in the file header to refresh.
- `simdb.py` — parses `../simdgen/amd64_simd_instrs.ml` into a queryable binding
  index (name, mnemonic, ext, operands, result kind, width, merge flag, ...).
- `generate.py` — the generator. Names each intrinsic
  `caml_avx512_<type>_<op>[_maskz|_mask]` via a grammar that reuses the avx2
  op-vocabulary, matches it to a binding (`pick_binding`), and (re)writes:
    1. the selection arms spliced into
       `backend/amd64/simd_selection.ml` between the `(* BEGIN/END GENERATED
       AVX512 *)` markers (function `select_operation_avx512`);
    2. `oxcaml/tests/simd/avx512/ops512_gen.ml` — per-intrinsic OCaml externals
       + a C-oracle external + a bit-exact comparison test;
    3. `oxcaml/tests/simd/avx512/ops512_gen_stubs.c` — C oracles, each calling
       the *same* Intel intrinsic the OxCaml builtin lowers to (so agreement is
       exact for every input class), plus `BUILTIN()` fallbacks.
- `coverage.py` — diffs the Intel target against what is exposed, grouped by
  reason/category, and cross-checks every exposed lowering's mnemonic. This is
  the "have we exposed everything Intel does?" answer.

## Masking

Full Intel parity: unmasked, zero-masking (`_maskz`, takes a `mask`) and
merge-masking (`_mask`, takes a leading merge-source + `mask`). Merge relies on
the simdgen `_K_merge` bindings (`res = Arg [|0|]`) — see the `merge` handling in
`../simdgen/simdgen.ml` `expand_mask`.

## Regenerating

```sh
python3 tools/avx512gen/generate.py          # rewrite arms + test + stubs
make -s boot-compiler                        # rebuild compiler (arms changed)
# run the bit-exact tests in the main context (boot compiler):
OXCAML_NAME_MANGLING=flat OXCAML_POLL_INSERTION=false \
  dune build --root=. --workspace=duneconf/main.ws \
  @oxcaml/tests/simd/avx512/runtest

python3 tools/avx512gen/coverage.py          # coverage summary + remaining work
python3 tools/avx512gen/coverage.py --list <reason>   # enumerate a gap bucket
```

If a needed instruction is missing from the simdgen DB, add the row(s) to
`../simdgen/amd64/amd64.csv` and `make -s boot-compiler` (the promote rule
regenerates `amd64_simd_instrs.ml`); the generator then picks it up.

## Status / extending

Currently emits the regular lane-wise non-immediate ops (arithmetic, logical,
min/max, abs, sqrt, variable shifts/rotates, getexp/scalef/rcp14/rsqrt14,
conflict/lzcnt) across all element types, widths and mask forms. Everything not
yet handled is logged by `coverage.py` (no silent caps). Extend by adding to the
`OP` table in `generate.py`, by handling new shapes (immediates → const-oracle;
mask-producing compares; converts with two types; permutes/shuffles; gather /
scatter), and by filling `amd64.csv` gaps.
