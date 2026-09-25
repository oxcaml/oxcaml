# missing_code

Source: `testsuite/tests/flambda2/simplify/missing_code.ml`
(with `missing_code_step1.ml`, `missing_code_step2.ml`)
Reference: `missing_code.compilers.reference` (compiler output, `check-ocamlopt.byte-output`)

Covers a rule not exercised by the main-directory tests: `S.Inline.Decision`
step 2 (Missing_code → keep) and the cross-module story of chapter 11 §5.

## Input (multi-unit)

`missing_code_step1.ml` defines nested functors `F`/`G`/`H`
(`[@@inline never]`); step1's `.cmx` exports metadata for all three code IDs.
`missing_code_step2.ml` applies `F`. The test then **removes
`missing_code_step1.cmx`** and compiles `missing_code.ml`, which transitively
references the functor code IDs whose code is no longer importable.

## Prediction (written before reading the reference)

- Per chapter 11 §5: a callee inlinable only if its *code* (not just
  `Code_metadata`) was exported and imported. With `missing_code_step1.cmx`
  deleted, `DE.find_code_exn` finds metadata but no code, so any attempt to
  inline `F`/`G`/`H` hits `S.Inline.Decision` **step 2** → `Missing_code` →
  keep. The functor stays a residual (un-inlined) application.
- Because the code (and its transitively-mentioned code IDs) can't be found, the
  compiler emits the "no cmx file / code could not be found" diagnostic rather
  than crashing. Predicted observable: a `no-cmx-file` warning naming
  `Missing_code_step1`, and successful compilation.

## Actual

```
File "_none_", line 1:
Warning 58 [no-cmx-file]: no cmx file was found in path for module
  Missing_code_step1, and its interface was not compiled with -opaque
```

Compilation succeeds with exactly the `no-cmx-file` warning for
`Missing_code_step1`.

## Verdict

MATCH. The missing-code path behaves as chapter 11 §5 / `S.Inline.Decision` step
2 describe: absent imported code is handled gracefully (keep, no inline) and the
compiler reports the missing `.cmx` rather than failing.

## Diagnosis

Not a mismatch, but a scope note: the reference is a coarse observable (a single
compiler warning), so it validates the *outcome* of the Missing_code path but not
the fexpr shape. Also worth flagging for the formalism's benefit — chapter 11 §2
NOTES say Missing_code "is where an `Inlining_impossible` warning is owed (the
code has outstanding CRs about emitting it)"; the warning actually emitted here
is Warning 58 `no-cmx-file` at cmx-load time, a different and coarser diagnostic
than the per-call-site `Inlining_impossible` warning the formalism refers to.
The two are consistent (both report absent code) but are distinct mechanisms;
the chapter's phrasing already marks the call-site warning as not-yet-emitted,
so this is a clarification, not a rule error.
