---
name: rocq-models
description: Change or add a Rocq model of a compiler pass (the .v files under rocq/, backend/rocq/, backend/ssa/rocq/ and backend/ssa/loop/rocq/), regenerate the extracted OCaml, or fix a proof after a change to the modelled code.
allowed-tools: Bash(make -C rocq all), Bash(make -C rocq clean), Bash(make -s boot-compiler), Bash(make -s fmt), Bash(git diff:*)
---

# Rocq models of compiler passes

Some analyses (currently the loop analyses behind `-ssa-delete-empty-loops`)
are defined and proved in Rocq, and the OCaml the compiler builds is extracted
from them. The extracted `.ml`/`.mli` files are committed and are the
implementation; never edit them by hand.

Read `rocq/HACKING.md` in full before touching a `.v` file. It gives the
directory and library layout, the four-part layout of a model file, how OxCaml
types and compiler data are modelled, what is trusted, the extraction pitfalls
and the proof style.

The cycle for any change:

1. Edit the `.v` files, keeping lines under 80 columns and a plain-words
   comment before every non-trivial lemma and theorem.
2. `make -C rocq all` (Rocq and ocamlformat on `PATH`) checks the proofs and
   regenerates the extracted files. After renaming `.v` files, `touch` the
   sources or `make -C rocq clean` first.
3. Check `git diff` of the extracted `.ml`/`.mli` shows only the intended
   change.
4. `make -s boot-compiler`, the pass's tests, `make -s fmt`.
5. Commit the `.v` sources and the regenerated files together.

A new extracted module must be listed in `rocq/Makefile`, in the root `dune`
module list and in the header of `backend/ssa/loop/rocq/Extract.v`.
