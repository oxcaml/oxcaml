# Working files

The primary goal is to develop the ikind system that will gradually replace the jkind system.
We are mainly working with typing/ikinds, and sometimes with jkind.ml and ctype.ml, and sometimes other files.

- typing/ikinds/lattice_intf.ml: Lattice signature required by the solver (bot/top/join/meet/leq/co_sub/etc.).
- typing/ikinds/global_counters(.ml/.mli): Global counters utility for lightweight profiling/debug (increment and list named counters).
- typing/ikinds/ldd(.ml/.mli): Lattice‑valued ZDD functor (hash‑consed nodes, joins/meets/residuals, solver plumbing, memoization, linear decomposition).
- typing/ikinds/ldd_jkind_solver(.ml/.mli): JKind solver functor interpreting church‑encoded kinds into lattice polynomials via LDD; exposes ckind, ops, normalize/leq/round_up, env hooks.
- typing/ikinds/product_lattice(.ml/.mli): Simple array‑based product lattice functor (prototype) with encode/decode and per‑axis ops; parameterized by axis sizes.
- typing/ikinds/axis_lattice(.ml/.mli): Product lattice for jkinds axes with conversions to/from modality bounds and common lattice constants; interface exposes lattice ops and axis mapping.
- typing/ikinds/axis_lattice_bits(.ml/.mli): Bitset-backed axis lattice offering the same API as axis_lattice for faster operations.
- typing/ikinds/ikinds.ml: Kind construction driver using Ldd_jkind_solver + Axis_lattice to compute kinds from Types.type_expr.

# Diagnostics

From your working terminal, trigger incremental builds and print diagnostics:
  `make hacking-emacs-builder`
Uses Dune RPC to build the boot targets and print errors immediately.
The user has to start a passive watcher in a separate terminal for this to work:
  `make hacking-emacs-poller`
Runs Dune in passive watch mode for the boot workspace with RPC server.

IMPORTANT: YOU MUST ALWAYS BUILD AFTER YOUR CHANGES JUST LIKE A SOFTWARE ENGINEER!
Ensure that all the code type checks and builds using `make hacking-emacs-builder`.

# Testing the compiler

Run the compiler produced by `make hacking-emacs-poller`/`builder` on a file using `_build/_bootinstall/bin/ocamlc <the-file>`.

# Testsuite: run one test and promote outputs

Run a single test
- Fresh staging + run one: `make test-one TEST=typing-jkind-bounds/subsumption/recursive.ml`
  - Accepts either `TEST=testsuite/tests/<path>` or the shorthand `TEST=<path>`; the helper maps to `tests/<path>` internally.
- Reuse existing staging (faster): `make test-one-no-rebuild TEST=typing-warnings/w17`
- Run a whole directory: `make test-one DIR=typing-warnings`

Promote expected outputs
- Promote results for one test after checking diffs:
  - Fresh staging + promote: `make promote-one TEST=typing-warnings/w17`
  - Reuse staging: `make promote-one-no-rebuild TEST=typing-warnings/w17`
- Promote all currently failing tests after a run: `make promote-failed`

We are primarily interested in the `testsuite/tests/typing-jkind-bounds/**` tests.

# Formatting

Run formatter: `make fmt`

