# Mode Bitfield Migration Plan

Goal: replace per-axis records with bitfields for efficiency.

## Stage 0 – Preliminaries
- Catalogue code that uses per-axis records, such as `Mode`, `Crossing`, `Modality`, etc.
- Confirm the existing `Axis_lattice` layout is suitable as the shared representation (or adjust the shared layout as needed) and outline property tests for lattice identities (`le`, `join`, `meet`, `subtract`).
- Capture the implicit API surface of the record layout (helpers, invariants that callers rely on, serializer/deserializer pairs, pretty-printers) so we can guarantee feature parity.

### Checklist
- Produce an inventory spreadsheet linking each axis record field to the code that reads/writes it (`typing/mode.ml`, `typing/mode_intf.ml`, `typing/ikinds/*.ml`, `typing/jkind_axis.ml`, `typing/typemode.ml`). Include notes on the direction of `le` comparisons (monadic axes are reversed today).
- Document the bit layout we rely on today (`typing/ikinds/axis_lattice.ml`) and flag any mismatches between that layout and the record field ordering so `Axis_bitfield` can be authoritative.
- Establish a baseline test harness capable of constructing arbitrary per-axis records and asserting round-trips through the existing conversion helpers. These tests become the regression oracle when we switch the backend.
- Agree on migration toggles (feature flag or env var) so we can flip between record and bitfield representations during development without destabilising the compiler pipeline.

### Deliverables
- Short design doc that explains the axis numbering, packing order, and ordering semantics, checked in next to the plan.
- List of call-sites that use `Mode.Comonadic.{join,meet,co_sub,...}` style helpers so we can prioritise shims.
- Draft property-test spec covering lattice laws and monotonicity guarantees for each axis; include input generation strategy.

## Stage 1 – Shared Bit Backend & Test Harness
- Extract the bitfield operations behind `Axis_lattice` into a reusable backend `Axis_bitfield` that owns the authoritative layout, masks, and primitive operations (get/set, join/meet, co-sub, optional flipped comparisons).
- Build versions of the domain-specific types (Mode, Crossing, Modality, etc.) over that backend, while keeping their exact public interfaces intact.
- Build targeted unit/property tests that cover round-tripping, lattice laws, and ordering semantics—these become the MVP validation suite.

### Checklist
- Define `Axis_bitfield` as a standalone module (likely under `typing/ikinds/`) with a minimal interface: `type t`, per-axis getters/setters keyed by the same metadata used today, lattice combinators, `mask` utilities, and conversions to/from the legacy record types.
- Provide functorised wrappers that allow `Mode.Value.Monadic`, `Mode.Value.Comonadic`, `Mode.Crossing`, and friends to be instantiated over either the record or bit backend without leaking representation details. This keeps the public signatures untouched while letting us switch implementations.
- Port existing helper modules (`Mode.Flat`, `Mode.Log`, `Mode_hint`, etc.) to depend on the interface, not the record type, and confirm no derived-type equalities leak through.
- Introduce expect tests or inline `dune runtest` targets that assert `legacy_record = decode (encode legacy_record)` and that ordering/joins between two records match the bit operations.

### Implementation Notes
- Keep the bit layout constant by making `Axis_bitfield` the single owner of offsets and masks; other code should depend on exported constants rather than recomputing them.
- Retain optional logging hooks so we can diff behaviour at runtime when toggling representations (e.g. by pluggable comparator that cross-checks record vs bitfield results in debug builds).
- Ensure the new backend is `[@@inline]`-friendly to avoid performance regressions when the compiler inlines lattice operations.

### Deliverables
- New module `Axis_bitfield` with accompanying `.mli` to codify the API guarantees.
- Shim modules for `Mode`, `Crossing`, `Modality`, etc., that simply `include` the old signatures while delegating to the bit backend.
- Test suite entries (unit + property tests) checked in under `testsuite/tests/mode_bitfield/` or similar, wired into CI.
- Benchmark harness draft (can be a script) that exercises representative code paths for later perf sign-off.

## Stage 2 – MVP Bitfield Backend (Correctness First)
- Switch the compiler to use the Mode, Crossing, Modality, etc. modules that use the bitfield backend.
- Run the compiler test suite at appropriate steps (or ask the user if it times out) to ensure correctness.

### Checklist
- Land the feature-flag plumbing and add a default-on configuration in development builds while retaining an opt-out for bisecting regressions.
- Perform module-by-module swaps (e.g. start with `Mode.Value.Monadic`, then `Mode.Value.Comonadic`, followed by `Mode.Crossing`) so that failures can be attributed precisely; keep each swap behind a reviewable patch.
- Regenerate any derived artefacts (docgen, `.mli` digests) to confirm that public signatures stayed the same.
- Run `dune runtest`, `make tests`, and the slower compiler integration suites; record runtimes to compare against Stage 0 baselines.
- Capture before/after dumps of representative typing traces (using `-drawcaml` or existing debug flags) to guarantee diagnostics remain identical.

### Exit Criteria
- All regression tests pass with the bit backend enabled.
- No diff in generated `.cm*` artefacts when compiling a curated corpus (e.g. the Jane Street internal test set); confirm via checksum comparison.
- Stakeholders in typing/inference sign off on diagnostic stability after targeted manual testing.

## Stage 3 – Broader Integration & Refactor
- Survey the compiler for any per-axis code that would benefit from additional primitives in Axis_bitfield to use bit-parallel bitops.
- Refactor the code to use those additional primitives.

### Checklist
- Identify hot paths in `typing/` and `bytecomp/` that still manually destructure per-axis records or perform per-axis loops; replace them with bulk bit operations or masks where profitable.
- Evaluate opportunities to unify `Axis_lattice_array` and `Axis_bitfield` helpers so that array-backed abstractions also share masks and conversions.
- Update documentation (`HACKING.md`, internal wiki) to describe the new primitives and when to use them.
- Collaborate with runtime/compiler engineers to run targeted benchmarks and capture perf deltas; feed numbers back into the success criteria.

### Deliverables
- Follow-up patches that simplify call-sites using the richer bit API.
- Performance memo summarising throughput/latency improvements (or lack thereof) after the refactor.
- Tracking issue enumerating remaining record-based helpers (if any) and prioritising their removal.

## Dangers & Mitigations
- **Monadic/comonadic ordering flip**: monadic axes use reversed ordering today. Ensure helper functions encode this flip, and add regression/property tests comparing old vs new `Mode.Const.le`/`join` results.
- **Bit layout drift**: if multiple modules re-declare offsets, we risk subtle divergence. Mandate the new backend exports canonical offsets/masks and add a CI lint that fails if any other module contains a literal matching the current layout constants.
- **Partial migration instability**: mixed record/bitfield code paths may silently allocate or copy more. Guard each stage with exhaustive integration tests and keep the feature flag until Stage 3 completes.
- **Tooling visibility**: debuggers and printing utilities currently assume records. Provide `pp` helpers on the bit backend and update dev tooling so debugging stays ergonomic.
- **External consumers**: downstream projects may depend on private modules. Communicate the change early and time releases so consumers can adapt; consider providing a compatibility alias module for one release cycle.

## Success Criteria
- Public interfaces remain unchanged; callers still use `Mode.Value` APIs.
- MVP (Stage 2) passes regression tests and can be benchmarked for correctness/perf feedback before proceeding.
- Final system replaces per-axis record manipulation with mask-based helpers everywhere, with maintained or improved performance and diagnostics.
- Repeatable performance measurements show non-regression (<1% slowdown) on baseline workloads, ideally an improvement.
- Debug tooling (printers, expect tests) continue to emit identical or deliberately improved output across the migration flag states.

## Rollout Checklist
- [ ] Stage 0 inventory doc merged and referenced from this plan.
- [ ] Axis_bitfield module and tests reviewed and landed behind a feature flag.
- [ ] Compiler compiled and tests green with flag on by default.
- [ ] Performance benchmark memo circulated with stakeholders.
- [ ] Documentation and tooling updates deployed.
- [ ] Feature flag removed and legacy record code deleted.

## Open Questions
- Do we need separate bit layouts for runtime vs typing components, or can a single layout serve both without penalising either side?
- Should the feature flag live in build configuration (dune/`configure`) or be runtime-togglable via environment variables for easier bisecting?
- Can we reuse existing generator infrastructure (e.g. `ppx_expect` or `QCheck`) for property tests, or do we need bespoke harnesses for lattice structures?
- Are there downstream consumers of the internal `Mode` records (e.g. tools in `runtime4/`) that require a compatibility layer during rollout?
