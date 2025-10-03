# Mode Bitfield Migration Plan

Goal: replace per-axis records with bitfields for efficiency.

## Stage 0 – Preliminaries
- Catalogue code that uses per-axis records, such as `Mode`, `Crossing`, `Modality`, etc.
- Confirm the existing `Axis_lattice` layout is suitable as the shared representation (or adjust the shared layout as needed) and outline property tests for lattice identities (`le`, `join`, `meet`, `subtract`).

## Stage 1 – Shared Bit Backend & Test Harness
- Extract the bitfield operations behind `Axis_lattice` into a reusable backend `Axis_bitfield` that owns the authoritative layout, masks, and primitive operations (get/set, join/meet, co-sub, optional flipped comparisons).
- Build versions of the domain-specific types (Mode, Crossing, Modality, etc.) over that backend, while keeping their exact public interfaces intact.
- Build targeted unit/property tests that cover round-tripping, lattice laws, and ordering semantics—these become the MVP validation suite.

## Stage 2 – MVP Bitfield Backend (Correctness First)
- Switch the compiler to use the Mode, Crossing, Modality, etc. modules that use the bitfield backend.
- Run the compiler test suite at appropriate steps (or ask the user if it times out) to ensure correctness.

## Stage 3 – Broader Integration & Refactor
- Survey the compiler for any per-axis code that would benefit from additional primitives in Axis_bitfield to use bit-parallel bitops.
- Refactor the code to use those additional primitives.

## Dangers & Mitigations
- **Monadic/comonadic ordering flip**: monadic axes use reversed ordering today. Ensure helper functions encode this flip, and add regression/property tests comparing old vs new `Mode.Const.le`/`join` results.

## Success Criteria
- Public interfaces remain unchanged; callers still use `Mode.Value` APIs.
- MVP (Stage 2) passes regression tests and can be benchmarked for correctness/perf feedback before proceeding.
- Final system replaces per-axis record manipulation with mask-based helpers everywhere, with maintained or improved performance and diagnostics.
