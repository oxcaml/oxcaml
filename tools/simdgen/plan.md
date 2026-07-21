    AVX512 Intrinsics: Generator, Selection, and C-Verified Tests

     Context

     The avx512-intrins branch already contains full backend support for AVX512:
     512-bit vector types (int64x8, float32x16, ..., plus mask/mask#,
     typing/predef.ml), ZMM and k-register classes (backend/amd64/regs.ml,
     proc.ml), EVEX encoding/emission (backend/x86_binary_emitter.ml,
     emit.ml), extension flags with implication closure (backend/amd64/arch.ml:
     -favx512f implies the mutually-required F/CD/DQ/BW/VL group), and — via
     tools/simdgen/simdgen.ml parsing tools/simdgen/amd64/amd64.csv — generated
     EVEX instruction descriptors in tools/simdgen/amd64_simd_instrs.ml
     (dune-promoted; pulled into the compiler by the root dune:64 copy_files#
     and the ocamloptcomp modules list at dune:609).

     What's missing is the user-facing surface: no AVX512 caml_* builtin names
     are recognized, so the instructions are unreachable from OCaml. The existing
     surface (SSE*/AVX/AVX2) is a hand-written ~900-arm string match in
     backend/amd64/simd_selection.ml; AVX512 adds ~4,100 intrinsics, so both the
     selection code and its tests must be generated.

     The tip commit added tools/simdgen/x86-intel.xml: Intel Intrinsics Guide
     data (v3.6.9, 7,146 intrinsics; name, CPUID flags, typed params, and
     <instruction form="zmm {k}, zmm, zmm" name="VADDPS"/>). The form operand
     vocabulary overlaps the CSV's (~50 tokens in scope) but needs a small
     normalization layer (below).

     Goal: extend simdgen to translate x86-intel.xml into (1) generated
     selection code, in the style of the existing select_operation_* arms,
     recognizing intrinsics named caml_<intel-name> (e.g. caml_mm512_add_ps),
     and (2) generated tests verifying every emitted intrinsic bit-for-bit against
     its C counterpart.

     Scope

     In scope: CPUID set non-empty and ⊆ {AVX512F, AVX512DQ, AVX512CD, AVX512BW,
     AVX512VL} — 4,092 intrinsics (counts independently re-verified):

     ┌───────────────────────────────────────────────────────────────────┬───────┬────────────────────────────────────────────────────────────────────────────┐
     │                               class                               │ count │                                disposition                                 │
     ├───────────────────────────────────────────────────────────────────┼───────┼────────────────────────────────────────────────────────────────────────────┤
     │ register-only forms (incl. imm, masked, er/sae, k-ops, GPR moves) │ 3,364 │ Phase 1                                                                    │
     ├───────────────────────────────────────────────────────────────────┼───────┼────────────────────────────────────────────────────────────────────────────┤
     │ memory forms (m8..m512, incl. masked loads/stores)                │ 272   │ Phase 2 (18 of these are memory-form-but-no-pointer-arg — see skip policy) │
     ├───────────────────────────────────────────────────────────────────┼───────┼────────────────────────────────────────────────────────────────────────────┤
     │ gather/scatter forms (vm32x..vm64z)                               │ 80    │ Phase 3                                                                    │
     ├───────────────────────────────────────────────────────────────────┼───────┼────────────────────────────────────────────────────────────────────────────┤
     │ sequence="TRUE"                                                   │ 348   │ library-level; skip (policy below)                                         │
     ├───────────────────────────────────────────────────────────────────┼───────┼────────────────────────────────────────────────────────────────────────────┤
     │ no <instruction> (casts, undefined)                               │ 28    │ skip (cast/const builtins already exist)                                   │
     └───────────────────────────────────────────────────────────────────┴───────┴────────────────────────────────────────────────────────────────────────────┘

     Every in-scope intrinsic must be either emitted or on an explicit,
     checked-in, human-reviewable skip list (name + reason, emitted by the
     generator); the generator hard-fails on any unhandled form and prints a
     coverage tally. No silent drops.

     Sequence policy (per discussion): intrinsics that compile to instruction
     sequences do not get compiler intrinsics — they are implemented at the
     library level by composing single-instruction intrinsics.
     _mm512_set1_ps/pd/epi8/epi16 and _mm512_set*/setr* are sequence="TRUE"
     → skipped (_mm512_set1_epi32/epi64 and all masked set1 variants are
     single VPBROADCAST instructions and stay in scope). The exception is
     operations not composable from other intrinsics — the flag-readers:
     _kortestz/_kortestc/_ktestz/_ktestc_mask{8,16,32,64} and
     _mm512_kortestz/c (26 names over KORTEST/KTEST B/W/D/Q) return an EFLAGS
     bit, i.e. instruction + SETcc. These map to a small curated set of new
     Simd.Seq pseudo-instructions, following Seq.ptestz/ptestc/ptestnzc
     (PTEST + SETcc, backend/amd64/simd.ml:75-162), with a per-name flag
     selector (ZF vs CF). The generator detects flag-readers (instruction result
     operand set empty, C return non-void) and routes them to the curated list,
     hard-failing on new unhandled ones. Skip-listed flag cases:
     _kortest/_ktest_mask*_u8 (8 names with an out-pointer for the second flag —
     composable from z/c variants) and _mm_comi_round_ss/sd (predicate+sae
     double imm, VCOMISS-vs-VUCOMISS mnemonic choice by predicate, EFLAGS result).

     Additional skip classes found by XML audit (must be keyed explicitly):
     - tech="SVML" — 190 entries; 188 are also sequence="TRUE" but
     _mm512[_mask]_log2_ps are NOT and carry a fake VLOG2PS instruction that
     exists in no hardware/CSV. Filter on tech explicitly.
     - Memory-form-but-no-pointer-arg (18): _mm512*_broadcast_{f32x4,f64x4,i32x4, i64x4} (only a memory-source instruction form exists; C arg is __m128),
     _mm512_cvtss_f32/cvtsd_f64 (store-shaped forms for low-lane extract —
     existing low_to builtins cover these), _mm512[_mask]_abs_{ps,pd} (VPANDD
     with a compiler-materialized constant operand). Skip with reasons initially;
     individual ones can be rescued later (e.g. broadcast_x4 via shufi/f 0x0).
     - 72 hardcoded-predicate compare pseudo-ops (_mm512_cmplt_epi32_mask etc.):
     single instructions with a constant imm not present as a C param. Emit them
     with the constant baked into the generated arm (derived from the name
     suffix per-mnemonic: VPCMP{,U}{D,Q,B,W} use _MM_CMPINT_*, VCMPP{S,D} use
     _CMP_*_O*), since generated-code style makes this trivial
     (instr vpcmpd_... ~i:1 args).

     Out of scope (follow-ups): a curated user library (like ocaml_simd in
     jane); other AVX512 extensions (FP16, VNNI, VBMI, ...); vectorizer use of
     512-bit ops.

     Design

     Data flow

     amd64/amd64.csv ──┐                ┌─> amd64_simd_instrs.ml    (existing, instruction descriptors)
                       ├── simdgen.exe ─┼─> amd64_simd_intrins.ml   (NEW, generated selection match)
     x86-intel.xml ────┘                └─> oxcaml/tests/simd/avx512/intrins/*  (NEW, generated tests)

     Intrinsic→instruction matching happens inside simdgen, where both
     representations exist: parse each XML intrinsic's form, normalize to the
     (mnemonic, operand kinds, evex flags) shape that expand_modifiers produced
     from the CSV, resolve to the registered instruction, and emit a selection arm
     that references the instruction's binding name (computed by the same
     binding/mangling code — guaranteeing no drift).

     1. simdgen: XML parsing and matching (tools/simdgen/)

     - Minimal dependency-free XML reader for this file's regular shape (same
     spirit as the hand-rolled CSV parser). Parse per intrinsic: name, tech,
     sequence, CPUID list, return/params (etype, type, varname,
     memwidth, immwidth, immtype), <instruction> list (mnemonic, form,
     xed), category.
     - Apply scope filter (CPUID) FIRST (this also excludes out-of-scope tokens
     like tmm/bnd), then the skip policy above.
     - Form matching — verified rules (parse_args itself cannot be reused
     verbatim since it also consumes the CSV's operand-encoding columns; reuse
     the token vocabulary + extract_modifiers, plus these normalizations):
       - bare k operand token (434 uses) → K (CSV only has k0..k7); bare
     {k} modifier (2,354 uses) → flags.k (CSV uses {k1}/{k2}; today's
     modifier parser would silently drop {k}).
       - match on (mnemonic, operand kinds ignoring memory width, modifiers):
     the XML's mem-width tokens are unreliable (53 masked-load forms say m64
     where the CSV says m256/m512; 60 truncating/compress stores use
     element widths).
       - GPR-width normalization: XML r8/r16 broadcast sources map to CSV
     reg → Temp [|R64|] (bindings vpbroadcastb_*_r64 etc.).
       - fix up the 18 forms that omit the k source operand entirely
     (movm_epi*, broadcastmb/mw — CSV correctly has k1).
       - modifiers can appear on any operand position, including the immediate
     (imm8 {sae}) and sources (ymm {er}, r64 {er}) — strip them
     position-independently.
       - VEX-shadowing: print_all (simdgen.ml:730) prefers VEX over EVEX on
     binding-name collisions; the matcher must detect when a required EVEX
     variant was shadowed and hard-fail (or use the VEX twin only when
     semantically equivalent, i.e. unmasked).
     - Multiple <instruction> entries (255 in scope): ALL are
     differing-mnemonic alternatives with identical operand shapes — 224 FMA
     (132/213/231), 30 permutex2var (VPERMI2/VPERMT2), 1 comi. The choice is
     forced by which C argument is the merge/overwrite target: mask_fmadd(a,b,c,k)
     → dst=a → VFMADD132/213 with the matching operand assignment;
     mask3_fmadd(a,b,c,k) → dst=c → VFMADD231 args [c;a;b;k];
     maskz_/plain → 213 by convention. permutex2var: dst=idx → VPERMI2,
     dst=a → VPERMT2. Encode as per-family operand-role rules in the generator.
     - Argument reordering — four verified mask shapes (histogram-audited;
     {z} forms have zero exceptions):
       a. canonical merge (src, k, a, b) → binding <mnem>..._K ~z:false
     (dst-as-src prepended, K appended by print_one/expand_mask) → args
     [src; a; b; k];
       b. zero-masking (k, a, b) → ~z:true → [a; b; k];
       c. k-destination masked ops (228 compares/tests, form k {k}, ...): the
     _K binding has NO ~z and no prepended dst (res = Res [K]), e.g.
     vcmpps_K_Z_Zm512_K; C (k1, a, b, imm) → [a; b; k1];
       d. non-first merge targets: blends (18; C (k, a, b), merge source is
     SRC1 → emit [a; a; b; k], duplicated arg), mask3_ FMA (64),
     mask2_permutex2var (15) — handled by the per-family role rules.
     In generated-code style each arm just pattern-matches the arg list and
     reorders/duplicates inline; no permutation machinery is needed.
     - Immediates (always last in C order — zero exceptions):
       - 94 intrinsics have 2–3 imm params (imm8+sae range/roundscale/fixupimm
     families; getmant packs two C enums into ONE hardware imm8 as
     sign<<2 | interval). The OCaml convention: all imm params come first,
     in C order; the generated arm pops N constants (generalize
     extract_constant to be called repeatedly) and combines/validates them.
       - imm ranges from immtype (_MM_CMPINT → 0..7, _CMP_ → 0..31,
     _MM_FROUND*, _MM_INDEX_SCALE → {1,2,4,8}, _MM_MANTISSA_*, ...)
     with immwidth as fallback (absent on 687/1,094 imm params) and 255 as
     last resort.
       - _MM_FROUND embedded-rounding arms dispatch the extracted value to
     pre-applied rounding variants (8/9/10/11 → Rnd_near/down/up/zero) and
     map _MM_FROUND_CUR_DIRECTION (4) to the non-ER instruction; {sae}
     similarly (~sae:() variant vs plain).

     2. The generated selection code (amd64_simd_intrins.ml)

     Per discussion: no structured intrinsic records — the generated artifact is a
     plain name → instruction match in the style of the existing
     select_operation_* arms:

     | "caml_mm512_add_ps" -> instr vaddps_Z_Z_Zm512 args
     | "caml_mm512_mask_add_ps" ->
       (match args with
        | [src; k; a; b] -> instr (vaddps_Z_Z_Zm512_K ~z:false) [src; a; b; k]
        | _ -> bad_arity op)
     | "caml_mm512_cmp_ps_mask" ->
       let i, args = extract_constant args ~max:31 op in
       instr vcmpps_K_Z_Zm512_K ~i args

     Representation details:

     - The ~z/~rnd/~sae:() bindings in amd64_simd_instrs.ml are functions
     by design — abstracted to avoid materializing the full combinatorial space
     of instruction records at top level. Instruction equality no longer relies
     on physical identity, so generated arms simply apply them inline at
     selection time (instr (vaddps_Z_Z_Zm512_K ~z:false) [...],
     instr (vaddps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) [...]); no top-level
     pre-application.
     - Module placement constraint (verified): the ocamloptcomp modules list
     (root dune:609) is arch-unconditional, so a file in tools/simdgen/ is
     also compiled on arm64 builds where Arch/Operation.Specific (Isimd _)
     refer to arm64. Therefore the generated file must depend only on
     Amd64_simd_defs/Amd64_simd_instrs — emit the match inside a functor
     parameterized over a tiny abstract sig (type expr, val instr : instr -> ?i:int -> expr list -> result option, val extract_constant,
     val simd_load/simd_store, val seq, ...). Arms pattern-match expr list
     directly (list reordering is type-agnostic). simd_selection.ml
     instantiates the functor with its existing helpers; on arm64 the functor is
     simply never applied. (Alternative if the functor proves awkward: generate
     into backend/amd64/ with a new dune rule there, so it's only built on
     amd64.)
     - Emit an .mli (or a tight functor result sig) so -warn-error +A in the
     root env stays clean.
     - Scale: ~3,700 arms is the same construct the hand-written selectors use
     (~900 arms); shard into sub-functions only if the single match turns out to
     stress the compiler.

     3. Selection integration (backend/amd64/simd_selection.ml)

     1. Instantiate the generated functor and chain it last in
     select_operation_cfg's or_else chain (simd_selection.ml:1123) so
     existing intrinsics keep priority. The chain is already invoked for
     [@@builtin] externals from cfg_selection.ml:414 (native-only;
     builtin=true from typing/primitive.ml:147).
     2. Extension gating per arm via Arch.Extension.enabled_instruction
     (arch.ml:207-229), which ANDs over the instr's ext array
     (multi-extension instrs, e.g. AVX512F+AVX512VL). Threaded through the
     functor sig. Note the failure mode when disabled: selection returns
     None → ordinary extcall → link error against the (nonexistent) C
     symbol — same as existing SSE/AVX intrinsics; state this in docs rather
     than claiming a diagnostic.
     3. pseudoregs_for_instr (simd_selection.ml:1167) is fully generic over
     instr records (verified incl. K-register Temp args, Arg result
     aliasing, VM 2-register operands) — no per-intrinsic code.
     4. Memory forms (Phases 2–3): simd_load/simd_store with
     Arch.identity_addressing; gathers/scatters with
     Iindexed2scaled (scale, 0) (AVX2 precedent simd_selection.ml:880-903).
     Verified: the emitter walks instruction operands and consumes the
     addressing registers at the (single) mem-capable operand's position, so
     the generated arm must place the address expression(s) at that position;
     purity comes from Simd.Mem (Load→load, Store→store) automatically.
     Before Phase 2, verify Isimd_mem handles a merge-masked load (vector
     src + mask + address) — flagged as residual risk by review.
     5. Flag-readers: new Simd.Seq ids (Kortestz/Kortestc/Ktestz/Ktestc ×
     B/W/D/Q) + SETcc emission in emit.ml, following the Ptest pattern.
     Note the CSV rows for KORTEST/KTEST erroneously mark the first k operand
     (w) — fix amd64/amd64.csv while adding these.

     4. Naming and signature mapping (C → OCaml)

     - Name: "caml" ^ intel_name. Verified: zero collisions between the 4,092
     generated names and all existing caml_* literals in simd_selection.ml /
     cmm_builtins.ml; generator re-asserts this (also against runtime/
     symbols) at generation time.
     - Types (verified against existing conventions in
     oxcaml/tests/simd/amd64/builtins.ml, load_store.ml, avx512/*.ml):
       - __m512/__m512d/__m512i → float32x16/float64x8/by-etype
     (int8x64, int16x32, int32x16, int64x8); same scheme for
     __m256*/__m128* onto existing types.
       - __mmask8/16/32/64 → mask (single width-agnostic type; C-boundary
     ABI passes masks in GPRs, handled by proc.ml:212-225 — verified
     working via basic512.ml mask_and/mask_ret).
       - imm params → leading (int [@untagged]) (existing convention;
     extract_constant pops the list head), multiple imms in C order.
       - scalar data: r32 → (int32 [@unboxed]), r64 → (int64 [@unboxed]),
     sub-word lanes → (int [@untagged]), float → float32, double →
     float (boxed types with per-param attributes when mixed with
     untagged imms; whole-declaration [@@unboxed] only when uniform).
       - pointers → nativeint# (addr); void return → type void : void
     (both per load_store.ml:11-29).

     5. Non-1:1 C↔OCaml mapping (loads/broadcasts, as in AVX2)

     - Register-form intrinsics map 1:1 (after the shape rules in §1).
     - Memory-form C intrinsics (loadu/store/expandloadu/compressstoreu/gathers/
     scatters) become OCaml intrinsics taking an addr argument, selected as
     Isimd_mem; Intel names are already distinct per form.
     - Instruction-level broadcast-memory variants (m32bcst/{1toN}) have no
     dedicated C intrinsics — the CSV-derived bcst variants go unreferenced.
     - Mask↔integer conversions (_cvtu32_mask16, kmov forms) coexist with the
     existing caml_mask_of_int64/caml_int64_of_mask builtins
     (cmm_builtins.ml:340); emit them for C-name parity unless a conflict
     arises.

     6. Generated tests (oxcaml/tests/simd/avx512/intrins/, new dir)

     Strategy: the C compiler's intrinsic is the oracle — a generalization of the
     existing float64_reference.ml + stubs.c pattern (OCaml builtin vs real
     _mm_max_pd), but generated. For each emitted intrinsic:

     - an OCaml external per the signature mapping, plus a
     BUILTIN(caml_<name>) { assert(0); } fallback symbol in the generated
     stubs (closures for [@@builtin] externals must link — cf. stubs512.c);
     - a C wrapper calling the real Intel intrinsic
     (__m512 ctest_mm512_add_ps(__m512 a, __m512 b)), called via a plain
     (non-builtin) [@@unboxed] external — the 512-bit/mask extcall ABI is
     already exercised by stubs512.c; wrappers live under
     #ifdef ARCH_AVX512 like the existing stubs;
     - a check comparing results bitwise on a deterministic input corpus
     (vectors via vec512_w0..w7-style extractors; masks via
     caml_int64_of_mask). Both sides execute the same hardware instruction,
     so results (incl. NaN payloads) must match exactly.

     Immediates need compile-time constants on both sides → one C wrapper + one
     OCaml call site per (intrinsic, imm-tuple). Value selection: all 32
     predicates for _CMP_; 0..7 for _MM_CMPINT; rounding/sae using the exact
     C macro combinations GCC/clang accept (_MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC = 8, ..., _MM_FROUND_CUR_DIRECTION) — arbitrary
     integers may be rejected by the C compiler; {1,2,4,8} for scales;
     {0, 1, mid, max} for plain imm8. Masks: {0, all-ones, 0xA5.., 0x5A..}
     via caml_mask_of_int64. Input corpus: hand-written test_values.ml with
     fixed edge-case + seeded-random vectors per element type (precedent:
     check_floats' fixed table + seeded random in utils.ml).

     Infrastructure (corrected per review):

     - Don't check the generated test sources in: generate the test .ml/.c
     files as dune rule targets inside the intrins dir (rules depend on
     simdgen.exe, x86-intel.xml, amd64/amd64.csv); only a dune.inc
     (from a gen/gen_dune.ml-style generator) is checked in, kept fresh by the
     existing (rule (alias runtest) (action (diff dune.inc dune.inc.gen)))
     pattern (avx512/dune:41-49). This removes cross-directory promotion
     entirely.
     - Group tests into a reasonable number of files (e.g. by category/extension)
     — no fine-grained sharding policy needed. Build variants: plain +
     -internal-assembler (the latter doubles as an EVEX encoding cross-check
     vs GAS).
     - C flags: add -mavx512cd to the existing set (confirmed missing from
     avx512/dune:26); OCaml side needs no -favx512cd (implication closure
     from -favx512f, arch.ml:150-163).
     - C oracle compiler: host/CI gcc 8.5 lacks many in-scope intrinsic
     names (_mm512_loadu_epi8, several _k*_mask* ops — added in gcc 9/10).
     Decide: compile the oracle stubs with clang (present on hosts) or add
     per-family version guards / generation-time probing; document the minimum
     and check what CI uses.
     - Same enabled_if as existing avx512 tests (amd64, non-macos, main
     context); assumes AVX512-capable build/test hosts, as today.
     - Phase 2 needs vec_aligned_alloc duplicated into the intrins stubs (it
     lives only in the parent dir's stubs archive).
     - Negative tests in oxcaml/tests/simd/errors/: out-of-range immediates
     (Bad_immediate). Note: gating negative tests must use -fno-avx512f
     (implication closure removes the whole group).

     7. Phasing

     1. Phase 0 — generator infra: XML parser, scope filter, classifier,
     matcher, coverage report + checked-in skip-list artifact; no compiler
     change. Success: report accounts for all 4,092.
     2. Phase 1 — register intrinsics (~3,364): generated functor match,
     chaining, Simd.Seq flag-readers, CSV fixes (KORTEST (w) quirk),
     generated tests. Includes masked shapes 1–4, er/sae, multi-imm, k-ops,
     GPR↔vector/mask moves.
     3. Phase 2 — loads/stores (272 minus the 18 no-pointer special cases):
     Isimd_mem arms incl. masked loads/stores; verify merge-masked-load
     support first; vec_aligned_alloc-based tests.
     4. Phase 3 — gathers/scatters (80): Iindexed2scaled + mask args
     (VSIB operands already modeled, loc_reg_count = 2); final audit of the
     skip list.

     Each phase ends green: make -s boot-compiler, make -s fmt,
     make runtest (dune tests — make -s test does NOT run
     oxcaml/tests/simd), and make -s test.

     Success criteria

     1. Coverage: every in-scope intrinsic (4,092) is either emitted or on the
     checked-in skip list with a reason; unknown forms are hard errors.
     2. Correctness: every emitted intrinsic has a generated test comparing
     the OCaml builtin against the real C intrinsic bit-for-bit over the corpus
     and enumerated imm/mask values; all pass on AVX512 hardware under both
     GAS and the internal assembler.
     3. Integration: recognition happens via the generated match only (plus
     the curated Simd.Seq flag-readers); extension gating behaves like
     existing intrinsics (no selection → extcall → link error, documented);
     bad immediates produce Bad_immediate compile errors.
     4. Reproducibility: amd64_simd_intrins.ml is promoted+checked in like
     amd64_simd_instrs.ml; test sources are build-time rule targets;
     dune.inc freshness enforced under the dune runtest alias (CI
     ci-no-coverage runs it).
     5. No regressions: make runtest + make -s test pass; existing
     SSE/AVX/AVX2 behavior unchanged; make -s boot-compiler time not
     meaningfully degraded.

     Key implementation gotchas (all verified against code/data)

     - ~z/~rnd/~sae:() bindings are intentionally functions (avoiding a
     combinatorial record explosion); instruction equality doesn't rely on
     physical identity, so apply them inline in generated arms.
     - Merge-masking arg order (dst-as-src prepended, K appended) holds only for
     vector-destination bindings; k-destination _K bindings have no ~z and
     no prepend — two different masked shapes, plus blends/mask3/mask2 roles.
     - print_all prefers VEX over EVEX on binding-name collisions — match on
     operand shape/flags, not names; detect shadowing.
     - extract_constant pops the head of the Cmm args → imm-first signatures;
     immediates must reach the backend as Cconst_int literals.
     - XML data defects: bare k/{k} tokens, wrong mem widths (53 loads / 60
     stores), omitted k operands (18), fake SVML instructions (2), r8/r16 vs
     CSV reg.
     - The generated module is compiled on all arches → functor over an abstract
     sig; depend only on Amd64_simd_defs/Amd64_simd_instrs at module level.
     - Root dune:609 ocamloptcomp modules list must gain amd64_simd_intrins;
     tools/simdgen/dune rule deps must include x86-intel.xml.
     - fmt hygiene: add new generator modules to
     tools/simdgen/.ocamlformat-enable; add amd64_simd_intrins.ml to the
     scripts/80ch.sh skip list (only amd64_simd_instrs.ml is skipped
     today); oxcaml/tests/* is already 80ch-exempt.
     - flags512.sexp lacks -mavx512cd; masks pass in k-regs OCaml→OCaml but
     GPRs at C boundaries (automatic kmov insertion — no plumbing needed);
     Win64 extcalls with SIMD/mask args are fatal (tests stay Unix-only).
     - Licensing: confirm the Intel intrinsics data license permits deriving
     checked-in source; avoid embedding Intel description text verbatim in
     generated files.

     Files to create/modify

     ┌──────────────────────────────────────────────────────────────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
     │                               file                               │                                                            change                                                             │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ tools/simdgen/simdgen.ml (+ new module, e.g. simdgen_intrins.ml) │ XML parser, matcher, selection/test emission; add module to (modules ...) in tools/simdgen/dune                               │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ tools/simdgen/amd64_simd_intrins.ml(+.mli)                       │ generated functor with the selection match                                                                                    │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ tools/simdgen/dune                                               │ promote rule for the new file (deps: exe, CSV, XML)                                                                           │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ tools/simdgen/amd64/amd64.csv                                    │ fixes surfaced by matching (e.g. KORTEST/KTEST result marking)                                                                │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ root dune                                                        │ add amd64_simd_intrins to ocamloptcomp modules (line ~609)                                                                    │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ backend/amd64/simd.ml                                            │ Simd.Seq kortest/ktest ids (+ emit.ml SETcc emission)                                                                         │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ backend/amd64/simd_selection.ml                                  │ instantiate generated functor; chain last in select_operation_cfg; generalize extract_constant for multi-imm                  │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ oxcaml/tests/simd/avx512/intrins/                                │ new dir: dune, checked-in dune.inc + gen/, hand-written test_values.ml + support stubs; test shards generated as rule targets │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ oxcaml/tests/simd/avx512/dune (or intrins dune)                  │ -mavx512cd; oracle-compiler decision                                                                                          │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ oxcaml/tests/simd/errors/                                        │ negative imm tests                                                                                                            │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ scripts/80ch.sh, tools/simdgen/.ocamlformat-enable               │ fmt exemptions                                                                                                                │
     ├──────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ jane/doc/extensions/_09-simd/                                    │ document 512-bit/mask types, -favx512*, caml_<intel-name> surface                                                             │
     └──────────────────────────────────────────────────────────────────┴───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

     Verification

     1. make -s boot-compiler after each step; make -s fmt before finishing.
     2. Regenerate and diff: rebuild simdgen.exe, re-emit
     amd64_simd_intrins.ml, git diff clean; review the skip-list artifact.
     3. make runtest (dune @runtest — this is what runs oxcaml/tests/simd/*
     and the dune.inc freshness diffs), scoped iteration via
     dune runtest oxcaml/tests/simd/avx512/intrins --root=. --workspace=duneconf/main.ws;
     then full make -s test (ocamltest suite) for regressions.
     4. Spot-check emitted assembly for representative intrinsics: plain
     (caml_mm512_add_ps), merge- and zero-masked, k-destination compare,
     embedded-rounding, blend, mask3_ FMA, kandw, kortestz.
     5. Confirm the C oracle stubs compile with the chosen compiler on CI hosts.
