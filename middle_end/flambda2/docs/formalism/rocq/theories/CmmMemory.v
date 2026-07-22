(* CmmMemory.v -- 19-cmm-memory-gc.md: allocation (heap and local),
   the region stack, the moving garbage collector, and resource
   exhaustion.

   This file closes the extension hooks of Cmm.v's Section CmmMachine
   (head_ext / step_ext / alloc_fails) and defines the full
   ch. 15 + ch. 19 Cmm machine (cmem_step / cmem_run /
   cmem_unit_behaves).

   Conventions: rocq/CORRESPONDENCE.md.  Wave 2; owner: Milner. *)

(* String BEFORE List: String.length would otherwise shadow the list
   length used in the allocation rules (same fix as Cmm.v line 23). *)
From Stdlib Require Import ZArith Bool String List.
From Flambda2 Require Import Base Cmm.
Import ListNotations.
Local Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(* Region handles as machine words.
   ENCODING NOTE: the doc uses iota both as a region's identity in RR
   and as the word value Cbeginregion returns / Cendregion consumes.
   Region handles are Base atoms; region_word injects them into words
   by their id (injective, so the Cendregion operand determines the
   handle).  The ch. 20 correspondence RR ~ R is at handle level and
   never inspects the word. *)
Definition region_word (iota : region_handle) : Z :=
  match iota with Mk_region_handle n => Z.of_nat n end.

(* ------------------------------------------------------------------ *)
(* Memory helpers for allocation.  Unlike Cmm.v's mem_write_bytes
   (which requires every target byte mapped), mem_map_bytes EXTENDS
   the byte-store domain: allocation is what makes bytes mapped. *)

Fixpoint mem_map_bytes (B : fmap Z Z) (a : Z) (bs : list Z)
  : fmap Z Z :=
  match bs with
  | [] => B
  | b :: bs' => mem_map_bytes (fupd Z.eqb B a b) (a + 1) bs'
  end.

(* Tag n bytes starting at a as belonging to region iota
   (CM.Alloc.Local's "tagged as belonging to iota"). *)
Fixpoint mem_tag_bytes (L : fmap Z region_handle) (a : Z) (n : nat)
    (iota : region_handle) : fmap Z region_handle :=
  match n with
  | O => L
  | S k => mem_tag_bytes (fupd Z.eqb L a iota) (a + 1) k iota
  end.

(* "a ... unmapped in M": no byte of the range is mapped. *)
Definition range_unmapped (B : fmap Z Z) (a : Z) (n : nat) : Prop :=
  forall i : Z, 0 <= i < Z.of_nat n -> B (a + i) = None.

(* The byte image of one allocated field.
   ENCODING NOTE: the doc's M'[a + 8*(i-1)] = v_i glosses fields as
   word-sized.  Fields are laid out consecutively at their natural
   sizes -- word-sized values (words, floats, a float32 stored in the
   low bits of a zero-extended word) take 8 bytes, vectors take
   16/32/64 (the vector alloc_block_kinds) -- which agrees with the
   doc whenever every field is word-sized.  CV_tuple is not a storable
   field (None: no such Calloc argument is well-formed). *)
Definition alloc_field_bytes (v : cmm_value) : option (list Z) :=
  match v with
  | CV_word n => Some (le_bytes 8 (to_unsigned64 n))
  | CV_flt f => Some (le_bytes 8 (f64_to_bits f))
  | CV_flt32 f => Some (le_bytes 8 (f32_to_bits f))
  | CV_vec128 b => Some (le_bytes 16 (vec128_to_bits b))
  | CV_vec256 b => Some (le_bytes 32 (vec256_to_bits b))
  | CV_vec512 b => Some (le_bytes 64 (vec512_to_bits b))
  | CV_tuple _ => None
  end.

Fixpoint alloc_fields_bytes (vs : list cmm_value)
  : option (list Z) :=
  match vs with
  | [] => Some []
  | v :: vs' =>
    match alloc_field_bytes v, alloc_fields_bytes vs' with
    | Some bs, Some rest => Some (bs ++ rest)
    | _, _ => None
    end
  end.

(* Reclamation (CM.Region.End): unmap -- bytes and tags -- every byte
   belonging to a block allocated in one of the popped regions. *)
Definition region_dead (popped : list region_handle)
    (iota : region_handle) : bool :=
  existsb (region_handle_eqb iota) popped.

Definition mem_reclaim (M : cmm_mem) (popped : list region_handle)
  : cmm_mem :=
  MkMem
    (fun b => match mem_local M b with
              | Some iota =>
                  if region_dead popped iota then None
                  else mem_bytes M b
              | None => mem_bytes M b
              end)
    (fun b => match mem_local M b with
              | Some iota =>
                  if region_dead popped iota then None
                  else Some iota
              | None => None
              end).

(* An allocation point: the current expression's redex is a Calloc
   (used by CM.Alloc.GC's trigger and CM.Alloc.Exhaustion). *)
Definition alloc_point (c : cm_config) : Prop :=
  exists E mode kind vs,
    cc_expr c = cm_plug E (Cop (Calloc mode kind) (map Cval vs)).

(* ENCODING NOTE (sanctioned, per reviewer finding KF-009): the doc's
   "hdr carries caml_local" premise on CM.Alloc.Local is a constraint
   on the header WORD, whose bit layout belongs to R.Header (ch. 17,
   Representation.v).  As with gc_reloc, we introduce the predicate
   here as a Parameter and let ch. 17 constrain it (local bit set,
   per local_block_header). *)
Parameter header_is_local : Z -> Prop.

(* ------------------------------------------------------------------ *)
(* 19 s1-s2: the redex-level rules (Cmm.v's head_ext hook). *)

Inductive cmem_head : cm_config -> list cm_event -> cm_config -> Prop :=

  (** RULE CM.Alloc.Heap (STATUS normative) -- 19-cmm-memory-gc.md
      CODE backend/cmm.mli#Calloc
      CODE backend/cmm_helpers.ml#make_alloc_generic

      Premises, in doc order: the redex shape (conclusion pattern);
      a fresh, 8-byte-aligned, block bytes unmapped in M ("after a GC
      if needed": the fused CM.Alloc.GC step below collects and then
      allocates atomically); M' maps the header word at a-8 and the
      fields
      from a.  The freshness range includes the header word, and a is
      required to lie in the canonical unsigned address range that
      loads and stores canonicalize into (to_unsigned64). *)
  | CM_Alloc_Heap :
      forall kind hdr vs bs a ce chi M TT RR,
      alloc_fields_bytes vs = Some bs ->
      a mod 8 = 0 ->
      8 <= a /\ a + Z.of_nat (length bs) < 2 ^ 64 ->
      range_unmapped (mem_bytes M) (a - 8) (8 + length bs) ->
      cmem_head
        (CmCfg (Cop (Calloc CAM_heap kind)
                    (Cval (CV_word hdr) :: map Cval vs))
               ce chi M TT RR) []
        (CmCfg (Cval (CV_word a)) ce chi
               (MkMem (mem_map_bytes (mem_bytes M) (a - 8)
                         (le_bytes 8 (to_unsigned64 hdr) ++ bs))
                      (mem_local M))
               TT RR)

  (** RULE CM.Region.Begin (STATUS normative) -- 19-cmm-memory-gc.md
      CODE backend/cmm.mli#Cbeginregion
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Begin_region
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Begin_try_region
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#variadic_primitive

      "iota fresh": not an open region and not the tag of any live
      local block.  (The ghost-region and stack-alloc-disabled erasure
      paths of the NOTES never produce a Cbeginregion, so they need no
      rule here: the ghost lowering Cconst_int 0 is already a value.) *)
  | CM_Region_Begin :
      forall iota ce chi M TT RR,
      ~ In iota RR ->
      (forall b, mem_local M b <> Some iota) ->
      cmem_head
        (CmCfg (Cop Cbeginregion []) ce chi M TT RR) []
        (CmCfg (Cval (CV_word (region_word iota))) ce chi M TT
               (iota :: RR))

  (** RULE CM.Alloc.Local (STATUS normative) -- 19-cmm-memory-gc.md
      CODE backend/cmm.mli#Calloc
      CODE backend/cmm_helpers.ml#local_block_header
      CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#alloc_mode_for_allocations_to_cmm

      Premises, in doc order: the redex shape with RR = iota :: _
      (conclusion pattern); a fresh ("in region iota's arena":
      ENCODING NOTE -- arenas are not modelled, freshness is w.r.t.
      the byte store, and membership in the arena is exactly the
      mem_local tagging); "hdr carries caml_local" is the
      header_is_local premise (a Parameter constrained by R.Header,
      ch. 17; see the ENCODING NOTE above); M' writes header and
      fields as CM.Alloc.Heap and tags the block's bytes as iota's. *)
  | CM_Alloc_Local :
      forall kind hdr vs bs a iota RR0 ce chi M TT,
      alloc_fields_bytes vs = Some bs ->
      header_is_local hdr ->
      a mod 8 = 0 ->
      8 <= a /\ a + Z.of_nat (length bs) < 2 ^ 64 ->
      range_unmapped (mem_bytes M) (a - 8) (8 + length bs) ->
      cmem_head
        (CmCfg (Cop (Calloc CAM_local kind)
                    (Cval (CV_word hdr) :: map Cval vs))
               ce chi M TT (iota :: RR0)) []
        (CmCfg (Cval (CV_word a)) ce chi
               (MkMem (mem_map_bytes (mem_bytes M) (a - 8)
                         (le_bytes 8 (to_unsigned64 hdr) ++ bs))
                      (mem_tag_bytes (mem_local M) (a - 8)
                         (8 + length bs) iota))
               TT (iota :: RR0))

  (** RULE CM.Region.End (STATUS normative) -- 19-cmm-memory-gc.md
      CODE backend/cmm.mli#Cendregion
      CODE middle_end/flambda2/terms/flambda_primitive.mli#End_region
      CODE middle_end/flambda2/terms/flambda_primitive.mli#End_try_region
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive

      Pops down to and discards region iota (RR = RR1 ++ iota :: RR0),
      reclaiming -- unmapping, bytes and tags -- every block allocated
      in iota or in a region above it.  Result is unit (word 1).
      (As with CM.Region.Begin, the ghost / stack-alloc-disabled
      erasure paths never produce a Cendregion.) *)
  | CM_Region_End :
      forall iota RR1 RR0 ce chi M TT,
      cmem_head
        (CmCfg (Cop Cendregion [Cval (CV_word (region_word iota))])
               ce chi M TT (RR1 ++ iota :: RR0)) []
        (CmCfg (Cval (CV_word 1)) ce chi
               (mem_reclaim M (RR1 ++ [iota])) TT RR0).

(* ------------------------------------------------------------------ *)
(* 19 s3: the garbage collector (Cmm.v's step_ext hook). *)

(* ENCODING NOTE: sanctioned oracle (coordinator-approved).  The doc
   models the GC "axiomatically ... Modelled, not implemented, here":
   an unconstrained existential step would ALLOW behaviors the doc
   forbids (arbitrary memory scrambling at every allocation point), so
   the oracle Parameter -- constrainable later, permitting nothing by
   itself -- is the faithful reading.
   gc_reloc phi c c' is the axiomatized moving collector,
   carrying the rule's conditions
     (i)   if H ~_L M then H ~_{phi o L} M'  (only L moves),
     (ii)  every Val root in ce, every Val word pending in the
           expression (already-evaluated operands of the plugged
           context, including the allocation's own field operands --
           the collected expression is the original with those words
           rewritten by phi), and every Val field in M' is updated by
           phi,
     (iii) Int/Float/naked values and their words are unchanged,
     (iv)  blocks unreachable from the Val roots may be dropped,
   none of which is statable before the machtype discipline and the
   representation relation ~ of ch. 17 exist; Representation.v states
   the ~-preservation conclusion as the Admitted conjecture the rule's
   STATUS marks it as.  Condition (v) -- local blocks are not moved --
   IS statable against mem_local and is a premise of the constructor
   below rather than folded into the oracle.
   TRIPWIRE (KF-048, reviewer watch W-31): TC.Let.Subst's Calloc
   exemption (cmm_touches_stacks, Cmm.v) depends on gc_reloc
   contributing NO provable steps.  An axiom cashing condition (iv)
   as provable EXISTENCE of a dropping step would break the
   exemption by concrete witness -- GC-assisted address recycling
   can restore a pinned M around an RR-reading local allocation --
   and flip that Admitted statement false with no change to the
   guard or ch. 18; constrain this oracle only alongside a guard
   revisit. *)
Parameter gc_reloc :
  fmap Z Z -> cm_config -> cm_config -> Prop.

Inductive cmem_gc : cm_config -> list cm_event -> cm_config -> Prop :=

  (** RULE CM.Alloc.GC (STATUS conjectured) -- 19-cmm-memory-gc.md
      CODE backend/cmm_helpers.ml#make_alloc_generic
      CODE backend/cmm.mli#machtype_component

      An allocation step may be preceded by a collection, ATOMICALLY:
      at an allocation point, the machine may take a single fused step
      that first relocates by some phi on live Val-reachable blocks
      satisfying (i)-(iv) (the gc_reloc oracle above) and (v): local
      blocks are not moved (phi is undefined on region-tagged bytes),
      and then performs the allocation (CM.Alloc.Heap/Local, the
      cmem_head premise, mirroring Cmm.v's CM.Context plugging) in the
      collected memory.  There is NO standalone collection transition,
      so every collection makes allocation progress and a run cannot
      stutter in GC steps.  The fused step still corresponds to the
      one Flambda allocation step in the ch. 20 simulation. *)
  | CM_Alloc_GC :
      forall phi e ce chi M TT RR E' redex' ce' M' tr e_res M'',
      alloc_point (CmCfg e ce chi M TT RR) ->
      gc_reloc phi (CmCfg e ce chi M TT RR)
                   (CmCfg (cm_plug E' redex') ce' chi M' TT RR) ->
      (forall b iota, mem_local M b = Some iota -> phi b = None) ->
      (exists mode kind vs,
         redex' = Cop (Calloc mode kind) (map Cval vs)) ->
      cmem_head (CmCfg redex' ce' chi M' TT RR) tr
                (CmCfg e_res ce' chi M'' TT RR) ->
      cmem_gc (CmCfg e ce chi M TT RR) tr
              (CmCfg (cm_plug E' e_res) ce' chi M'' TT RR).

(* ------------------------------------------------------------------ *)
(* The full ch. 15 + ch. 19 machine: Cmm.v's Section hooks closed. *)

Definition cmem_step (P : cmm_program)
  : cm_config -> list cm_event -> cm_config -> Prop :=
  cm_step P cmem_head cmem_gc.

Definition cmem_run (P : cmm_program)
  : cm_config -> list cm_event -> cm_config -> Prop :=
  cm_run P cmem_head cmem_gc.

Definition cmem_diverges (P : cmm_program) : cm_config -> Prop :=
  cm_diverges P cmem_head cmem_gc.

Definition cmem_reacts (P : cmm_program)
  : cm_config -> cm_event_stream -> Prop :=
  cm_reacts P cmem_head cmem_gc.

(* ------------------------------------------------------------------ *)
(* 19 s4: the Addr discipline. *)

(* The GC write barriers: the alloc=false external calls that consume
   a field address (CM.Addr.NoSurvive NOTES).  The doc's
   addr_array_initialize is a HELPER (cmm_helpers.ml), not an extern:
   it emits a Cextcall to caml_initialize, so it needs no entry.
   ENCODING NOTE: Cmm.v's Cextcall drops the alloc flag, so
   "alloc=true Cextcall" is approximated as any external call other
   than these barriers.  The whitelist is an under-approximation of
   the dropped flag (fewer calls classified alloc=false than the code
   allows), restorable by re-adding the alloc flag to Cextcall if
   ch. 18 ever needs it. *)
Definition gc_write_barriers : list string :=
  ["caml_modify"%string; "caml_initialize"%string].

Definition is_gc_write_barrier (f : string) : bool :=
  existsb (String.eqb f) gc_write_barriers.

(* Does e syntactically contain a GC-permitting point: a Calloc, a
   Capply, or an external call that may allocate? *)
Fixpoint expr_allocates (e : cmm_expr) : bool :=
  let fix go (es : list cmm_expr) : bool :=
    match es with
    | [] => false
    | e' :: es' => expr_allocates e' || go es'
    end in
  let fix goh (hs : list cmm_static_handler) : bool :=
    match hs with
    | [] => false
    | SHandler _ _ b _ :: hs' => expr_allocates b || goh hs'
    end in
  match e with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _
  | Cconst_float32 _ | Cconst_vec128 _ | Cconst_vec256 _
  | Cconst_vec512 _ | Cconst_symbol _ | Cvar _ | Cinvalid _
  | Cval _ => false
  | Clet _ e1 e2 => expr_allocates e1 || expr_allocates e2
  | Csequence e1 e2 => expr_allocates e1 || expr_allocates e2
  | Ctuple es => go es
  | Cop (Calloc _ _) _ => true
  | Cop (Capply _ _ _) _ => true
  | Cop (Cextcall f _) args =>
      negb (is_gc_write_barrier f) || go args
  | Cop _ args => go args
  | Cifthenelse c1 e1 e2 =>
      expr_allocates c1 || expr_allocates e1 || expr_allocates e2
  | Cswitch s _ cases => expr_allocates s || go cases
  | Ccatch _ hs body => goh hs || expr_allocates body
  | Cexit _ args _ => go args
  end.

(* The syntactic Addr discipline of CM.Addr.NoSurvive's premise: every
   Cadda occurs only in a Cadda CHAIN that is the immediately consumed
   address operand of a Cload / Cstore / GC-write-barrier Cextcall
   (never let-bound or otherwise bare), and every operand evaluated
   while a link of the chain is live (each chain offset, left-to-right
   15 s4, and every operand after the address) is free of
   GC-permitting points, so the address is never held across one.
   Chains arise from array_indexing (cmm_helpers.ml), which emits
   nested Cadda -- Cadda [Cadda [ptr; scaled]; const] -- on ordinary
   dynamic-index array loads/stores; each Cadda is consumed at once
   and the chain's root is a Val base.
   ENCODING NOTE: the addr_pos flag means "this position may be a
   Cadda chain"; it is set exactly on the address operand of the
   three consuming forms and on a chain's base, and a bare Cadda
   anywhere else is rejected.  Requiring every chain offset to be
   non-allocating is marginally stronger than the discipline needs
   for the innermost offset (no Addr is live yet while it runs), but
   matches the emitted shapes (scaled indices and constants). *)
Definition is_cadda (e : cmm_expr) : bool :=
  match e with Cop Cadda [_; _] => true | _ => false end.

Fixpoint expr_addr_ok' (addr_pos : bool) (e : cmm_expr) : bool :=
  let fix go (es : list cmm_expr) : bool :=
    match es with
    | [] => true
    | e' :: es' => expr_addr_ok' false e' && go es'
    end in
  let fix go_noalloc (es : list cmm_expr) : bool :=
    match es with
    | [] => true
    | e' :: es' =>
        expr_addr_ok' false e' && negb (expr_allocates e')
        && go_noalloc es'
    end in
  let fix goh (hs : list cmm_static_handler) : bool :=
    match hs with
    | [] => true
    | SHandler _ _ b _ :: hs' => expr_addr_ok' false b && goh hs'
    end in
  match e with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _
  | Cconst_float32 _ | Cconst_vec128 _ | Cconst_vec256 _
  | Cconst_vec512 _ | Cconst_symbol _ | Cvar _ | Cinvalid _
  | Cval _ => true
  | Clet _ e1 e2 =>
      expr_addr_ok' false e1 && expr_addr_ok' false e2
  | Csequence e1 e2 =>
      expr_addr_ok' false e1 && expr_addr_ok' false e2
  | Ctuple es => go es
  | Cop Cadda [b; o] =>
      addr_pos && expr_addr_ok' true b
      && expr_addr_ok' false o && negb (expr_allocates o)
  | Cop Cadda _ => false
  | Cop (Cload _ _ _) [a] => expr_addr_ok' true a
  | Cop (Cstore _ _) (a :: rest) =>
      expr_addr_ok' true a
      && (if is_cadda a then go_noalloc rest else go rest)
  | Cop (Cextcall f _) (a :: rest) =>
      (if is_cadda a then is_gc_write_barrier f else true)
      && expr_addr_ok' true a
      && (if is_cadda a then go_noalloc rest else go rest)
  | Cop _ args => go args
  | Cifthenelse c1 e1 e2 =>
      expr_addr_ok' false c1 && expr_addr_ok' false e1
      && expr_addr_ok' false e2
  | Cswitch s _ cases => expr_addr_ok' false s && go cases
  | Ccatch _ hs body => goh hs && expr_addr_ok' false body
  | Cexit _ args _ => go args
  end.

Definition expr_addr_ok : cmm_expr -> bool := expr_addr_ok' false.

(** RULE CM.Addr.NoSurvive (STATUS normative) -- 19-cmm-memory-gc.md
    CODE backend/cmm.mli#machtype_component
    CODE backend/cmm_helpers.ml#field_address
    CODE backend/cmm_helpers.ml#setfield_computed

    ENCODING NOTE: the machine is untyped, so "Addr-typed value" has
    no runtime representation; the rule's premise -- to_cmm's Cadda
    results are recomputed inline and consumed at once, never
    let-bound or held across an alloc=true point -- is transcribed as
    the syntactic predicate expr_addr_ok above, and the rule is stated
    as: the discipline is an invariant of execution (for programs all
    of whose code satisfies it).  The doc's conclusion -- CM.Alloc.GC's
    phi never invalidates a subsequently used value -- follows because
    a discipline-satisfying configuration never holds a pending Cadda
    address while an operand containing a GC-permitting point remains
    to be evaluated, and control transfers discard pending contexts. *)
Theorem CM_Addr_NoSurvive :
  forall (P : cmm_program),
  (forall s fd, cp_funs P s = Some fd ->
     expr_addr_ok (fd_body fd) = true) ->
  forall c0 tr c,
  expr_addr_ok (cc_expr c0) = true ->
  cc_kenv c0 = kenv_empty ->
  cmem_run P c0 tr c ->
  expr_addr_ok (cc_expr c) = true.
Admitted.

(* ------------------------------------------------------------------ *)
(* 19 s5: resource exhaustion (Cmm.v's alloc_fails hook). *)

(** RULE CM.Alloc.Exhaustion (STATUS normative) -- 19-cmm-memory-gc.md
    CODE backend/cmm_helpers.ml#make_alloc_generic

    An allocation that cannot be satisfied -- no free memory / stack
    overflow -- halts the run: any configuration whose redex is a
    Calloc (out of memory, "after a possible CM.Alloc.GC": the failing
    run simply takes no GC step) or a Capply (stack overflow) may be
    the final configuration of a resource-exhausted run
    (CM.Unit.Final's CMO_resource_exhaustion, Cmm.v).
    ENCODING NOTE: call redexes are included as the stack-overflow
    points; exhaustion inside a callee also surfaces at the enclosing
    Capply redex (the nested cm_returns premise is simply never
    satisfied), at the cost of dropping the failing callee's partial
    event trace from the outcome -- ch. 20 states the simulation
    modulo this outcome, so the trace loss is not load-bearing. *)
Definition cmem_alloc_fails (c : cm_config) : Prop :=
  (exists E mode kind vs,
     cc_expr c = cm_plug E (Cop (Calloc mode kind) (map Cval vs)))
  \/
  (exists E ty rc callees vs,
     cc_expr c =
       cm_plug E (Cop (Capply ty rc callees) (map Cval vs))).

(* The whole-unit observation of the full machine (CM.Unit.Final with
   all three hooks closed). *)
Definition cmem_unit_behaves (P : cmm_program)
  : cm_config -> cm_outcome -> Prop :=
  cm_unit_behaves P cmem_head cmem_gc cmem_alloc_fails.
