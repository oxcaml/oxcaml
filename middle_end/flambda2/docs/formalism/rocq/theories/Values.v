(** * Values.v — runtime values, heap, machine configuration

    Mechanizes 04-opsem.md sections 1.1-1.8 (runtime values, heap H,
    environment rho, continuation environment K, trap stack T, region
    stack R, machine configuration) and the refined heap-object taxonomy
    of 06-primitives-memory.md "Heap objects".

    Owner: Plotkin (wave 2).  Imports: Base, Syntax. *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax.
Import ListNotations.

Set Implicit Arguments.

Open Scope Z_scope.

(** ** List and option helpers used by the machine and the prim files *)

(** Evaluate/transform a list, failing if any element fails. *)
Fixpoint option_traverse (A B : Type) (f : A -> option B) (l : list A)
  : option (list B) :=
  match l with
  | [] => Some []
  | a :: l' =>
      match f a, option_traverse f l' with
      | Some b, Some bs => Some (b :: bs)
      | _, _ => None
      end
  end.

(** Functional update of list element [i]; [None] if out of range.
    Used by the block/array write denotations (ch. 06). *)
Fixpoint list_set (A : Type) (i : nat) (a : A) (l : list A)
  : option (list A) :=
  match i, l with
  | O, _ :: l' => Some (a :: l')
  | S i', b :: l' =>
      match list_set i' a l' with
      | Some l'' => Some (b :: l'')
      | None => None
      end
  | _, [] => None
  end.

(** ** Runtime values — 04-opsem.md §1.1

    Grammar owned by ch. 04 (also quoted in README.md "Runtime values
    and heap objects").  One constructor per value form, doc names with
    a [V_] prefix. *)

Inductive value : Type :=
| V_tagged_imm (n : Z)        (* kind Value: an OCaml "int" *)
| V_naked_imm (n : Z)         (* kind Naked_immediate *)
| V_naked_int8 (n : Z)
| V_naked_int16 (n : Z)
| V_naked_int32 (n : Z)
| V_naked_int64 (n : Z)
| V_naked_nativeint (n : Z)
| V_naked_float (f : float64)
| V_naked_float32 (f : float32)
| V_naked_vec128 (b : vec128)
| V_naked_vec256 (b : vec256)
| V_naked_vec512 (b : vec512)
| V_ptr (a : address)         (* pointer to the heap object at a *)
| V_clos (l : location) (f : function_slot)
                              (* pointer to function slot f of the
                                 set-of-closures block at l *)
| V_null                      (* the null pointer (for `_ or_null`) *)
| V_region (i : region_handle)
| V_rec_info.                 (* inert stand-in for a Rec_info_expr.t *)

(** The kind of a runtime value (ch. 03 kind grammar; 04-opsem.md
    §1.1's value forms are listed by kind).  Every value form has
    exactly one kind, so this is a total function; the prim rules'
    premises "kind(v) = kappa" are [value_kind v = kappa]. *)
Definition value_kind (v : value) : kind :=
  match v with
  | V_tagged_imm _ | V_ptr _ | V_clos _ _ | V_null => K_value
  | V_naked_imm _ => K_naked_immediate
  | V_naked_int8 _ => K_naked_int8
  | V_naked_int16 _ => K_naked_int16
  | V_naked_int32 _ => K_naked_int32
  | V_naked_int64 _ => K_naked_int64
  | V_naked_nativeint _ => K_naked_nativeint
  | V_naked_float _ => K_naked_float
  | V_naked_float32 _ => K_naked_float32
  | V_naked_vec128 _ => K_naked_vec128
  | V_naked_vec256 _ => K_naked_vec256
  | V_naked_vec512 _ => K_naked_vec512
  | V_region _ => K_region
  | V_rec_info => K_rec_info
  end.

(** ** Heap objects — 06-primitives-memory.md "Heap objects"

    Ch. 04 §1.2 gives the coarse taxonomy; ch. 06 refines it.  This is
    the refined grammar (one constructor per object form, doc names
    with an [HO_] prefix). *)

(** [lazy_block_tag] (for [HO_Lazy] below) comes from Syntax.v, which
    owns it because [UP_make_lazy] carries it as a payload. *)

(** The per-function-slot "arity-info" of a [Closures] object
    (04-opsem.md §1.2: "the code id it will call and the arity
    information needed for indirect dispatch (§4.2)").
    ENCODING NOTE: the doc leaves "arity-info" abstract; the
    OS.Apply.IndirectUnknownArity rules compare the
    argument count against the callee's arity, with a tupled-function
    adjustment, so we record the callee's params_arity and the tupled
    flag. *)
Record arity_info : Type := mk_arity_info {
  ai_params_arity : arity;    (* Code_metadata.params_arity *)
  ai_is_tupled : bool         (* Code_metadata.is_tupled *)
}.

(** The arity the indirect-dispatch rules compare against |sbar|.
    Tupled functions "expect their arguments as a single tuple block
    that the generic-apply path unpacks; the 'arity' compared here is
    the one after that adjustment" (OS.Apply.IndirectUnknownArity.Full
    NOTES) — i.e. 1, the tuple itself (20-to-cmm-soundness.md: the
    closinfo arity is negated for Tupled, so a tupled callee never
    full-matches its raw param count).  Non-tupled: the unarized
    argument count. *)
Definition ai_arity (ai : arity_info) : nat :=
  if ai_is_tupled ai then 1
  else length (unarize (ai_params_arity ai)).

Inductive heap_object : Type :=
| HO_Block (t : tag) (mu : mutability) (fields : list value)
    (* scannable block: tag, mutability, fields *)
| HO_FloatBlock (mu : mutability) (fields : list float64)
    (* naked-float block (runtime Double_array_tag) *)
| HO_MixedBlock (t : tag) (mu : mutability) (sigma : mixed_block_shape)
    (fields : list value)
    (* mixed block, |fields| = p + m LOGICAL fields; the physical
       prefix/suffix split is a to_cmm concern (P.MixedShape.Offset) *)
| HO_Array (ak : array_kind) (mu : mutability) (elems : list value)
    (* unarized element sequence (ch. 06 "Access-kind taxonomy") *)
| HO_Bytes (mu : mutability) (bytes : list Z)
    (* string/bytes; each byte in [0,255].
       ENCODING NOTE: byte sequences as list Z (vector-width accesses
       assemble Z-based bit patterns from bytes) *)
| HO_Bigstring (bytes : list Z)
    (* off-heap byte buffer, always mutable *)
| HO_Bigarray (bk : bigarray_kind) (layout : bigarray_layout)
    (dims : list Z) (elems : list value)
    (* ENCODING NOTE: elements stored as runtime values at their
       STORED width (taxonomy table, ch. 06); decode_bk/encode_bk on
       access belong to the bigarray denotations (PrimMemoryB) *)
| HO_Closures (funs : fmap function_slot (code_id * arity_info))
    (venv : fmap value_slot value)
    (* set of closures: 04-opsem.md §1.2 *)
| HO_Boxed (k : kind) (contents : value)
    (* boxed number of boxable kind k; contents the naked number
       (05-primitives-scalar.md: H(l) = Boxed(kappa, nv)) *)
| HO_Lazy (t : lazy_block_tag) (v : value)
| HO_Code (c : code0).
    (* a piece of code, keyed by code id (04-opsem.md §1.5) *)

(** ** Heap — 04-opsem.md §1.3

    [H : (address + Code_id) -> heap object], i.e. keyed by [heap_key]
    (Base.v).  ENCODING NOTE: region placement of an allocation is NOT
    recorded in [H]: ch. 06 ("the value produced and the heap update
    are the same either way") elides it, and End_region's reclamation
    is explicitly conceptual (P.Unary.EndRegion leaves H unchanged). *)

Definition heap : Type := fmap heap_key heap_object.

Definition heap_upd (H : heap) (hk : heap_key) (o : heap_object)
  : heap := fupd heap_key_eqb H hk o.

Definition heap_get_addr (H : heap) (a : address) : option heap_object :=
  H (HK_addr a).

Definition heap_get_code (H : heap) (cid : code_id)
  : option heap_object := H (HK_code cid).

(** [alloc(o, H) = (l, H[l |-> o])] for some fresh l — ch. 06 "Heap
    objects".  Relational: l is an arbitrary fresh dynamic location. *)
Definition alloc (o : heap_object) (H : heap) (l : location) (H' : heap)
  : Prop :=
  fresh_for H (HK_addr (Addr_loc l)) /\
  H' = heap_upd H (HK_addr (Addr_loc l)) o.

(** ** The iota-class and the fold simulation core
       (06-primitives-memory.md P.Binary.PhysEqual; 13-soundness.md
       section 1; CORRESPONDENCE entries 44 (ITEM-8 AMENDMENT) and
       75) *)

(** The iota-class: immutable heap objects the compiler is licensed
    to share, duplicate, or lift — immutable (NOT Immutable_unique)
    blocks and arrays, boxed numbers, strings, and sets of closures.
    Immutable_unique objects (extension constructors) are
    identity-pinned and compare exactly, like mutable objects;
    bigstrings, bigarrays, lazy blocks (forcing mutates) and code are
    outside the class.  Locality is immaterial (ch. 06 NOTES): alloc
    mode is erased at allocation (entry 10 — region placement is not
    recorded in H), so LOCAL immutable objects classify iota even
    though present sharing practice excludes locals — a deliberate
    spec-widening in the sound direction (headroom for e.g. future
    local re-boxing). *)
Definition iota_object (o : heap_object) : Prop :=
  match o with
  | HO_Block _ Immutable _ => True
  | HO_FloatBlock Immutable _ => True
  | HO_MixedBlock _ Immutable _ _ => True
  | HO_Array _ Immutable _ => True
  | HO_Bytes Immutable _ => True
  | HO_Closures _ _ => True
  | HO_Boxed _ _ => True
  | _ => False
  end.

(** The structurally-folded subclass: iota-objects MINUS sets of
    closures.  Closures are fold LEAVES (entry 75, ruling (c)):
    identified through the location relation plus literal slot
    equality, never structurally recursed — which delivers the
    sharing license (the observation instance's relation is
    non-injective on iota) while keeping fold-equality underivable
    on distinct closures under the diagonal instance, matching the
    prover's never-fold-closure-equality discipline. *)
Definition iota_struct (o : heap_object) : Prop :=
  match o with
  | HO_Closures _ _ => False
  | _ => iota_object o
  end.

(** An address whose target is an iota-object of H (the class of an
    allocated address never changes: immutability is permanent and
    addresses are not reused). *)
Definition iota_addr (H : heap) (a : address) : Prop :=
  exists o, H (HK_addr a) = Some o /\ iota_object o.

(** An iota-operand (P.Binary.PhysEqual's premise vocabulary): a
    value pointing to an iota-object of H.  V_clos is a
    set-of-closures view, always iota; immediates, null, regions and
    rec_info are not heap operands at all. *)
Definition is_iota (H : heap) (v : value) : Prop :=
  match v with
  | V_ptr a => iota_addr H a
  | V_clos _ _ => True
  | _ => False
  end.

(** The fold simulation core (entry-44 ITEM-8 AMENDMENT): ONE
    heap-parameterized, object-class-aware similarity.  The location
    relation [b] is the only channel for non-iota pointers and for
    closures; identity-pinning disciplines on [b] are INSTANTIATION
    properties (Soundness.v's observation instance constrains [b] to
    a bijection on non-iota targets with initial-heap pins; the
    diagonal instance below pins [b] to address equality).
    Iota-objects other than closures additionally relate by
    STRUCTURAL DESCENT through the two heaps — 13 section 1's
    folding: one shared target block may stand for two source blocks
    and vice versa (CSE sharing; re-boxing), and a dynamic
    iota-location may relate to a symbol (lifting).
    The fold is INDUCTIVE under the model's acyclicity (entry-44
    ruling (b)): dynamic iota-allocations cannot self-reference
    (their constructors take already-evaluated values), static
    recursion passes through code ids
    (WF.Syntax.StaticRecThroughCode), and closures and non-iota
    objects are relation-leaves, so descent is well-founded.
    TRIPWIRE (W-35): real OCaml's [let rec x = 1 :: x] builds cyclic
    immutables via the alloc_dummy/update_dummy MUTATION idiom,
    absent from the model (and if modeled, the block is built
    Mutable, hence a leaf); any future extension minting cyclic
    iota-objects breaks inductive totality and must revisit this
    fold. *)
Inductive fold_vsim (b : address -> address -> Prop) (H H' : heap)
  : value -> value -> Prop :=
| FV_tagged_imm : forall n,
    fold_vsim b H H' (V_tagged_imm n) (V_tagged_imm n)
| FV_naked_imm : forall n,
    fold_vsim b H H' (V_naked_imm n) (V_naked_imm n)
| FV_naked_int8 : forall n,
    fold_vsim b H H' (V_naked_int8 n) (V_naked_int8 n)
| FV_naked_int16 : forall n,
    fold_vsim b H H' (V_naked_int16 n) (V_naked_int16 n)
| FV_naked_int32 : forall n,
    fold_vsim b H H' (V_naked_int32 n) (V_naked_int32 n)
| FV_naked_int64 : forall n,
    fold_vsim b H H' (V_naked_int64 n) (V_naked_int64 n)
| FV_naked_nativeint : forall n,
    fold_vsim b H H' (V_naked_nativeint n) (V_naked_nativeint n)
| FV_naked_float : forall f,
    fold_vsim b H H' (V_naked_float f) (V_naked_float f)
| FV_naked_float32 : forall f,
    fold_vsim b H H' (V_naked_float32 f) (V_naked_float32 f)
| FV_naked_vec128 : forall v,
    fold_vsim b H H' (V_naked_vec128 v) (V_naked_vec128 v)
| FV_naked_vec256 : forall v,
    fold_vsim b H H' (V_naked_vec256 v) (V_naked_vec256 v)
| FV_naked_vec512 : forall v,
    fold_vsim b H H' (V_naked_vec512 v) (V_naked_vec512 v)
| FV_ptr : forall a a',
    b a a' ->
    fold_vsim b H H' (V_ptr a) (V_ptr a')
| FV_ptr_iota : forall a a' o o',
    H (HK_addr a) = Some o ->
    H' (HK_addr a') = Some o' ->
    iota_struct o ->
    iota_struct o' ->
    fold_osim b H H' o o' ->
    fold_vsim b H H' (V_ptr a) (V_ptr a')
| FV_clos : forall l l' f,
    b (Addr_loc l) (Addr_loc l') ->
    fold_vsim b H H' (V_clos l f) (V_clos l' f)
| FV_null : fold_vsim b H H' V_null V_null
| FV_region : forall i,
    fold_vsim b H H' (V_region i) (V_region i)
| FV_rec_info : fold_vsim b H H' V_rec_info V_rec_info

(** Object-level similarity: same shape, fields related pointwise;
    Closures objects are leaves (see above).  No clause for HO_Code:
    code lives under HK_code keys, never at an address, so it is not
    part of the first-order observation. *)
with fold_osim (b : address -> address -> Prop) (H H' : heap)
  : heap_object -> heap_object -> Prop :=
| FO_block : forall t mu vs vs',
    Forall2 (fold_vsim b H H') vs vs' ->
    fold_osim b H H' (HO_Block t mu vs) (HO_Block t mu vs')
| FO_float_block : forall mu fs,
    fold_osim b H H' (HO_FloatBlock mu fs) (HO_FloatBlock mu fs)
| FO_mixed_block : forall t mu sigma vs vs',
    Forall2 (fold_vsim b H H') vs vs' ->
    fold_osim b H H' (HO_MixedBlock t mu sigma vs)
      (HO_MixedBlock t mu sigma vs')
| FO_array : forall ak mu vs vs',
    Forall2 (fold_vsim b H H') vs vs' ->
    fold_osim b H H' (HO_Array ak mu vs) (HO_Array ak mu vs')
| FO_bytes : forall mu bs,
    fold_osim b H H' (HO_Bytes mu bs) (HO_Bytes mu bs)
| FO_bigstring : forall bs,
    fold_osim b H H' (HO_Bigstring bs) (HO_Bigstring bs)
| FO_bigarray : forall bk layout dims vs vs',
    Forall2 (fold_vsim b H H') vs vs' ->
    fold_osim b H H' (HO_Bigarray bk layout dims vs)
      (HO_Bigarray bk layout dims vs')
| FO_closures : forall funs venv funs' venv',
    fold_osim b H H' (HO_Closures funs venv) (HO_Closures funs' venv')
| FO_boxed : forall k v v',
    fold_vsim b H H' v v' ->
    fold_osim b H H' (HO_Boxed k v) (HO_Boxed k v')
| FO_lazy : forall t v v',
    fold_vsim b H H' v v' ->
    fold_osim b H H' (HO_Lazy t v) (HO_Lazy t v').

(** Heap consistency of a location relation: every pair of [b] is
    populated on both sides, by fold-similar objects.  Instantiation
    material (Soundness.v seeds [b] at the observation roots). *)
Definition fold_heap_sim (b : address -> address -> Prop)
    (H H' : heap) : Prop :=
  forall a a', b a a' ->
    exists o o',
      H (HK_addr a) = Some o /\
      H' (HK_addr a') = Some o' /\
      fold_osim b H H' o o'.

(** The DIAGONAL instance (entry 75): fold-equality of two values
    within ONE heap — P.Binary.PhysEqual's "equal up to folding of
    immutable heap objects (13 section 1)".  [b] is pinned to
    address equality, so non-iota pointers and closures compare as
    machine words while iota-objects compare structurally:
    simulation against self. *)
Definition fold_eq (H : heap) (v1 v2 : value) : Prop :=
  fold_vsim (fun a a' => a = a') H H v1 v2.

(** Reflexivity family (entry 75; load-bearing TWICE: it witnesses
    13 section 1's coincidence sentence — programs whose
    observations never consult loose identity get refinement in
    both directions — and it constructs the code-matching abstract
    behavior in the re-posed INV.ToCmm.Simulates' determinacy
    bridge).  Any reflexive location relation gives reflexivity
    outright: pointers and closures go through [b]; no structural
    descent is needed. *)
Lemma Forall2_diag :
  forall (A : Type) (R : A -> A -> Prop) (l : list A),
    (forall x, R x x) -> Forall2 R l l.
Proof.
  intros A R l HR. induction l; constructor; auto.
Qed.

Lemma fold_vsim_refl :
  forall (b : address -> address -> Prop) (H H' : heap) v,
    (forall a, b a a) -> fold_vsim b H H' v v.
Proof.
  intros b H H' v Hb. destruct v; constructor; auto.
Qed.

(** Object-level reflexivity excludes code: [fold_osim] deliberately
    has no HO_Code clause (code lives under HK_code keys, never at an
    address), so the diagonal holds for every ADDRESSABLE object. *)
Lemma fold_osim_refl :
  forall (b : address -> address -> Prop) (H H' : heap) o,
    (forall a, b a a) ->
    (forall c, o <> HO_Code c) ->
    fold_osim b H H' o o.
Proof.
  intros b H H' o Hb Hnc.
  assert (Hv : forall v, fold_vsim b H H' v v).
  { intro v. apply fold_vsim_refl. exact Hb. }
  destruct o.
  - apply FO_block. apply Forall2_diag. exact Hv.
  - apply FO_float_block.
  - apply FO_mixed_block. apply Forall2_diag. exact Hv.
  - apply FO_array. apply Forall2_diag. exact Hv.
  - apply FO_bytes.
  - apply FO_bigstring.
  - apply FO_bigarray. apply Forall2_diag. exact Hv.
  - apply FO_closures.
  - apply FO_boxed. apply Hv.
  - apply FO_lazy. apply Hv.
  - exfalso. exact (Hnc _ eq_refl).
Qed.

Lemma fold_eq_refl : forall (H : heap) v, fold_eq H v v.
Proof.
  intros H v. apply fold_vsim_refl. intro a. reflexivity.
Qed.

(** ** Value environment rho — 04-opsem.md §1.4

    [rho : (Variable + Symbol) -> value], i.e. keyed by [name]. *)

Definition env : Type := fmap name value.

Definition env_upd (rho : env) (n : name) (v : value) : env :=
  fupd name_eqb rho n v.

Definition env_upd_var (rho : env) (x : variable) (v : value) : env :=
  env_upd rho (Name_var x) v.

(** [rho[xbar |-> vbar]] — simultaneous extension, leftmost first.
    Callers are responsible for |xs| = |vs| (the rules premise it). *)
Definition env_upd_vars (rho : env) (xs : list variable)
  (vs : list value) : env :=
  fold_left (fun r p => env_upd_var r (fst p) (snd p)) (combine xs vs) rho.

Definition env_get_var (rho : env) (x : variable) : option value :=
  rho (Name_var x).

(** ** Trap and region stacks — 04-opsem.md §1.7, §1.8 *)

Definition trap_stack : Type := list continuation.

Definition region_stack : Type := list region_handle.

(** ** Continuation environment K — 04-opsem.md §1.6

    Entries follow the doc grammar

      entry ::= Handler <xbar, e, rho_def, K_def, d>
              | Return  <xbar, dst, rho_c, K_c, T_c, R_c>
              | Exn     <x_b, k_x, vbar_extra, K_c, T_c, R_c>

    plus the two halting entries of OS.Unit.Init (Halt_return,
    Halt_exn) and the recursive-group entry CE_rec.

    ENCODING NOTE (the K' knot; CORRESPONDENCE.md catalog #1):
    OS.LetCont.Rec defines K' = K[k_i |-> Handler<..., K', ...>], an
    infinite term if stored literally.  Instead the entry for a member
    of a recursive group is [CE_rec], which stores the SYNTACTIC group
    (invariant params, handlers) plus the defining environments
    (rho_def, the outer K_def) and trap depth d; [bind_rec_group]
    below re-ties the fixed point at lookup time.  Opsem.v's
    apply-cont rule for a [CE_rec] entry unfolds one step: it enters
    the named handler with continuation environment
    [bind_rec_group K_def rho_def zs group d] — the same K' again.
    Provably the same unfolding; strictly positive. *)

Inductive centry : Type :=
| CE_handler (params : list variable) (body : expr) (rho_def : env)
    (K_def : fmap continuation centry) (d : nat)
| CE_rec (inv_params : list variable)
    (group : list (continuation * cont_handler))
    (rho_def : env) (K_def : fmap continuation centry) (d : nat)
| CE_return (params : list variable) (dst : result_continuation)
    (rho_c : env) (K_c : fmap continuation centry)
    (T_c : trap_stack) (R_c : region_stack)
| CE_exn (x_b : variable) (k_x : continuation)
    (extra_args : list value) (K_c : fmap continuation centry)
    (T_c : trap_stack) (R_c : region_stack)
| CE_halt_return
| CE_halt_exn.

Definition kenv : Type := fmap continuation centry.

Definition kenv_upd (K : kenv) (k : continuation) (ce : centry) : kenv :=
  fupd continuation_eqb K k ce.

(** Membership / lookup in a recursive handler group (association on
    the continuation name). *)
Fixpoint group_lookup (k : continuation)
  (g : list (continuation * cont_handler)) : option cont_handler :=
  match g with
  | [] => None
  | (k', h) :: g' =>
      if continuation_eqb k k' then Some h else group_lookup k g'
  end.

Definition group_mem (k : continuation)
  (g : list (continuation * cont_handler)) : bool :=
  match group_lookup k g with
  | Some _ => true
  | None => false
  end.

(** Tie the knot for a recursive continuation group (OS.LetCont.Rec):
    every member k_i of the group maps to the [CE_rec] entry recording
    the group and its definition context; all other continuations fall
    through to the outer [K]. *)
Definition bind_rec_group (K : kenv) (rho : env)
  (zs : list variable) (g : list (continuation * cont_handler))
  (d : nat) : kenv :=
  fun k =>
    if group_mem k g then Some (CE_rec zs g rho K d) else K k.

(** ** Machine configuration — 04-opsem.md §1 head, README.md

    ENCODING NOTE: the expression position of a configuration is a
    [control], not a bare [expr].  [Ctl_jump k vs] is the doc's
    synthetic "Apply_cont k (values vbar)" — an apply-cont whose
    arguments are already VALUES — produced by the boundary rules
    OS.ApplyCont.Return / OS.ApplyCont.ExnBoundary and consumed by the
    apply-cont rules exactly as the doc's "equivalently, directly bind
    k_c's parameters to vbar via OS.ApplyCont in K_c" prescribes.
    Boundary entries can chain (tail calls, nested raises), so the
    synthetic form must itself be a machine state.  Note that
    [Ctl_jump] carries VALUES, not simples: it is machine-internal
    and is NOT an extension of the language syntax of 02-syntax.md
    (no source term ever contains a [Ctl_jump]). *)

Inductive control : Type :=
| Ctl_expr (e : expr)
| Ctl_jump (k : continuation) (vs : list value).

Record config : Type := mk_config {
  c_expr : control;
  c_rho : env;
  c_K : kenv;
  c_H : heap;
  c_T : trap_stack;
  c_R : region_stack
}.

(** ** Evaluation of Simples *)

(** The value denoted by a constant (04-opsem.md §1.4: "Constants c
    (Reg_width_const.t) denote the obvious value"; inventory from
    02-syntax.md).
    ENCODING NOTE: [Poison (kappa, msg)] is "a deliberately-invalid
    value" (02-syntax.md) with no runtime denotation; evaluating it
    yields [None] (the machine is stuck), consistent with its role as
    a placeholder for invalid code. *)
Definition const_value (c : const) : option value :=
  match c with
  | Const_naked_immediate n => Some (V_naked_imm n)
  | Const_tagged_immediate n => Some (V_tagged_imm n)
  | Const_naked_float f => Some (V_naked_float f)
  | Const_naked_float32 f => Some (V_naked_float32 f)
  | Const_naked_int8 n => Some (V_naked_int8 n)
  | Const_naked_int16 n => Some (V_naked_int16 n)
  | Const_naked_int32 n => Some (V_naked_int32 n)
  | Const_naked_int64 n => Some (V_naked_int64 n)
  | Const_naked_nativeint n => Some (V_naked_nativeint n)
  | Const_naked_vec128 b => Some (V_naked_vec128 b)
  | Const_naked_vec256 b => Some (V_naked_vec256 b)
  | Const_naked_vec512 b => Some (V_naked_vec512 b)
  | Const_null => Some V_null
  | Const_poison _ _ => None
  end.

(** RULE OS.Simple.Eval (STATUS normative) — 04-opsem.md
    CODE middle_end/flambda2/term_basics/simple.mli#t
    CODE middle_end/flambda2/term_basics/coercion.mli#t

      [[x]]rho = rho(x)     [[sym]]rho = ptr sym
      [[c]]rho = the value denoted by c
      [[s @ co]]rho = [[s]]rho          (coercions erased)

    ENCODING NOTE: symbols consult rho first, falling back to
    [ptr sym].  04-opsem.md par. 1.4 says rho "resolves variables and
    symbols to values", and OS.Let.Static binds each set-of-closures
    symbol sym_j to [clos l f_j] in rho (a location-based closure
    value; the Closures block lives at a fresh l even for static
    sets).  A literal [[sym]]rho = ptr sym would make those bindings
    dead and strand static-closure projections.  The fallback keeps
    OS.Simple.Eval's equation true for every symbol rho does not
    rebind (Block_like symbols, which OS.Let.Static binds to exactly
    [ptr sym], and imported symbols never bound at all). *)
Definition simple_eval (rho : env) (s : simple) : option value :=
  match s with
  | Simple_name (Name_var x) _co => rho (Name_var x)
  | Simple_name (Name_sym sym) _co =>
      match rho (Name_sym sym) with
      | Some v => Some v
      | None => Some (V_ptr (Addr_sym sym))
      end
  | Simple_const c => const_value c
  end.

(** [[sbar]]rho, argument lists. *)
Definition simple_eval_list (rho : env) (ss : list simple)
  : option (list value) :=
  option_traverse (simple_eval rho) ss.

(** ** Primitive denotation types — README.md judgment forms

    [[p]](vbar; H) = (v, H') or undef  (chs. 05, 06), and the
    region-augmented form [[p]](vbar; H, R) = (v, H', R') (ch. 06,
    "Region delimiters").  The prim chapters' files (PrimScalar,
    PrimMemoryA/B) each define a relation of these types; Machine.v
    takes their union.

    ENCODING NOTE: denotations are typed over [prim_op] (the bare
    operator, Syntax.v) rather than the applied [prim] carrying its
    argument simples: the doc's judgment [[p]](vbar; H) consumes the
    already-evaluated vbar, so the operator is the only syntax the
    denotation may inspect.  OS.Let.Prim obtains vbar as
    [simple_eval_list rho (prim_args p)] and dispatches on
    [prim_op_of p]. *)

Inductive prim_result : Type :=
| PR_ok (v : value) (H' : heap)
| PR_undef.

Definition denotation : Type :=
  prim_op -> list value -> heap -> prim_result -> Prop.

Inductive prim_result_r : Type :=
| PRr_ok (v : value) (H' : heap) (R' : region_stack)
| PRr_undef.

Definition denotation_r : Type :=
  prim_op -> list value -> heap -> region_stack -> prim_result_r -> Prop.
