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
