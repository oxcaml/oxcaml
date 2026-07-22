(* Cmm.v -- chapter 15 (15-cmm.md): Core Cmm, syntax and small-step machine.

   Owner: Milner (wave 1).  Imports: Base.

   Section map (doc section -> here):
     15 s0  target assumptions   -> comment below
     15 s1  syntax               -> cmm_op / cmm_expr (CM.Syntax.Fragment)
     15 s2  values and memory    -> cmm_value, cm_read / cm_write
                                    (CM.Mem.LoadStore)
     15 s3  machine state        -> cm_config and friends
     15 s4  evaluation contexts  -> cm_ectx / cm_plug (CM.Context)
     15 s5  pure operations      -> cm_op_pure (CM.Op.Pure, CM.Op.TupleField)
     15 s6  memory operations    -> CM.Load, CM.Store
     15 s7  control              -> CM.If .. CM.Raise
     15 s8  application          -> CM.Apply, CM.Extcall
     15 s9  invalid              -> CM.Invalid
     15 s10 observation          -> CM.Unit.Final

   Target assumptions (15-cmm.md s0): 64-bit little-endian,
   size_addr = size_int = size_float = 8 bytes; word size W = 64 and byte
   order LE are baked in below without further comment. *)

(* String BEFORE List: String.length would otherwise shadow the list
   length used throughout the machine rules (Hopper, compile round 2). *)
From Stdlib Require Import ZArith Bool String List.
From Flambda2 Require Import Base.

Import ListNotations.
Local Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(* Atoms owned by this file (not in Base's atom list): Cmm variables
   (Backend_var.t) and static-catch labels (Lambda.static_label). *)

Inductive backend_var : Type := Mk_backend_var (id : nat).

Definition backend_var_eqb (x y : backend_var) : bool :=
  match x, y with
  | Mk_backend_var a, Mk_backend_var b => Nat.eqb a b
  end.

Inductive static_label : Type := Mk_static_label (id : nat).

Definition static_label_eqb (x y : static_label) : bool :=
  match x, y with
  | Mk_static_label a, Mk_static_label b => Nat.eqb a b
  end.

(* ------------------------------------------------------------------ *)
(* Word helpers: unsigned/signed readings of a 64-bit machine word
   ("0 <= n < 2^64, or its signed reading", 15-cmm.md s2).  These are
   robust to whichever canonical range Base.wrap fixes. *)

Definition to_unsigned64 (z : Z) : Z := z mod 2 ^ 64.

Definition to_signed64 (z : Z) : Z :=
  let u := to_unsigned64 z in
  if u <? 2 ^ 63 then u else u - 2 ^ 64.

(* Sign-extend the low [bits] bits of an unsigned value. *)
Definition sign_extend (bits : Z) (u : Z) : Z :=
  let u' := u mod 2 ^ bits in
  if u' <? 2 ^ (bits - 1) then u' else u' - 2 ^ bits.

(* ------------------------------------------------------------------ *)
(* Backend float interface: the shared per-op float set (f64_ / f32_
   prefixed) and the vecN_to_bits/of_bits conversions come from Base.v,
   per the
   shared-ops ruling (CORRESPONDENCE catalog entry 5, amended): ch. 18/
   20 commuting statements must relate literally the same operations on
   both language sides.  Notably f32_of_Z is a SINGLE rounding (the
   site the int->float32 double-rounding concern is measured against;
   CM.Op.Pure NOTES, ch. 13 s4.7, ch. 18), and f64_to_Z/f32_to_Z
   truncate toward zero with None when unrepresentable (the
   "Cstatic_cast out of range" partial case of CM.Op.Pure). *)

(* ------------------------------------------------------------------ *)
(* 15 s1: auxiliary enumerations of the syntax fragment.  Constructor
   names follow backend/cmm.mli / lambda/lambda.mli.
   ENCODING NOTE: where two code-level types share constructor names
   (static_cast and reinterpret_cast both have Float_of_float32;
   machtype components clash with float_width's Float32) the
   constructors carry SC_ / RC_ / MC_ prefixes.  Cload's mutability
   (Asttypes.mutable_flag) and Cmm.Alloc_mode are renamed
   cmm_mutable_flag / cmm_alloc_mode to avoid clashing with Base's
   Flambda-side mutability and alloc modes. *)

Inductive float_width : Type := Float64 | Float32.

(* machtype: static classification for register allocation and GC
   (15-cmm.md s2); Valx2 is outside the doc's fragment. *)
Inductive machtype_component : Type :=
  | MC_val | MC_addr | MC_int | MC_float | MC_float32
  | MC_vec128 | MC_vec256 | MC_vec512.

Definition machtype : Type := list machtype_component.

Inductive integer_comparison : Type :=
  | Ceq | Cne | Clt | Cgt | Cle | Cge
  | Cult | Cugt | Cule | Cuge.

Inductive float_comparison : Type :=
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt
  | CFle | CFnle | CFge | CFnge.

Inductive memory_chunk : Type :=
  | Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int
  | Word_val
  | Single (reg : float_width)
  | Double
  | Onetwentyeight_unaligned
  | Onetwentyeight_aligned
  | Twofiftysix_unaligned
  | Twofiftysix_aligned
  | Fivetwelve_unaligned
  | Fivetwelve_aligned.

(* Scalar fragment only: the vector variants of both cast types are
   omitted (SIMD casts are out of scope; CM.Syntax.Fragment). *)
Inductive reinterpret_cast : Type :=
  | RC_int_of_value
  | RC_value_of_int
  | RC_float_of_float32
  | RC_float32_of_float
  | RC_float_of_int64
  | RC_int64_of_float
  | RC_float32_of_int32
  | RC_int32_of_float32.

Inductive static_cast : Type :=
  | SC_float_of_int (w : float_width)
  | SC_int_of_float (w : float_width)
  | SC_float_of_float32
  | SC_float32_of_float.

Inductive initialization_or_assignment : Type :=
  | Initialization
  | Assignment.

Inductive ccatch_flag : Type := Normal | Recursive | Exn_handler.

Inductive trap_action : Type :=
  | Push (lbl : static_label)
  | Pop (lbl : static_label).

Inductive exit_label : Type :=
  | Return_lbl
  | Lbl (lbl : static_label).

Inductive raise_kind : Type :=
  | Raise_regular | Raise_reraise | Raise_notrace.

Inductive region_close : Type :=
  | Rc_normal | Rc_nontail | Rc_close_at_apply.

Inductive cmm_mutable_flag : Type := CMut_immutable | CMut_mutable.

(* Cmm.Alloc_mode is a distinct code-level type from Flambda's. *)
Inductive cmm_alloc_mode : Type := CAM_heap | CAM_local.

Inductive boxed_integer : Type :=
  | Boxed_nativeint | Boxed_int32 | Boxed_int64.

Inductive alloc_block_kind : Type :=
  | Alloc_block_kind_other
  | Alloc_block_kind_closure
  | Alloc_block_kind_float
  | Alloc_block_kind_float32
  | Alloc_block_kind_vec128
  | Alloc_block_kind_vec256
  | Alloc_block_kind_vec512
  | Alloc_block_kind_boxed_int (bi : boxed_integer)
  | Alloc_block_kind_float_array
  | Alloc_block_kind_float32_u_array
  | Alloc_block_kind_int_u_array
  | Alloc_block_kind_int8_u_array
  | Alloc_block_kind_int16_u_array
  | Alloc_block_kind_int32_u_array
  | Alloc_block_kind_int64_u_array
  | Alloc_block_kind_vec128_u_array
  | Alloc_block_kind_vec256_u_array
  | Alloc_block_kind_vec512_u_array.

(* ENCODING NOTE: Cmm symbols.  The code's Cmm symbols are linkage-name
   strings; to_cmm derives them injectively from Flambda symbols and
   code ids.  We model a Cmm symbol as that image directly, which is
   what chapters 17/18/20 relate across the boundary. *)
Inductive cmm_symbol : Type :=
  | CS_sym (s : symbol)
  | CS_code (cid : code_id).

(* ------------------------------------------------------------------ *)
(* 15 s2: machine values.
   ENCODING NOTE: CV_tuple is the transient k-value bundle of
   CM.Op.TupleField (Ctuple [] = void = CV_tuple []); it only arises
   from Ctuple / multi-value returns and is only consumed by
   Ctuple_field or a return. *)

Inductive cmm_value : Type :=
  | CV_word (n : Z)
  | CV_flt (f : float64)
  | CV_flt32 (f : float32)
  | CV_vec128 (b : vec128)
  | CV_vec256 (b : vec256)
  | CV_vec512 (b : vec512)
  | CV_tuple (vs : list cmm_value).

(* Multi-value result bundling: a single value is itself, any other
   arity is a CV_tuple bundle (CM.Op.TupleField NOTES). *)
Definition cm_bundle (vs : list cmm_value) : cmm_value :=
  match vs with
  | [v] => v
  | _ => CV_tuple vs
  end.

(** RULE CM.Syntax.Fragment (STATUS descriptive) -- 15-cmm.md
    CODE backend/cmm.mli#expression
    CODE backend/cmm.mli#operation
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr

    The artifact is the pair of inductives cmm_op / cmm_expr (with
    cmm_static_handler): the fragment of Cmm.expression/Cmm.operation
    that to_cmm emits for the in-scope Flambda fragment.  Omitted, per
    the rule: SIMD casts, Cprobe(_is_enabled), Cphantom_let, effect
    operations.
    ENCODING NOTE: Debuginfo.t arguments are dropped throughout (no
    semantic content).  Cextcall keeps only func and ty; the remaining
    fields (ty_args, alloc, builtin, returns, effects, coeffects) are
    calling-convention/scheduling detail unused by the rules.
    ENCODING NOTE: Cval is a runtime-only form (a value in expression
    position, produced by reduction); source programs never contain
    it.  It plays the role of the doc's implicit "expressions may be
    reduced to values in place" convention (s4). *)
Inductive cmm_op : Type :=
  | Caddi | Csubi | Cmuli | Cdivi | Cmodi
  | Cand | Cor | Cxor
  | Clsl | Clsr | Casr
  | Ccmpi (c : integer_comparison)
  | Caddv | Cadda
  | Cnegf (w : float_width)
  | Caddf (w : float_width)
  | Csubf (w : float_width)
  | Cmulf (w : float_width)
  | Cdivf (w : float_width)
  | Ccmpf (w : float_width) (c : float_comparison)
  | Cstatic_cast (sc : static_cast)
  | Creinterpret_cast (rc : reinterpret_cast)
  | Ctuple_field (n : nat) (tys : list machtype)
  | Cload (chunk : memory_chunk) (mut : cmm_mutable_flag)
          (is_atomic : bool)
  | Cstore (chunk : memory_chunk) (init : initialization_or_assignment)
  | Calloc (mode : cmm_alloc_mode) (kind : alloc_block_kind)
  | Capply (result_type : machtype) (region : region_close)
           (callees : option (list cmm_symbol))
  | Cextcall (func : string) (ty : machtype)
  | Craise (rk : raise_kind)
  | Cbeginregion | Cendregion
  | Copaque.

Inductive cmm_expr : Type :=
  | Cconst_int (n : Z)
  | Cconst_natint (n : Z)
  | Cconst_float (f : float64)
  | Cconst_float32 (f : float32)
  | Cconst_vec128 (b : vec128)
  | Cconst_vec256 (b : vec256)
  | Cconst_vec512 (b : vec512)
  | Cconst_symbol (s : cmm_symbol)
  | Cvar (x : backend_var)
  | Clet (x : backend_var) (e1 : cmm_expr) (e2 : cmm_expr)
  | Csequence (e1 : cmm_expr) (e2 : cmm_expr)
  | Ctuple (es : list cmm_expr)
  | Cop (op : cmm_op) (args : list cmm_expr)
  | Cifthenelse (cond : cmm_expr) (e_then : cmm_expr) (e_else : cmm_expr)
  | Cswitch (scrut : cmm_expr) (index : list nat) (cases : list cmm_expr)
  | Ccatch (flag : ccatch_flag) (handlers : list cmm_static_handler)
           (body : cmm_expr)
  | Cexit (lbl : exit_label) (args : list cmm_expr)
          (tas : list trap_action)
  | Cinvalid (message : string)
  | Cval (w : cmm_value)

with cmm_static_handler : Type :=
  | SHandler (lbl : static_label) (params : list backend_var)
             (body : cmm_expr) (is_cold : bool).

(* ------------------------------------------------------------------ *)
(* Mention predicates (shared; coordinator placement ruling -- the
   one host for ToCmmControl.v's kernel premises, ToCmmData.v's
   TC.Let.Subst stack guard, and ToCmmSoundness.v's sink guards).
   The two mention predicates are conservative
   OVER-approximations of free occurrence: binders count as mentions
   (Clet and handler params for variables; handler labels for
   labels), and Cexit trap actions count as label mentions.  This is
   the safe direction for every consumer: agreement and
   non-interference premises stated over these predicates only get
   STRONGER, and the intended instantiations (translation-fresh
   binders and labels, exit-free defining expressions) mention
   nothing at all. *)

Fixpoint cmm_mentions (x : backend_var) (e : cmm_expr) : bool :=
  let fix go_list (es : list cmm_expr) : bool :=
    match es with
    | [] => false
    | e0 :: rest => cmm_mentions x e0 || go_list rest
    end in
  let fix go_handlers (hs : list cmm_static_handler) : bool :=
    match hs with
    | [] => false
    | SHandler _ ps b _ :: rest =>
        existsb (backend_var_eqb x) ps || cmm_mentions x b
        || go_handlers rest
    end in
  match e with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _
  | Cconst_float32 _ | Cconst_vec128 _ | Cconst_vec256 _
  | Cconst_vec512 _ | Cconst_symbol _ | Cinvalid _ | Cval _ => false
  | Cvar y => backend_var_eqb x y
  | Clet y e1 e2 =>
      backend_var_eqb x y || cmm_mentions x e1 || cmm_mentions x e2
  | Csequence e1 e2 => cmm_mentions x e1 || cmm_mentions x e2
  | Ctuple es => go_list es
  | Cop _ es => go_list es
  | Cifthenelse e1 e2 e3 =>
      cmm_mentions x e1 || cmm_mentions x e2 || cmm_mentions x e3
  | Cswitch e0 _ cases => cmm_mentions x e0 || go_list cases
  | Ccatch _ hs b => go_handlers hs || cmm_mentions x b
  | Cexit _ es _ => go_list es
  end.

Definition exit_label_mentions (l : static_label) (lbl : exit_label)
  : bool :=
  match lbl with
  | Return_lbl => false
  | Lbl l' => static_label_eqb l l'
  end.

Definition trap_action_mentions (l : static_label) (ta : trap_action)
  : bool :=
  match ta with
  | Push l' | Pop l' => static_label_eqb l l'
  end.

Fixpoint cmm_mentions_label (l : static_label) (e : cmm_expr)
  : bool :=
  let fix go_list (es : list cmm_expr) : bool :=
    match es with
    | [] => false
    | e0 :: rest => cmm_mentions_label l e0 || go_list rest
    end in
  let fix go_handlers (hs : list cmm_static_handler) : bool :=
    match hs with
    | [] => false
    | SHandler l' _ b _ :: rest =>
        static_label_eqb l l' || cmm_mentions_label l b
        || go_handlers rest
    end in
  match e with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _
  | Cconst_float32 _ | Cconst_vec128 _ | Cconst_vec256 _
  | Cconst_vec512 _ | Cconst_symbol _ | Cvar _ | Cinvalid _
  | Cval _ => false
  | Clet _ e1 e2 =>
      cmm_mentions_label l e1 || cmm_mentions_label l e2
  | Csequence e1 e2 =>
      cmm_mentions_label l e1 || cmm_mentions_label l e2
  | Ctuple es => go_list es
  | Cop _ es => go_list es
  | Cifthenelse e1 e2 e3 =>
      cmm_mentions_label l e1 || cmm_mentions_label l e2
      || cmm_mentions_label l e3
  | Cswitch e0 _ cases => cmm_mentions_label l e0 || go_list cases
  | Ccatch _ hs b => go_handlers hs || cmm_mentions_label l b
  | Cexit lbl es tas =>
      exit_label_mentions l lbl || go_list es
      || existsb (trap_action_mentions l) tas
  end.

Definition trap_action_pops (ta : trap_action) : bool :=
  match ta with Push _ => false | Pop _ => true end.

(* Does e contain a Pop trap action, a Craise or Capply operator,
   or a Cendregion operation anywhere (handlers included)?
   Consumed by ToCmmData.v's TC.Let.Subst kernel (reviewer findings
   KF-043, KF-044): such an expression READS the ambient stack
   shape -- CM.Raise and trap_apply pattern-match the top of TT
   (CM.Catch.Exn installs handlers without pushing TT, so no raise
   is ever internally caught), CM.Region.End pattern-matches RR
   down to the handle, and Capply tunnels both reads through the
   callee body (cm_returns hands the callee the caller's RR;
   CM.Apply.Raise re-plugs a raise at the call site) -- so a run
   under one stack proves nothing about runs under another.
   Push-only trap actions and Cbeginregion remain allowed: a Push
   cannot restore a pinned TT, and Cbeginregion alone cannot
   restore the pinned RR without a matching Cendregion.  Cextcall
   needs no flag: CM.Extcall emits a CME_extern event
   unconditionally, so a run containing one is never silent.
   Calloc needs no flag despite CM.Alloc.Local READING RR (it
   pattern-matches RR = iota :: RR0): the kernel pins M at both
   endpoints, every allocation strictly extends M, and the only
   M-shrinking operation is mem_reclaim behind the flagged
   Cendregion -- CM.Alloc.GC contributes no provable steps while
   gc_reloc is an unconstrained Parameter (see the tripwire note
   there) -- so M-pinned silent runs perform no allocation at all
   (KF-048).  Syntactic and conservative, the safe direction; every
   LPE defining expression is free of all of these. *)
Fixpoint cmm_touches_stacks (e : cmm_expr) : bool :=
  let fix go_list (es : list cmm_expr) : bool :=
    match es with
    | [] => false
    | e0 :: rest => cmm_touches_stacks e0 || go_list rest
    end in
  let fix go_handlers (hs : list cmm_static_handler) : bool :=
    match hs with
    | [] => false
    | SHandler _ _ b _ :: rest =>
        cmm_touches_stacks b || go_handlers rest
    end in
  match e with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _
  | Cconst_float32 _ | Cconst_vec128 _ | Cconst_vec256 _
  | Cconst_vec512 _ | Cconst_symbol _ | Cvar _ | Cinvalid _
  | Cval _ => false
  | Clet _ e1 e2 => cmm_touches_stacks e1 || cmm_touches_stacks e2
  | Csequence e1 e2 => cmm_touches_stacks e1 || cmm_touches_stacks e2
  | Ctuple es => go_list es
  | Cop op es =>
      (match op with
       | Cendregion => true
       | Craise _ => true
       | Capply _ _ _ => true
       | _ => false
       end)
      || go_list es
  | Cifthenelse e1 e2 e3 =>
      cmm_touches_stacks e1 || cmm_touches_stacks e2
      || cmm_touches_stacks e3
  | Cswitch e0 _ cases => cmm_touches_stacks e0 || go_list cases
  | Ccatch _ hs b => go_handlers hs || cmm_touches_stacks b
  | Cexit _ es tas => go_list es || existsb trap_action_pops tas
  end.

(* ENCODING NOTE (sanctioned Parameter; the nominal-occurrence
   pattern of Syntax.v's binder set): the Barendregt condition on an
   output term -- all Clet and handler-parameter variable binders
   and all handler labels pairwise distinct, occurring nowhere
   outside their own scopes.  Premises-only; consumed by ch. 20's
   simulation statement (reviewer watch W-27): the machine's
   variable and label environments are flat (CM.Head.Let and
   CM.Catch.NonRec persist bindings), so juxtaposed translated
   subterms cross-pollute unless binders are globally fresh.  The
   intended instantiation is the code's freshness discipline
   (Backend_var.create_local, next_raise_count); a real definition
   would be a grammar-sized collector pair over two namespaces. *)
Parameter cmm_unique_binders : cmm_expr -> Prop.

(* ------------------------------------------------------------------ *)
(* 15 s2: memory.  Byte-addressed, little-endian; addresses are machine
   words read as Z; bytes are Z in [0, 255]. *)

(* ENCODING NOTE: ch. 15 treats M as bare bytes, but ch. 19's rules on
   the SAME machine state need block ownership: CM.Alloc.Local writes
   blocks "tagged as belonging to iota" and CM.Region.End reclaims
   "every block allocated in iota".  So M carries the byte store plus
   the local-block tag map (each byte of each live locally-allocated
   block -> owning region; heap bytes untagged).  Tagging bytes rather
   than block bases makes block extents available to CM.Region.End's
   reclamation without decoding headers.  Ch. 15's rules read and
   write only mem_bytes and preserve mem_local. *)
Record cmm_mem : Type := MkMem {
  mem_bytes : fmap Z Z;
  mem_local : fmap Z region_handle
}.

(** RULE CM.Mem.LoadStore (STATUS normative) -- 15-cmm.md
    CODE backend/cmm.mli#memory_chunk
    CODE backend/cmm.mli#size_of_memory_chunk
    CODE backend/cmm_helpers.ml#mk_load_immut

    The artifact is the pair cm_read / cm_write below (with their
    helpers): little-endian b-byte access at a chunk's width, sign- or
    zero-extended per the chunk's signedness; None (undef) if any byte
    is unmapped or the chunk's alignment is violated.  The _aligned
    vector chunks require 16/32/64-byte alignment; all other chunks
    have no alignment requirement (amd64 allows unaligned access). *)

Definition size_of_memory_chunk (k : memory_chunk) : nat :=
  match k with
  | Byte_unsigned | Byte_signed => 1%nat
  | Sixteen_unsigned | Sixteen_signed => 2%nat
  | Thirtytwo_unsigned | Thirtytwo_signed => 4%nat
  | Single _ => 4%nat
  | Word_int | Word_val | Double => 8%nat
  | Onetwentyeight_unaligned | Onetwentyeight_aligned => 16%nat
  | Twofiftysix_unaligned | Twofiftysix_aligned => 32%nat
  | Fivetwelve_unaligned | Fivetwelve_aligned => 64%nat
  end.

Definition chunk_alignment (k : memory_chunk) : Z :=
  match k with
  | Onetwentyeight_aligned => 16
  | Twofiftysix_aligned => 32
  | Fivetwelve_aligned => 64
  | _ => 1
  end.

Fixpoint mem_read_bytes (B : fmap Z Z) (a : Z) (n : nat)
  : option (list Z) :=
  match n with
  | O => Some []
  | S k =>
    match B a, mem_read_bytes B (a + 1) k with
    | Some b, Some bs => Some (b :: bs)
    | _, _ => None
    end
  end.

(* Assemble little-endian bytes (head = lowest address = least
   significant). *)
Definition le_word (bs : list Z) : Z :=
  fold_right (fun b acc => b + 256 * acc) 0 bs.

(* The low n bytes of z, little-endian. *)
Fixpoint le_bytes (n : nat) (z : Z) : list Z :=
  match n with
  | O => []
  | S k => (z mod 256) :: le_bytes k (z / 256)
  end.

(* Writing requires every target byte to be mapped ("undef if a is
   unmapped"); ch. 19's allocation rules are what extend the domain. *)
Fixpoint mem_write_bytes (B : fmap Z Z) (a : Z) (bs : list Z)
  : option (fmap Z Z) :=
  match bs with
  | [] => Some B
  | b :: bs' =>
    match B a with
    | None => None
    | Some _ => mem_write_bytes (fupd Z.eqb B a b) (a + 1) bs'
    end
  end.

Definition cm_read (M : cmm_mem) (a : Z) (k : memory_chunk)
  : option cmm_value :=
  if negb (a mod chunk_alignment k =? 0) then None
  else
    match mem_read_bytes (mem_bytes M) a (size_of_memory_chunk k) with
    | None => None
    | Some bs =>
      let u := le_word bs in
      match k with
      | Byte_unsigned | Sixteen_unsigned | Thirtytwo_unsigned =>
          Some (CV_word (wrap 64 u))
      | Byte_signed => Some (CV_word (wrap 64 (sign_extend 8 u)))
      | Sixteen_signed => Some (CV_word (wrap 64 (sign_extend 16 u)))
      | Thirtytwo_signed => Some (CV_word (wrap 64 (sign_extend 32 u)))
      | Word_int | Word_val => Some (CV_word (wrap 64 u))
      (* Single decodes a 32-bit float; loaded to a Float register it
         becomes a double per the Float_of_float32 convention. *)
      | Single Float32 => Some (CV_flt32 (f32_of_bits u))
      | Single Float64 => Some (CV_flt (f64_of_f32 (f32_of_bits u)))
      | Double => Some (CV_flt (f64_of_bits u))
      | Onetwentyeight_unaligned | Onetwentyeight_aligned =>
          Some (CV_vec128 (vec128_of_bits u))
      | Twofiftysix_unaligned | Twofiftysix_aligned =>
          Some (CV_vec256 (vec256_of_bits u))
      | Fivetwelve_unaligned | Fivetwelve_aligned =>
          Some (CV_vec512 (vec512_of_bits u))
      end
    end.

(* The bit pattern a value stores under a chunk; None on a value/chunk
   shape mismatch (undef). *)
Definition cm_value_bits (k : memory_chunk) (w : cmm_value)
  : option Z :=
  match k, w with
  | (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
     | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val),
    CV_word n => Some (to_unsigned64 n)
  | Single Float32, CV_flt32 f => Some (f32_to_bits f)
  (* Storing a Float register single: narrow to single precision. *)
  | Single Float64, CV_flt f => Some (f32_to_bits (f32_of_f64 f))
  | Double, CV_flt f => Some (f64_to_bits f)
  | (Onetwentyeight_unaligned | Onetwentyeight_aligned), CV_vec128 b =>
      Some (vec128_to_bits b)
  | (Twofiftysix_unaligned | Twofiftysix_aligned), CV_vec256 b =>
      Some (vec256_to_bits b)
  | (Fivetwelve_unaligned | Fivetwelve_aligned), CV_vec512 b =>
      Some (vec512_to_bits b)
  | _, _ => None
  end.

Definition cm_write (M : cmm_mem) (a : Z) (k : memory_chunk)
    (w : cmm_value) : option cmm_mem :=
  if negb (a mod chunk_alignment k =? 0) then None
  else
    match cm_value_bits k w with
    | None => None
    | Some bits =>
      match mem_write_bytes (mem_bytes M) a
              (le_bytes (size_of_memory_chunk k) bits) with
      | None => None
      | Some B' => Some (MkMem B' (mem_local M))
      end
    end.

(* ------------------------------------------------------------------ *)
(* 15 s3: machine state  <ce, chi, M, TT, RR>  over a current
   expression e_c. *)

Definition cmm_venv : Type := fmap backend_var cmm_value.

(* ENCODING NOTE (catalog #1, the K' knot, Cmm instance): the doc's
   CM.Catch.Rec defines chi' = chi[lbl_i |-> CHandler <..., chi', ...>],
   an infinite term if stored literally.  As in Values.v's CE_rec, a
   recursive group's entries store the *syntactic* group plus the
   definition-time environments (CHandler_rec), and chi_lookup re-ties
   the fixed point at lookup time (chi_bind_rec).  Provably the same
   unfolding; strictly positive. *)
Inductive cmm_handler : Type :=
  | CHandler (params : list backend_var) (body : cmm_expr)
             (ce_def : cmm_venv)
             (chi_def : static_label -> option cmm_handler)
             (d : nat) (kind : ccatch_flag)
  | CHandler_rec (group : list cmm_static_handler)
                 (ce_def : cmm_venv)
                 (chi_def : static_label -> option cmm_handler)
                 (d : nat).

Definition cmm_kenv : Type := fmap static_label cmm_handler.

(* Install a recursive group: every label of the group maps to an entry
   recording the whole group and the *outer* environments. *)
Definition chi_bind_rec (group : list cmm_static_handler)
    (ce_def : cmm_venv) (chi : cmm_kenv) (d : nat) : cmm_kenv :=
  fold_left
    (fun chi' h =>
       match h with
       | SHandler lbl _ _ _ =>
           fupd static_label_eqb chi' lbl
             (CHandler_rec group ce_def chi d)
       end)
    group chi.

Fixpoint assoc_handler (group : list cmm_static_handler)
    (lbl : static_label) : option (list backend_var * cmm_expr) :=
  match group with
  | [] => None
  | SHandler l xs body _ :: rest =>
      if static_label_eqb l lbl then Some (xs, body)
      else assoc_handler rest lbl
  end.

(* The doc's "chi(lbl) = CHandler <xbar, e_h, ce_def, chi_def, d, kind>"
   premise is chi_lookup below: a direct entry reads off its fields; a
   recursive-group entry re-ties chi' per CM.Catch.Rec (kind Normal). *)
Record cmm_handler_view : Type := CHView
  { chv_params : list backend_var;
    chv_body : cmm_expr;
    chv_ce : cmm_venv;
    chv_chi : cmm_kenv;
    chv_depth : nat;
    chv_kind : ccatch_flag }.

Definition chi_lookup (chi : cmm_kenv) (lbl : static_label)
  : option cmm_handler_view :=
  match chi lbl with
  | Some (CHandler xs body ce chd d k) =>
      Some (CHView xs body ce chd d k)
  | Some (CHandler_rec group ce chd d) =>
      match assoc_handler group lbl with
      | Some (xs, body) =>
          Some (CHView xs body ce (chi_bind_rec group ce chd d) d Normal)
      | None => None
      end
  | None => None
  end.

Record cm_config : Type := CmCfg
  { cc_expr : cmm_expr;
    cc_venv : cmm_venv;
    cc_kenv : cmm_kenv;
    cc_mem : cmm_mem;
    cc_traps : list static_label;
    cc_regions : list region_handle }.

Definition venv_empty : cmm_venv := fun _ => None.
Definition kenv_empty : cmm_kenv := fun _ => None.

Fixpoint venv_bind (ce : cmm_venv) (xs : list backend_var)
    (vs : list cmm_value) : cmm_venv :=
  match xs, vs with
  | x :: xs', v :: vs' => venv_bind (fupd backend_var_eqb ce x v) xs' vs'
  | _, _ => ce
  end.

(* Executing trap actions left-to-right (CM.Exit.Trap): Push lbl pushes
   lbl; Pop lbl requires TT = lbl :: TT0 and pops it. *)
Fixpoint trap_apply (tas : list trap_action)
    (TT : list static_label) : option (list static_label) :=
  match tas with
  | [] => Some TT
  | Push lbl :: tas' => trap_apply tas' (lbl :: TT)
  | Pop lbl :: tas' =>
    match TT with
    | lbl' :: TT0 =>
        if static_label_eqb lbl lbl' then trap_apply tas' TT0 else None
    | [] => None
    end
  end.

(* A translated unit's global context: function declarations and the
   link-time symbol layout.
   ENCODING NOTE: the code addresses functions through linkage; here
   cp_symaddr fixes each symbol's address in M and cp_funs gives each
   code symbol's declaration.  Chapters 17/20 constrain both. *)
Record cmm_fundecl : Type := Mk_cmm_fundecl
  { fd_params : list backend_var;
    fd_body : cmm_expr }.

Record cmm_program : Type := Mk_cmm_program
  { cp_funs : fmap cmm_symbol cmm_fundecl;
    cp_symaddr : fmap cmm_symbol Z }.

(* Observable events: external calls are the only source of observable
   I/O and external mutation (CM.Extcall NOTES).  The event carries the
   CALL-TIME memory: the doc's observed "trace of Cextern effects"
   (CM.Unit.Final) is a trace of applications of the relation
   Cextern(func, vbar, M), and externals read through pointer
   arguments, so the argument-reachable memory at the call is part of
   the observation.  Mirrors Opsem.v's Ev_ccall_* carrying the
   call-time heap (same field order: callee, args, memory, results);
   ch. 20's per-event correspondence relates the two memories on the
   argument-reachable fragment through the representation relation. *)
Inductive cm_event : Type :=
  | CME_extern (func : string) (args : list cmm_value)
               (mem : cmm_mem) (rets : list cmm_value).

(* ENCODING NOTE: sanctioned oracle.  The Cmm-typed image of the
   axiomatized external relation Cextern of 04-opsem.md s6.3; ch. 20
   states the compatibility of the two through the representation
   relation.  cextern_c f args M rets M' reads: calling f on args in
   memory M may return rets leaving memory M'. *)
Parameter cextern_c :
  string -> list cmm_value -> cmm_mem ->
  list cmm_value -> cmm_mem -> Prop.

(* ------------------------------------------------------------------ *)
(* 15 s4: left-to-right evaluation contexts. *)

Inductive cm_ectx : Type :=
  | CE_hole
  | CE_let (x : backend_var) (E : cm_ectx) (e2 : cmm_expr)
  | CE_seq (E : cm_ectx) (e2 : cmm_expr)
  | CE_tuple (done : list cmm_value) (E : cm_ectx)
             (rest : list cmm_expr)
  | CE_op (op : cmm_op) (done : list cmm_value) (E : cm_ectx)
          (rest : list cmm_expr)
  | CE_if (E : cm_ectx) (e_then : cmm_expr) (e_else : cmm_expr)
  | CE_switch (E : cm_ectx) (index : list nat) (cases : list cmm_expr)
  | CE_exit (lbl : exit_label) (done : list cmm_value) (E : cm_ectx)
            (rest : list cmm_expr) (tas : list trap_action).

Fixpoint cm_plug (E : cm_ectx) (e : cmm_expr) : cmm_expr :=
  match E with
  | CE_hole => e
  | CE_let x E' e2 => Clet x (cm_plug E' e) e2
  | CE_seq E' e2 => Csequence (cm_plug E' e) e2
  | CE_tuple done E' rest =>
      Ctuple (map Cval done ++ cm_plug E' e :: rest)
  | CE_op op done E' rest =>
      Cop op (map Cval done ++ cm_plug E' e :: rest)
  | CE_if E' et ef => Cifthenelse (cm_plug E' e) et ef
  | CE_switch E' index cases => Cswitch (cm_plug E' e) index cases
  | CE_exit lbl done E' rest tas =>
      Cexit lbl (map Cval done ++ cm_plug E' e :: rest) tas
  end.

(* ------------------------------------------------------------------ *)
(* 15 s5: [[op]]c, the pure/arithmetic denotations, on untagged 64-bit
   words and IEEE floats -- the same functions as the Flambda
   naked-number denotations of ch. 05. *)

Definition word_of_bool (b : bool) : cmm_value :=
  CV_word (if b then 1 else 0).

Definition eval_intcmp (c : integer_comparison) (m n : Z) : bool :=
  match c with
  | Ceq => to_unsigned64 m =? to_unsigned64 n
  | Cne => negb (to_unsigned64 m =? to_unsigned64 n)
  | Clt => to_signed64 m <? to_signed64 n
  | Cgt => to_signed64 n <? to_signed64 m
  | Cle => to_signed64 m <=? to_signed64 n
  | Cge => to_signed64 n <=? to_signed64 m
  | Cult => to_unsigned64 m <? to_unsigned64 n
  | Cugt => to_unsigned64 n <? to_unsigned64 m
  | Cule => to_unsigned64 m <=? to_unsigned64 n
  | Cuge => to_unsigned64 n <=? to_unsigned64 m
  end.

Definition eval_fcmp64 (c : float_comparison) (x y : float64) : bool :=
  match c with
  | CFeq => f64_eqb x y
  | CFneq => negb (f64_eqb x y)
  | CFlt => f64_ltb x y
  | CFnlt => negb (f64_ltb x y)
  | CFgt => f64_ltb y x
  | CFngt => negb (f64_ltb y x)
  | CFle => f64_leb x y
  | CFnle => negb (f64_leb x y)
  | CFge => f64_leb y x
  | CFnge => negb (f64_leb y x)
  end.

Definition eval_fcmp32 (c : float_comparison) (x y : float32) : bool :=
  match c with
  | CFeq => f32_eqb x y
  | CFneq => negb (f32_eqb x y)
  | CFlt => f32_ltb x y
  | CFnlt => negb (f32_ltb x y)
  | CFgt => f32_ltb y x
  | CFngt => negb (f32_ltb y x)
  | CFle => f32_leb x y
  | CFnle => negb (f32_leb x y)
  | CFge => f32_leb y x
  | CFnge => negb (f32_leb y x)
  end.

(* Cstatic_cast (Float_of_int Float32) is a SINGLE rounding: this is
   the exact site the int->float32 double-rounding concern (ch. 13
   s4.7, ch. 18) is measured against (CM.Op.Pure NOTES). *)
Definition cm_static_cast (sc : static_cast) (v : cmm_value)
  : option cmm_value :=
  match sc, v with
  | SC_float_of_int Float64, CV_word n =>
      Some (CV_flt (f64_of_Z (to_signed64 n)))
  | SC_float_of_int Float32, CV_word n =>
      Some (CV_flt32 (f32_of_Z (to_signed64 n)))
  | SC_int_of_float Float64, CV_flt f =>
    match f64_to_Z f with
    | Some z => Some (CV_word (wrap 64 z))
    | None => None
    end
  | SC_int_of_float Float32, CV_flt32 f =>
    match f32_to_Z f with
    | Some z => Some (CV_word (wrap 64 z))
    | None => None
    end
  | SC_float_of_float32, CV_flt32 f => Some (CV_flt (f64_of_f32 f))
  | SC_float32_of_float, CV_flt f => Some (CV_flt32 (f32_of_f64 f))
  | _, _ => None
  end.

(* ENCODING NOTE: where cmm.mli leaves upper bits unspecified
   (Float_of_float32 writes only the bottom 32 bits;
   Int32_of_float32's upper word), we fix them: zero for float bit
   patterns, sign-extension for the int32 result (matching to_cmm's
   int32 sign-extension discipline, ch. 18). *)
Definition cm_reinterpret_cast (rc : reinterpret_cast) (v : cmm_value)
  : option cmm_value :=
  match rc, v with
  | RC_int_of_value, CV_word n => Some (CV_word n)
  | RC_value_of_int, CV_word n => Some (CV_word n)
  | RC_float_of_float32, CV_flt32 f =>
      Some (CV_flt (f64_of_bits (f32_to_bits f)))
  | RC_float32_of_float, CV_flt f =>
      Some (CV_flt32 (f32_of_bits (f64_to_bits f mod 2 ^ 32)))
  | RC_float_of_int64, CV_word n =>
      Some (CV_flt (f64_of_bits (to_unsigned64 n)))
  | RC_int64_of_float, CV_flt f =>
      Some (CV_word (wrap 64 (f64_to_bits f)))
  | RC_float32_of_int32, CV_word n =>
      Some (CV_flt32 (f32_of_bits (to_unsigned64 n mod 2 ^ 32)))
  | RC_int32_of_float32, CV_flt32 f =>
      Some (CV_word (wrap 64 (sign_extend 32 (f32_to_bits f))))
  | _, _ => None
  end.

(* [[op]]c for the CM.Op.Pure op set; None = undef (Cdivi/Cmodi by 0,
   Cstatic_cast out of range) or a shape mismatch.  Any op outside the
   set (loads, stores, allocs, calls, ...) is None here and has its own
   rule.
   ENCODING NOTE: shift amounts outside [0, 63] are modeled as undef;
   the doc leaves them unspecified and to_cmm never emits them
   unguarded.  Cdivi/Cmodi are truncated (round-toward-zero) division
   with two's-complement wrap, so min_int / -1 wraps to min_int (the
   Arch.division_crashes_on_overflow guard is a hardware-trap
   property, not a value property; CM.Op.Pure NOTES). *)
Definition cm_op_pure (op : cmm_op) (vs : list cmm_value)
  : option cmm_value :=
  match op, vs with
  | Caddi, [CV_word m; CV_word n] => Some (CV_word (wrap 64 (m + n)))
  | Caddv, [CV_word m; CV_word n] => Some (CV_word (wrap 64 (m + n)))
  | Cadda, [CV_word m; CV_word n] => Some (CV_word (wrap 64 (m + n)))
  | Csubi, [CV_word m; CV_word n] => Some (CV_word (wrap 64 (m - n)))
  | Cmuli, [CV_word m; CV_word n] => Some (CV_word (wrap 64 (m * n)))
  | Cdivi, [CV_word m; CV_word n] =>
      if to_unsigned64 n =? 0 then None
      else Some (CV_word (wrap 64 (Z.quot (to_signed64 m)
                                          (to_signed64 n))))
  | Cmodi, [CV_word m; CV_word n] =>
      if to_unsigned64 n =? 0 then None
      else Some (CV_word (wrap 64 (Z.rem (to_signed64 m)
                                         (to_signed64 n))))
  | Cand, [CV_word m; CV_word n] =>
      Some (CV_word (wrap 64 (Z.land (to_unsigned64 m)
                                     (to_unsigned64 n))))
  | Cor, [CV_word m; CV_word n] =>
      Some (CV_word (wrap 64 (Z.lor (to_unsigned64 m)
                                    (to_unsigned64 n))))
  | Cxor, [CV_word m; CV_word n] =>
      Some (CV_word (wrap 64 (Z.lxor (to_unsigned64 m)
                                     (to_unsigned64 n))))
  | Clsl, [CV_word m; CV_word n] =>
      let s := to_signed64 n in
      if (0 <=? s) && (s <? 64)
      then Some (CV_word (wrap 64 (Z.shiftl (to_unsigned64 m) s)))
      else None
  | Clsr, [CV_word m; CV_word n] =>
      let s := to_signed64 n in
      if (0 <=? s) && (s <? 64)
      then Some (CV_word (wrap 64 (Z.shiftr (to_unsigned64 m) s)))
      else None
  | Casr, [CV_word m; CV_word n] =>
      let s := to_signed64 n in
      if (0 <=? s) && (s <? 64)
      then Some (CV_word (wrap 64 (Z.shiftr (to_signed64 m) s)))
      else None
  | Ccmpi c, [CV_word m; CV_word n] =>
      Some (word_of_bool (eval_intcmp c m n))
  | Cnegf Float64, [CV_flt f] => Some (CV_flt (f64_neg f))
  | Cnegf Float32, [CV_flt32 f] => Some (CV_flt32 (f32_neg f))
  | Caddf Float64, [CV_flt x; CV_flt y] =>
      Some (CV_flt (f64_add x y))
  | Caddf Float32, [CV_flt32 x; CV_flt32 y] =>
      Some (CV_flt32 (f32_add x y))
  | Csubf Float64, [CV_flt x; CV_flt y] =>
      Some (CV_flt (f64_sub x y))
  | Csubf Float32, [CV_flt32 x; CV_flt32 y] =>
      Some (CV_flt32 (f32_sub x y))
  | Cmulf Float64, [CV_flt x; CV_flt y] =>
      Some (CV_flt (f64_mul x y))
  | Cmulf Float32, [CV_flt32 x; CV_flt32 y] =>
      Some (CV_flt32 (f32_mul x y))
  | Cdivf Float64, [CV_flt x; CV_flt y] =>
      Some (CV_flt (f64_div x y))
  | Cdivf Float32, [CV_flt32 x; CV_flt32 y] =>
      Some (CV_flt32 (f32_div x y))
  | Ccmpf Float64 c, [CV_flt x; CV_flt y] =>
      Some (word_of_bool (eval_fcmp64 c x y))
  | Ccmpf Float32 c, [CV_flt32 x; CV_flt32 y] =>
      Some (word_of_bool (eval_fcmp32 c x y))
  | Cstatic_cast sc, [v] => cm_static_cast sc v
  | Creinterpret_cast rc, [v] => cm_reinterpret_cast rc v
  | Copaque, [v] => Some v
  | _, _ => None
  end.

(* ------------------------------------------------------------------ *)
(* 15 s7 (stated early; used by cm_returns below): reaching the
   enclosing function's return. *)

(** RULE CM.Exit.Return (STATUS normative) -- 15-cmm.md
    CODE backend/cmm.mli#exit_label
    CODE backend/cmm_helpers.ml#trap_return

    A Cexit(Return_lbl, [vbar], tabar) is not a step to another
    configuration: it returns vbar from the current function body, with
    TT updated by the trap actions (a Pop lbl requires TT = lbl :: TT0;
    to_cmm does not emit a Push on a return exit).  The pending
    evaluation context is discarded (s4).  cm_return_config c vs TT'
    reads: configuration c has reached a return exit with result values
    vs and updated trap stack TT'. *)
Inductive cm_return_config
  : cm_config -> list cmm_value -> list static_label -> Prop :=
  | CM_Exit_Return :
      forall E vs tas ce chi M TT TT' RR,
      trap_apply tas TT = Some TT' ->
      cm_return_config
        (CmCfg (cm_plug E (Cexit Return_lbl (map Cval vs) tas))
               ce chi M TT RR)
        vs TT'.

(* An uncaught exception: a raise with the base (empty) trap stack
   (used by cm_escapes in the machine below and by CM.Unit.Final). *)
Definition cm_uncaught_config (c : cm_config) (v_exn : cmm_value)
  : Prop :=
  exists E rk v_extra,
    cc_expr c =
      cm_plug E (Cop (Craise rk) (Cval v_exn :: map Cval v_extra)) /\
    cc_traps c = [].

(* Infinite event traces, for CM.Unit.Final's reactive divergence
   (mirrors Opsem.v's event_stream; CORRESPONDENCE entry 11). *)
CoInductive cm_event_stream : Type :=
  | CMS_cons (e : cm_event) (s : cm_event_stream).

Fixpoint stream_prepend (tr : list cm_event) (s : cm_event_stream)
  : cm_event_stream :=
  match tr with
  | [] => s
  | e :: tr' => CMS_cons e (stream_prepend tr' s)
  end.

(* ------------------------------------------------------------------ *)
(* The machine.
   ENCODING NOTE: ch. 19 (CmmMemory.v) contributes further rules to
   this same transition system: Calloc / Cbeginregion / Cendregion as
   redex steps, and the moving GC as a configuration-level step.  Since
   an Inductive is closed, the machine is parameterized over those two
   extension hooks; Cmm.v alone uses them nowhere, and CmmMemory.v
   closes them (defining the full ch. 15+19 cm_step).  Labels: a step
   emits a (possibly empty) list of observable events; only CM.Extcall
   emits. *)

Section CmmMachine.

Variable P : cmm_program.
Variable head_ext : cm_config -> list cm_event -> cm_config -> Prop.
Variable step_ext : cm_config -> list cm_event -> cm_config -> Prop.
(* ch. 19/20 hook: configurations halted by allocation failure /
   resource exhaustion (CM.Unit.Final's fifth outcome). *)
Variable alloc_fails : cm_config -> Prop.

(* cm_head: steps of a redex at the top of the current expression.
   These never transfer control (CM.Context's proviso); the control
   transfers (Cexit / Craise) live in cm_step and discard the
   surrounding context. *)
Inductive cm_head : cm_config -> list cm_event -> cm_config -> Prop :=

  (* ENCODING NOTE: the doc gives no rule ids for the trivial
     structural steps (constants, Cvar, Clet, Csequence, Ctuple
     completion); the constructors CM_Head_Const*, CM_Head_Var,
     CM_Head_Let, CM_Head_Seq, CM_Head_Tuple transcribe the s1 grammar
     glosses ("bind x to the value of ce1 in ce2", "evaluate ce1 for
     effect, then ce2", ...). *)
  | CM_Head_ConstInt :
      forall n ce chi M TT RR,
      cm_head (CmCfg (Cconst_int n) ce chi M TT RR) []
              (CmCfg (Cval (CV_word (wrap 64 n))) ce chi M TT RR)
  | CM_Head_ConstNatint :
      forall n ce chi M TT RR,
      cm_head (CmCfg (Cconst_natint n) ce chi M TT RR) []
              (CmCfg (Cval (CV_word (wrap 64 n))) ce chi M TT RR)
  | CM_Head_ConstFloat :
      forall f ce chi M TT RR,
      cm_head (CmCfg (Cconst_float f) ce chi M TT RR) []
              (CmCfg (Cval (CV_flt f)) ce chi M TT RR)
  | CM_Head_ConstFloat32 :
      forall f ce chi M TT RR,
      cm_head (CmCfg (Cconst_float32 f) ce chi M TT RR) []
              (CmCfg (Cval (CV_flt32 f)) ce chi M TT RR)
  | CM_Head_ConstVec128 :
      forall b ce chi M TT RR,
      cm_head (CmCfg (Cconst_vec128 b) ce chi M TT RR) []
              (CmCfg (Cval (CV_vec128 b)) ce chi M TT RR)
  | CM_Head_ConstVec256 :
      forall b ce chi M TT RR,
      cm_head (CmCfg (Cconst_vec256 b) ce chi M TT RR) []
              (CmCfg (Cval (CV_vec256 b)) ce chi M TT RR)
  | CM_Head_ConstVec512 :
      forall b ce chi M TT RR,
      cm_head (CmCfg (Cconst_vec512 b) ce chi M TT RR) []
              (CmCfg (Cval (CV_vec512 b)) ce chi M TT RR)
  | CM_Head_ConstSymbol :
      forall s a ce chi M TT RR,
      cp_symaddr P s = Some a ->
      cm_head (CmCfg (Cconst_symbol s) ce chi M TT RR) []
              (CmCfg (Cval (CV_word a)) ce chi M TT RR)
  | CM_Head_Var :
      forall x w ce chi M TT RR,
      ce x = Some w ->
      cm_head (CmCfg (Cvar x) ce chi M TT RR) []
              (CmCfg (Cval w) ce chi M TT RR)
  | CM_Head_Let :
      forall x w e2 ce chi M TT RR,
      cm_head (CmCfg (Clet x (Cval w) e2) ce chi M TT RR) []
              (CmCfg e2 (fupd backend_var_eqb ce x w) chi M TT RR)
  | CM_Head_Seq :
      forall w e2 ce chi M TT RR,
      cm_head (CmCfg (Csequence (Cval w) e2) ce chi M TT RR) []
              (CmCfg e2 ce chi M TT RR)
  | CM_Head_Tuple :
      forall vs ce chi M TT RR,
      cm_head (CmCfg (Ctuple (map Cval vs)) ce chi M TT RR) []
              (CmCfg (Cval (CV_tuple vs)) ce chi M TT RR)

  (** RULE CM.Op.Pure (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#operation
      CODE backend/cmm_helpers.ml#add_int
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple

      op in {Caddi,Csubi,Cmuli,Cdivi,Cmodi,Cand,Cor,Cxor,Clsl,Clsr,
      Casr,Ccmpi _,Cnegf _,Caddf _,Csubf _,Cmulf _,Cdivf _,Ccmpf _,
      Cstatic_cast _,Creinterpret_cast _,Caddv,Cadda,Copaque};
      [[op]]c(wbar) = w, undef on the partial cases.  cm_op_pure is
      [[.]]c; it is None outside the op set and on undef. *)
  | CM_Op_Pure :
      forall op vs v ce chi M TT RR,
      cm_op_pure op vs = Some v ->
      cm_head (CmCfg (Cop op (map Cval vs)) ce chi M TT RR) []
              (CmCfg (Cval v) ce chi M TT RR)

  (** RULE CM.Op.TupleField (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Ctuple_field
      CODE backend/cmm_helpers.ml#tuple_field
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_external_call

      Projects component n out of a k-value bundle; tys is
      register-allocation info, not part of the value semantics. *)
  | CM_Op_TupleField :
      forall n tys vs v ce chi M TT RR,
      nth_error vs n = Some v ->
      cm_head (CmCfg (Cop (Ctuple_field n tys)
                          [Cval (CV_tuple vs)]) ce chi M TT RR) []
              (CmCfg (Cval v) ce chi M TT RR)

  (** RULE CM.Load (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Cload
      CODE backend/cmm_helpers.ml#mk_load_immut

      undef (no step) if read(M,a,k) is undef (s2).  Mutability is a
      hint; it does not change the value read. *)
  | CM_Load :
      forall k mu a v ce chi M TT RR,
      cm_read M (to_unsigned64 a) k = Some v ->
      cm_head (CmCfg (Cop (Cload k mu false) [Cval (CV_word a)])
                     ce chi M TT RR) []
              (CmCfg (Cval v) ce chi M TT RR)

  (** RULE CM.Store (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Cstore
      CODE backend/cmm_helpers.ml#setfield_computed

      The bare store is totalized to word 1 (unit); its value is never
      consumed on its own (to_cmm emits Csequence(Cstore .., Cconst_int
      1)).  The Initialization/Assignment distinction selects the GC
      write barrier, which does not change M's value semantics at this
      level. *)
  | CM_Store :
      forall k init a v M' ce chi M TT RR,
      cm_write M (to_unsigned64 a) k v = Some M' ->
      cm_head (CmCfg (Cop (Cstore k init)
                          [Cval (CV_word a); Cval v]) ce chi M TT RR) []
              (CmCfg (Cval (CV_word 1)) ce chi M' TT RR)

  (** RULE CM.If (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Cifthenelse
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch

      e_t if n <> 0, e_f if n = 0. *)
  | CM_If :
      forall n e_t e_f ce chi M TT RR,
      cm_head (CmCfg (Cifthenelse (Cval (CV_word n)) e_t e_f)
                     ce chi M TT RR) []
              (CmCfg (if to_unsigned64 n =? 0 then e_f else e_t)
                     ce chi M TT RR)

  (** RULE CM.Switch (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Cswitch
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch
      CODE backend/cmm_helpers.ml#transl_switch_clambda

      0 <= n < |index| is enforced by the nth_error premise on the
      unsigned reading of n; n outside the table has no transition. *)
  | CM_Switch :
      forall n index cases i ce_i ce chi M TT RR,
      nth_error index (Z.to_nat (to_unsigned64 n)) = Some i ->
      nth_error cases i = Some ce_i ->
      cm_head (CmCfg (Cswitch (Cval (CV_word n)) index cases)
                     ce chi M TT RR) []
              (CmCfg ce_i ce chi M TT RR)

  (** RULE CM.Catch.NonRec (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Ccatch
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_not_inlined
      CODE backend/cmm_helpers.ml#create_ccatch

      Install the handler (chi_def = chi before adding lbl: a Normal
      catch is non-recursive) and run the body. *)
  | CM_Catch_NonRec :
      forall lbl xs e_h cold e_body ce chi M TT RR,
      cm_head (CmCfg (Ccatch Normal [SHandler lbl xs e_h cold] e_body)
                     ce chi M TT RR) []
              (CmCfg e_body ce
                     (fupd static_label_eqb chi lbl
                        (CHandler xs e_h ce chi (length TT) Normal))
                     M TT RR)

  (** RULE CM.Catch.Rec (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Ccatch
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_rec

      chi' = chi[lbl_i |-> CHandler <xs_i, e_i, ce, chi', |TT|,
      Normal>]: every handler's chi_def is the extended chi' (a fixed
      point).  Encoded via chi_bind_rec / CHandler_rec, which re-ties
      chi' at lookup (ENCODING NOTE at cmm_handler; catalog #1). *)
  | CM_Catch_Rec :
      forall group e_body ce chi M TT RR,
      cm_head (CmCfg (Ccatch Recursive group e_body) ce chi M TT RR) []
              (CmCfg e_body ce
                     (chi_bind_rec group ce chi (length TT)) M TT RR)

  (** RULE CM.Catch.Exn (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Exn_handler
      CODE backend/cmm_helpers.ml#trywith
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_exn_handler

      CM.Catch.NonRec with the kind tag Exn_handler; installing it
      does NOT touch TT (all trap-stack motion is via explicit trap
      actions and CM.Raise; modelling entry as a push would
      double-push). *)
  | CM_Catch_Exn :
      forall lbl_h x_exn xs_extra e_h cold e_body ce chi M TT RR,
      cm_head (CmCfg (Ccatch Exn_handler
                        [SHandler lbl_h (x_exn :: xs_extra) e_h cold]
                        e_body)
                     ce chi M TT RR) []
              (CmCfg e_body ce
                     (fupd static_label_eqb chi lbl_h
                        (CHandler (x_exn :: xs_extra) e_h ce chi
                           (length TT) Exn_handler))
                     M TT RR)

  (** RULE CM.Apply (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Capply
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply0

      w_code addresses a function whose code, applied to the argument
      values, RETURNS: the nested run of that function's body (fresh
      variable and catch environments, trap stack at its own base,
      region stack starting at the caller's RR) reaches a
      Cexit(Return_lbl, [rbar], []) with memory M', traps balanced
      back to the base and regions back at RR (cm_returns).  ce, chi,
      TT, RR are unchanged across the call;
      the callee's events are the step's label ("the callee's Cextern
      events are part of the caller's trace").  A nested run has
      exactly three outcomes, each visible at the call site: it
      returns (this rule), it raises an exception that exhausts its
      own trap stack (CM.Apply.Raise), or it diverges -- making the
      caller's run diverge (CM.Unit.Final; cm_diverges/cm_reacts).
      ENCODING NOTE: "w_code addresses a function" is
      cp_symaddr/cp_funs agreement on some symbol. *)
  | CM_Apply :
      forall rt rc callees wcode vargs fsym fd tr rvals M'
             ce chi M TT RR,
      cp_symaddr P fsym = Some (to_unsigned64 wcode) ->
      cp_funs P fsym = Some fd ->
      length (fd_params fd) = length vargs ->
      cm_returns (fd_body fd)
                 (venv_bind venv_empty (fd_params fd) vargs)
                 M RR tr rvals M' ->
      cm_head (CmCfg (Cop (Capply rt rc callees)
                          (Cval (CV_word wcode) :: map Cval vargs))
                     ce chi M TT RR)
              tr
              (CmCfg (Cval (cm_bundle rvals)) ce chi M' TT RR)

  (** RULE CM.Apply.Raise (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Capply
      CODE backend/cmm.mli#Craise

      As CM.Apply, but the nested run RAISES: it reaches a Craise of
      v_exn at its base (empty) trap frame, with memory M' and region
      stack RR' (cm_escapes).  The call site steps to a re-raise of
      the escaped value, which the caller's TT then handles (CM.Raise
      / CM.Catch.Exn) or which reaches the base frame as an uncaught
      exception (CM.Unit.Final) -- the runtime's single global
      exception stack, transcribed into the nested-run presentation.
      RR' (not RR) in the conclusion: an escaping raise does not
      implicitly close regions the callee opened (see cm_escapes'
      comment); they stay open for the handler's explicit Cendregion.
      Raise_reraise: the raise kind affects only backtrace
      recording. *)
  | CM_Apply_Raise :
      forall rt rc callees wcode vargs fsym fd tr v_exn M' RR'
             ce chi M TT RR,
      cp_symaddr P fsym = Some (to_unsigned64 wcode) ->
      cp_funs P fsym = Some fd ->
      length (fd_params fd) = length vargs ->
      cm_escapes (fd_body fd)
                 (venv_bind venv_empty (fd_params fd) vargs)
                 M RR tr v_exn M' RR' ->
      cm_head (CmCfg (Cop (Capply rt rc callees)
                          (Cval (CV_word wcode) :: map Cval vargs))
                     ce chi M TT RR)
              tr
              (CmCfg (Cop (Craise Raise_reraise) [Cval v_exn])
                     ce chi M' TT RR')

  (** RULE CM.Extcall (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Cextcall
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_external_call

      (rbar, M') in Cextern(func, vbar, M): the axiomatized external,
      as OS.Apply.CCall; the only source of observable I/O (the emitted
      event).  A raising external transfers to the current trap
      handler (not modeled; the rule covers the returning case). *)
  | CM_Extcall :
      forall func ty vargs rvals M' ce chi M TT RR,
      cextern_c func vargs M rvals M' ->
      cm_head (CmCfg (Cop (Cextcall func ty) (map Cval vargs))
                     ce chi M TT RR)
              [CME_extern func vargs M rvals]
              (CmCfg (Cval (cm_bundle rvals)) ce chi M' TT RR)

  (* ch. 19 hook (see ENCODING NOTE above the Section). *)
  | CM_Head_Ext :
      forall c tr c',
      head_ext c tr c' ->
      cm_head c tr c'

with cm_step : cm_config -> list cm_event -> cm_config -> Prop :=

  (** RULE CM.Context (STATUS descriptive) -- 15-cmm.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
      CODE backend/cmm.mli#expression

      Reduction is congruent under evaluation contexts for
      value-producing redexes.  Left-to-right order is a MODELING
      CHOICE (Cmm does not fix Cop argument order); to_cmm
      pre-sequences interacting effects, making the residual order
      unobservable.  The proviso "r' does not itself transfer control"
      holds by construction: cm_head contains no control transfers;
      the control rules below carry their own (discarded) context E,
      which is the "a control transfer discards the surrounding E"
      clause.
      ENCODING NOTE: descriptive status, but the congruence must be a
      real constructor for the machine to run; the descriptive content
      is the evaluation-order choice documented above. *)
  | CM_Context :
      forall E r r' tr ce chi M TT RR ce' chi' M' TT' RR',
      cm_head (CmCfg r ce chi M TT RR) tr
              (CmCfg r' ce' chi' M' TT' RR') ->
      cm_step (CmCfg (cm_plug E r) ce chi M TT RR) tr
              (CmCfg (cm_plug E r') ce' chi' M' TT' RR')

  (** RULE CM.Exit (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Cexit
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation

      Static jump to an enclosing catch, binding its params to the
      argument values, running in the handler's definition
      environments.  The pending evaluation context E is discarded
      (s4).  |TT| = d holds by the trap-balance invariant. *)
  | CM_Exit :
      forall E lbl vs hv ce chi M TT RR,
      chi_lookup chi lbl = Some hv ->
      length (chv_params hv) = length vs ->
      cm_step (CmCfg (cm_plug E (Cexit (Lbl lbl) (map Cval vs) []))
                     ce chi M TT RR) []
              (CmCfg (chv_body hv)
                     (venv_bind (chv_ce hv) (chv_params hv) vs)
                     (chv_chi hv) M TT RR)

  (** RULE CM.Exit.Trap (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#trap_action
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise

      As CM.Exit, with tabar <> [] executed left-to-right on TT
      (trap_apply). *)
  | CM_Exit_Trap :
      forall E lbl vs tas TT' hv ce chi M TT RR,
      tas <> [] ->
      trap_apply tas TT = Some TT' ->
      chi_lookup chi lbl = Some hv ->
      length (chv_params hv) = length vs ->
      cm_step (CmCfg (cm_plug E (Cexit (Lbl lbl) (map Cval vs) tas))
                     ce chi M TT RR) []
              (CmCfg (chv_body hv)
                     (venv_bind (chv_ce hv) (chv_params hv) vs)
                     (chv_chi hv) M TT' RR)

  (** RULE CM.Raise (STATUS normative) -- 15-cmm.md
      CODE backend/cmm.mli#Craise
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise
      CODE backend/cmm_helpers.ml#raise_prim

      Pops the top trap frame and enters its handler with the
      exception value (first operand) and the extra handler args
      (remaining operands).  raise_kind affects only the backtrace.
      The pending context E is discarded. *)
  | CM_Raise :
      forall E rk v_exn v_extra lbl_h TT' x_exn xs_extra body ce_def
             chi_def d ce chi M TT RR,
      TT = lbl_h :: TT' ->
      chi_lookup chi lbl_h =
        Some (CHView (x_exn :: xs_extra) body ce_def chi_def d
                     Exn_handler) ->
      length xs_extra = length v_extra ->
      cm_step (CmCfg (cm_plug E (Cop (Craise rk)
                                     (Cval v_exn :: map Cval v_extra)))
                     ce chi M TT RR) []
              (CmCfg body
                     (venv_bind ce_def (x_exn :: xs_extra)
                                (v_exn :: v_extra))
                     chi_def M TT' RR)

  (* ch. 19 hook: configuration-level steps (the moving GC). *)
  | CM_Step_Ext :
      forall c tr c',
      step_ext c tr c' ->
      cm_step c tr c'

with cm_run : cm_config -> list cm_event -> cm_config -> Prop :=
  | CM_Run_Refl : forall c, cm_run c [] c
  | CM_Run_Step :
      forall c tr1 c' tr2 c'',
      cm_step c tr1 c' ->
      cm_run c' tr2 c'' ->
      cm_run c (tr1 ++ tr2) c''

(* A nested whole-function run (CM.Apply): the body, in its initial
   environment with fresh variable/handler environments, a trap stack
   at its own base, and the CALLER's region stack RR (so a callee may
   allocate into its caller's open regions -- local-returning
   functions), runs to a return exit with the trap stack balanced back
   to the base and the region stack back at RR. *)
with cm_returns : cmm_expr -> cmm_venv -> cmm_mem ->
                  list region_handle ->
                  list cm_event -> list cmm_value -> cmm_mem -> Prop :=
  | CM_Returns :
      forall body ce0 M RR tr c_f vs,
      cm_run (CmCfg body ce0 kenv_empty M [] RR) tr c_f ->
      cm_return_config c_f vs [] ->
      cc_regions c_f = RR ->
      cm_returns body ce0 M RR tr vs (cc_mem c_f)

(* A nested whole-function run that RAISES (CM.Apply.Raise): the body,
   in its initial environment (region stack inherited, as cm_returns),
   runs to a raise at its base (empty) trap stack.  The final region
   stack is NOT required back at RR: the runtime's unwind restores
   only the exception-handler frame (caml_raise_exn /
   RESTORE_EXN_HANDLER_OCAML, runtime/amd64.S -- caml_local_sp is
   untouched), so regions the callee opened and did not end remain
   open; the handler that catches the exception reclaims them via its
   explicit Cendregion (CM.Region.End's pop-down-to shape). *)
with cm_escapes : cmm_expr -> cmm_venv -> cmm_mem ->
                  list region_handle ->
                  list cm_event -> cmm_value -> cmm_mem ->
                  list region_handle -> Prop :=
  | CM_Escapes :
      forall body ce0 M RR tr c_f v_exn,
      cm_run (CmCfg body ce0 kenv_empty M [] RR) tr c_f ->
      cm_uncaught_config c_f v_exn ->
      cm_escapes body ce0 M RR tr v_exn (cc_mem c_f)
                 (cc_regions c_f).

(* ------------------------------------------------------------------ *)
(* 15 s10: observation and final states. *)

(* Normal unit termination: Cexit(Return_lbl, [vbar], []) with TT
   balanced to the base. *)
Definition cm_unit_final_config (c : cm_config) (vs : list cmm_value)
  : Prop :=
  exists E,
    cc_expr c = cm_plug E (Cexit Return_lbl (map Cval vs) []) /\
    cc_traps c = [].

Definition cm_no_step (c : cm_config) : Prop :=
  forall tr c', ~ cm_step c tr c'.

(* A configuration sitting at a call redex, paired with the nested
   start configuration the call would run (CM.Apply's premises minus
   the nested run's outcome). *)
Definition cm_call_frame (c : cm_config) (c_callee : cm_config)
  : Prop :=
  exists E rt rc callees wcode vargs fsym fd,
    cc_expr c = cm_plug E (Cop (Capply rt rc callees)
                    (Cval (CV_word wcode) :: map Cval vargs)) /\
    cp_symaddr P fsym = Some (to_unsigned64 wcode) /\
    cp_funs P fsym = Some fd /\
    length (fd_params fd) = length vargs /\
    c_callee = CmCfg (fd_body fd)
                     (venv_bind venv_empty (fd_params fd) vargs)
                     kenv_empty (cc_mem c) [] (cc_regions c).

(* The observable prefix of a run, following the active branch into a
   call whose nested run never comes back (CM.Unit.Final's
   "hereditarily" clause).
   ENCODING NOTE: caller-level cm_run only crosses a call when the
   nested run returns or escapes (CM.Apply / CM.Apply.Raise); events
   emitted inside a never-returning callee are reachable only by
   descending at the call frame, which this relation and the two
   coinductives below do.  This is the transcription device for "a
   run reaching a call whose nested run diverges, hereditarily". *)
Inductive cm_reach : cm_config -> list cm_event -> cm_config -> Prop :=
  | CM_Reach_Run :
      forall c tr c',
      cm_run c tr c' ->
      cm_reach c tr c'
  | CM_Reach_Enter :
      forall c tr1 c1 c_callee tr2 c',
      cm_run c tr1 c1 ->
      cm_call_frame c1 c_callee ->
      cm_reach c_callee tr2 c' ->
      cm_reach c (tr1 ++ tr2) c'.

(* Silent divergence: an infinite sequence of label-[] steps and/or
   silent descents into diverging callees. *)
CoInductive cm_diverges : cm_config -> Prop :=
  | CM_Diverges_Step :
      forall c c',
      cm_step c [] c' ->
      cm_diverges c' ->
      cm_diverges c
  | CM_Diverges_Call :
      forall c c_callee,
      cm_call_frame c c_callee ->
      cm_diverges c_callee ->
      cm_diverges c.

(* Reactive divergence: an infinite event trace.  Each constructor
   contributes a nonempty burst of events reached after finitely many
   steps and descents (cm_reach), so the stream is fully generated.
   ENCODING NOTE: mirrors Opsem.v's Beh_react (CORRESPONDENCE entry
   11); the burst shape (rather than a per-step coinductive) is
   forced by nested runs, and the nonempty-burst premise keeps
   silently-diverging programs out of cm_reacts. *)
CoInductive cm_reacts : cm_config -> cm_event_stream -> Prop :=
  | CM_Reacts_Burst :
      forall c tr c' s',
      cm_reach c tr c' ->
      tr <> [] ->
      cm_reacts c' s' ->
      cm_reacts c (stream_prepend tr s').

Inductive cm_outcome : Type :=
  | CMO_normal (module_image : cmm_mem) (tr : list cm_event)
  | CMO_uncaught (v_exn : cmm_value) (tr : list cm_event)
  | CMO_diverges (tr : list cm_event)
  | CMO_reacts (s : cm_event_stream)
  | CMO_undef
  | CMO_resource_exhaustion.

(** RULE CM.Unit.Final (STATUS normative) -- 15-cmm.md
    CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
    CODE backend/cmm.mli#Cdata

    A Cmm run is one of: normal termination (observing the module
    block image in M and the trace of Cextern effects), termination by
    uncaught exception, divergence (an infinite step sequence, or a
    run reaching a call whose nested run diverges, hereditarily),
    undefined behaviour (reaching Cinvalid or a stuck read/store), or
    resource exhaustion (the one outcome absent from the Flambda
    machine; ch. 20 hook alloc_fails).
    ENCODING NOTE: CMO_normal carries the whole final memory; the
    restriction to "the bytes of the module block reachable from the
    module symbol" is applied by ch. 20 through the representation
    relation of ch. 17, which is where the image is compared to the
    Flambda observation.  The doc's single divergence outcome is
    split CMO_diverges (finite trace, then silence) / CMO_reacts
    (infinite trace), mirroring the Flambda side's
    Beh_diverge/Beh_react (CORRESPONDENCE entry 11); their union is
    the doc's divergence.  Escaped exceptions surface as re-raises
    (CM.Apply.Raise), so CMO_uncaught needs no descent device. *)
Inductive cm_unit_behaves (c0 : cm_config) : cm_outcome -> Prop :=
  | CM_Unit_Normal :
      forall tr c_f vs,
      cm_run c0 tr c_f ->
      cm_unit_final_config c_f vs ->
      cm_unit_behaves c0 (CMO_normal (cc_mem c_f) tr)
  | CM_Unit_Uncaught :
      forall tr c_f v_exn,
      cm_run c0 tr c_f ->
      cm_uncaught_config c_f v_exn ->
      cm_unit_behaves c0 (CMO_uncaught v_exn tr)
  | CM_Unit_Diverges :
      forall tr c_f,
      cm_reach c0 tr c_f ->
      cm_diverges c_f ->
      cm_unit_behaves c0 (CMO_diverges tr)
  | CM_Unit_Reacts :
      forall s,
      cm_reacts c0 s ->
      cm_unit_behaves c0 (CMO_reacts s)
  | CM_Unit_Undef :
      forall tr c_f,
      cm_run c0 tr c_f ->
      cm_no_step c_f ->
      (forall vs, ~ cm_unit_final_config c_f vs) ->
      (forall v, ~ cm_uncaught_config c_f v) ->
      ~ alloc_fails c_f ->
      cm_unit_behaves c0 CMO_undef
  | CM_Unit_Resource :
      forall tr c_f,
      cm_run c0 tr c_f ->
      alloc_fails c_f ->
      cm_unit_behaves c0 CMO_resource_exhaustion.

End CmmMachine.

(* ------------------------------------------------------------------ *)
(* 15 s9: Invalid. *)

(** RULE CM.Invalid (STATUS normative) -- 15-cmm.md
    CODE backend/cmm.mli#Cinvalid
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid
    CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#invalid

    A Cinvalid configuration has no transition (undefined behaviour);
    a correct pipeline never reaches it.
    ENCODING NOTE: stated under the hypotheses that the ch. 19
    extension hooks do not step Cinvalid configurations either (they
    do not: their redexes are Calloc/Cbeginregion/Cendregion and
    GC-eligible configurations). *)
Theorem CM_Invalid :
  forall (P : cmm_program)
         (head_ext step_ext :
            cm_config -> list cm_event -> cm_config -> Prop),
  (forall msg ce chi M TT RR tr c',
     ~ head_ext (CmCfg (Cinvalid msg) ce chi M TT RR) tr c') ->
  (forall msg ce chi M TT RR tr c',
     ~ step_ext (CmCfg (Cinvalid msg) ce chi M TT RR) tr c') ->
  forall msg ce chi M TT RR tr c',
    ~ cm_step P head_ext step_ext
        (CmCfg (Cinvalid msg) ce chi M TT RR) tr c'.
Admitted.
