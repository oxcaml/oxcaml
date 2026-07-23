(* ToCmmData.v -- ch. 18 (18-to-cmm-data.md): to_cmm stage 2, the
   data half of the Flambda->Cmm translation: simples, Let-bindings
   and the delayed-binding state, and primitive lowering.

   Section map (doc section -> here):
     s1  simples + correctness schema -> tc_simple + TC_Simple_sound;
                                         TC.Prim.Sound (with s3-s7)
     s2  let-bindings, let-subst      -> data_state (V + the two
                                         stage stacks), tc_let_binding
     s3  tagging and boxing           -> tc_prim
     s4  blocks                       -> tc_prim
     s5  closures                     -> tc_prim
     s6  scalar conversions           -> tc_prim
     s7  strings/arrays/bigarrays     -> tc_prim
     (finally) ch. 16 hook closure: ToCmmControl's Section Variables
     instantiated at data_state and the judgments above.

   Increment-order note: TC.Let.Prim and TC.Prim.Sound both quantify
   over the per-primitive emissions (s3-s7), so tc_prim precedes the
   TC.Let.* rules and the schema in this file, although the doc
   presents s2 before s3.  The s2 STATE (data_state) comes first: it
   is the type every judgment's environment carries.

   Ch. 17's Parameters (slot offsets, header oracle words) are used
   via Representation.v; none are redeclared here.

   Conventions: rocq/CORRESPONDENCE.md.  Wave 5; owner: Milner. *)

(* String is imported BEFORE List so that List's [length] wins the
   name clash (the Representation.v lesson); [List.length] is written
   qualified throughout regardless. *)
From Stdlib Require Import ZArith Bool String List.
Import ListNotations.
Open Scope Z_scope.

From Flambda2 Require Import Base Syntax Values Cmm PrimMemoryA
  CmmMemory Machine Representation ToCmmControl.

(* ================================================================== *)
(* 1. The data state D (18 s2: to_cmm_env's delayed bindings)         *)
(* ================================================================== *)

(* One delayed let-binding: the Flambda variable and its defining Cmm
   expression (to_cmm_env.ml keeps translated Cmm on both stacks). *)
Record delayed_binding : Type := Mk_delayed_binding {
  db_var : variable;
  db_defn : cmm_expr
}.

(* classify_let_binding's classification (to_cmm_effects.ml), the
   dispatch of TC.Let.Prim. *)
Inductive let_binding_class : Type :=
  | LB_drop_defining_expr
  | LB_regular
  | LB_may_inline_once
  | LB_must_inline_once
  | LB_must_inline_and_duplicate.

(* effect_stages (TC.Let.Subst premise text): a stage is a set of
   coeffect-only bindings, or one effectful binding. *)
Inductive effect_stage : Type :=
  | ES_coeffect_only (bs : list delayed_binding)
  | ES_effectful (b : delayed_binding).

(* validity_stages: Depend_on_control_flow is a set of bindings that
   must not be hoisted above a preceding branch (a pure binding is
   off effect_stages but ON validity_stages); Control_flow_point is a
   barrier nothing crosses. *)
Inductive validity_stage : Type :=
  | VS_depend_on_control_flow (bs : list delayed_binding)
  | VS_control_flow_point.

(* D = <V, effect_stages, validity_stages>: V is the doc's simple-
   translation map (a variable's Cmm expression -- a Cvar once bound,
   or the delayed defining expression itself), and the TWO parallel
   stage stacks are TC.Let.Subst's (add_binding_to_env threads a
   binding onto both).  This record closes ch. 16's data_state hook
   (tc_env's tc_data component). *)
Record data_state : Type := Mk_data_state {
  ds_V : fmap variable cmm_expr;
  ds_effect_stages : list effect_stage;
  ds_validity_stages : list validity_stage
}.

Definition tcenv : Type := tc_env data_state.

Definition tc_V (th : tcenv) : fmap variable cmm_expr :=
  ds_V (tc_data th).

Definition upd_V (th : tcenv) (x : variable) (e : cmm_expr) : tcenv :=
  Mk_tc_env (tc_phi th)
    (Mk_data_state
       (fupd variable_eqb (ds_V (tc_data th)) x e)
       (ds_effect_stages (tc_data th))
       (ds_validity_stages (tc_data th))).

(* ================================================================== *)
(* 2. Simples (18 s1)                                                 *)
(* ================================================================== *)

(* "the Cmm constant for c" (TC.Simple): the R.Val.* encodings as
   literal Cmm constants.  tagged_imm n |-> Cconst_int (2n+1); naked
   numbers the corresponding Cconst_* (sub-word ints sign-extended,
   matching R.Val.NakedNumber; int64/nativeint as Cconst_natint per
   to_cmm_shared.ml#const); null is word 0 (R.Val.Pointer).  Poison
   has no denotation (Values.v const_value) and no image. *)
Definition cmm_const_image (c : const) : option cmm_expr :=
  match c with
  | Const_naked_immediate n => Some (Cconst_int n)
  | Const_tagged_immediate n => Some (Cconst_int (2 * n + 1))
  | Const_naked_float f => Some (Cconst_float f)
  | Const_naked_float32 f => Some (Cconst_float32 f)
  | Const_naked_int8 n => Some (Cconst_int (sign_extend 8 n))
  | Const_naked_int16 n => Some (Cconst_int (sign_extend 16 n))
  | Const_naked_int32 n => Some (Cconst_int (sign_extend 32 n))
  | Const_naked_int64 n => Some (Cconst_natint n)
  | Const_naked_nativeint n => Some (Cconst_natint n)
  | Const_naked_vec128 b => Some (Cconst_vec128 b)
  | Const_naked_vec256 b => Some (Cconst_vec256 b)
  | Const_naked_vec512 b => Some (Cconst_vec512 b)
  | Const_null => Some (Cconst_int 0)
  | Const_poison _ _ => None
  end.

(** RULE TC.Simple (CLAIM normative) -- 18-to-cmm-data.md
    CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#simple
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#bind_var_to_simple

    Theta |- x ~>v V(x)  (a Cvar, or the delayed defining expression:
    bind_var_to_simple / add_alias);
    Theta |- sym ~>v Cconst_symbol sym;
    Theta |- c ~>v the Cmm constant for c;
    Theta |- (s @ co) ~>v whatever s translates to (coercions are
    identity at runtime, OS.Simple.Eval, and erased).
    The correctness sentence below the rule's line is the companion
    conjecture TC_Simple_sound, under this same header. *)
Inductive tc_simple (th : tcenv) : simple -> cmm_expr -> Prop :=
  | TC_Simple_Var : forall x e,
      tc_V th x = Some e ->
      tc_simple th (Simple_name (Name_var x) Coercion_id) e
  | TC_Simple_Sym : forall sym,
      tc_simple th (Simple_name (Name_sym sym) Coercion_id)
        (Cconst_symbol (CS_sym sym))
  | TC_Simple_Const : forall c e,
      cmm_const_image c = Some e ->
      tc_simple th (Simple_const c) e
  | TC_Simple_Coerce : forall n co e,
      tc_simple th (Simple_name n Coercion_id) e ->
      tc_simple th (Simple_name n co) e.

(* "the value of e in ce, M" (TC.Simple's correctness sentence): from
   e in that environment and memory, the full ch. 15+19 machine runs
   silently to a value.  A delayed defining expression may read or
   allocate (coeffects, generative effects), so the rest of the final
   configuration is existential. *)
Definition cm_evals_to (P : cmm_program) (c : cm_config)
    (w : cmm_value) : Prop :=
  exists c', cmem_run P c [] c' /\ cc_expr c' = Cval w.

(* rho ~ ce (TC.Simple's premise): each rho(x) related to the
   ce-value of V(x).
   ENCODING NOTE: the doc writes the premise over variables only
   ("each rho(x)").  OS.Simple.Eval's encoding consults rho first for
   SYMBOLS too (rho-first-with-fallback, catalog 16: OS.Let.Static
   rebinds static set-of-closures symbols to clos values), so the
   premise gains the matching symbol conjunct; without it the sym
   clause of the sentence is false for rho-rebound symbols. *)
Definition env_rep (P : cmm_program) (th : tcenv) (L : loc_map)
    (ce : cmm_venv) (chi : cmm_kenv) (M : cmm_mem)
    (TT : list static_label) (RR : list region_handle)
    (rho : env) : Prop :=
  (forall x v,
     rho (Name_var x) = Some v ->
     exists e w,
       tc_V th x = Some e /\
       cm_evals_to P (CmCfg e ce chi M TT RR) w /\
       rep_val L v w) /\
  (forall sym v,
     rho (Name_sym sym) = Some v ->
     exists w,
       cm_evals_to P (CmCfg (Cconst_symbol (CS_sym sym)) ce chi M TT RR)
         w /\
       rep_val L v w).

(* L agrees with the link-time symbol table on symbol addresses
   (R.Heap's symbol clause, stated as the premise TC_Simple_sound
   needs for the fallback ptr-sym case). *)
Definition symaddr_agree (P : cmm_program) (L : loc_map) : Prop :=
  forall sym, L (Addr_sym sym) = cp_symaddr P (CS_sym sym).

(* TC.Simple, the sentence below the line (same header as above):
   for every simple s and value v = [[s]]rho, if rho ~ ce then
   Theta |- s ~>v e implies v related to the value of e in ce, M. *)
Theorem TC_Simple_sound :
  forall (P : cmm_program) (th : tcenv) (L : loc_map)
         (rho : env) (ce : cmm_venv) (chi : cmm_kenv) (M : cmm_mem)
         (TT : list static_label) (RR : list region_handle)
         (s : simple) (v : value) (e : cmm_expr),
    simple_eval rho s = Some v ->
    env_rep P th L ce chi M TT RR rho ->
    symaddr_agree P L ->
    tc_simple th s e ->
    exists w,
      cm_evals_to P (CmCfg e ce chi M TT RR) w /\
      rep_val L v w.
Admitted.

(* ================================================================== *)
(* 3. Smart-constructor images (cmm_helpers shapes for s3-s7)         *)
(* ================================================================== *)

(* tag_int e = 2e+1; untag_int e = floor(e/2) (arithmetic shift).
   The code's peepholes (constant folding, asr-pattern folds) are
   elided: the rules state the canonical shapes. *)
Definition tag_int_image (e : cmm_expr) : cmm_expr :=
  Cop Caddi [Cop Clsl [e; Cconst_int 1]; Cconst_int 1].

Definition untag_int_image (e : cmm_expr) : cmm_expr :=
  Cop Casr [e; Cconst_int 1].

(* cmm_helpers sign_extend at width [bits]:
   (e << (64-bits)) asr (64-bits). *)
Definition sign_extend_image (bits : Z) (e : cmm_expr) : cmm_expr :=
  Cop Casr [Cop Clsl [e; Cconst_int (64 - bits)];
            Cconst_int (64 - bits)].

(* field_address a off (WORD offset): Cadda by 8*off, folded to the
   bare pointer when off = 0 (TC.Prim.BlockLoad's parenthetical). *)
Definition field_address_image (a : cmm_expr) (off : Z) : cmm_expr :=
  if off =? 0 then a else Cop Cadda [a; Cconst_int (8 * off)].

(* infix_field_address a off (WORD offset, possibly negative): Caddv
   by 8*off, same zero fold (TC.Prim.ProjectFunctionSlot). *)
Definition infix_field_address_image (a : cmm_expr) (off : Z)
  : cmm_expr :=
  if off =? 0 then a else Cop Caddv [a; Cconst_int (8 * off)].

(* C.return_unit: evaluate the store for effect, produce unit (1). *)
Definition return_unit_image (e : cmm_expr) : cmm_expr :=
  Csequence e (Cconst_int 1).

(* A void-typed runtime-function call (caml_modify and kin). *)
Definition extcall_void_image (fn : String.string)
    (args : list cmm_expr) : cmm_expr :=
  Cop (Cextcall fn []) args.

(* Flambda modes -> Cmm's (the Alloc_local region variable is the
   abstract side's bookkeeping; the Cmm side allocates in the
   innermost open region, CM.Alloc.Local). *)
Definition cmm_mode (am : alloc_mode_alloc) : cmm_alloc_mode :=
  match am with
  | Alloc_heap => CAM_heap
  | Alloc_local _ => CAM_local
  end.

(* Header color of a fresh allocation: white on the heap, caml_local
   in a region (R.Header's color table; CM.Alloc.Local's
   header_is_local premise, which ch. 17 constrains). *)
Definition alloc_col (am : alloc_mode_alloc) : Z :=
  match am with
  | Alloc_heap => col_white
  | Alloc_local _ => col_local
  end.

Definition cmm_mut (mu : mutability) : cmm_mutable_flag :=
  match mu with
  | Mutable => CMut_mutable
  | Immutable | Immutable_unique => CMut_immutable
  end.

Definition cmm_init (ia : init_or_assign)
  : initialization_or_assignment :=
  match ia with
  | IOA_initialization => Initialization
  | IOA_assignment _ => Assignment
  end.

(* Alloc_block_kind of each boxed number (TC.Prim.BoxUnbox names
   box_float / box_int_gen / box_vecN and Alloc_block_kind_vecN). *)
Definition box_alloc_kind (bn : boxable_number) : alloc_block_kind :=
  match bn with
  | BN_naked_float => Alloc_block_kind_float
  | BN_naked_float32 => Alloc_block_kind_float32
  | BN_naked_int32 => Alloc_block_kind_boxed_int Boxed_int32
  | BN_naked_int64 => Alloc_block_kind_boxed_int Boxed_int64
  | BN_naked_nativeint => Alloc_block_kind_boxed_int Boxed_nativeint
  | BN_naked_vec128 => Alloc_block_kind_vec128
  | BN_naked_vec256 => Alloc_block_kind_vec256
  | BN_naked_vec512 => Alloc_block_kind_vec512
  end.

(* Box_number's Calloc: header, then the ops word for the custom
   kinds, then the payload -- headers, ops-word strings, and offsets
   byte-for-byte the R.Obj.Boxed clauses (Representation.v), so the
   commuting obligation relates the two directly.  Little-endian
   target -- ENDIANNESS-DEPENDENT per the doc NOTES.
   ENCODING NOTE (emission review, Church): CF-3 -- the code emits
   the ops word as a Cconst_symbol (box_int_gen, box_float32); the
   Cconst_natint (custom_ops_word ...) here is the same word under
   symaddr_agree, following R.Obj.Boxed's byte-for-byte framing.
   CF-4 -- box_int_gen wraps the int32 payload in an explicit
   32-bit sign_extend before storing; elided here, value-equal: a
   naked_int32 word is already sign-extended
   (R.Val.NakedNumber). *)
Definition box_number_image (bn : boxable_number)
    (am : alloc_mode_alloc) (e : cmm_expr) : cmm_expr :=
  let col := alloc_col am in
  let alloc := Cop (Calloc (cmm_mode am) (box_alloc_kind bn)) in
  match bn with
  | BN_naked_float =>
      alloc [Cconst_natint (hdr_word double_tag 1 col 0); e]
  | BN_naked_float32 =>
      alloc [Cconst_natint (hdr_word custom_tag 2 col 0);
             Cconst_natint (custom_ops_word "caml_float32_ops"%string);
             e]
  | BN_naked_int32 =>
      alloc [Cconst_natint (hdr_word custom_tag 2 col 0);
             Cconst_natint (custom_ops_word "caml_int32_ops"%string);
             e]
  | BN_naked_int64 =>
      alloc [Cconst_natint (hdr_word custom_tag 2 col 0);
             Cconst_natint (custom_ops_word "caml_int64_ops"%string);
             e]
  | BN_naked_nativeint =>
      alloc
        [Cconst_natint (hdr_word custom_tag 2 col 0);
         Cconst_natint (custom_ops_word "caml_nativeint_ops"%string);
         e]
  | BN_naked_vec128 =>
      alloc [Cconst_natint (hdr_word 0 2 col 1); e]
  | BN_naked_vec256 =>
      alloc [Cconst_natint (hdr_word 0 4 col 1); e]
  | BN_naked_vec512 =>
      alloc [Cconst_natint (hdr_word 0 8 col 1); e]
  end.

(* Unbox_number's payload load: at offset 8 for the custom kinds
   (skip the ops word), at 0 for a bare float or a boxed vector;
   vector chunks always UNALIGNED (boxed vectors are only
   word-aligned).  The box-of-unbox and static-symbol peepholes of
   the doc NOTES are elided. *)
Definition unbox_number_image (bn : boxable_number) (a : cmm_expr)
  : cmm_expr :=
  match bn with
  | BN_naked_float => Cop (Cload Double CMut_immutable false) [a]
  | BN_naked_float32 =>
      Cop (Cload (Single Float32) CMut_immutable false)
          [Cop Cadda [a; Cconst_int 8]]
  | BN_naked_int32 =>
      Cop (Cload Thirtytwo_signed CMut_immutable false)
          [Cop Cadda [a; Cconst_int 8]]
  | BN_naked_int64 | BN_naked_nativeint =>
      Cop (Cload Word_int CMut_immutable false)
          [Cop Cadda [a; Cconst_int 8]]
  | BN_naked_vec128 =>
      Cop (Cload Onetwentyeight_unaligned CMut_immutable false) [a]
  | BN_naked_vec256 =>
      Cop (Cload Twofiftysix_unaligned CMut_immutable false) [a]
  | BN_naked_vec512 =>
      Cop (Cload Fivetwelve_unaligned CMut_immutable false) [a]
  end.

(* R.Header word of a make_block allocation, by block kind (n = the
   field count; a mixed block's size comes from its shape, whose
   flat suffix may pack sub-word). *)
Definition make_block_header (bk : block_kind) (n col : Z) : Z :=
  match bk with
  | BK_values t _ => hdr_word (tag_z t) n col 0
  | BK_naked_floats => hdr_word double_array_tag n col 0
  | BK_mixed t sigma =>
      hdr_word (tag_z t) (size_in_words sigma) col
        (Z.of_nat (value_prefix_size sigma) + 1)
  end.

(* The field's chunk per access kind (TC.Prim.BlockLoad's "Word_val /
   Word_int / Double / ..."). *)
Definition bafk_chunk (fk : block_access_field_kind) : memory_chunk :=
  match fk with
  | BAFK_any_value => Word_val
  | BAFK_immediate => Word_int
  end.

Definition bak_field_chunk (bak : block_access_kind) : memory_chunk :=
  match bak with
  | BAK_values _ _ fk => bafk_chunk fk
  | BAK_naked_floats _ => Double
  | BAK_mixed _ _ (MBAFK_value_prefix fk) _ => bafk_chunk fk
  | BAK_mixed _ _ (MBAFK_flat_suffix fse) _ => fse_chunk fse
  end.

(* off = i for uniform blocks, Mixed_block_shape.offset_in_words for
   mixed (which is i itself on the value prefix). *)
Definition bak_field_offset (bak : block_access_kind) (i : Z) : Z :=
  match bak with
  | BAK_mixed _ _ _ sigma => offset_in_words sigma (Z.to_nat i)
  | _ => i
  end.

(* Block_set's store at the field address (setfield_computed's
   dispatch): a heap value assignment MUST go through caml_modify
   (GC write barrier), local / initializing value writes take
   caml_modify_local / caml_initialize; immediate fields are plain
   Cstore(Word_int); naked-float and mixed flat fields are plain
   chunked Cstores.
   ENCODING NOTE (emission review, Church): CF-1 -- unlike the
   two-argument field-ADDRESS forms of caml_modify and
   caml_initialize, caml_modify_local is the THREE-argument runtime
   call taking the block, the UNTAGGED word index, and the value
   (cmm_helpers.ml addr_array_set_local and setfield's local case).
   CF-5 -- the code wraps sub-word stores' values in an explicit
   low_bits; elided here, value-equal: the chunk store truncates
   identically.
   CF-6 -- the immediate-field store is a constant Assignment:
   block_set wraps even static field indices as expressions
   (to_cmm_primitive.ml int_const) and always dispatches through
   setfield_computed, whose Simple case is int_array_set
   (cmm_helpers.ml, Assignment always), so the init-flag drop is
   unconditional for immediate fields. *)
Definition block_set_image (bak : block_access_kind)
    (ia : init_or_assign) (i : Z) (a v : cmm_expr) : cmm_expr :=
  let addr := field_address_image a (bak_field_offset bak i) in
  match bak with
  | BAK_naked_floats _ =>
      Cop (Cstore Double Assignment) [addr; v]
  | BAK_mixed _ _ (MBAFK_flat_suffix fse) _ =>
      Cop (Cstore (fse_chunk fse) Assignment) [addr; v]
  | BAK_values _ _ BAFK_immediate
  | BAK_mixed _ _ (MBAFK_value_prefix BAFK_immediate) _ =>
      Cop (Cstore Word_int Assignment) [addr; v]
  | BAK_values _ _ BAFK_any_value
  | BAK_mixed _ _ (MBAFK_value_prefix BAFK_any_value) _ =>
      match ia with
      | IOA_initialization =>
          extcall_void_image "caml_initialize"%string [addr; v]
      | IOA_assignment Assign_heap =>
          extcall_void_image "caml_modify"%string [addr; v]
      | IOA_assignment Assign_local =>
          extcall_void_image "caml_modify_local"%string
            [a; Cconst_int (bak_field_offset bak i); v]
      end
  end.

(* Scalar_type widening to the machine word (TC.Prim.NumConv's
   "widen source to nativeint"): every naked int is already a
   (sign-extended) word at the Cmm level (R.Val.NakedNumber, 64-bit
   target), so only a tagged source needs an untag.  The 63-bit
   range of immediates is the value domain's business (05), not the
   emission's. *)
Definition widen_to_word (src : standard_int_or_float) (e : cmm_expr)
  : option cmm_expr :=
  match src with
  | SIF_tagged_immediate => Some (untag_int_image e)
  | SIF_naked_immediate | SIF_naked_int8 | SIF_naked_int16
  | SIF_naked_int32 | SIF_naked_int64 | SIF_naked_nativeint => Some e
  | SIF_naked_float | SIF_naked_float32 => None
  end.

(* Scalar_type narrowing from the machine word: tag for a tagged
   destination, sign_extend for the sub-word destinations (the doc's
   "then sign_extend for int32 dst"), identity at word width. *)
Definition narrow_from_word (dst : standard_int_or_float)
    (e : cmm_expr) : option cmm_expr :=
  match dst with
  | SIF_tagged_immediate => Some (tag_int_image e)
  | SIF_naked_immediate | SIF_naked_int64 | SIF_naked_nativeint =>
      Some e
  | SIF_naked_int8 => Some (sign_extend_image 8 e)
  | SIF_naked_int16 => Some (sign_extend_image 16 e)
  | SIF_naked_int32 => Some (sign_extend_image 32 e)
  | SIF_naked_float | SIF_naked_float32 => None
  end.

(* Num_conv's four emission classes, dispatched on (src, dst).  For
   int -> float32 the float case below emits ONE
   Cstatic_cast (SC_float_of_int Float32) on the widened source
   (single rounding): the double-rounding bug is NOT here (doc s6;
   Cmm.v's single-rounding note).  CF-9 (emission review): the code
   short-circuits src = dst to the bare argument
   (arithmetic_conversion); the int identity classes here still
   emit the tag-of-untag / sign_extend shapes -- value-equal
   canonical forms. *)
Definition num_conv_image (src dst : standard_int_or_float)
    (e : cmm_expr) : option cmm_expr :=
  match src, dst with
  | SIF_naked_float, SIF_naked_float => Some e
  | SIF_naked_float32, SIF_naked_float32 => Some e
  | SIF_naked_float, SIF_naked_float32 =>
      Some (Cop (Cstatic_cast SC_float32_of_float) [e])
  | SIF_naked_float32, SIF_naked_float =>
      Some (Cop (Cstatic_cast SC_float_of_float32) [e])
  | SIF_naked_float, _ =>
      narrow_from_word dst
        (Cop (Cstatic_cast (SC_int_of_float Float64)) [e])
  | SIF_naked_float32, _ =>
      narrow_from_word dst
        (Cop (Cstatic_cast (SC_int_of_float Float32)) [e])
  | _, SIF_naked_float =>
      option_map
        (fun ew => Cop (Cstatic_cast (SC_float_of_int Float64)) [ew])
        (widen_to_word src e)
  | _, SIF_naked_float32 =>
      option_map
        (fun ew => Cop (Cstatic_cast (SC_float_of_int Float32)) [ew])
        (widen_to_word src e)
  | _, _ =>
      match widen_to_word src e with
      | Some ew => narrow_from_word dst ew
      | None => None
      end
  end.

(* chunk(w) for string-like access (TC.Prim.StringLoad's list).
   ENCODING NOTE: the 32-bit case is the signed chunk -- the code
   composes unaligned_load_32 with a sign extension, and a
   naked_int32 word is sign-extended (R.Val.NakedNumber); the folded
   single chunk reads the same word -- a value-level equality.
   Reviewer-confirmed (Knuth): the site note suffices.  The 16-bit
   signed case composes the identical unaligned_load_16 +
   sign-extend fold (CF-8, emission review); the 8-bit chunks load
   directly, no fold. *)
Definition saw_chunk (w : string_accessor_width) : memory_chunk :=
  match w with
  | SAW_eight => Byte_unsigned
  | SAW_eight_signed => Byte_signed
  | SAW_sixteen => Sixteen_unsigned
  | SAW_sixteen_signed => Sixteen_signed
  | SAW_thirty_two => Thirtytwo_signed
  | SAW_single => Single Float32
  | SAW_sixty_four => Word_int
  | SAW_one_twenty_eight al =>
      if al then Onetwentyeight_aligned else Onetwentyeight_unaligned
  | SAW_two_fifty_six al =>
      if al then Twofiftysix_aligned else Twofiftysix_unaligned
  | SAW_five_twelve al =>
      if al then Fivetwelve_aligned else Fivetwelve_unaligned
  end.

(* The base pointer: the string/bytes block itself, or the bigstring
   data pointer loaded from field 1 (ptr_out_of_heap). *)
Definition string_like_base (slv : string_like_value)
    (str : cmm_expr) : cmm_expr :=
  match slv with
  | SLV_string | SLV_bytes => str
  | SLV_bigstring =>
      Cop (Cload Word_int CMut_mutable false)
          [field_address_image str 1]
  end.

Definition bytes_like_base (blv : bytes_like_value)
    (str : cmm_expr) : cmm_expr :=
  match blv with
  | BLV_bytes => str
  | BLV_bigstring =>
      Cop (Cload Word_int CMut_mutable false)
          [field_address_image str 1]
  end.

Definition alk_is_vector (alk : array_load_kind) : bool :=
  match alk with
  | ALK_naked_vec128s | ALK_naked_vec256s | ALK_naked_vec512s => true
  | _ => false
  end.

Definition ask_is_vector (ask : array_set_kind) : bool :=
  match ask with
  | ASK_naked_vec128s | ASK_naked_vec256s | ASK_naked_vec512s => true
  | _ => false
  end.

(* The vector LOAD/SET kind's chunk -- ALWAYS unaligned
   (TC.Prim.ArrayAccess.Vector: unboxed vector arrays are only
   word-aligned). *)
Definition alk_vec_chunk (alk : array_load_kind)
  : option memory_chunk :=
  match alk with
  | ALK_naked_vec128s => Some Onetwentyeight_unaligned
  | ALK_naked_vec256s => Some Twofiftysix_unaligned
  | ALK_naked_vec512s => Some Fivetwelve_unaligned
  | _ => None
  end.

Definition ask_vec_chunk (ask : array_set_kind)
  : option memory_chunk :=
  match ask with
  | ASK_naked_vec128s => Some Onetwentyeight_unaligned
  | ASK_naked_vec256s => Some Twofiftysix_unaligned
  | ASK_naked_vec512s => Some Fivetwelve_unaligned
  | _ => None
  end.

(* log2 of the SOURCE array's element width in bytes (element_width_
   log2's table = R.Obj.Array's width table, Representation.v). *)
Definition array_log2 (ak : array_kind) : option Z :=
  option_map Z.log2 (array_elem_width ak).

(* array_indexing log2 arr i (cmm_helpers): the element address from
   the TAGGED index i, the untag folded into the scale.
   Constructors: the two folds the rule text states (a constant
   index; the (c<<1)|1 re-tag pattern) and the generic
   untag-then-scale form.
   ENCODING NOTE: for the generic case the code emits a
   strength-reduced form, arr + (i << (k-1)) - 2^(k-1); for every
   arr and every tagged index i = 2n+1 it computes the SAME address
   value as the direct form here (both are arr + n * 2^k) -- that
   equality is the fact ch. 20 consumes.  The doc states only the
   two folds and the untag-into-scale contract, so the generic
   constructor uses the direct form the contract describes.  The
   fold constructors may overlap the generic one -- harmless
   nondeterminism (cf. the switch-table over-approximation,
   ToCmmControl.v).  Reviewer-confirmed (Knuth): keep the direct
   shape; the .v-vs-code shape divergence is the code-verification
   lane's. *)
Inductive array_indexing_image (k : Z) (arr : cmm_expr)
  : cmm_expr -> cmm_expr -> Prop :=
  | AII_const : forall n,
      array_indexing_image k arr (Cconst_int n)
        (Cop Cadda [arr; Cconst_int (Z.shiftl (Z.shiftr n 1) k)])
  | AII_retag : forall c,
      array_indexing_image k arr (tag_int_image c)
        (Cop Cadda [arr; Cop Clsl [c; Cconst_int k]])
  | AII_generic : forall i,
      array_indexing_image k arr i
        (Cop Cadda [arr; Cop Clsl [untag_int_image i; Cconst_int k]]).

(* Array_set's store at the element address (int_array_set /
   addr_array_store / float_array_set): value assignments through
   the write barrier as for blocks; None on the vector kinds (the
   .Vector rule) -- their scalar path is a fatal error in the
   code.  arr and i (the TAGGED index) ride along for the local
   case only: caml_modify_local takes the block and the untagged
   index rather than the element address (CF-1, as at
   block_set_image; the code's addr_array_set_local passes arr and
   untag_int ofs). *)
Definition array_set_image (ask : array_set_kind)
    (arr i addr v : cmm_expr) : option cmm_expr :=
  match ask with
  | ASK_immediates | ASK_gc_ignorable_values =>
      Some (Cop (Cstore Word_int Assignment) [addr; v])
  | ASK_values IOA_initialization =>
      Some (extcall_void_image "caml_initialize"%string [addr; v])
  | ASK_values (IOA_assignment Assign_heap) =>
      Some (extcall_void_image "caml_modify"%string [addr; v])
  | ASK_values (IOA_assignment Assign_local) =>
      Some (extcall_void_image "caml_modify_local"%string
              [arr; untag_int_image i; v])
  | ASK_naked_floats =>
      Some (Cop (Cstore Double Assignment) [addr; v])
  | ASK_naked_float32s =>
      Some (Cop (Cstore (Single Float32) Assignment) [addr; v])
  | ASK_naked_ints | ASK_naked_int64s | ASK_naked_nativeints =>
      Some (Cop (Cstore Word_int Assignment) [addr; v])
  | ASK_naked_int32s =>
      Some (Cop (Cstore Thirtytwo_signed Assignment) [addr; v])
  | ASK_naked_int16s =>
      Some (Cop (Cstore Sixteen_signed Assignment) [addr; v])
  | ASK_naked_int8s =>
      Some (Cop (Cstore Byte_signed Assignment) [addr; v])
  | ASK_naked_vec128s | ASK_naked_vec256s | ASK_naked_vec512s => None
  end.

(* chunk(bk) per bigarray_word_kind (TC.Prim.BigarrayAccess NOTES):
   Single{Float64} for the widening Float32 kind vs Single{Float32}
   for Float32_t; None for float16 (Sixteen_unsigned plus the
   float_of_float16 conversion) and the complex kinds (TWO loads
   boxed with box_complex, an allocation) -- see the header note on
   the rule. *)
Definition ba_chunk (bk : bigarray_kind) : option memory_chunk :=
  match bk with
  | BGK_float32 => Some (Single Float64)
  | BGK_float32_t => Some (Single Float32)
  | BGK_float64 => Some Double
  | BGK_sint8 => Some Byte_signed
  | BGK_uint8 => Some Byte_unsigned
  | BGK_sint16 => Some Sixteen_signed
  | BGK_uint16 => Some Sixteen_unsigned
  | BGK_int32 => Some Thirtytwo_signed
  | BGK_int64 => Some Word_int
  | BGK_int_width_int => Some Word_int
  | BGK_targetint_width_int => Some Word_int
  | BGK_float16 | BGK_complex32 | BGK_complex64 => None
  end.

(* The bigarray data pointer: a Mutable Word_int load of descriptor
   field 1. *)
Definition bigarray_data_image (ba : cmm_expr) : cmm_expr :=
  Cop (Cload Word_int CMut_mutable false) [field_address_image ba 1].

(* ================================================================== *)
(* 4. Primitive emissions (18 s3-s7)                                  *)
(* ================================================================== *)

(* The per-primitive emission judgment: the defining Cmm expression
   of TC.Let.Prim, arguments through ~>v (TC.Prim.Sound's framing).
   Scope note (FIDELITY W-17): the atomic primitives
   (BP_atomic_load_field, the TP_atomic and QP_atomic families)
   have no stated TC lowering anywhere in the doc (their images use
   Catomic forms
   outside ch. 15's fragment), so tc_prim mirrors the doc and states
   no clause for them; likewise the primitives ch. 18 gives no
   per-primitive rule for (scalar arithmetic, comparisons, is_int,
   get_tag, ...). *)
Inductive tc_prim (th : tcenv) : prim -> cmm_expr -> Prop :=

  (** RULE TC.Prim.TagUntag (CLAIM normative) -- 18-to-cmm-data.md
      CODE backend/cmm_helpers.ml#tag_int
      CODE backend/cmm_helpers.ml#untag_int
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#arithmetic_conversion

      Tag_immediate ~> tag_int e = Cop(Caddi,[Cop(Clsl,[e;1]);1]);
      Untag_immediate ~> untag_int e = Cop(Casr,[e;1]).  Total on
      the value range (undef-free); commutes with R.Val.Imm.  The
      Env.Untag marker of the NOTES is ch. 16's Switch business. *)
  | TC_Prim_TagUntag_Tag : forall s e,
      tc_simple th s e ->
      tc_prim th (P_unary UP_tag_immediate s) (tag_int_image e)
  | TC_Prim_TagUntag_Untag : forall s e,
      tc_simple th s e ->
      tc_prim th (P_unary UP_untag_immediate s) (untag_int_image e)

  (** RULE TC.Prim.BoxUnbox (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#box_number
      CODE backend/cmm_helpers.ml#box_int_gen
      CODE backend/cmm_helpers.ml#unbox_int
      CODE backend/cmm_helpers.ml#box_vector
      CODE backend/cmm_helpers.ml#unbox_vector

      Box_number kappa: the Calloc of kappa's boxed layout
      (box_number_image = R.Obj.Boxed's clauses); Unbox_number
      kappa: the payload load (unbox_number_image; offset 8 for
      custom blocks, 0 for float/vector, vector chunks UNALIGNED).
      ENDIANNESS-DEPENDENT for int32.  Allocation detail (heap vs
      local, GC) is ch. 19's. *)
  | TC_Prim_BoxUnbox_Box : forall bn am s e,
      tc_simple th s e ->
      tc_prim th (P_unary (UP_box_number bn am) s)
        (box_number_image bn am e)
  | TC_Prim_BoxUnbox_Unbox : forall bn s a,
      tc_simple th s a ->
      tc_prim th (P_unary (UP_unbox_number bn) s)
        (unbox_number_image bn a)

  (** RULE TC.Prim.ReinterpretBoxedVector (CLAIM normative)
      -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive

      Literally the identity: no load, no allocation, no cast (a
      boxed vector and an all-flat tag-0 mixed block of the same
      width are laid out identically). *)
  | TC_Prim_ReinterpretBoxedVector : forall s e,
      tc_simple th s e ->
      tc_prim th (P_unary UP_reinterpret_boxed_vector s) e

  (** RULE TC.Prim.MakeBlock (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
      CODE backend/cmm_helpers.ml#make_alloc_generic

      A Calloc whose first argument is the header Cconst_natint
      (R.Header via make_block_header) and whose remaining arguments
      are the field words.
      ENCODING NOTE: (i) the Alloc_block_kind classification is not
      stated by the rule and is left unconstrained (abk); (ii) the
      large-heap-block caml_alloc_shr_check_gc + fill_fields path is
      elided -- the doc NOTES say it yields the same layout; (iii)
      the per-field chunks of the Naked_floats/Mixed cases live in
      the machine's alloc_fields_bytes (CM.Alloc.Heap), not in the
      emitted argument list; (iv) the code asserts a nonempty field
      list (make_alloc_generic) while es = [] is admitted here
      (CF-10, emission review) -- the rule text states no arity
      premise, and empty blocks are not produced upstream (atoms
      are static).  Mixed flat-suffix packing is
      little-endian-only. *)
  | TC_Prim_MakeBlock : forall bk mut am ss es abk,
      Forall2 (tc_simple th) ss es ->
      tc_prim th (P_variadic (VP_make_block bk mut am) ss)
        (Cop (Calloc (cmm_mode am) abk)
             (Cconst_natint
                (make_block_header bk (Z.of_nat (List.length es))
                   (alloc_col am))
              :: es))

  (** RULE TC.Prim.BlockLoad (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_load
      CODE backend/cmm_helpers.ml#get_field_computed
      CODE backend/cmm_helpers.ml#field_address

      Cop(Cload{chunk; mutability}, [field_address a off]) with
      off = i for uniform blocks or offset_in_words for mixed, and
      the chunk per the access kind.  Mixed sub-word loads are
      little-endian-only. *)
  | TC_Prim_BlockLoad : forall bak mu i s a,
      tc_simple th s a ->
      tc_prim th (P_unary (UP_block_load bak mu i) s)
        (Cop (Cload (bak_field_chunk bak) (cmm_mut mu) false)
             [field_address_image a (bak_field_offset bak i)])

  (** RULE TC.Prim.BlockSet (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_set
      CODE backend/cmm_helpers.ml#setfield_computed

      block_set_image's dispatch (caml_modify for a heap value
      assignment -- the write barrier is why to_cmm does not emit a
      bare Cstore(Word_val)), returning Cmm unit. *)
  | TC_Prim_BlockSet : forall bak ia i s1 s2 a v,
      tc_simple th s1 a ->
      tc_simple th s2 v ->
      tc_prim th (P_binary (BP_block_set bak ia i) s1 s2)
        (return_unit_image (block_set_image bak ia i a v))

  (** RULE TC.Prim.ProjectFunctionSlot (CLAIM normative)
      -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple
      CODE backend/cmm_helpers.ml#infix_field_address

      A Caddv by the signed byte offset 8*(off(f2) - off(f1))
      (negative when f2 precedes f1; the bare pointer when f2 = f1).
      No load, no allocation. *)
  | TC_Prim_ProjectFunctionSlot : forall f1 f2 s a,
      tc_simple th s a ->
      tc_prim th (P_unary (UP_project_function_slot f1 f2) s)
        (infix_field_address_image a
           (fun_slot_offset f2 - fun_slot_offset f1))

  (** RULE TC.Prim.ProjectValueSlot (CLAIM normative)
      -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple
      CODE backend/cmm_helpers.ml#get_field_computed

      An Immutable load at offset off(w) - off(f) from the closure
      pointer (the environment is immutable once built).
      ENCODING NOTE: "chunk from the value slot's kind" -- value
      slots carry no kind in the syntax, so the chunk is
      existential, exactly as R.Obj.Closures' environment clause
      reads it back (Representation.v). *)
  | TC_Prim_ProjectValueSlot : forall f w s a ch,
      tc_simple th s a ->
      tc_prim th (P_unary (UP_project_value_slot f w) s)
        (Cop (Cload ch CMut_immutable false)
             [field_address_image a
                (val_slot_offset w - fun_slot_offset f)])

  (** RULE TC.Prim.NumConv (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#arithmetic_conversion
      CODE backend/cmm_helpers.ml#float32_of_int
      CODE backend/cmm.mli#static_cast

      num_conv_image's four classes: Int->Int width-adjust,
      Float<->Float single casts, Int->Float widen-then-ONE-cast
      (for dst = Float32 a SINGLE Cstatic_cast (Float_of_int
      Float32): single rounding, the int->float32 double-rounding
      is NOT introduced here), Float->Int cast-then-narrow. *)
  | TC_Prim_NumConv : forall src dst s e e',
      tc_simple th s e ->
      num_conv_image src dst e = Some e' ->
      tc_prim th (P_unary (UP_num_conv src dst) s) e'

  (** RULE TC.Prim.StringLoad (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#string_like_load
      CODE backend/cmm_helpers.ml#unaligned_load_16

      Load: Cop(Cload{chunk(w)}, [base + j]) with the UNTAGGED index
      j and base the string/bytes block or the bigstring data
      pointer (field 1).  Set (P.Ternary.BytesOrBigstringSet, the
      NOTES' store dual, chunk selection identical): the Cstore at
      the same address, returning unit.  Loads are Mutable: even
      String-tagged bases may alias mutable bytes (the deprecated
      unsafe accessors; cf. S.Rewrite's CSE caveat).  The
      byte-splitting !allow_unaligned_access path is elided (LE with
      unaligned access here, per the NOTES).  CF-7 (emission
      review): the code adds bigstring data pointers with Caddi
      (ptr_out_of_heap) and string/bytes bases with the Cadda form;
      the uniform Cadda here is value-equal -- the distinction is
      GC-typing metadata, divergent only out of heap. *)
  | TC_Prim_StringLoad_Load : forall slv w s1 s2 str j,
      tc_simple th s1 str ->
      tc_simple th s2 j ->
      tc_prim th (P_binary (BP_string_or_bigstring_load slv w) s1 s2)
        (Cop (Cload (saw_chunk w) CMut_mutable false)
             [Cop Cadda [string_like_base slv str; j]])
  | TC_Prim_StringLoad_Set : forall blv w s1 s2 s3 str j v,
      tc_simple th s1 str ->
      tc_simple th s2 j ->
      tc_simple th s3 v ->
      tc_prim th
        (P_ternary (TP_bytes_or_bigstring_set blv w) s1 s2 s3)
        (return_unit_image
           (Cop (Cstore (saw_chunk w) Assignment)
                [Cop Cadda [bytes_like_base blv str; j]; v]))

  (** RULE TC.Prim.ArrayAccess (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_set0
      CODE backend/cmm_helpers.ml#array_indexing

      Load: Cop(Cload{chunk(ak); Mutable}, [array_indexing log2(ak)
      arr i]) on the TAGGED index; Set: array_set_image at the same
      address, returning unit.  Vector load/set kinds take the
      .Vector rule below; unboxed-product sources have no chunk
      (array_elem_chunk = None) and no clause, matching the code's
      fatal error.  Out-of-bounds is undef (frontend-inserted
      checks).  Array_length is not lowered here (no per-primitive
      rule in ch. 18; the NOTES' header-shift remark is R.Obj.Array
      context).
      CF-2/KF-045 (emission review, Church; doc ruling APPLIED to
      18-to-cmm-data.md): the load is constant CMut_mutable --
      array_load takes no mutability parameter (the primitive's mu
      is dropped at dispatch), so even immutable-array loads emit
      Mutable and the backend CSE opportunity is not taken; a CR in
      array_load asks for block_load-style mutability threading --
      flip this row back to cmm_mut mu if that refactor lands. *)
  | TC_Prim_ArrayAccess_Load : forall ak alk mu s1 s2 arr i ch k addr,
      tc_simple th s1 arr ->
      tc_simple th s2 i ->
      alk_is_vector alk = false ->
      array_elem_chunk ak = Some ch ->
      array_log2 ak = Some k ->
      array_indexing_image k arr i addr ->
      tc_prim th (P_binary (BP_array_load ak alk mu) s1 s2)
        (Cop (Cload ch CMut_mutable false) [addr])
  | TC_Prim_ArrayAccess_Set :
      forall ak ask s1 s2 s3 arr i v k addr st,
      tc_simple th s1 arr ->
      tc_simple th s2 i ->
      tc_simple th s3 v ->
      ask_is_vector ask = false ->
      array_log2 ak = Some k ->
      array_indexing_image k arr i addr ->
      array_set_image ask arr i addr v = Some st ->
      tc_prim th (P_ternary (TP_array_set ak ask) s1 s2 s3)
        (return_unit_image st)

  (** RULE TC.Prim.ArrayAccess.Vector (CLAIM normative)
      -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load_vector
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_set_vector
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load

      The byte offset is (untag_int i) << log2(w(ak)) -- scaled by
      the SOURCE array's element width, while the chunk follows the
      LOAD/SET kind (cross-kind reinterpret access); ALWAYS the
      _unaligned chunk.
      ENCODING NOTE: unboxed-product sources are excluded via
      array_elem_width = None, so the model is strictly NARROWER
      than the CODE: a real vec128-load-from-2/4-vec128-product
      path exists in array_load and has no constructor here.  The
      exception is NOTES-level in the doc (18-to-cmm-data.md
      lines 453-454, "as are Unboxed_product sources (except
      vec128 loads from 2/4-vec128 products)"); the rule's
      above-the-line form covers only the Naked_vecNs kinds.
      Reviewer-confirmed (Knuth) as an honest narrowing on that
      basis.
      CF-11 (emission review, Church): the vector load is constant
      CMut_mutable -- array_load_vector takes no mutability
      parameter and dispatches to unaligned_load_{128,256,512},
      i.e. load_chunk, whose mk_load_mut is always Mutable; the
      primitive's mu is dropped.  The scalar Load row above is
      likewise constant CMut_mutable (KF-045 ruled, doc edit
      landed; see its CF-2/KF-045 note) -- the two rows now tell
      the same story. *)
  | TC_Prim_ArrayAccess_Vector_Load :
      forall ak alk mu s1 s2 arr i w ch,
      tc_simple th s1 arr ->
      tc_simple th s2 i ->
      alk_vec_chunk alk = Some ch ->
      array_elem_width ak = Some w ->
      tc_prim th (P_binary (BP_array_load ak alk mu) s1 s2)
        (Cop (Cload ch CMut_mutable false)
             [Cop Cadda
                [arr; Cop Clsl [untag_int_image i;
                                Cconst_int (Z.log2 w)]]])
  | TC_Prim_ArrayAccess_Vector_Set :
      forall ak ask s1 s2 s3 arr i v w ch,
      tc_simple th s1 arr ->
      tc_simple th s2 i ->
      tc_simple th s3 v ->
      ask_vec_chunk ask = Some ch ->
      array_elem_width ak = Some w ->
      tc_prim th (P_ternary (TP_array_set ak ask) s1 s2 s3)
        (return_unit_image
           (Cop (Cstore ch Assignment)
                [Cop Cadda
                   [arr; Cop Clsl [untag_int_image i;
                                   Cconst_int (Z.log2 w)]];
                 v]))

  (** RULE TC.Prim.BigarrayAccess (CLAIM normative)
      -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#bigarray_load_or_store
      CODE backend/cmm_helpers.ml#bigarray_load
      CODE backend/cmm_helpers.ml#bigarray_store

      Load: a Mutable chunked load at array_indexing
      log2(elt_size(bk)) over the data pointer (descriptor field 1),
      TAGGED offset; Set: the store dual, returning unit;
      Bigarray_get_alignment n (the NOTES): (n-1) land (data + i).
      num_dimensions and layout are ignored (already resolved by
      P.Bigarray.Indexing).
      ENCODING NOTE: ba_chunk = None excludes cases the CODE
      handles (the model is strictly narrower, not merely silent
      where the doc is): the float16 variant (Sixteen_unsigned +
      float_of_float16 conversion) and the complex kinds.  The
      complex emission is MATERIALLY different, not a shape
      variant: TWO loads at addr and addr + elt_size/2, then
      box_complex -- an ALLOCATION (the generative effect in 06).
      Both are NOTES-level in the doc (18-to-cmm-data.md lines
      477-480); the rule's above-the-line form is the single
      chunked load.  Reviewer-confirmed (Knuth) as an honest
      narrowing on that basis. *)
  | TC_Prim_BigarrayAccess_Load :
      forall dims bk bl s1 s2 ba i ch addr,
      tc_simple th s1 ba ->
      tc_simple th s2 i ->
      ba_chunk bk = Some ch ->
      array_indexing_image (Z.log2 (ba_elt_size bk))
        (bigarray_data_image ba) i addr ->
      tc_prim th (P_binary (BP_bigarray_load dims bk bl) s1 s2)
        (Cop (Cload ch CMut_mutable false) [addr])
  | TC_Prim_BigarrayAccess_Set :
      forall dims bk bl s1 s2 s3 ba i v ch addr,
      tc_simple th s1 ba ->
      tc_simple th s2 i ->
      tc_simple th s3 v ->
      ba_chunk bk = Some ch ->
      array_indexing_image (Z.log2 (ba_elt_size bk))
        (bigarray_data_image ba) i addr ->
      tc_prim th (P_ternary (TP_bigarray_set dims bk bl) s1 s2 s3)
        (return_unit_image (Cop (Cstore ch Assignment) [addr; v]))
  | TC_Prim_BigarrayAccess_GetAlignment : forall n s1 s2 ba j,
      tc_simple th s1 ba ->
      tc_simple th s2 j ->
      tc_prim th (P_binary (BP_bigarray_get_alignment n) s1 s2)
        (Cop Cand
             [Cconst_int (n - 1);
              Cop Caddi [bigarray_data_image ba; j]])

  (** RULE TC.Prim.BigarrayLength (CLAIM normative)
      -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
      CODE backend/cmm_helpers.ml#field_address

      A Mutable Word_int load of descriptor field 4 + d (matching
      the reading_from_a_block(Mutable) classification: not
      CSE-able across effectful code). *)
  | TC_Prim_BigarrayLength : forall d s ba,
      tc_simple th s ba ->
      tc_prim th (P_unary (UP_bigarray_length d) s)
        (Cop (Cload Word_int CMut_mutable false)
             [field_address_image ba (4 + Z.of_nat d)]).

(* ================================================================== *)
(* 5. The per-primitive soundness schema (doc s1)                     *)
(* ================================================================== *)

(* Scope note: the doc states one schema over [[p]](vbar; H)
   (denot_prim, Machine.v).  The region-threading denotation form
   (denot_prim_r, ch. 06 "Region delimiters") is not restated: the
   doc states a single schema, and ch. 20 threads R alongside H when
   it consumes the per-primitive obligations.
   ENCODING NOTE (statement granularity; cataloged with main):
   - the undef clause ("e_p is undef or unconstrained") imposes no
     obligation, so only the PR_ok implication is stated;
   - the run's trace is existential: the doc's schema does not
     constrain events (the extcall-emitting images caml_initialize /
     caml_modify are runtime services, not observable C calls);
   - "L' extending L" is stated pointwise on the address map. *)

(** RULE TC.Prim.Sound (CLAIM normative) -- 18-to-cmm-data.md
    CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple
    CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_complex
    CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects

    Premises in doc order: the emission e_p (tc_prim, "let
    TC.Prim.<p> give its Cmm emission e_p"); the argument simples
    evaluate abstractly (simple_eval_list on prim_args) and the
    environments are related (env_rep + symaddr_agree, which give
    "the argument simples translate to Cmm values that relate to
    the abstract argument values" through TC_Simple_sound); H
    related to M (rep_heap at the program's symbol table); the
    denotation yields (v, H').  Conclusion: evaluating e_p yields a
    Cmm value w and memory M' with v related to w and H' related to
    M' at some L' extending L. *)
Theorem TC_Prim_Sound :
  forall (P : cmm_program) (th : tcenv) (L : loc_map) (rho : env)
         (ce : cmm_venv) (chi : cmm_kenv) (M : cmm_mem)
         (TT : list static_label) (RR : list region_handle)
         (p : prim) (e_p : cmm_expr) (vbar : list value)
         (v : value) (H H' : heap),
    tc_prim th p e_p ->
    simple_eval_list rho (prim_args p) = Some vbar ->
    env_rep P th L ce chi M TT RR rho ->
    symaddr_agree P L ->
    rep_heap (cp_symaddr P) H L M ->
    denot_prim (prim_op_of p) vbar H (PR_ok v H') ->
    exists w tr ce' chi' M' TT' RR' L',
      cmem_run P (CmCfg e_p ce chi M TT RR) tr
                 (CmCfg (Cval w) ce' chi' M' TT' RR') /\
      rep_val L' v w /\
      rep_heap (cp_symaddr P) H' L' M' /\
      (forall a z, L a = Some z -> L' a = Some z).
Admitted.

(* ================================================================== *)
(* 6. Let-bindings: the classify dispatch (doc s2)                    *)
(* ================================================================== *)

(* classify_let_binding's dispatch table (TC.Let.Prim's premise):
   how a Let-bound primitive's translation enters the environment,
   indexed by the selected let_binding_class.  The classifier's
   inputs (num_occurrences, effects_and_coeffects) are elided, as
   classify_handler's are in ch. 16, so every class is admissible
   in the relation.
   ENCODING NOTE (binding placement; cataloged with main): the doc
   binds Drop/Regular bindings in D ("Do_not_inline") and
   materializes their Clets at flush time; ch. 16's tc_flush hook
   is a side condition and cannot emit, so the Clet is placed at
   the binding point here, through the emitted context.  The gap
   between the two placements -- bind-point Clet vs flush-point
   Clet -- is exactly the reordering TC.Let.Subst governs.  The
   inline classes substitute the defining expression through V
   (doc-literal).  The stage stacks of data_state stay empty in
   this closure; they are the vocabulary of TC.Let.Subst's premise
   text.
   UNDER-APPROXIMATION (coordinator ruling, with obligation): the
   concrete closure emits bind-point placements ONLY, so real
   to_cmm outputs -- which sink bindings to flush points -- are
   related through the TC.Let.Subst-closure of this relation, not
   the bare relation.  Ch. 20's simulation statement must be posed
   against that closure (restated in ToCmmSoundness.v's header) or
   real outputs would come out unrelated.  The closure's DEFINITION
   in ToCmmSoundness.v is also where the doc's validity side
   conditions (hoisting above a branch, Control_flow_point),
   drop-of-unused-pure, and flush placement itself must be carried
   or their absence disclosed: the closure performs exactly the
   flush/hoist-direction reorderings where the doc's garbage-value
   hazard lives, and no Rocq artifact currently represents that
   half (reviewer-booked: Knuth W-26, fires at ToCmmSoundness.v
   intake). *)
Inductive let_prim_ext (th : tcenv) (x : variable) (e_p : cmm_expr)
  : let_binding_class -> tcenv -> (cmm_expr -> cmm_expr) -> Prop :=
  | LPE_drop_defining_expr :
      let_prim_ext th x e_p LB_drop_defining_expr th (fun b => b)
  | LPE_regular : forall bv,
      let_prim_ext th x e_p LB_regular
        (upd_V th x (Cvar bv)) (fun b => Clet bv e_p b)
  | LPE_may_inline_once :
      let_prim_ext th x e_p LB_may_inline_once
        (upd_V th x e_p) (fun b => b)
  | LPE_must_inline_once :
      let_prim_ext th x e_p LB_must_inline_once
        (upd_V th x e_p) (fun b => b)
  | LPE_must_inline_and_duplicate :
      let_prim_ext th x e_p LB_must_inline_and_duplicate
        (upd_V th x e_p) (fun b => b).

(* ENCODING NOTE (kernel statement; cataloged with main): the doc's
   conclusion is prose ("the delayed-binding discipline is a
   behaviour-preserving reordering").  The statement below is the
   kernel the LPE inline rows need: a silent, memory-preserving
   evaluation of a delayed defining expression is reproducible at
   any later program point with the same variable environment and
   memory.  The PROVIDED quadruple maps as follows: "a coeffect-only
   binding may not cross a write" and "an effectful binding is a
   barrier" become the M-unchanged hypotheses (an intervening
   effectful stage changes M); the validity clauses (hoisting above
   a branch, Control_flow_point) concern the flush/hoist direction,
   which the bind-point-Clet closure above never performs, so they
   are not represented; the recursive-continuation restriction is
   ch. 16's (FM_entering_loop at loop headers).  Singleton-binding
   granularity: the stage stacks are quantified away, not
   threaded.  The conclusion pins M, TT' and RR'
   (reviewer-required, Knuth): restoration is not automatic --
   trap pushes and region opens are silent transitions -- so the
   hypothesis' pinning is balance content the kernel must transfer,
   or a spliced re-evaluation could leave a pushed handler or open
   region and break the surrounding ch. 20 simulation.
   The chi-agreement premise is load-bearing, not caution
   (reviewer finding KF-038): chi' is otherwise free, and CM.Exit
   resolves a bare Cexit through the config's kenv, so an e_dfn
   mentioning a label is refutable under a chi' rebinding it.
   cmm_mentions_label (Cmm.v) over-approximates free occurrence,
   which only strengthens the premise; every LPE defining
   expression contains no Cexit or Ccatch at all, so the intended
   instantiations discharge it vacuously.
   The stack guard is likewise load-bearing (reviewer findings
   KF-043, KF-044): TT' and RR' are quantified freely, and an e_dfn
   that READS the ambient stack shape has silent balanced runs
   under one stack and none under another.  Witnesses (Knuth): a
   Pop-then-re-Push exit through a self-contained Ccatch runs under
   TT = l :: TT0 but is stuck (trap_apply = None) under TT' = [];
   a Cendregion/Cbeginregion pair over an empty region runs under
   RR = [iota] (End pops, Begin may re-pick iota) but End is stuck
   under RR' = [] -- note the RR witness mentions no labels, so the
   chi premise alone cannot exclude it.  Further (KF-044): a direct
   Craise reads the ambient TT top and jumps through chi at a label
   e_dfn never mentions (CM.Catch.Exn installs handlers without
   pushing TT, so no raise is ever internally caught), and Capply
   tunnels both reads through a callee body in cp_funs, invisible
   to any syntactic scan of e_dfn (cm_returns hands the callee the
   caller's RR; CM.Apply.Raise re-plugs a raise at the call site).
   cmm_touches_stacks (Cmm.v) therefore flags Pop trap actions,
   Cendregion, Craise and Capply syntactically; pushes stay allowed
   (a Push cannot restore a pinned TT), Cbeginregion alone cannot
   restore the pinned RR, and Cextcall needs no flag (CM.Extcall
   emits an event unconditionally, so silence excludes it).  Every
   LPE defining expression discharges the guard vacuously. *)

(** RULE TC.Let.Subst (CLAIM normative) -- 18-to-cmm-data.md
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#bind_variable
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#add_binding_to_env
    CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects

    If the defining expression contains no trap-stack Pop, raise,
    call, or region end (the syntactic stack guard), and it
    evaluated silently at the bind point, restoring M, TT and RR (a
    coeffect-only, placement-respecting evaluation), then it
    evaluates to the same
    value at the use site, whatever the control state there
    (agreeing on any static labels the expression mentions) --
    restoring that site's TT and RR likewise. *)
Theorem TC_Let_Subst :
  forall (P : cmm_program) (e_dfn : cmm_expr) (ce : cmm_venv)
         (chi chi' : cmm_kenv) (M : cmm_mem)
         (TT TT' : list static_label) (RR RR' : list region_handle)
         (w : cmm_value) (ce2 : cmm_venv) (chi2 : cmm_kenv),
    cmm_touches_stacks e_dfn = false ->
    cmem_run P (CmCfg e_dfn ce chi M TT RR) []
               (CmCfg (Cval w) ce2 chi2 M TT RR) ->
    (forall l,
        cmm_mentions_label l e_dfn = true -> chi' l = chi l) ->
    exists ce3 chi3,
      cmem_run P (CmCfg e_dfn ce chi' M TT' RR') []
                 (CmCfg (Cval w) ce3 chi3 M TT' RR').
Admitted.

(* ================================================================== *)
(* 7. Ch. 16 hook closures: the small judgments                       *)
(* ================================================================== *)

(* simples_translate: the argument-list form of ~>v.  The skip row
   is remove_skipped_args (to_cmm_expr.ml): arguments of
   proven-unused parameters are dropped, so the list form is not
   pointwise (the ch. 16 hook-inventory note).
   ENCODING NOTE (cataloged with main): WHICH positions are skipped
   is an analysis input elided here, like classify_handler's
   occurrence hints, so the relation admits any subset of
   positions; ch. 16 pairs the result with the callee's parameter
   list, which fixes the arity at the CALL sites.  The two
   raise-operand sites (TC.ApplyCont.Raise, TC.Apply.ExnWrapper's
   extras), where the result feeds a Craise tail with no parameter
   pairing, instead pin the length directly (reviewer finding
   KF-042). *)
Inductive tc_simples (th : tcenv)
  : list simple -> list cmm_expr -> Prop :=
  | TCS_nil : tc_simples th [] []
  | TCS_cons : forall s ss e es,
      tc_simple th s e ->
      tc_simples th ss es ->
      tc_simples th (s :: ss) (e :: es)
  | TCS_skip : forall s ss es,
      tc_simples th ss es ->
      tc_simples th (s :: ss) es.

(* bind_var_to_simple (to_cmm_env): an alias binding in V --
   TC.Let.Simple's semantics, reused by ch. 16 for Inline
   continuation-argument binding. *)
Definition tc_bind_var_to_simple
    (th : tcenv) (x : variable) (s : simple) (th' : tcenv) : Prop :=
  exists e, tc_simple th s e /\ th' = upd_V th x e.

(* bind_variable for call results (ch. 16's bind_var_to_cmm hook).
   ENCODING NOTE (cataloged with main): the hook signature has no
   emission slot, so the binding is substitution-style (the call
   expression enters V); its effect-ordering soundness is
   TC.Let.Subst's, as for let_prim_ext's inline classes. *)
Definition tc_bind_var_to_cmm
    (th : tcenv) (x : variable) (e : cmm_expr) (th' : tcenv)
  : Prop :=
  th' = upd_V th x e.

(* var_binds: handler parameters bound to fresh Cmm variables at
   handler entry (a V extension, positional).  Freshness of the
   Cmm variables is ch. 16's obligation at the use sites. *)
Inductive tc_var_binds
  : tcenv -> list (variable * kind_ws) -> list backend_var
    -> tcenv -> Prop :=
  | TVB_nil : forall th, tc_var_binds th [] [] th
  | TVB_cons : forall th p ps bv bvs th',
      tc_var_binds (upd_V th (fst p) (Cvar bv)) ps bvs th' ->
      tc_var_binds th (p :: ps) (bv :: bvs) th'.

(* tc_flush: the flush side condition.  Under let_prim_ext's
   bind-point placement (see its note) nothing is ever delayed
   onto the stage stacks, so a flushed environment is one with
   empty stacks -- trivially maintained, recorded as the concrete
   meaning of the ch. 16 side conditions. *)
Definition tc_flush_data (m : flush_mode) (th : tcenv) : Prop :=
  ds_effect_stages (tc_data th) = [] /\
  ds_validity_stages (tc_data th) = [].

(* machtype_of_kind: kappa-hat's Cmm register class (ch. 15's
   machtype_component); subkind and nullability are erased -- a
   machtype is a register-width classification.  K_region and
   K_rec_info values have no runtime representation (void). *)
Definition machtype_of_kind_data (kw : kind_ws) : machtype :=
  match ws_kind kw with
  | K_value => [MC_val]
  | K_naked_number NN_naked_float => [MC_float]
  | K_naked_number NN_naked_float32 => [MC_float32]
  | K_naked_number NN_naked_vec128 => [MC_vec128]
  | K_naked_number NN_naked_vec256 => [MC_vec256]
  | K_naked_number NN_naked_vec512 => [MC_vec512]
  | K_naked_number _ => [MC_int]
  | K_region | K_rec_info => []
  end.

(* ================================================================== *)
(* 8. The TC.Let family and the ch. 16 hook closure                   *)
(* ================================================================== *)

(* The closure vars of a dynamic set of closures, bound positionally
   to their clos-value projections off the block base (bv0):
   base + 8 * off(f) through Caddv (infix_field_address_image is
   the identity at offset 0, the first slot). *)
Fixpoint bind_closure_vars (th : tcenv) (bv0 : backend_var)
    (pairs : list (bound_var * (function_slot * code_id_in_fd)))
  : tcenv :=
  match pairs with
  | [] => th
  | (bv, (f, _)) :: rest =>
      bind_closure_vars
        (upd_V th (bv_var bv)
           (infix_field_address_image (Cvar bv0)
              (fun_slot_offset f)))
        bv0 rest
  end.

(* strided_field_address sym byte_off: Cadda by a BYTE offset,
   folded to the bare pointer at 0.  The offset already includes the
   Update_kind stride; field_address_image above is the stride-8
   special case. *)
Definition strided_field_address_image (a : cmm_expr) (byte_off : Z)
  : cmm_expr :=
  if byte_off =? 0 then a else Cop Cadda [a; Cconst_int byte_off].

(* ------------------------------------------------------------------
   The Or_variable / Dynamically_computed holes of a static-constant
   group, in emission order, as (symbol, byte offset, variable): the
   store target is addr(sym) + byte offset, the stored value is the
   variable's translation.  This walk pins TC.Let.Static's update
   list to the holes of scg (reviewer finding KF-035: with the list
   free, the rule over-approximated the translation relation --
   us = [] with holes present, and stores into other constants'
   fields, were behavior-changing slack). *)

(* Boxed-number hole: the code's index-0 target (static_boxed_number
   passes ~index:0; make_update stores at the bare symbol).  For
   Boxed_float and the three vector kinds word 0 IS the payload; for
   the four CUSTOM kinds (Boxed_int32/int64/nativeint/float32) word 0
   is the OPS word -- the payload sits at +8 per R.Obj
   (Representation.v; 17-representation.md s4) -- so the deferred
   update clobbers the ops pointer and leaves the payload 0.
   Fidelity finding KF-040, compiler-bug candidate, kept CODE-FAITHFUL
   by coordinator ruling: latent in production (no producer creates
   Var payloads for the custom four), reachable from fexpr; ch. 20
   carves these holes out of the simulation statement.  If the
   compiler is fixed upstream, this definition flips to key on the
   constructor (8 for the custom four, 0 otherwise) as a strictly
   local change. *)
Definition ov_hole {A} (ov : or_variable A) : list (Z * variable) :=
  match ov with
  | OV_var x => [(0, x)]
  | OV_const _ => []
  end.

(* Array / float-block element holes at the given BYTE stride
   (Update_kind strides: 8 for word fields, the element width for
   packed int8/16/32 and float32 arrays, 16/32/64 for vectors). *)
Fixpoint ov_holes {A} (stride i : Z)
    (fields : list (or_variable A)) : list (Z * variable) :=
  match fields with
  | [] => []
  | OV_var x :: rest =>
      (stride * i, x) :: ov_holes stride (i + 1) rest
  | OV_const _ :: rest => ov_holes stride (i + 1) rest
  end.

(* Block-field (simple) holes: one per variable field, coercions
   looked through as in update_field, at word offsets; each field
   advances by its size in words (mixed-block vector suffix fields
   span 2/4/8 words -- Update_kind.field_size_in_words). *)
Fixpoint simple_holes (i : Z) (fs : list (simple * Z))
  : list (Z * variable) :=
  match fs with
  | [] => []
  | (Simple_name (Name_var x) _, sz) :: rest =>
      (8 * i, x) :: simple_holes (i + sz) rest
  | (_, sz) :: rest => simple_holes (i + sz) rest
  end.

Definition flat_suffix_word_size (e : flat_suffix_element) : Z :=
  match e with
  | FS_naked_vec128 => 2
  | FS_naked_vec256 => 4
  | FS_naked_vec512 => 8
  | _ => 1
  end.

Definition block_field_sizes (shape : scannable_block_shape)
    (n : nat) : list Z :=
  match shape with
  | Value_only => repeat 1 n
  | Mixed_record sigma =>
      repeat 1 (value_prefix_size sigma)
      ++ map flat_suffix_word_size (flat_suffix sigma)
  end.

Definition sc_holes (sc : static_const) : list (Z * variable) :=
  match sc with
  | SC_set_of_closures _ => [] (* value-slot holes: soc_holes below *)
  | SC_block _ _ shape fields =>
      simple_holes 0
        (combine fields (block_field_sizes shape (List.length fields)))
  | SC_boxed_float32 v => ov_hole v
  | SC_boxed_float v => ov_hole v
  | SC_boxed_int32 v => ov_hole v
  | SC_boxed_int64 v => ov_hole v
  | SC_boxed_nativeint v => ov_hole v
  | SC_boxed_vec128 v => ov_hole v
  | SC_boxed_vec256 v => ov_hole v
  | SC_boxed_vec512 v => ov_hole v
  | SC_immutable_float_block fields => ov_holes 8 0 fields
  | SC_immutable_float_array fields => ov_holes 8 0 fields
  | SC_immutable_float32_array fields => ov_holes 4 0 fields
  | SC_immutable_int_array fields => ov_holes 8 0 fields
  | SC_immutable_int8_array fields => ov_holes 1 0 fields
  | SC_immutable_int16_array fields => ov_holes 2 0 fields
  | SC_immutable_int32_array fields => ov_holes 4 0 fields
  | SC_immutable_int64_array fields => ov_holes 8 0 fields
  | SC_immutable_nativeint_array fields => ov_holes 8 0 fields
  | SC_immutable_vec128_array fields => ov_holes 16 0 fields
  | SC_immutable_vec256_array fields => ov_holes 32 0 fields
  | SC_immutable_vec512_array fields => ov_holes 64 0 fields
  | SC_immutable_value_array fields =>
      simple_holes 0 (combine fields (repeat 1 (List.length fields)))
  | SC_empty_array _ | SC_mutable_string _ | SC_immutable_string _ =>
      []
  end.

(* Value-slot holes of a STATIC set of closures: the code stores
   relative to one designated closure symbol of the set (the first
   function slot in layout order).  The designated slot is a
   Slot_offsets output, so f is existential here; every choice
   addresses the same word, base + 8 * off(vs), since
   addr(sym_f) = base + 8 * off(f) (ch. 17 layout). *)
Inductive soc_holes (closure_syms : list (function_slot * symbol))
  : list (value_slot * simple) -> list (symbol * Z * variable)
    -> Prop :=
| Soc_holes_nil : soc_holes closure_syms [] []
| Soc_holes_var : forall vs x co rest hs f sym,
    In (f, sym) closure_syms ->
    soc_holes closure_syms rest hs ->
    soc_holes closure_syms
      ((vs, Simple_name (Name_var x) co) :: rest)
      ((sym, 8 * (val_slot_offset vs - fun_slot_offset f), x) :: hs)
| Soc_holes_other : forall vs s rest hs,
    (forall x co, s <> Simple_name (Name_var x) co) ->
    soc_holes closure_syms rest hs ->
    soc_holes closure_syms ((vs, s) :: rest) hs.

(* Holes of one binder/constant pair.  Relies on WF.Syntax's
   binder/constant pairing (code binders pair with code entries;
   set-of-closures binders with SC_set_of_closures). *)
Inductive static_pair_holes
  : bound_static_pattern -> static_const_or_code
    -> list (symbol * Z * variable) -> Prop :=
| SPH_block_like : forall sym sc,
    static_pair_holes (BSP_block_like sym) (SCC_static_const sc)
      (map (fun h => (sym, fst h, snd h)) (sc_holes sc))
| SPH_set_of_closures : forall closure_syms soc hs,
    soc_holes closure_syms (soc_value_slots soc) hs ->
    static_pair_holes (BSP_set_of_closures closure_syms)
      (SCC_static_const (SC_set_of_closures soc)) hs
| SPH_code : forall cid c,
    static_pair_holes (BSP_code cid) (SCC_code c) []
| SPH_deleted_code : forall cid,
    static_pair_holes (BSP_code cid) SCC_deleted_code [].

(* The whole group's holes, binder order then field order (the
   code's fold over the group; prev_updates threads in this
   order). *)
Inductive static_group_holes
  : list bound_static_pattern -> list static_const_or_code
    -> list (symbol * Z * variable) -> Prop :=
| SGH_nil : static_group_holes [] [] []
| SGH_cons : forall bsp scc hs bst scg hs',
    static_pair_holes bsp scc hs ->
    static_group_holes bst scg hs' ->
    static_group_holes (bsp :: bst) (scc :: scg) (hs ++ hs').

(* One deferred symbol-field update (update_opt / make_update) for
   the hole (sym, off, x): the variable's translation stored at
   addr(sym) + off.  Two shapes (reviewer finding KF-036): kind
   Pointer -- UK.pointers, i.e. value fields and value slots not
   known immediate -- goes through setfield / caml_initialize ("the
   GC must see static field updates"); every other Update_kind is a
   plain initializing Cstore.  ENCODING NOTE: the arm is not keyed
   per hole, and the Cstore chunk is existential, as at
   TC.Prim.ProjectValueSlot (the hole's kind is not recorded here;
   R.Obj.* fixes the layout per constant). *)
Definition static_update (th : tcenv) (h : symbol * Z * variable)
    (u : cmm_expr) : Prop :=
  match h with
  | (sym, off, x) =>
      exists e,
        tc_V th x = Some e /\
        (u = return_unit_image
               (extcall_void_image "caml_initialize"%string
                  [strided_field_address_image
                     (Cconst_symbol (CS_sym sym)) off; e])
         \/ exists ch,
              u = Cop (Cstore ch Initialization)
                    [strided_field_address_image
                       (Cconst_symbol (CS_sym sym)) off; e])
  end.

Fixpoint seq_updates (us : list cmm_expr) (body : cmm_expr)
  : cmm_expr :=
  match us with
  | [] => body
  | u :: us' => Csequence u (seq_updates us' body)
  end.

Section ToCmmDataClosure.

(* The six hooks main ruled stay OPEN (no owning rules in any
   chapter; the doc places the calling conventions at CM.Apply /
   CM.Extcall, ch. 15, and ch. 20 treats them as axiomatized
   there; lookup_code / classify_handler / debug_flag are unit- and
   build-level inputs).  Same signatures as ToCmmControl's
   Variables. *)
Variable lookup_code : code_id -> option code0.
Variable classify_handler :
  continuation -> cont_handler -> expr -> cont_class -> Prop.
Variable debug_flag : bool.
Variable indirect_call_image :
  tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop.
Variable indirect_full_call_image :
  tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop.
Variable extcall_image :
  tcenv -> apply_expr -> list cmm_expr -> cmm_expr -> Prop.

(* The TC.Let-star family: ch. 16's let_binding_ext hook made
   concrete (the env extension and the emitted binding context,
   identity when the binding is delayed or dropped). *)
Inductive tc_let_binding
  : tcenv -> bound_pattern -> named -> tcenv
    -> (cmm_expr -> cmm_expr) -> Prop :=

  (** RULE TC.Let.Simple (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_expr0
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#bind_var_to_simple

      x is bound to the simple's translation in V (an alias;
      add_alias if s is a var); no Cmm is emitted -- the value is
      substituted at uses.  Matches OS.Let.Simple. *)
  | TC_Let_Simple : forall th bv s e,
      tc_simple th s e ->
      tc_let_binding th (BPat_singleton bv) (N_simple s)
        (upd_V th (bv_var bv) e) (fun body => body)

  (** RULE TC.Let.Prim (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_prim
      CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding

      The defining emission is tc_prim's ("the emission of the
      primitive itself is TC.Prim.<p>"); classify_let_binding
      selects the class and the environment is extended accordingly
      (let_prim_ext; bind_variable_to_primitive).  A dropped pure
      binding is still translated so its own arguments can be
      inlined -- the tc_prim premise stays in the drop row.  The
      class is unconstrained here (occurrence inputs elided; see
      let_prim_ext's notes for the placement encoding).  Matches
      OS.Let.Prim.Pure / OS.Let.Prim.Effect. *)
  | TC_Let_Prim : forall th bv p e_p cls th' ctx,
      tc_prim th p e_p ->
      let_prim_ext th (bv_var bv) e_p cls th' ctx ->
      tc_let_binding th (BPat_singleton bv) (N_prim p) th' ctx

  (** RULE TC.Let.SetOfClosures (CLAIM normative)
      -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#let_dynamic_set_of_closures

      A Calloc of a closure_tag block laid out as R.Obj.Closures --
      per function slot the infix header (when off(f) > 0) then the
      2-word (code pointer, closinfo) or 3-word (generic entry,
      closinfo, code pointer) layout, then the captured environment
      at the value-slot offsets; am selects heap vs region
      allocation (19); each bound var is bound to its clos-value
      projection.  The word list reuses ch. 17's layout vocabulary
      byte-for-byte (infix_hdr, pack_closure_info, closinfo_arity,
      slot_is_two_word, generic_entry_word, the slot-offset
      Parameters), so the R_Obj_Closures obligation relates
      directly.
      ENCODING NOTES: startenv, each slot's is_last flag, and the
      words of FD_deleted slots are Slot_offsets layout outputs the
      rule does not determine -- existential/unconstrained, exactly
      as in R_Obj_Closures; the arity comes from the code metadata
      through the lookup_code hook (mk_arity_info of
      c0_params_arity / c0_is_tupled); the large-block path is
      elided as at TC.Prim.MakeBlock; bound-var name modes are
      ignored. *)
  | TC_Let_SetOfClosures :
      forall th bvs soc am bv0 (ws : list cmm_expr) startenv,
      List.length bvs = List.length (soc_function_decls soc) ->
      (forall f cid full_only,
         In (f, FD_code_id cid full_only) (soc_function_decls soc) ->
         exists c0 (is_last : bool),
           lookup_code cid = Some c0 /\
           let ai := mk_arity_info (c0_params_arity c0)
                       (c0_is_tupled c0) in
           (fun_slot_offset f > 0 ->
              nth_error ws (Z.to_nat (fun_slot_offset f - 1))
                = Some (Cconst_natint
                          (infix_hdr (fun_slot_offset f)))) /\
           (if slot_is_two_word ai
            then
              nth_error ws (Z.to_nat (fun_slot_offset f))
                = Some (Cconst_symbol (CS_code cid)) /\
              nth_error ws (Z.to_nat (fun_slot_offset f + 1))
                = Some (Cconst_natint
                          (pack_closure_info (closinfo_arity ai)
                             (startenv - fun_slot_offset f)
                             is_last))
            else
              nth_error ws (Z.to_nat (fun_slot_offset f))
                = Some (Cconst_natint (generic_entry_word ai)) /\
              nth_error ws (Z.to_nat (fun_slot_offset f + 1))
                = Some (Cconst_natint
                          (pack_closure_info (closinfo_arity ai)
                             (startenv - fun_slot_offset f)
                             is_last)) /\
              nth_error ws (Z.to_nat (fun_slot_offset f + 2))
                = Some (Cconst_symbol (CS_code cid)))) ->
      (forall vs s,
         In (vs, s) (soc_value_slots soc) ->
         exists e,
           tc_simple th s e /\
           nth_error ws (Z.to_nat (val_slot_offset vs))
             = Some e) ->
      tc_let_binding th (BPat_set_of_closures bvs)
        (N_set_of_closures soc am)
        (bind_closure_vars th bv0
           (combine bvs (soc_function_decls soc)))
        (fun body =>
           Clet bv0
             (Cop (Calloc (cmm_mode am) Alloc_block_kind_closure)
                (Cconst_natint
                   (hdr_word closure_tag
                      (Z.of_nat (List.length ws))
                      (alloc_col am) 0)
                 :: ws))
             body)

  (** RULE TC.Let.Static (CLAIM normative) -- 18-to-cmm-data.md
      CODE middle_end/flambda2/to_cmm/to_cmm_static.ml#static_consts
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_expr0

      Static constants become Cdata phrases at their symbols per
      R.Obj.-star (black-marked headers, R.Header); Or_variable
      holes filled from rho become deferred stores sequenced before
      the body (update_opt).  Cmm image of OS.Let.Static.
      ENCODING NOTE (cataloged with main and reviewed by the
      fidelity reviewer): Cdata phrases are link-time data outside
      cmm_expr, and the let_binding_ext signature has no program or
      memory slot, so the layout obligation is NOT expressed here --
      it is R.Obj.-star on the initial memory (ch. 17's
      rep_obj/rep_heap), consumed by ch. 20's initial-state premise.
      The relation records the env extension (none: symbols
      translate by TC_Simple_Sym) and the sequenced updates:
      static_group_holes pins the update list to the holes of scg,
      in binder then field order, and static_update pins each
      store's target and shape (reviewer findings KF-035/KF-036;
      with the list free, us = [] with holes present and stores
      into other constants' fields were behavior-changing slack).
      Disclosed residue: within a set of closures the code emits in
      layout-offset order, the model in value-slot list order
      (initializing stores to distinct words of an object not yet
      published); and an update whose translated value is a Cvar is
      hoisted by the code to that variable's binding site
      (add_symbol_init) -- the doc's sequenced-before-body
      placement is what is modeled.
      The second clause is the rule's NOTES sentence: RecInfo
      bindings (Singleton, Rec_info) emit nothing; matches
      OS.Let.RecInfo. *)
  | TC_Let_Static :
      forall th bst scg hs (us : list cmm_expr),
      static_group_holes bst scg hs ->
      Forall2 (static_update th) hs us ->
      tc_let_binding th (BPat_static bst) (N_static_consts scg)
        th (fun body => seq_updates us body)
  | TC_Let_RecInfo : forall th bv ri,
      tc_let_binding th (BPat_singleton bv) (N_rec_info ri)
        th (fun body => body).

(* The ch. 16 judgment at the concrete ch. 18 state: the nine data
   hooks closed at the judgments above, the six ruled hooks left
   quantified.  Arguments are ToCmmControl's Section Variables in
   declaration order. *)
Definition tc_expr_data : tcenv -> expr -> cmm_expr -> Prop :=
  tc_expr data_state
    tc_simple
    tc_simples
    tc_bind_var_to_simple
    tc_bind_var_to_cmm
    tc_var_binds
    tc_flush_data
    machtype_of_kind_data
    tc_let_binding
    lookup_code
    classify_handler
    debug_flag
    indirect_call_image
    indirect_full_call_image
    extcall_image.

End ToCmmDataClosure.
