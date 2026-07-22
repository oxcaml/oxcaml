(* ===================================================================== *)
(* TypeGrammar.v -- flambda2 formalism, chapter 07: the types abstract   *)
(* domain.  Type grammar (sec. 2), typing environments (sec. 3),         *)
(* environment extensions (sec. 5).  Concretization (sec. 4) lives in   *)
(* Concretization.v; meet/join/provers (ch. 08) in MeetJoin.v.           *)
(*                                                                       *)
(* Owner: Scott (Team Hilbert).  Wave 2.                                 *)
(* Imports: Base (wave 0), Syntax (wave 1).                              *)
(* ===================================================================== *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax.

(* ENCODING NOTE: finite sets in the docs (Targetint_31_63.Set.t,
   Name.Set.t, String_info.Set.t, Float.Set.t, ...) are encoded as lists;
   membership is List.In, and set-level equations in later chapters are
   stated up to membership extensionality.  Non-emptiness side conditions
   are stated explicitly where the docs require them
   (T.Grammar.NakedNumber.NonEmptySet). *)

(** RULE T.Role.SinglePass (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.mli#t
    Simplify performs one pass over each function body; the types domain
    provides no widening operator, and meet/join (ch. 08) are the only
    combinators, join applied once per control-flow merge.
    ENCODING NOTE: this is an architectural absence claim (no widening
    operator exists in the development); it is not expressible as a Prop
    about the objects defined here, so it is recorded as a documented
    anchor.  Reported to main for the CORRESPONDENCE catalog. *)
Definition T_Role_SinglePass_documented : Prop := True.

(* --------------------------------------------------------------------- *)
(* Basic auxiliary data                                                   *)
(* --------------------------------------------------------------------- *)

(* Binding times (ch. 07 sec. 3.1): reserved times order the name
   universe; locally-defined variables get successive times from
   bt_earliest_var upward. *)
Definition binding_time := nat.
Definition bt_consts : binding_time := 0.
Definition bt_symbols : binding_time := 1.
Definition bt_imported_variables : binding_time := 2.
Definition bt_earliest_var : binding_time := 3.

(* Scopes (ch. 07 sec. 3.3).  The level stack itself is not modeled (see
   the ENCODING NOTE at cut_as_extension below); scopes are bare
   identifiers ordered as naturals. *)
Definition scope_id := nat.

(* Block_size.t: a Targetint_31_63.t size, encoded as Z. *)
Definition block_size := Z.

(* Relations of the naked-immediate relational domain
   (type_grammar.ml#Relation.descr). *)
Inductive irelation : Type :=
  | Rel_is_null
  | Rel_is_int
  | Rel_get_tag.

Definition irelation_eqb (r1 r2 : irelation) : bool :=
  match r1, r2 with
  | Rel_is_null, Rel_is_null => true
  | Rel_is_int, Rel_is_int => true
  | Rel_get_tag, Rel_get_tag => true
  | _, _ => false
  end.

(* An inverse-relations map: relation |-> set of Names (Name.Set.t as a
   list, per the sets ENCODING NOTE above). *)
Definition inverse_relations := fmap irelation (list name).

(* String_info.t: length plus, for known constants, contents (bytes as
   Z, one per char). *)
Inductive string_contents : Type :=
  | SC_unknown_or_mutable
  | SC_contents (bytes : list Z).

Record string_info : Type := Mk_string_info
  { si_size : Z;
    si_contents : string_contents }.

(* Set_of_closures_contents.t: the function and value slots present in a
   closure block (index lattice of the closures row_like). *)
Record set_of_closures_contents : Type := Mk_set_of_closures_contents
  { socc_function_slots : list function_slot;
    socc_value_slots : list value_slot }.

(* Nullability flag on a Value head (ch. 07 sec. 2.1).  The optional
   variable in Maybe_null is the Naked_immediate variable holding the
   is_null test of this value. *)
Inductive is_null_ty : Type :=
  | Not_null
  | Maybe_null (is_null_var : option variable).

(* --------------------------------------------------------------------- *)
(* Type_descr: the three-way split                                        *)
(* --------------------------------------------------------------------- *)

(** RULE T.Grammar.TypeDescr (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_descr.mli#Descr.t
    CODE middle_end/flambda2/types/grammar/type_descr.mli#descr
    For each kind kappa, a type is Unknown, Bottom, a head (No_alias), or
    an alias (Equals s) to a Simple.  Unknown is the top element, Bottom
    the least.  Encoded exactly as the code exposes it: an
    Or_unknown_or_bottom whose Ok payload is No_alias head | Equals s. *)
Inductive alias_or_head (H : Type) : Type :=
  | No_alias (h : H)
  | Equals (s : simple).
Arguments No_alias {H}.
Arguments Equals {H}.

Definition type_descr (H : Type) : Type :=
  or_unknown_or_bottom (alias_or_head H).

(* --------------------------------------------------------------------- *)
(* Row_like indices                                                       *)
(* --------------------------------------------------------------------- *)

(** RULE T.Grammar.RowLike.Index (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.mli#row_like_index_domain
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#row_like_index_domain
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#check_field_tys
    CODE middle_end/flambda2/types/grammar/more_type_creators.ml#unknown_from_shape
    A row-like index is Known x (the singleton {x}) or At_least x (every
    index y with x contained in y).  Block cases are indexed by
    (Tag, Block_size, Block_shape) and map to a field-type array; closure
    cases are indexed by (Function_slot, Set_of_closures_contents) and
    map to a closures_entry (instantiations in the mutual grammar below:
    row_like_block_case / row_like_closures_case).  Each case
    additionally carries an env_extension (sec. 5).  The Block_shape on a
    block index fixes the per-field kind of the maps_to array
    (check_field_tys / unknown_from_shape). *)
Inductive row_like_index_domain (Lat : Type) : Type :=
  | Rl_known (l : Lat)
  | Rl_at_least (l : Lat).
Arguments Rl_known {Lat}.
Arguments Rl_at_least {Lat}.

Record row_like_index (Lat Shape : Type) : Type := Mk_row_like_index
  { rli_domain : row_like_index_domain Lat;
    rli_shape : Shape }.
Arguments Mk_row_like_index {Lat Shape}.
Arguments rli_domain {Lat Shape}.
Arguments rli_shape {Lat Shape}.

(* --------------------------------------------------------------------- *)
(* Naked-number heads (non-recursive)                                     *)
(* --------------------------------------------------------------------- *)

(** RULE T.Grammar.NakedImmediate.Relational (STATUS normative)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#is_int_for_scrutinee
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#get_tag_for_block
    VERIFIED 14-validation/issue5721.md
    VERIFIED 14-validation/naked_immediates_many_relations.md
    A Naked_immediate type is a finite set of immediate values (or
    Unknown, i.e. the set component absent) together with a map from
    relations (Is_null, Is_int, Get_tag) to sets of Names; an entry
    R |-> N asserts that this immediate equals R applied to each name in
    N.  The three constructors mirror the code exactly; the unified
    (descr) view is the pair (set Or_unknown, relations map).
    The construction-helper and meet-time-reduction behaviors pinned by
    this rule (is_null builds `= RWC.is_null const`, is_int_for_scrutinee
    builds `= true` unconditionally on a constant scrutinee,
    get_tag_for_block builds Unknown on a constant; the reducer resolves
    constant relations via Relation.of_const and deliberately discards a
    Bottom result) are modeled with the meet-time reducer in MeetJoin.v
    (ch. 08), which is where the code exercises them. *)
Inductive head_naked_immediate : Type :=
  | Naked_immediates (s : list Z)
  | Inverse_relations (m : inverse_relations)
  | Naked_immediates_and_inverse_relations
      (s : list Z) (m : inverse_relations).

(** RULE T.Grammar.NakedNumber.NonEmptySet (STATUS normative)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_float
    For each naked number kind other than Naked_immediate, a head is a
    finite set of constants of that kind, maintained non-empty; the
    empty set is represented by the overall Bottom type rather than an
    empty head (well-formedness head_nonempty below).  No interval or
    relational form exists. *)
Definition head_naked_float := list float64.
Definition head_naked_float32 := list float32.
Definition head_naked_int8 := list Z.
Definition head_naked_int16 := list Z.
Definition head_naked_int32 := list Z.
Definition head_naked_int64 := list Z.
Definition head_naked_nativeint := list Z.
Definition head_naked_vec128 := list vec128.
Definition head_naked_vec256 := list vec256.
Definition head_naked_vec512 := list vec512.

Definition head_nonempty {A : Type} (h : list A) : Prop := h <> nil.

(** RULE T.Grammar.RecInfoRegion.Trivial (STATUS normative)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_region
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_rec_info
    head_of_kind_region carries no information (unit): the Region
    lattice is {Bottom, Unknown} plus aliases.  head_of_kind_rec_info is
    a Rec_info_expr.t whose interpretation belongs to inlining (ch. 11);
    the types domain treats it opaquely. *)
Definition head_rec_info := rec_info_expr.
Definition head_region := unit.

(* --------------------------------------------------------------------- *)
(* The type grammar (mutual)                                              *)
(* --------------------------------------------------------------------- *)

(* ftype mirrors ch. 07 sec. 2: the top-level constructor is the kind,
   and underneath each sits a type_descr over that kind's head. *)
Inductive ftype : Type :=
  | FT_value (td : type_descr head_value)
  | FT_naked_immediate (td : type_descr head_naked_immediate)
  | FT_naked_float (td : type_descr head_naked_float)
  | FT_naked_float32 (td : type_descr head_naked_float32)
  | FT_naked_int8 (td : type_descr head_naked_int8)
  | FT_naked_int16 (td : type_descr head_naked_int16)
  | FT_naked_int32 (td : type_descr head_naked_int32)
  | FT_naked_int64 (td : type_descr head_naked_int64)
  | FT_naked_nativeint (td : type_descr head_naked_nativeint)
  | FT_naked_vec128 (td : type_descr head_naked_vec128)
  | FT_naked_vec256 (td : type_descr head_naked_vec256)
  | FT_naked_vec512 (td : type_descr head_naked_vec512)
  | FT_rec_info (td : type_descr head_rec_info)
  | FT_region (td : type_descr head_region)

(* head_value (ch. 07 sec. 2.1): nullability x non-null part.  The two
   Or_unknown_or_bottom layers (here and in type_descr) are deliberately
   redundant: they express "unknown but not null", "bottom but maybe
   null" (the type of Null), etc. *)
with head_value : Type :=
  | Mk_head_value
      (non_null : or_unknown_or_bottom head_value_non_null)
      (is_null : is_null_ty)

with head_value_non_null : Type :=
  (** RULE T.Grammar.Variant (STATUS normative) -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null
      CODE middle_end/flambda2/types/grammar/type_grammar.ml#create_variant
      A Variant head is the disjoint union of an immediates arm (a
      Naked_immediate type or Unknown) and a blocks arm (a
      row_like_for_blocks or Unknown), each of which may independently
      be Bottom.  Its is_int/get_tag fields optionally name the
      Naked_immediate variables equal to %is_int and %get_tag of the
      value. *)
  | HV_variant
      (is_int : option variable)
      (immediates : or_unknown ftype)
      (get_tag : option variable)
      (blocks : or_unknown row_like_for_blocks)
      (extensions : variant_extensions)
      (is_unique : bool)
  | HV_mutable_block (am : alloc_mode_types)
  | HV_boxed_float32 (t : ftype) (am : alloc_mode_types)
  | HV_boxed_float (t : ftype) (am : alloc_mode_types)
  | HV_boxed_int32 (t : ftype) (am : alloc_mode_types)
  | HV_boxed_int64 (t : ftype) (am : alloc_mode_types)
  | HV_boxed_nativeint (t : ftype) (am : alloc_mode_types)
  | HV_boxed_vec128 (t : ftype) (am : alloc_mode_types)
  | HV_boxed_vec256 (t : ftype) (am : alloc_mode_types)
  | HV_boxed_vec512 (t : ftype) (am : alloc_mode_types)
  | HV_closures
      (by_function_slot : row_like_for_closures)
      (am : alloc_mode_types)
  | HV_string (infos : list string_info)
  | HV_array
      (element_kind : or_unknown_or_bottom kind_ws)
      (length : ftype)
      (contents : or_unknown array_contents)
      (am : alloc_mode_types)

(* Array contents (ch. 07 sec. 2.5): Immutable { fields } | Mutable. *)
with array_contents : Type :=
  | AC_immutable (fields : list ftype)
  | AC_mutable

(** RULE T.Grammar.Disjunction.Extensions (STATUS normative)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#variant_extensions
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#row_like_case
    Each row_like case carries an env_extension (the env_ext argument of
    row_like_block_case / row_like_closures_case below), and a Variant
    carries variant_extensions (No_extensions, or Ext with a
    when_immediate and a when_block extension).  These hold constraints
    implied by selecting that disjunct; gamma of the disjunct is
    intersected with gamma of its extension (sec. 5).  Only meet on
    Row_like structures is guaranteed to exploit them, not join. *)
with variant_extensions : Type :=
  | No_extensions
  | Ext
      (when_immediate : fmap name ftype)
      (when_block : fmap name ftype)

(* Row_like for blocks (ch. 07 sec. 2.3). *)
with row_like_for_blocks : Type :=
  | Mk_row_like_for_blocks
      (known_tags : fmap tag (or_unknown row_like_block_case))
      (other_tags : or_bottom row_like_block_case)
      (am : alloc_mode_types)

with row_like_block_case : Type :=
  | Mk_row_like_block_case
      (maps_to : list ftype)
      (index : row_like_index block_size block_shape)
      (env_ext : fmap name ftype)

(* Row_like for closures (ch. 07 sec. 2.3).  other_closures is
   documented in the code as always Bottom and slated for removal; we
   keep the field for fidelity. *)
with row_like_for_closures : Type :=
  | Mk_row_like_for_closures
      (known_closures : fmap function_slot row_like_closures_case)
      (other_closures : or_bottom row_like_closures_case)

with row_like_closures_case : Type :=
  | Mk_row_like_closures_case
      (maps_to : closures_entry)
      (index : row_like_index set_of_closures_contents unit)
      (env_ext : fmap name ftype)

with closures_entry : Type :=
  | Mk_closures_entry
      (function_types : fmap function_slot (or_unknown function_type))
      (closure_types : fmap function_slot ftype)
      (value_slot_types : fmap value_slot ftype)

with function_type : Type :=
  | Mk_function_type
      (ft_code_id : code_id)
      (ft_rec_info : ftype).

(* Environment extensions (ch. 07 sec. 5): a set of equations to be
   layered onto E.  FROZEN interface name.  The when_immediate /
   when_block / env_ext arguments in the mutual block above are
   definitionally this type (spelled out there because a Definition
   cannot forward-reference the mutual block). *)
Definition env_extension : Type := fmap name ftype.

(* --------------------------------------------------------------------- *)
(* Small helpers on the grammar                                           *)
(* --------------------------------------------------------------------- *)

Definition hv_non_null (h : head_value)
  : or_unknown_or_bottom head_value_non_null :=
  match h with Mk_head_value nn _ => nn end.

Definition hv_is_null (h : head_value) : is_null_ty :=
  match h with Mk_head_value _ isnull => isnull end.

Definition descr_alias {H : Type} (td : type_descr H) : option simple :=
  match td with
  | Oub_ok (Equals s) => Some s
  | _ => None
  end.

(* The Simple aliased by an Equals type, at any kind. *)
Definition ftype_alias_simple (T : ftype) : option simple :=
  match T with
  | FT_value td => descr_alias td
  | FT_naked_immediate td => descr_alias td
  | FT_naked_float td => descr_alias td
  | FT_naked_float32 td => descr_alias td
  | FT_naked_int8 td => descr_alias td
  | FT_naked_int16 td => descr_alias td
  | FT_naked_int32 td => descr_alias td
  | FT_naked_int64 td => descr_alias td
  | FT_naked_nativeint td => descr_alias td
  | FT_naked_vec128 td => descr_alias td
  | FT_naked_vec256 td => descr_alias td
  | FT_naked_vec512 td => descr_alias td
  | FT_rec_info td => descr_alias td
  | FT_region td => descr_alias td
  end.

Definition kind_of_ftype (T : ftype) : kind :=
  match T with
  | FT_value _ => K_value
  | FT_naked_immediate _ => K_naked_immediate
  | FT_naked_float _ => K_naked_float
  | FT_naked_float32 _ => K_naked_float32
  | FT_naked_int8 _ => K_naked_int8
  | FT_naked_int16 _ => K_naked_int16
  | FT_naked_int32 _ => K_naked_int32
  | FT_naked_int64 _ => K_naked_int64
  | FT_naked_nativeint _ => K_naked_nativeint
  | FT_naked_vec128 _ => K_naked_vec128
  | FT_naked_vec256 _ => K_naked_vec256
  | FT_naked_vec512 _ => K_naked_vec512
  | FT_rec_info _ => K_rec_info
  | FT_region _ => K_region
  end.

Definition unknown_of_kind (k : kind) : ftype :=
  match k with
  | K_value => FT_value Oub_unknown
  | K_naked_immediate => FT_naked_immediate Oub_unknown
  | K_naked_float => FT_naked_float Oub_unknown
  | K_naked_float32 => FT_naked_float32 Oub_unknown
  | K_naked_int8 => FT_naked_int8 Oub_unknown
  | K_naked_int16 => FT_naked_int16 Oub_unknown
  | K_naked_int32 => FT_naked_int32 Oub_unknown
  | K_naked_int64 => FT_naked_int64 Oub_unknown
  | K_naked_nativeint => FT_naked_nativeint Oub_unknown
  | K_naked_vec128 => FT_naked_vec128 Oub_unknown
  | K_naked_vec256 => FT_naked_vec256 Oub_unknown
  | K_naked_vec512 => FT_naked_vec512 Oub_unknown
  | K_rec_info => FT_rec_info Oub_unknown
  | K_region => FT_region Oub_unknown
  end.

Definition bottom_of_kind (k : kind) : ftype :=
  match k with
  | K_value => FT_value Oub_bottom
  | K_naked_immediate => FT_naked_immediate Oub_bottom
  | K_naked_float => FT_naked_float Oub_bottom
  | K_naked_float32 => FT_naked_float32 Oub_bottom
  | K_naked_int8 => FT_naked_int8 Oub_bottom
  | K_naked_int16 => FT_naked_int16 Oub_bottom
  | K_naked_int32 => FT_naked_int32 Oub_bottom
  | K_naked_int64 => FT_naked_int64 Oub_bottom
  | K_naked_nativeint => FT_naked_nativeint Oub_bottom
  | K_naked_vec128 => FT_naked_vec128 Oub_bottom
  | K_naked_vec256 => FT_naked_vec256 Oub_bottom
  | K_naked_vec512 => FT_naked_vec512 Oub_bottom
  | K_rec_info => FT_rec_info Oub_bottom
  | K_region => FT_region Oub_bottom
  end.

(* any_value = Value TD.unknown: the unrestricted top of kind Value
   (admits Null).  The non-null top is the distinct any_non_null_value
   (type_grammar.ml#any_non_null_value). *)
Definition any_value : ftype := FT_value Oub_unknown.

Definition any_non_null_value : ftype :=
  FT_value (Oub_ok (No_alias (Mk_head_value Oub_unknown Not_null))).

(* alias_type_of kappa s: the Equals-s type at kind kappa
   (type_grammar.mli#alias_type_of). *)
Definition alias_type_of (k : kind) (s : simple) : ftype :=
  match k with
  | K_value => FT_value (Oub_ok (Equals s))
  | K_naked_immediate => FT_naked_immediate (Oub_ok (Equals s))
  | K_naked_float => FT_naked_float (Oub_ok (Equals s))
  | K_naked_float32 => FT_naked_float32 (Oub_ok (Equals s))
  | K_naked_int8 => FT_naked_int8 (Oub_ok (Equals s))
  | K_naked_int16 => FT_naked_int16 (Oub_ok (Equals s))
  | K_naked_int32 => FT_naked_int32 (Oub_ok (Equals s))
  | K_naked_int64 => FT_naked_int64 (Oub_ok (Equals s))
  | K_naked_nativeint => FT_naked_nativeint (Oub_ok (Equals s))
  | K_naked_vec128 => FT_naked_vec128 (Oub_ok (Equals s))
  | K_naked_vec256 => FT_naked_vec256 (Oub_ok (Equals s))
  | K_naked_vec512 => FT_naked_vec512 (Oub_ok (Equals s))
  | K_rec_info => FT_rec_info (Oub_ok (Equals s))
  | K_region => FT_region (Oub_ok (Equals s))
  end.

(* --------------------------------------------------------------------- *)
(* Typing environments (ch. 07 sec. 3)                                    *)
(* --------------------------------------------------------------------- *)

(* Per-name entry of the name->type map: the stored type, the name's
   binding time, and its (unscoped) name mode
   (cached_level.mli#names_to_types). *)
Record name_entry : Type := Mk_name_entry
  { ne_type : ftype;
    ne_binding_time : binding_time;
    ne_mode : name_mode }.

(* FROZEN record (team brief).  ENCODING NOTE: relative to
   typing_env.ml#t, the machine_width / resolver /
   binding_time_resolver / next_binding_time fields are dropped
   (cross-unit import machinery is out of scope, ch. 07 sec. 6), and the
   level stack (prev_levels / current_level) is abstracted away --
   te_types is the flattened name->type map of the current Cached_level
   and cut_as_extension (below) is the opaque interface to levels.
   te_canonical is the aliases domain's path-compressed
   canonical_elements map (aliases.mli): the canonical element of s's
   class, absent when s is its own canonical.  Coercions on aliases are
   not tracked (equality up to coercion; ch. 07 sec. 6 scope ledger).
   te_code_age maps a code id to the *older* code id it supersedes
   (newer |-> older, code_age_relation.ml). *)
Record tenv : Type := Mk_tenv
  { te_types : fmap name name_entry;
    te_canonical : fmap simple simple;
    te_defined_symbols : list symbol;
    te_code_age : fmap code_id code_id;
    te_min_binding_time : binding_time;
    te_is_bottom : bool }.

(* Canonical element of s's alias class. *)
Definition canonical (E : tenv) (s : simple) : simple :=
  match te_canonical E s with
  | Some c => c
  | None => s
  end.

(* Reflexive-transitive closure of te_code_age: cid is the same as, or a
   newer version of, cid' (the newer_version_of preorder; used by
   T.Gamma.Value.Closures / T.Gamma.Closures.CodeAgeLoose in
   Concretization.v; meet_code_id itself is not modeled -- MeetJoin.v
   holds te_code_age constant through descent). *)
Inductive code_age_newer_eq (E : tenv) : code_id -> code_id -> Prop :=
  | CAN_refl : forall cid, code_age_newer_eq E cid cid
  | CAN_step : forall cid cid' cid'',
      te_code_age E cid = Some cid' ->
      code_age_newer_eq E cid' cid'' ->
      code_age_newer_eq E cid cid''.

Definition binding_time_of_name (E : tenv) (n : name) : binding_time :=
  match te_types E n with
  | Some ne => ne_binding_time ne
  | None =>
      match n with
      | Name_sym _ => bt_symbols
      | Name_var _ => bt_imported_variables
      end
  end.

(* --- SYNC PENDING Syntax.v --------------------------------------------
   The next three definitions depend on the constructor inventory of
   `simple` (ch. 02), owned by Church.  Requested from Church in Syntax.v:
     simple_of_name : name -> simple   (the injection)
     simple_name : simple -> option name
       (the underlying name, looking through coercions; None iff the
        simple is a constant)
   If the shipped names differ, only this block changes.
   ----------------------------------------------------------------------- *)

Definition simple_is_const (s : simple) : bool :=
  match simple_name s with
  | None => true
  | Some _ => false
  end.

(* Occurrence of a name in a simple (through coercions). *)
Definition simple_mentions (s : simple) (n : name) : Prop :=
  simple_name s = Some n.

Definition binding_time_of_simple (E : tenv) (s : simple)
  : binding_time :=
  match simple_name s with
  | None => bt_consts
  | Some n => binding_time_of_name E n
  end.

(* --------------------------------------------------------------------- *)
(* Free-name occurrence in a type                                         *)
(* --------------------------------------------------------------------- *)

(* ENCODING NOTE: the docs' "free names of T" (used by
   T.Env.Equation.Closed) is an inductive occurrence relation over the
   grammar.  The Rec_info head is opaque (ch. 07 sec. 2.6), so
   occurrences inside Rec_info_expr payloads are not tracked; index
   lattices (tags, sizes, slot sets) contain no names by construction
   (ch. 07 sec. 2.3, "indices are plain data, never types"). *)

Definition hni_mentions (h : head_naked_immediate) (n : name) : Prop :=
  match h with
  | Naked_immediates _ => False
  | Inverse_relations m
  | Naked_immediates_and_inverse_relations _ m =>
      exists r ns, m r = Some ns /\ In n ns
  end.

Definition boxed_payload (h : head_value_non_null) : option ftype :=
  match h with
  | HV_boxed_float32 t _ | HV_boxed_float t _
  | HV_boxed_int32 t _ | HV_boxed_int64 t _ | HV_boxed_nativeint t _
  | HV_boxed_vec128 t _ | HV_boxed_vec256 t _ | HV_boxed_vec512 t _ =>
      Some t
  | _ => None
  end.

Inductive tmentions : ftype -> name -> Prop :=
  | TM_alias : forall T s n,
      ftype_alias_simple T = Some s ->
      simple_mentions s n ->
      tmentions T n
  | TM_value_head : forall hv n,
      hv_mentions hv n ->
      tmentions (FT_value (Oub_ok (No_alias hv))) n
  | TM_naked_immediate_head : forall h n,
      hni_mentions h n ->
      tmentions (FT_naked_immediate (Oub_ok (No_alias h))) n

with hv_mentions : head_value -> name -> Prop :=
  | HVM_non_null : forall h inl n,
      hvnn_mentions h n ->
      hv_mentions (Mk_head_value (Oub_ok h) inl) n
  | HVM_is_null : forall nn x,
      hv_mentions (Mk_head_value nn (Maybe_null (Some x))) (Name_var x)

with hvnn_mentions : head_value_non_null -> name -> Prop :=
  | NNM_variant_is_int : forall x imms gt bl ex u,
      hvnn_mentions (HV_variant (Some x) imms gt bl ex u) (Name_var x)
  | NNM_variant_immediates : forall ii T gt bl ex u n,
      tmentions T n ->
      hvnn_mentions (HV_variant ii (Ou_known T) gt bl ex u) n
  | NNM_variant_get_tag : forall ii imms x bl ex u,
      hvnn_mentions (HV_variant ii imms (Some x) bl ex u) (Name_var x)
  | NNM_variant_blocks : forall ii imms gt rl ex u n,
      rlfb_mentions rl n ->
      hvnn_mentions (HV_variant ii imms gt (Ou_known rl) ex u) n
  | NNM_variant_extensions : forall ii imms gt bl ex u n,
      varext_mentions ex n ->
      hvnn_mentions (HV_variant ii imms gt bl ex u) n
  | NNM_boxed : forall h t n,
      boxed_payload h = Some t ->
      tmentions t n ->
      hvnn_mentions h n
  | NNM_closures : forall rl am n,
      rlfc_mentions rl n ->
      hvnn_mentions (HV_closures rl am) n
  | NNM_array_length : forall ek len ac am n,
      tmentions len n ->
      hvnn_mentions (HV_array ek len ac am) n
  | NNM_array_contents : forall ek len ac am n,
      ac_mentions ac n ->
      hvnn_mentions (HV_array ek len (Ou_known ac) am) n

with ac_mentions : array_contents -> name -> Prop :=
  | ACM_field : forall tys T n,
      In T tys ->
      tmentions T n ->
      ac_mentions (AC_immutable tys) n

with varext_mentions : variant_extensions -> name -> Prop :=
  | VEM_imm : forall wi wb n,
      ext_mentions wi n ->
      varext_mentions (Ext wi wb) n
  | VEM_block : forall wi wb n,
      ext_mentions wb n ->
      varext_mentions (Ext wi wb) n

with rlfb_mentions : row_like_for_blocks -> name -> Prop :=
  | RLFBM_known : forall kt ot am t c n,
      kt t = Some (Ou_known c) ->
      rlbc_mentions c n ->
      rlfb_mentions (Mk_row_like_for_blocks kt ot am) n
  | RLFBM_other : forall kt c am n,
      rlbc_mentions c n ->
      rlfb_mentions (Mk_row_like_for_blocks kt (Ob_ok c) am) n

with rlbc_mentions : row_like_block_case -> name -> Prop :=
  | RLBCM_field : forall tys idx eps T n,
      In T tys ->
      tmentions T n ->
      rlbc_mentions (Mk_row_like_block_case tys idx eps) n
  | RLBCM_ext : forall tys idx eps n,
      ext_mentions eps n ->
      rlbc_mentions (Mk_row_like_block_case tys idx eps) n

with rlfc_mentions : row_like_for_closures -> name -> Prop :=
  | RLFCM_known : forall kc oc f c n,
      kc f = Some c ->
      rlcc_mentions c n ->
      rlfc_mentions (Mk_row_like_for_closures kc oc) n
  | RLFCM_other : forall kc c n,
      rlcc_mentions c n ->
      rlfc_mentions (Mk_row_like_for_closures kc (Ob_ok c)) n

with rlcc_mentions : row_like_closures_case -> name -> Prop :=
  | RLCCM_maps_to : forall ce idx eps n,
      centry_mentions ce n ->
      rlcc_mentions (Mk_row_like_closures_case ce idx eps) n
  | RLCCM_ext : forall ce idx eps n,
      ext_mentions eps n ->
      rlcc_mentions (Mk_row_like_closures_case ce idx eps) n

with centry_mentions : closures_entry -> name -> Prop :=
  | CEM_function_type : forall fts cts vts f ft n,
      fts f = Some (Ou_known ft) ->
      fnty_mentions ft n ->
      centry_mentions (Mk_closures_entry fts cts vts) n
  | CEM_closure_type : forall fts cts vts f T n,
      cts f = Some T ->
      tmentions T n ->
      centry_mentions (Mk_closures_entry fts cts vts) n
  | CEM_value_slot : forall fts cts vts w T n,
      vts w = Some T ->
      tmentions T n ->
      centry_mentions (Mk_closures_entry fts cts vts) n

with fnty_mentions : function_type -> name -> Prop :=
  | FTM_rec_info : forall cid T n,
      tmentions T n ->
      fnty_mentions (Mk_function_type cid T) n

with ext_mentions : env_extension -> name -> Prop :=
  | EXTM_dom : forall (eps : env_extension) n T,
      eps n = Some T ->
      ext_mentions eps n
  | EXTM_rng : forall (eps : env_extension) n' T n,
      eps n' = Some T ->
      tmentions T n ->
      ext_mentions eps n.

(* --------------------------------------------------------------------- *)
(* Environment rules (ch. 07 sec. 3)                                      *)
(* --------------------------------------------------------------------- *)

Definition name_bound_in (E : tenv) (n : name) : Prop :=
  (exists ne, te_types E n = Some ne) \/
  (exists s, n = Name_sym s /\ In s (te_defined_symbols E)).

(** RULE T.Env.Equation.Closed (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/typing_env.ml#invariant_for_new_equation
    A new equation `name = T` may only mention (in T's free names) names
    already bound in E; a locally-unbound free name is a fatal error.
    Encoded as the admissibility condition on adding the equation. *)
Definition T_Env_Equation_Closed (E : tenv) (n : name) (T : ftype)
  : Prop :=
  forall n', tmentions T n' -> name_bound_in E n'.

(** RULE T.Env.Find.SymbolDefault (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'
    CODE middle_end/flambda2/types/env/typing_env.ml#initial_symbol_type
    A defined symbol with no equation has type any_value = Value
    TD.unknown, the unrestricted top of kind Value (admits Null; the
    non-null top any_non_null_value is distinct and not used here).  A
    symbol from a missing .cmx also defaults to any_value; a variable
    from a missing .cmx defaults to unknown of its kind (at binding time
    bt_imported_variables, cf. binding_time_of_name). *)
Definition tenv_find_default (n : name) (k : kind) : ftype :=
  match n with
  | Name_sym _ => any_value
  | Name_var _ => unknown_of_kind k
  end.

(** RULE T.Env.Find.Bottom (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'
    CODE middle_end/flambda2/types/env/typing_env.ml#make_bottom
    If is_bottom E, then find E x kappa = Bottom (bottom_like of kappa)
    for every name x. *)
Definition tenv_find (E : tenv) (n : name) (k : kind) : ftype :=
  if te_is_bottom E then bottom_of_kind k
  else
    match te_types E n with
    | Some ne => ne_type ne
    | None => tenv_find_default n k
    end.

(** RULE T.Env.Find.Canonical (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn
    CODE middle_end/flambda2/types/env/typing_env.ml#get_canonical_simple_exn
    type_simple_in_term_exn(E, s) returns (alias_type_of kappa canonical,
    canonical), where canonical is the canonical element of s's alias
    class and kappa is the kind of s; callers see the canonical name
    rather than the queried one.  A constant resolves to its own exact
    type (type_for_const): a constant is always its class's canonical
    (T.Env.ConstCanonicalPersists), so the returned alias points at the
    constant itself; the exact singleton type of a constant is the
    concretization-side reading (Concretization.v).
    ENCODING NOTE: name-mode floors on canonical lookups are not modeled
    (te_canonical is the unconstrained canonical map). *)
Definition type_simple_in_term (E : tenv) (k : kind) (s : simple)
  : ftype * simple :=
  let c := canonical E s in
  (alias_type_of k c, c).

(** RULE T.Env.Canonical.Least (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/aliases.mli#get_canonical_element_exn
    CODE middle_end/flambda2/types/env/binding_time.ml#consts
    CODE middle_end/flambda2/types/env/aliases.mli#Alias_set.find_best
    The canonical element of an alias class is the element of least
    binding time; the categories order constants < symbols < variables
    (consts=0, symbols=1, variables >= earliest_var=3), so constants are
    preferred, then symbols, then the earliest variable.
    ENCODING NOTE: environment invariants (this rule,
    NoEqualsOnCanonical, stored-equation closedness) are encoded as
    named Prop predicates on tenv, conjoined in tenv_wf below: the
    aliases domain is data (te_canonical) in this model, so the rule is
    a representation invariant, not a freestanding theorem.  The
    min_name_mode / min_binding_time floor on queries is not modeled
    (see T.Env.Find.Canonical). *)
Definition T_Env_Canonical_Least (E : tenv) : Prop :=
  forall s s',
    canonical E s' = canonical E s ->
    (binding_time_of_simple E (canonical E s)
       <= binding_time_of_simple E s')%nat.

(** RULE T.Env.Canonical.NoEqualsOnCanonical (STATUS normative)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/typing_env.ml#invariant_for_alias
    No canonical name may be given an Equals (alias) type; dually,
    non-canonical names carry Equals equations resolving toward the
    canonical element.
    ENCODING NOTE: only the first sentence is encoded.  The dual
    sentence is the stored-representation half -- the shape of the
    OCaml Equals chains, per T.Env.AliasesAuthoritative parts (b)/(c)
    -- and has no counterpart in this model, where reads resolve
    through te_canonical by construction. *)
Definition T_Env_Canonical_NoEqualsOnCanonical (E : tenv) : Prop :=
  forall n ne,
    te_types E n = Some ne ->
    canonical E (simple_of_name n) = simple_of_name n ->
    ftype_alias_simple (ne_type ne) = None.

(** RULE T.Env.Canonical.ConcreteOnCanonical (STATUS descriptive)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/typing_env.ml#replace_equation
    Concrete (non-alias) types are stored only on canonical names.  When
    the debug flag concrete_types_only_on_canonicals is set, adding a
    concrete type to a non-canonical name is a fatal error.  Guarded by
    a debug flag rather than always enforced, hence descriptive: it
    documents the intended representation invariant. *)
Definition T_Env_Canonical_ConcreteOnCanonical_documented : Prop := True.

(* Well-formedness of a typing environment: the maintained
   representation invariants of ch. 07 sec. 3.  Packages (in order) the
   named conjuncts for the rules T.Env.Canonical.Least,
   T.Env.Canonical.NoEqualsOnCanonical, and T.Env.Equation.Closed; each
   id's rule comment lives on its own named Prop above, not here.
   The last two conjuncts are the representation invariants behind
   T.Env.AliasesAuthoritative part (a) (te_canonical is
   path-compressed, so canonical is idempotent) and
   T.Env.ConstCanonicalPersists (a constant is its own canonical);
   without them both theorems below are refutable -- nothing else in
   tenv_wf stops a wf env from canonicalizing a constant away or
   carrying a canonical 2-cycle (KF-049). *)
Definition tenv_wf (E : tenv) : Prop :=
  T_Env_Canonical_Least E /\
  T_Env_Canonical_NoEqualsOnCanonical E /\
  (forall n ne,
      te_types E n = Some ne ->
      T_Env_Equation_Closed E n (ne_type ne)) /\
  (forall s, canonical E (canonical E s) = canonical E s) /\
  (forall c, simple_is_const c = true -> canonical E c = c).

(** RULE T.Env.ConstCanonicalPersists (STATUS conjectured)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/meet_env.ml#add_alias_between_canonicals
    CODE middle_end/flambda2/types/env/aliases.mli#find_best
    CODE middle_end/flambda2/types/env/binding_time.ml#consts
    CODE middle_end/flambda2/types/env/meet_env.ml#record_demotion
    If x's alias class contains a constant c, then (unless the env is
    bottom) canonical(x) = c, and no alias class contains two distinct
    constants (merging two constant canonicals bottoms the env).
    ENCODING NOTE: the doc rule quantifies over finite sequences of
    downwards-descent operations (add_equation / add_env_extension /
    add_alias / meet) with no cut/join boundary; those operations are
    modeled in MeetJoin.v, and preservation across them is deferred with
    the proofs.  Stated here as the static class invariant on wf
    environments that those operations maintain, and proved from
    tenv_wf's constants-self-canonical conjunct (KF-049); the doc's
    no-two-distinct-constants clause is the instantiation of s at the
    second constant. *)
Theorem T_Env_ConstCanonicalPersists :
  forall E, tenv_wf E ->
  te_is_bottom E = false ->
  forall s c,
    simple_is_const c = true ->
    canonical E c = canonical E s ->
    canonical E s = c.
Proof.
  intros E HW Hbot s c Hc Heq.
  destruct HW as (_ & _ & _ & _ & Hconst).
  rewrite <- Heq.
  exact (Hconst c Hc).
Qed.

(** RULE T.Env.AliasesAuthoritative (STATUS conjectured)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/aliases.ml#add
    CODE middle_end/flambda2/types/env/meet_env.ml#record_demotion
    CODE middle_end/flambda2/types/expand_head.ml#expand_head0
    CODE middle_end/flambda2/types/env/typing_env.ml#invariant_for_alias
    (a) the aliases domain is path-compressed: canonical_elements maps
    every non-canonical member directly to its class canonical.
    ENCODING NOTE: parts (b) and (c) of the doc rule contrast the code's
    stored Equals equations (not path-compressed; may form chains)
    against the aliases domain and observe that all reads resolve
    through the latter.  In this model reads go through te_canonical by
    construction (canonical / type_simple_in_term), which encodes (c);
    the stored-chain shape (b) is a statement about the OCaml
    representation with no counterpart here.  Part (a) is stated, and
    proved from tenv_wf's idempotence conjunct, which carries the
    path-compression invariant this rule asserts (KF-049). *)
Theorem T_Env_AliasesAuthoritative :
  forall E, tenv_wf E ->
  forall s, canonical E (canonical E s) = canonical E s.
Proof.
  intros E HW s.
  destruct HW as (_ & _ & _ & Hidem & _).
  exact (Hidem s).
Qed.

(* --------------------------------------------------------------------- *)
(* Scopes, levels, existentials (ch. 07 sec. 3.3)                         *)
(* --------------------------------------------------------------------- *)

(** RULE T.Env.Scope.Existential (STATUS normative) -- 07-types-domain.md
    CODE middle_end/flambda2/types/env/binding_time.ml#With_name_mode.scoped_name_mode
    CODE middle_end/flambda2/types/env/typing_env.mli#cut
    A variable with binding time strictly earlier than E's
    min_binding_time is out of scope; its name mode is reported as
    In_types, making it an existential variable that may occur inside
    types but not in terms.  Existentials are introduced when cutting a
    level at scope exit (cut / cut_as_extension below). *)
Definition scoped_name_mode (E : tenv) (bt : binding_time)
    (m : name_mode) : name_mode :=
  if Nat.ltb bt (te_min_binding_time E) then NM_in_types else m.

(* ENCODING NOTE: the level stack (One_level, prev_levels /
   current_level, Typing_env_level) is not modeled; cut_as_extension is
   the sanctioned opaque Parameter standing for `cut E ~cut_after`
   returned as a Typing_env_extension (typing_env.mli#cut_as_extension).
   Its normative content (the extension consists of exactly the
   equations and definitions added at levels strictly after the given
   scope) is deferred with the level model; join consumes it in
   MeetJoin.v. *)
Parameter cut_as_extension : tenv -> scope_id -> env_extension.
