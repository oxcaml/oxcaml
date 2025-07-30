(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let reaperdbg_env = Sys.getenv_opt "REAPERDBG"

let reaperdbg_flags =
  String.split_on_char ',' (Option.value ~default:"" reaperdbg_env)

let debug = reaperdbg_env <> None

(* This needs to be set before creating any datalog rules. *)
let () =
  if debug && List.mem "prov" reaperdbg_flags
  then Datalog.Schedule.enable_provenance_for_debug ()

module Field = Global_flow_graph.Field

type 'a unboxed_fields =
  | Not_unboxed of 'a
  | Unboxed of 'a unboxed_fields Field.Map.t

let rec pp_unboxed_elt pp_unboxed ppf = function
  | Not_unboxed x -> pp_unboxed ppf x
  | Unboxed fields -> Field.Map.print (pp_unboxed_elt pp_unboxed) ppf fields

let print_unboxed_fields = pp_unboxed_elt

let is_local_field (f : Field.t) =
  match f with
  | Value_slot vs ->
    Compilation_unit.is_current (Value_slot.get_compilation_unit vs)
  | Function_slot fs ->
    Compilation_unit.is_current (Function_slot.get_compilation_unit fs)
  | Block _ | Code_of_closure _ | Apply _ | Code_id_of_call_witness _ | Is_int
  | Get_tag ->
    false

let is_not_local_field f = not (is_local_field f)

(* CR-someday ncourant: track fields that are known to be constant, here and in
   changed_representation, to avoid having them be represented. This is a bit
   complex for two main reasons:

   - At this point in the dependency solver, we do not know the specific value
   of the constant but only that it is one (an alias to all_constants)

   - For symbols, this could break dominator scoping. *)
type unboxed = Variable.t unboxed_fields Field.Map.t

type changed_representation =
  (* CR ncourant: this is currently never produced, because we need to rewrite
     the value_kinds to account for changed representations before enabling
     this *)
  | Block_representation of
      (int * Flambda_primitive.Block_access_kind.t) unboxed_fields Field.Map.t
      * int
  | Closure_representation of
      Value_slot.t unboxed_fields Field.Map.t
      * Function_slot.t Function_slot.Map.t (* old -> new *)
      * Function_slot.t (* OLD current function slot *)

let pp_changed_representation ff = function
  | Block_representation (fields, size) ->
    Format.fprintf ff "(fields %a) (size %d)"
      (Field.Map.print
         (pp_unboxed_elt (fun ff (field, _) -> Format.pp_print_int ff field)))
      fields size
  | Closure_representation (fields, function_slots, fs) ->
    Format.fprintf ff "(fields %a) (function_slots %a) (current %a)"
      (Field.Map.print (pp_unboxed_elt Value_slot.print))
      fields
      (Function_slot.Map.print Function_slot.print)
      function_slots Function_slot.print fs

type result =
  { db : Datalog.database;
    unboxed_fields : unboxed Code_id_or_name.Map.t;
    changed_representation : changed_representation Code_id_or_name.Map.t
  }

let pp_result ppf res = Format.fprintf ppf "%a@." Datalog.print res.db

module Syntax = struct
  include Datalog

  let ( let$ ) xs f = compile xs f

  let ( ==> ) h c = where h (deduce c)
end

module Cols = struct
  let n = Code_id_or_name.datalog_column_id

  let f = Global_flow_graph.Field.Encoded.datalog_column_id

  let cf = Global_flow_graph.CoField.Encoded.datalog_column_id
end

let rel1_r ?provenance name schema =
  let r = Datalog.create_relation ?provenance ~name schema in
  r, fun x -> Datalog.atom r [x]

let rel1 ?provenance name schema = snd (rel1_r ?provenance name schema)

let rel2_r ?provenance name schema =
  let r = Datalog.create_relation ?provenance ~name schema in
  r, fun x y -> Datalog.atom r [x; y]

let rel2 ?provenance name schema = snd (rel2_r ?provenance name schema)

let rel3 ?provenance name schema =
  let r = Datalog.create_relation ?provenance ~name schema in
  fun x y z -> Datalog.atom r [x; y; z]

(**
   [usages] and [sources] are dual. They build the same relation
   from [accessor] and [rev_constructor].
   [any_usage] and [any_source] are the tops.
   [field_usages] and [field_sources]
   [field_usages_top] and [field_sources_top]
   [cofield_usages] and [cofield_sources]
*)

(** [usages x y] y is an alias of x, and there is an actual use for y.

    For performance reasons, we don't want to represent [usages x y] when
    x is top ([any_usage x] is valid). If x is top the any_usage predicate subsumes
    this property.

    We avoid building this relation in that case, but it is possible to have both
    [usages x y] and [any_usage x] depending on the resolution order.

    [usages x x] is used to represent the actual use of x.
*)
let usages_rel = rel2 "usages" Cols.[n; n]

(** [field_usages x f y] y is an use of the field f of x
    and there is an actual use for y.
    Exists only if [accessor y f x].
    (this avoids the quadratic blowup of building the complete alias graph)

    We avoid building this relation if [field_usages_top x f], but it is possible to have both
    [field_usages x f _] and [field_usages_top x f] depending on the resolution order.
*)
let field_usages_rel = rel3 "field_usages" Cols.[n; f; n]

(** [any_usage x] x is used in an uncontrolled way *)
let _any_usage_pred = Global_flow_graph.any_usage_pred

(** [field_usages_top x f] the field f of x is used in an uncontroled way.
    It could be for instance, a value escaping the current compilation unit,
    or passed as argument to an non axiomatized function or primitive.
    Exists only if [accessor y f x] for some y.
    (this avoids propagating large number of fields properties on many variables)
*)
let field_usages_top_rel = rel2 "field_usages_top" Cols.[n; f]

(** [sources x y] y is a source of x, and there is an actual source for y.

    For performance reasons, we don't want to represent [sources x y] when
    x is top ([any_source x] is valid). If x is top the any_source predicate subsumes
    this property.

    We avoid building this relation in that case, but it is possible to have both
    [sources x y] and [any_source x] depending on the resolution order.

    [sources x x] is used to represent the actual source of x.
*)
let sources_rel = rel2 "sources" Cols.[n; n]

(** [any_source x] the special extern value 'any_source' is a source of x
    it represents the top for the sources.
    It can be produced for instance by an argument from an escaping function
    or the result of non axiomatized primitives and external symbols.
    Right now functions coming from other files are considered unknown *)
let _any_source_pred = Global_flow_graph.any_source_pred

(** [field_sources x f y] y is a source of the field f of x,
    and there is an actual source for y.
    Exists only if [constructor x f y].
    (this avoids the quadratic blowup of building the complete alias graph)

    We avoid building this relation if [field_sources_top x f], but it is possible to have both
    [field_sources x f _] and [field_sources_top x f] depending on the resolution order.

*)
let field_sources_rel = rel3 "field_sources" Cols.[n; f; n]

(** [field_sources_top x f] the special extern value is a source for the field f of x *)
let field_sources_top_rel = rel2 "field_sources_top" Cols.[n; f]
(* CR pchambart: is there a reason why this is called top an not any source ? *)

let cofield_sources_rel = rel3 "cofield_sources" Cols.[n; cf; n]

let cofield_usages_rel = rel3 "cofield_usages" Cols.[n; cf; n]

let rev_alias_rel = rel2 ~provenance:false "rev_alias" Cols.[n; n]

let rev_use_rel = rel2 ~provenance:false "rev_use" Cols.[n; n]

let rev_constructor_rel = rel3 ~provenance:false "rev_constructor" Cols.[n; f; n]

let rev_accessor_rel = rel3 ~provenance:false "rev_accessor" Cols.[n; f; n]

let rev_coaccessor_rel = rel3 ~provenance:false "rev_coaccessor" Cols.[n; cf; n]

let rev_coconstructor_rel =
  rel3 ~provenance:false "rev_coconstructor" Cols.[n; cf; n]

(* The program is abstracted as a series of relations concerning the reading and
   writing of fields of values.

   There are 5 different relations:

   - [alias to_ from] corresponds to [let to_ = from]

   - [accessor to_ relation base] corresponds to [let to_ = base.relation]

   - [constructor base relation from] corresponds to constructing a block [let
   base = { relation = from }]

   - [propagate if_used to_ from] means [alias to_ from], but only if [is_used]
   is used

   - [use to_ from] corresponds to [let to_ = f(from)], creating an arbitrary
   result [to_] and consuming [from].

   We perform an analysis that computes the ways each value can be used: either
   entirely, not at all, or, for each of its fields, how that field might be
   used. We also perform a reverse analysis that computes where each value can
   come from: either an arbitrary source (for use and values coming from outside
   the compilation unit), or a given constructor. *)

(* Local fields are value and function slots that originate from the current
   compilation unit. As such, all sources and usages from these fields will
   necessarily correspond to either code in the current compilation unit, or a
   resimplified version of it.

   The consequence of this is that we can consider them not to have [any_usage],
   nor to have [any_source], even if the block containing them has [any_usage]
   or [any_source]. Instead, we need to add an alias from [x] to [y] if [x] if
   stored in a field of [source], [y] is read from the same field of [usage],
   and [source] might flow to [usage]. *)

let reading_field_rel = rel2 "reading_field" Cols.[f; n]

let escaping_field_rel = rel2 "escaping_field" Cols.[f; n]

let filter_field f x =
  let open! Syntax in
  filter (fun [x] -> f (Field.decode x)) [x]

let datalog_schedule =
  let open Global_flow_graph in
  let open! Syntax in
  (* Reverse relations, because datalog does not implement a more efficient
     representation yet. Datalog iterates on the first key of a relation first.
     those reversed relations allows to select a different key. *)
  (*
   rev_alias
   * alias To From
   * => rev_alias From To

   rev_accessor
   * accessor To Rel Base
   * => rev_accessor Base Rel To

   rev_constructor
   * constructor Base Rel From
   * => rev_constructor From Rel Base

   rev_coaccessor
   * coaccessor To Rel Base
   * => rev_coaccessor Base Rel To

   rev_coconstructor
   * coconstructor Base Rel From
   * => rev_coconstructor From Rel Base
   *)
  let rev_alias =
    let$ [to_; from] = ["to_"; "from"] in
    [alias_rel to_ from] ==> rev_alias_rel from to_
  in
  let rev_use =
    let$ [to_; from] = ["to_"; "from"] in
    [use_rel to_ from] ==> rev_use_rel from to_
  in
  let rev_accessor =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [accessor_rel to_ relation base] ==> rev_accessor_rel base relation to_
  in
  let rev_constructor =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [constructor_rel base relation from]
    ==> rev_constructor_rel from relation base
  in
  let rev_coaccessor =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [coaccessor_rel to_ relation base] ==> rev_coaccessor_rel base relation to_
  in
  let rev_coconstructor =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [coconstructor_rel base relation from]
    ==> rev_coconstructor_rel from relation base
  in
  (* propagate

     The [propagate] relation is part of the input of the solver, with the
     intended meaning of this rule. That is an alias if [is_used] is used. *)
  let alias_from_usage_propagate =
    let$ [if_used; to_; from] = ["if_used"; "to_"; "from"] in
    [any_usage_pred if_used; propagate_rel if_used to_ from]
    ==> alias_rel to_ from
  in
  (* usages rules:

     By convention the Base name applies to something that represents a block value
     (something on which an accessor or a constructor applies)

     usage_accessor and usage_coaccessor are the relation initialisation: they define
     what we mean by 'actually using' something. usage_alias propagatess usage to aliases.

     An 'actual use' comes from either a top (any_usage predicate) or through an accessor
     (or coaccessor) on an used variable

     All those rules are constrained not to apply when any_usage is valid. (see [usages]
     definition comment)

   any_usage_from_alias_any_usage
   *  alias To From
   *  /\ any_usage To
   *  => any_usage From

   usage_accessor (1 & 2)
   *  not (any_usage Base)
   *  /\ accessor To Rel Base
   *  /\ (usages To Var \/ any_usage To)
   *  => usages Base Base

   usage_coaccessor (1 & 2)
   *  not (any_usage Base)
   *  /\ coaccessor To Rel Var
   *  /\ (sources To Var \/ any_source To)
   *  => usages Base Base

   usage_alias
   * not (any_usage From)
   * /\ not (any_usage To)
   * /\ usages To Usage
   * /\ alias To From
   * => usages From Usage

   *)
  let any_usage_from_alias_any_usage =
    let$ [to_; from] = ["to_"; "from"] in
    [alias_rel to_ from; any_usage_pred to_] ==> any_usage_pred from
  in
  let usages_accessor_1 =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ not (any_usage_pred base);
      accessor_rel to_ relation base;
      usages_rel to_ _var ]
    ==> usages_rel base base
  in
  let usages_accessor_2 =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ not (any_usage_pred base);
      accessor_rel to_ relation base;
      any_usage_pred to_ ]
    ==> usages_rel base base
  in
  let usages_coaccessor_1 =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ not (any_usage_pred base);
      sources_rel to_ _var;
      coaccessor_rel to_ relation base ]
    ==> usages_rel base base
  in
  let usages_coaccessor_2 =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ not (any_usage_pred base);
      any_source_pred to_;
      coaccessor_rel to_ relation base ]
    ==> usages_rel base base
  in
  let usages_alias =
    let$ [to_; from; usage] = ["to_"; "from"; "usage"] in
    [ not (any_usage_pred from);
      not (any_usage_pred to_);
      usages_rel to_ usage;
      alias_rel to_ from ]
    ==> usages_rel from usage
  in
  (* accessor-usage

   field_usages_from_accessor_field_usages_top
   *  not (any_usage Base)
   *  /\ any_usage To
   *  /\ accessor To Rel Base
   *  => field_usages_top Base Rel

   field_usages_from_accessor_field_usages
   * not (any_usage Base)
   * /\ not (any_usage To)
   * /\ not (field_usages_top Base Rel)
   * /\ accessor To Rel Base
   * /\ usages To Var
   * => field_usages Base Rel To

   *)
  let field_usages_from_accessor_field_usages_top =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ not (any_usage_pred base);
      any_usage_pred to_;
      accessor_rel to_ relation base;
      filter_field is_not_local_field relation ]
    ==> field_usages_top_rel base relation
  in
  let field_usages_from_accessor_field_usages_top_local =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ not (any_usage_pred base);
      any_usage_pred to_;
      accessor_rel to_ relation base;
      filter_field is_local_field relation ]
    ==> field_usages_rel base relation to_
  in
  let field_usages_from_accessor_field_usages =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ not (any_usage_pred base);
      not (any_usage_pred to_);
      not (field_usages_top_rel base relation);
      accessor_rel to_ relation base;
      usages_rel to_ _var ]
    ==> field_usages_rel base relation to_
  in
  (* coaccessor-usages

   cofield_usages_from_coaccessor (1 & 2)
   * not (any_usage Base)
   * /\ coaccessor To Rel Base
   * /\ (sources To Var \/ any_source To)
   * => cofield_usages Base Rel To

   *)
  let cofield_usages_from_coaccessor1 =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ not (any_usage_pred base);
      coaccessor_rel to_ relation base;
      sources_rel to_ _var ]
    ==> cofield_usages_rel base relation to_
  in
  let cofield_usages_from_coaccessor2 =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ not (any_usage_pred base);
      any_source_pred to_;
      coaccessor_rel to_ relation base ]
    ==> cofield_usages_rel base relation to_
  in
  (* constructor-usages

   alias_from_accessed_constructor_1
   * not (any_usage From)
   * /\ not (field_usages_top BaseUse Rel)
   * /\ not (any_usage Base)
   * /\ constructor Base Rel From
   * /\ usages Base BaseUse
   * /\ field_usages BaseUse Rel To
   * => alias To From

   *)
  let alias_from_accessed_constructor_1 =
    let$ [base; base_use; relation; from; to_] =
      ["base"; "base_use"; "relation"; "from"; "to_"]
    in
    [ not (any_usage_pred from);
      not (field_usages_top_rel base_use relation);
      not (any_usage_pred base);
      constructor_rel base relation from;
      usages_rel base base_use;
      field_usages_rel base_use relation to_ ]
    ==> alias_rel to_ from
  in
  let alias_from_accessed_constructor_1_local =
    let$ [base; base_use; relation; from; to_] =
      ["base"; "base_use"; "relation"; "from"; "to_"]
    in
    [ filter_field is_local_field relation;
      constructor_rel base relation from;
      usages_rel base base_use;
      field_usages_rel base_use relation to_ ]
    ==> alias_rel to_ from
  in
  (*
   any_usage_from_accessed_constructor
   * constructor Base Rel From
   * /\ not (any_usage Base)
   * /\ usages Base BaseUse
   * /\ field_usages_top BaseUse Rel
   * => any_usage From
   *)
  let any_usage_from_accessed_constructor =
    let$ [base; base_use; relation; from] =
      ["base"; "base_use"; "relation"; "from"]
    in
    [ constructor_rel base relation from;
      not (any_usage_pred base);
      usages_rel base base_use;
      field_usages_top_rel base_use relation ]
    ==> any_usage_pred from
  in
  (*
   any_usage_from_constructor_any_usage
   * any_usage Base
   * /\ constructor Base Rel From
   * => any_usage From

   any_usage_from_coaccessor_any_source
   * any_source Base
   * /\ rev_coaccessor Base Rel To
   * => any_usage To
   *)
  let any_usage_from_constructor_any_usage =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [ any_usage_pred base;
      constructor_rel base relation from;
      filter_field is_not_local_field relation ]
    ==> any_usage_pred from
  in
  let escaping_local_field =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [ any_usage_pred base;
      constructor_rel base relation from;
      filter_field is_local_field relation ]
    ==> and_
          [escaping_field_rel relation from; field_usages_rel base relation from]
  in
  let any_usage_from_coaccessor_any_source =
    let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
    [any_source_pred base; rev_coaccessor_rel base relation to_]
    ==> any_usage_pred to_
  in
  (* sources: see explanation on usage

   any_source_from_alias_any_source
   * rev_alias From To
   * /\ any_source From
   * => any_source To

   sources_constructor (1 & 2)
   *  not (any_source Base)
   *  /\ rev_constructor From Rel Base
   *  /\ (sources From Var \/ any_source From)
   *  => sources Base Base

   sources_coconstructor (1 & 2)
   *  not (any_source Base)
   *  /\ rev_coconstructor From Rel Var
   *  /\ (sources From Var \/ any_source From)
   *  => sources Base Base

   usage_alias
   * not (any_source From)
   * /\ not (any_source From)
   * /\ sources From Source
   * /\ rev_alias From To
   * => sources To Source

   *)
  let any_source_from_alias_any_source =
    let$ [from; to_] = ["from"; "to_"] in
    [rev_alias_rel from to_; any_source_pred from] ==> any_source_pred to_
  in
  let sources_constructor_1 =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ not (any_source_pred base);
      sources_rel from _var;
      rev_constructor_rel from relation base ]
    ==> sources_rel base base
  in
  let sources_constructor_2 =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ not (any_source_pred base);
      any_source_pred from;
      rev_constructor_rel from relation base ]
    ==> sources_rel base base
  in
  let sources_coconstructor_1 =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ not (any_source_pred base);
      usages_rel from _var;
      rev_coconstructor_rel from relation base ]
    ==> sources_rel base base
  in
  let sources_coconstructor_2 =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ not (any_source_pred base);
      any_usage_pred from;
      rev_coconstructor_rel from relation base ]
    ==> sources_rel base base
  in
  let sources_alias =
    let$ [from; to_; source] = ["from"; "to_"; "source"] in
    [ not (any_source_pred from);
      not (any_source_pred to_);
      sources_rel from source;
      rev_alias_rel from to_ ]
    ==> sources_rel to_ source
  in
  (* constructor-sources
   field_sources_from_constructor_field_sources_top
   * not (any_source Base)
   * /\ any_source From
   * /\ rev_constructor From Rel Base
   * => field_sources_top Base Rel

   field_sources_from_constructor_field_sources
   * not (any_source Base)
   * /\ not (any_source From)
   * /\ not (field_sources_top Base Rel)
   * /\ rev_constructor From Rel Base
   * /\ sources From Var
   * => field_sources Base Rel From
   *)
  let field_sources_from_constructor_field_sources_top =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ not (any_source_pred base);
      any_source_pred from;
      rev_constructor_rel from relation base;
      filter_field is_not_local_field relation ]
    ==> field_sources_top_rel base relation
  in
  let field_sources_from_constructor_field_sources_top_local =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ not (any_source_pred base);
      any_source_pred from;
      rev_constructor_rel from relation base;
      filter_field is_local_field relation ]
    ==> field_sources_rel base relation from
  in
  let field_sources_from_constructor_field_sources =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ not (any_source_pred base);
      not (any_source_pred from);
      not (field_sources_top_rel base relation);
      rev_constructor_rel from relation base;
      sources_rel from _var ]
    ==> field_sources_rel base relation from
  in
  (* coaccessor-sources

   cofield_sources_from_coconstructor (1 & 2)
   * not (any_source Base)
   * /\ rev_coconstructor From Rel Base
   * /\ (usages From Var \/ any_usage From)
   * => cofield_sources Base Rel From

   *)
  let cofield_sources_from_coconstrucor1 =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ not (any_source_pred base);
      rev_coconstructor_rel from relation base;
      usages_rel from _var ]
    ==> cofield_sources_rel base relation from
  in
  let cofield_sources_from_coconstrucor2 =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ not (any_source_pred base);
      any_usage_pred from;
      rev_coconstructor_rel from relation base ]
    ==> cofield_sources_rel base relation from
  in
  (* coconstructor-uses XXX

   alias_from_coaccessed_coconstructor
   * not (any_usage Base)
   * /\ coconstructor Base Rel From
   * /\ usages Base BaseUse
   * /\ cofield_usages BaseUse Rel To
   * => alias From To

   *)
  let alias_from_coaccessed_coconstructor =
    let$ [base; base_use; relation; from; to_] =
      ["base"; "base_use"; "relation"; "from"; "to_"]
    in
    [ not (any_usage_pred base);
      coconstructor_rel base relation from;
      usages_rel base base_use;
      cofield_usages_rel base_use relation to_ ]
    ==> alias_rel from to_
  in
  (*
   any_source_from_coconstructor_any_usage
   * any_usage Base
   * /\ coconstructor Base Rel From
   * => any_source From
   *)
  let any_source_from_coconstructor_any_usage =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [any_usage_pred base; coconstructor_rel base relation from]
    ==> any_source_pred from
  in
  (* accessor-sources
   alias_from_accessed_constructor_2
   * not (any_source To)
   * /\ not (field_sources_top BaseSource Rel)
   * /\ not (any_source Base)
   * /\ rev_accessor Base Rel To
   * /\ sources Base BaseSource
   * /\ field_sources BaseSource Rel From
   * => alias To From

   any_source_from_accessed_constructor
   * rev_accessor Base Rel To
   * /\ not (any_source Base)
   * /\ sources Base BaseSource
   * /\ field_sources_top BaseSource Rel
   * => any_source To
   *)
  let alias_from_accessed_constructor_2 =
    let$ [base; base_source; relation; to_; from] =
      ["base"; "base_source"; "relation"; "to_"; "from"]
    in
    [ not (any_source_pred to_);
      not (field_sources_top_rel base_source relation);
      not (any_source_pred base);
      rev_accessor_rel base relation to_;
      sources_rel base base_source;
      field_sources_rel base_source relation from ]
    ==> alias_rel to_ from
  in
  let alias_from_accessed_constructor_2_local =
    let$ [base; base_source; relation; to_; from] =
      ["base"; "base_source"; "relation"; "to_"; "from"]
    in
    [ filter_field is_local_field relation;
      rev_accessor_rel base relation to_;
      sources_rel base base_source;
      field_sources_rel base_source relation from ]
    ==> alias_rel to_ from
  in
  let any_source_from_accessed_constructor =
    let$ [base; base_source; relation; to_] =
      ["base"; "base_source"; "relation"; "to_"]
    in
    [ rev_accessor_rel base relation to_;
      not (any_source_pred base);
      sources_rel base base_source;
      field_sources_top_rel base_source relation ]
    ==> any_source_pred to_
  in
  (*
   any_source_from_accessor_any_source
   * any_source Base
   * /\ rev_accessor Base Rel To
   * => any_source To
   *)
  let any_source_from_accessor_any_source =
    let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
    [ any_source_pred base;
      rev_accessor_rel base relation to_;
      filter_field is_not_local_field relation ]
    ==> any_source_pred to_
  in
  let reading_local_field =
    let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
    [ any_source_pred base;
      rev_accessor_rel base relation to_;
      filter_field is_local_field relation ]
    ==> and_
          [reading_field_rel relation to_; field_sources_rel base relation to_]
  in
  let reading_escaping =
    let$ [relation; from; to_] = ["relation"; "from"; "to_"] in
    [escaping_field_rel relation from; reading_field_rel relation to_]
    ==> alias_rel to_ from
  in
  (* ... *)
  (*
   alias_from_coaccessed_coconstructor_2
   * not (any_source Base)
   * /\ rev_coaccessor Base Rel To
   * /\ sources Base BaseSource
   * /\ cofield_sources BaseSource Rel From
   * => alias From To
   *)
  let alias_from_coaccessed_coconstructor_2 =
    let$ [base; base_source; relation; to_; from] =
      ["base"; "base_source"; "relation"; "to_"; "from"]
    in
    [ not (any_source_pred base);
      rev_coaccessor_rel base relation to_;
      sources_rel base base_source;
      cofield_sources_rel base_source relation from ]
    ==> alias_rel from to_
  in
  (* use *)
  (*
   any_usage_from_use (1 & 2)
   * use To From
   * /\ (usages To Var \/ any_usage To)
   * => any_usage From
   *)
  (*
   any_source_use (1 & 2)
   * rev_use From To
   * /\ (sources From Var \/ any_source From)
   * => any_source To
   *)
  let any_usage_from_use_1 =
    let$ [to_; from; _var] = ["to_"; "from"; "_var"] in
    [usages_rel to_ _var; use_rel to_ from] ==> any_usage_pred from
  in
  let any_usage_from_use_2 =
    let$ [to_; from] = ["to_"; "from"] in
    [any_usage_pred to_; use_rel to_ from] ==> any_usage_pred from
  in
  let any_source_use_1 =
    let$ [from; to_; _var] = ["from"; "to_"; "_var"] in
    [sources_rel from _var; rev_use_rel from to_] ==> any_source_pred to_
  in
  let any_source_use_2 =
    let$ [from; to_] = ["from"; "to_"] in
    [any_source_pred from; rev_use_rel from to_] ==> any_source_pred to_
  in
  Datalog.Schedule.(
    fixpoint
      [ saturate
          [ rev_accessor;
            rev_constructor;
            rev_coaccessor;
            rev_coconstructor;
            rev_use;
            any_source_use_1;
            any_source_use_2;
            alias_from_usage_propagate;
            any_usage_from_alias_any_usage;
            any_source_from_alias_any_source;
            any_usage_from_constructor_any_usage;
            any_usage_from_coaccessor_any_source;
            any_usage_from_use_1;
            any_usage_from_use_2;
            any_usage_from_accessed_constructor;
            any_source_from_accessed_constructor;
            any_source_from_accessor_any_source;
            any_source_from_coconstructor_any_usage;
            escaping_local_field;
            reading_local_field;
            reading_escaping;
            rev_alias ];
        saturate
          [ alias_from_accessed_constructor_1;
            alias_from_accessed_constructor_1_local;
            alias_from_accessed_constructor_2;
            alias_from_accessed_constructor_2_local;
            alias_from_coaccessed_coconstructor;
            alias_from_coaccessed_coconstructor_2;
            field_usages_from_accessor_field_usages;
            field_usages_from_accessor_field_usages_top;
            field_usages_from_accessor_field_usages_top_local;
            cofield_usages_from_coaccessor1;
            cofield_usages_from_coaccessor2;
            field_sources_from_constructor_field_sources;
            field_sources_from_constructor_field_sources_top;
            field_sources_from_constructor_field_sources_top_local;
            cofield_sources_from_coconstrucor1;
            cofield_sources_from_coconstrucor2;
            usages_accessor_1;
            usages_accessor_2;
            usages_coaccessor_1;
            usages_coaccessor_2;
            usages_alias;
            sources_constructor_1;
            sources_constructor_2;
            sources_coconstructor_1;
            sources_coconstructor_2;
            sources_alias;
            rev_alias ] ])

let exists_with_parameters cursor params db =
  Datalog.Cursor.fold_with_parameters cursor params db ~init:false
    ~f:(fun [] _ -> true)

let mk_exists_query params existentials f =
  Datalog.(
    compile_with_parameters params [] (fun params [] ->
        foreach existentials (fun existentials ->
            where (f params existentials) (yield []))))

let is_function_slot : Field.t -> _ = function[@ocaml.warning "-4"]
  | Function_slot _ -> true
  | _ -> false

type usages = Usages of unit Code_id_or_name.Map.t [@@unboxed]

(** Computes all usages of a set of variables (input).
    Sets are represented as unit maps for convenience with datalog.
    Usages is represented as a set of variables: those are the variables
    where the input variables flow with live accessor.

    [follow_known_arity_calls] specifies that if the set of variables
    corresponds to a closure that is called by an known arity call, we
    should look at the [my_closure] value of the corresponding code_id as well.
    This is only necessary if the set of variables can correspond to a closure
    *and* the set of variables contains variables that are not the allocation
    point of the set of closures.

    The reason for this is that for a given closure that is called, the
    [usages] do not usually include the uses of the closure inside the code of
    the closure itself. However, when we allocate a set of closures, we include
    an alias between the allocated closures and their [my_closure] variable
    inside the corresponding code. As such, the usages at an allocation point
    are always representative of all the uses, and as such, do not require to
    follow the calls.

    Function slots are considered as aliases for this analysis. *)
let get_all_usages :
    follow_known_arity_calls:bool ->
    Datalog.database ->
    unit Code_id_or_name.Map.t ->
    usages =
  (* CR-someday ncourant: once the datalog API supports something cleaner, use
     it. *)
  let out_tbl, out = rel1_r "out" Cols.[n] in
  let in_tbl, in_ = rel1_r "in_" Cols.[n] in
  let open! Syntax in
  let open! Global_flow_graph in
  let base, for_closures, function_slots =
    ( (let$ [x; y] = ["x"; "y"] in
       [in_ x; usages_rel x y] ==> out y),
      (let$ [x; apply_witness; call_witness; code_id; my_closure_of_code_id; y]
           =
         [ "x";
           "apply_witness";
           "call_witness";
           "code_id";
           "my_closure_of_code_id";
           "y" ]
       in
       [ out x;
         rev_accessor_rel x
           (Term.constant
              (Field.encode (Code_of_closure Known_arity_code_pointer)))
           apply_witness;
         sources_rel apply_witness call_witness;
         constructor_rel call_witness
           (Term.constant (Field.encode Code_id_of_call_witness))
           code_id;
         code_id_my_closure_rel code_id my_closure_of_code_id;
         usages_rel my_closure_of_code_id y ]
       ==> out y),
      let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
      [ out x;
        field_usages_rel x field y;
        filter_field is_function_slot field;
        usages_rel y z ]
      ==> out z )
  in
  fun ~follow_known_arity_calls db s ->
    let db = Datalog.set_table in_tbl s db in
    let rs =
      if follow_known_arity_calls
      then [base; for_closures; function_slots]
      else [base; function_slots]
    in
    let db = Datalog.Schedule.run (Datalog.Schedule.saturate rs) db in
    Usages (Datalog.get_table out_tbl db)

let fieldc_map_to_field_map m =
  Global_flow_graph.Field.Encoded.Map.fold
    (fun k r acc -> Field.Map.add (Field.decode k) r acc)
    m Field.Map.empty

type field_usage =
  | Used_as_top
  | Used_as_vars of unit Code_id_or_name.Map.t

(** For an usage set (argument s), compute the way its fields are used.
    As function slots are transparent for [get_usages], functions slot
    usages are ignored here.
*)
let get_fields : Datalog.database -> usages -> field_usage Field.Map.t =
  (* CR-someday ncourant: likewise here; I find this function particulartly
     ugly. *)
  let out_tbl1, out1 = rel1_r "out1" Cols.[f] in
  let out_tbl2, out2 = rel2_r "out2" Cols.[f; n] in
  let in_tbl, in_ = rel1_r "in_" Cols.[n] in
  let open! Syntax in
  let open! Global_flow_graph in
  let rs =
    [ (let$ [x; field] = ["x"; "field"] in
       [ in_ x;
         field_usages_top_rel x field;
         filter_field (fun x -> Stdlib.not (is_function_slot x)) field ]
       ==> out1 field);
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ in_ x;
         field_usages_rel x field y;
         not (out1 field);
         filter_field (fun x -> Stdlib.not (is_function_slot x)) field ]
       ==> out2 field y) ]
  in
  fun db (Usages s) ->
    let db = Datalog.set_table in_tbl s db in
    let db =
      List.fold_left
        (fun db r -> Datalog.Schedule.(run (saturate [r])) db)
        db rs
    in
    fieldc_map_to_field_map
      (Field.Encoded.Map.merge
         (fun k x y ->
           match x, y with
           | None, None -> assert false
           | Some _, Some _ ->
             Misc.fatal_errorf "Got two results for field %a" Field.print
               (Field.decode k)
           | Some (), None -> Some Used_as_top
           | None, Some m -> Some (Used_as_vars m))
         (Datalog.get_table out_tbl1 db)
         (Datalog.get_table out_tbl2 db))

type set_of_closures_def =
  | Not_a_set_of_closures
  | Set_of_closures of (Function_slot.t * Code_id_or_name.t) list

let get_set_of_closures_def :
    Datalog.database -> Code_id_or_name.t -> set_of_closures_def =
  let q =
    Datalog.(
      compile_with_parameters ["x"] [] (fun [x] [] ->
          foreach ["field"; "y"] (fun [field; y] ->
              where
                [ Global_flow_graph.constructor_rel x field y;
                  filter_field is_function_slot field ]
                (yield [field; y]))))
  in
  fun db v ->
    let l =
      Datalog.Cursor.fold_with_parameters q [v] db ~init:[] ~f:(fun [f; y] l ->
          ( (match[@ocaml.warning "-4"] Field.decode f with
            | Function_slot fs -> fs
            | _ -> assert false),
            y )
          :: l)
    in
    match l with [] -> Not_a_set_of_closures | _ :: _ -> Set_of_closures l

let any_usage_pred_query =
  let open! Global_flow_graph in
  mk_exists_query ["X"] [] (fun [x] [] -> [any_usage_pred x])

(* CR pchambart: should rename: mutiple potential top is_used_as_top (should be
   obviously different from has use) *)
let is_top db x = exists_with_parameters any_usage_pred_query [x] db

(* CR pchambart: field_used should rename to mean that this is the specific
   field of a given variable. *)
let has_use, _field_used =
  let open! Global_flow_graph in
  let usages_query =
    mk_exists_query ["X"] ["Y"] (fun [x] [y] -> [usages_rel x y])
  in
  let used_field_top_query =
    mk_exists_query ["X"; "F"] ["U"] (fun [x; f] [u] ->
        [usages_rel x u; field_usages_top_rel u f])
  in
  let used_field_query =
    mk_exists_query ["X"; "F"] ["U"; "V"] (fun [x; f] [u; v] ->
        [usages_rel x u; field_usages_rel u f v])
  in
  ( (fun db x ->
      exists_with_parameters any_usage_pred_query [x] db
      || exists_with_parameters usages_query [x] db),
    fun db x field ->
      let field = Field.encode field in
      exists_with_parameters any_usage_pred_query [x] db
      || exists_with_parameters used_field_top_query [x; field] db
      || exists_with_parameters used_field_query [x; field] db )

let field_of_constructor_is_used =
  rel2 "field_of_constructor_is_used" Cols.[n; f]

let field_used =
  let field_of_constructor_is_used_query =
    mk_exists_query ["X"; "F"] [] (fun [x; f] [] ->
        [field_of_constructor_is_used x f])
  in
  fun db x field ->
    exists_with_parameters field_of_constructor_is_used_query
      [x; Field.encode field]
      db

let any_source_query =
  let open! Global_flow_graph in
  mk_exists_query ["X"] [] (fun [x] [] -> [any_source_pred x])

let has_source =
  let open! Global_flow_graph in
  let has_source_query =
    mk_exists_query ["X"] ["Y"] (fun [x] [y] -> [sources_rel x y])
  in
  fun db x ->
    exists_with_parameters any_source_query [x] db
    || exists_with_parameters has_source_query [x] db

let cofield_has_use =
  let open! Global_flow_graph in
  let cofield_query =
    mk_exists_query ["X"; "F"] ["S"; "T"] (fun [x; f] [s; t] ->
        [sources_rel x s; cofield_sources_rel s f t])
  in
  fun db x cofield ->
    let cofield = CoField.encode cofield in
    exists_with_parameters any_source_query [x] db
    || exists_with_parameters cofield_query [x; cofield] db

(* CR pchambart: Should rename to remove not local in the name (the notion does
   not exists right now)*)
let not_local_field_has_source =
  let open! Global_flow_graph in
  let any_source_query =
    mk_exists_query ["X"] [] (fun [x] [] -> [any_source_pred x])
  in
  let field_any_source_query =
    mk_exists_query ["X"; "F"] ["S"] (fun [x; f] [s] ->
        [sources_rel x s; field_sources_top_rel s f])
  in
  let field_source_query =
    mk_exists_query ["X"; "F"] ["S"; "V"] (fun [x; f] [s; v] ->
        [sources_rel x s; field_sources_rel s f v])
  in
  fun db x field ->
    let field = Field.encode field in
    exists_with_parameters any_source_query [x] db
    || exists_with_parameters field_any_source_query [x; field] db
    || exists_with_parameters field_source_query [x; field] db

let cannot_change_witness_calling_convention =
  rel1 "cannot_change_witness_calling_convention" Cols.[n]

let cannot_change_calling_convention =
  rel1 "cannot_change_calling_convention" Cols.[n]

let cannot_change_representation0 = rel1 "cannot_change_representation0" Cols.[n]

let cannot_change_representation1 = rel1 "cannot_change_representation1" Cols.[n]

let cannot_change_representation = rel1 "cannot_change_representation" Cols.[n]

let cannot_unbox0 = rel1 "cannot_unbox0" Cols.[n]

let cannot_unbox = rel1 "cannot_unbox" Cols.[n]

let to_unbox = rel1 "to_unbox" Cols.[n]

let to_change_representation = rel1 "to_change_representation" Cols.[n]

let datalog_rules =
  let open! Syntax in
  let open! Global_flow_graph in
  let fld f = Term.constant (Field.encode f) in
  let real_field (i : Field.t) =
    match[@ocaml.warning "-4"] i with
    | Code_of_closure _ | Apply _ | Code_id_of_call_witness -> false
    | _ -> true
  in
  let _is_code_field : Field.t -> _ = function[@ocaml.warning "-4"]
    | Code_of_closure _ -> true
    | _ -> false
  in
  let _is_apply_field : Field.t -> _ = function[@ocaml.warning "-4"]
    | Apply _ -> true
    | _ -> false
  in
  [ (let$ [base; relation; from] = ["base"; "relation"; "from"] in
     [ constructor_rel base relation from;
       any_usage_pred base;
       filter_field is_not_local_field relation ]
     ==> field_of_constructor_is_used base relation);
    (let$ [base; relation; from; usage] =
       ["base"; "relation"; "from"; "usage"]
     in
     [ constructor_rel base relation from;
       usages_rel base usage;
       field_usages_top_rel usage relation ]
     ==> field_of_constructor_is_used base relation);
    (let$ [base; relation; from; usage; _v] =
       ["base"; "relation"; "from"; "usage"; "_v"]
     in
     [ constructor_rel base relation from;
       usages_rel base usage;
       field_usages_rel usage relation _v ]
     ==> field_of_constructor_is_used base relation);
    (let$ [base; relation; from; x] = ["base"; "relation"; "from"; "x"] in
     [ constructor_rel base relation from;
       any_usage_pred base;
       reading_field_rel relation x;
       any_usage_pred x ]
     ==> field_of_constructor_is_used base relation);
    (let$ [base; relation; from; x; y] =
       ["base"; "relation"; "from"; "x"; "y"]
     in
     [ constructor_rel base relation from;
       any_usage_pred base;
       reading_field_rel relation x;
       usages_rel x y ]
     ==> field_of_constructor_is_used base relation);
    (let$ [usage; base; relation; from; _v] =
       ["usage"; "base"; "relation"; "from"; "_v"]
     in
     [ constructor_rel base relation from;
       sources_rel usage base;
       filter_field is_local_field relation;
       any_usage_pred base;
       field_usages_rel usage relation _v ]
     ==> field_of_constructor_is_used base relation);
    (let$ [usage; base; relation; from] =
       ["usage"; "base"; "relation"; "from"]
     in
     [ constructor_rel base relation from;
       sources_rel usage base;
       filter_field is_local_field relation;
       any_usage_pred base;
       field_usages_top_rel usage relation ]
     ==> field_of_constructor_is_used base relation);
    (* CR ncourant: this marks any [Apply] field as
       [field_of_constructor_is_used], as long as the function is called.
       Shouldn't that be gated behind a [cannot_change_calling_convetion]? *)
    (* (let$ [base; relation; from; coderel; call_witness] = ["base";
       "relation"; "from"; "coderel"; "call_witness"] in [ constructor_rel base
       relation from; filter_field is_apply_field relation; constructor_rel base
       coderel call_witness; any_usage_pred indirect_call_witness; filter_field
       is_code_field coderel ] ==> field_of_constructor_is_used base
       relation); *)
    (* CR ncourant: should this be reenabled? I think this is no longer
       necessary because we remove unused arguments of continuations, including
       return continuations. *)
    (* If any usage is possible, do not change the representation. Note that
       this rule will change in the future, when local value slots are properly
       tracked: a closure will only local value slots that has any_use will
       still be able to have its representation changed. *)
    (let$ [x] = ["x"] in
     [any_usage_pred x] ==> cannot_change_representation0 x);
    (* If there exists an alias which has another source, and which uses any
       real field of our allocation, we cannot change the representation. This
       currently requires 4 rules due to the absence of disjunction in the
       datalog engine. *)
    (let$ [allocation_id; alias; alias_source; field; _v] =
       ["allocation_id"; "alias"; "alias_source"; "field"; "_v"]
     in
     [ usages_rel allocation_id alias;
       sources_rel alias alias_source;
       distinct Cols.n alias_source allocation_id;
       filter_field real_field field;
       field_usages_rel alias field _v ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id; alias; alias_source; field] =
       ["allocation_id"; "alias"; "alias_source"; "field"]
     in
     [ usages_rel allocation_id alias;
       sources_rel alias alias_source;
       distinct Cols.n alias_source allocation_id;
       filter_field real_field field;
       field_usages_top_rel alias field ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id; alias; field; _v] =
       ["allocation_id"; "alias"; "field"; "_v"]
     in
     [ usages_rel allocation_id alias;
       any_source_pred alias;
       filter_field real_field field;
       field_usages_rel alias field _v ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id; alias; field] = ["allocation_id"; "alias"; "field"] in
     [ usages_rel allocation_id alias;
       any_source_pred alias;
       filter_field real_field field;
       field_usages_top_rel alias field ]
     ==> cannot_change_representation0 allocation_id);
    (* If the allocation has a source distinct from itself, its representation
       cannot be changed (in fact, in that case, it shouldn't even be an
       allocation). *)
    (let$ [allocation_id; source] = ["allocation_id"; "source"] in
     [sources_rel allocation_id source; distinct Cols.n source allocation_id]
     ==> cannot_change_representation0 allocation_id);
    (* Used but not its own source: either from any source, or it has no source
       at all and it is dead code. In either case, do not unbox or change the
       representation. *)
    (let$ [allocation_id; usage] = ["allocation_id"; "usage"] in
     [ usages_rel allocation_id usage;
       not (sources_rel allocation_id allocation_id) ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id] = ["allocation_id"] in
     [any_source_pred allocation_id]
     ==> cannot_change_representation0 allocation_id);
    (let$ [call_witness; code_id] = ["call_witness"; "code_id"] in
     [constructor_rel call_witness (fld Code_id_of_call_witness) code_id]
     ==> cannot_change_representation0 call_witness);
    (* Note this rule is here to still allow changing the calling convention of
       symbols /!\ when adding back the local value slots, there should be a few
       more rules here *)
    (* TODO this is wrong: some closures can have their representation changed
       but not their calling convention *)
    (let$ [x] = ["x"] in
     [any_usage_pred x] ==> cannot_change_witness_calling_convention x);
    (let$ [allocation_id; alias; alias_source; _v] =
       ["allocation_id"; "alias"; "alias_source"; "_v"]
     in
     [ usages_rel allocation_id alias;
       sources_rel alias alias_source;
       distinct Cols.n alias_source allocation_id;
       field_usages_rel alias (fld Code_id_of_call_witness) _v ]
     ==> cannot_change_witness_calling_convention allocation_id);
    (let$ [allocation_id; alias; alias_source] =
       ["allocation_id"; "alias"; "alias_source"]
     in
     [ usages_rel allocation_id alias;
       sources_rel alias alias_source;
       distinct Cols.n alias_source allocation_id;
       field_usages_top_rel alias (fld Code_id_of_call_witness) ]
     ==> cannot_change_witness_calling_convention allocation_id);
    (let$ [allocation_id; alias; _v] = ["allocation_id"; "alias"; "_v"] in
     [ usages_rel allocation_id alias;
       any_source_pred alias;
       field_usages_rel alias (fld Code_id_of_call_witness) _v ]
     ==> cannot_change_witness_calling_convention allocation_id);
    (let$ [allocation_id; alias] = ["allocation_id"; "alias"] in
     [ usages_rel allocation_id alias;
       any_source_pred alias;
       field_usages_top_rel alias (fld Code_id_of_call_witness) ]
     ==> cannot_change_witness_calling_convention allocation_id);
    (let$ [allocation_id; source] = ["allocation_id"; "source"] in
     [sources_rel allocation_id source; distinct Cols.n source allocation_id]
     ==> cannot_change_witness_calling_convention allocation_id);
    (* Used but not its own source: either from any source, or it has no source
       at all and it is dead code. In either case, do not unbox *)
    (let$ [allocation_id; usage] = ["allocation_id"; "usage"] in
     [ usages_rel allocation_id usage;
       not (sources_rel allocation_id allocation_id) ]
     ==> cannot_change_witness_calling_convention allocation_id);
    (let$ [allocation_id] = ["allocation_id"] in
     [any_source_pred allocation_id]
     ==> cannot_change_witness_calling_convention allocation_id);
    (* If the calling convention of a witness cannot be changed, the calling
       convention of its code_id cannot be either. From now on,
       [cannot_change_witness_calling_convention] should no longer be used. *)
    (let$ [call_witness; code_id; _v] = ["call_witness"; "code_id"; "_v"] in
     [ constructor_rel call_witness (fld Code_id_of_call_witness) code_id;
       usages_rel call_witness _v;
       cannot_change_witness_calling_convention call_witness ]
     ==> cannot_change_calling_convention code_id);
    (let$ [call_witness; code_id] = ["call_witness"; "code_id"] in
     [ constructor_rel call_witness (fld Code_id_of_call_witness) code_id;
       any_usage_pred call_witness;
       cannot_change_witness_calling_convention call_witness ]
     ==> cannot_change_calling_convention code_id);
    (* CR ncourant: we're preventing changing the calling convention of
       functions called with Indirect_unknown_arity. We could still allow
       changing the calling convention, but this would require wrappers for
       over- and partial applications, as well as untupling. As these wrappers
       are complex to write correctly, this is not done yet. *)
    (let$ [call_witness; codeid; set_of_closures] =
       ["call_witness"; "codeid"; "set_of_closures"]
     in
     [ rev_constructor_rel call_witness
         (fld (Code_of_closure Unknown_arity_code_pointer))
         set_of_closures;
       any_usage_pred call_witness;
       constructor_rel call_witness (fld Code_id_of_call_witness) codeid ]
     ==> cannot_change_calling_convention codeid);
    (let$ [call_witness; codeid; set_of_closures; _v] =
       ["call_witness"; "codeid"; "set_of_closures"; "_v"]
     in
     [ rev_constructor_rel call_witness
         (fld (Code_of_closure Unknown_arity_code_pointer))
         set_of_closures;
       usages_rel call_witness _v;
       constructor_rel call_witness (fld Code_id_of_call_witness) codeid ]
     ==> cannot_change_calling_convention codeid);
    (* CR-someday ncourant: we completely prevent changing the representation of
       symbols. While allowing them to be unboxed is difficult, due to symbols
       being always values, we could at least change their representation. This
       would require rewriting in the types, which is not done yet. *)
    (let$ [x; _source] = ["x"; "_source"] in
     [ sources_rel x _source;
       filter
         (fun [x] ->
           Code_id_or_name.pattern_match x
             ~symbol:(fun _ -> true)
             ~var:(fun _ -> false)
             ~code_id:(fun _ -> false))
         [x] ]
     ==> cannot_change_representation0 x);
    (* If the representation of any closure in a set of closures cannot be
       changed, the representation of all the closures in the set cannot be
       changed. *)
    (let$ [x] = ["x"] in
     [cannot_change_representation0 x] ==> cannot_change_representation1 x);
    (let$ [x; field; y] = ["x"; "field"; "y"] in
     [ constructor_rel x field y;
       filter_field is_function_slot field;
       cannot_change_representation0 x ]
     ==> cannot_change_representation1 y);
    (let$ [x] = ["x"] in
     [cannot_change_representation1 x] ==> cannot_change_representation x);
    (* Due to value_kinds rewriting not taking representation changes into
       account for now, blocks cannot have their representation changed, so we
       prevent it here. *)
    (let$ [x; field; y] = ["x"; "field"; "y"] in
     [ constructor_rel x field y;
       filter_field
         (fun (f : Field.t) ->
           match f with
           | Block _ | Is_int | Get_tag -> true
           | Value_slot _ | Function_slot _ | Code_of_closure _ | Apply _
           | Code_id_of_call_witness ->
             false)
         field ]
     ==> cannot_change_representation x);
    (* The use of [cannot_change_representation1] is here to still allow
       unboxing of blocks, even if we cannot change their representation due to
       the value_kind limitation. *)
    (let$ [x] = ["x"] in
     [cannot_change_representation1 x] ==> cannot_unbox0 x);
    (* This is repeated from the earlier occurrence in
       [cannot_change_representation0]. It is here because in the future, when
       we want to allow the changing of the representation of local value slots,
       it will remain necessary. *)
    (let$ [x] = ["x"] in
     [any_usage_pred x] ==> cannot_unbox0 x);
    (* (let$ [x; field] = ["x"; "field"] in [ field_of_constructor_is_used x
       field; filter_field field_cannot_be_destructured field ] ==>
       cannot_unbox0 x); *)
    (* Unboxing a closure requires changing its calling convention, as we must
       pass the value slots as extra arguments. Thus, we prevent unboxing of
       closures if their calling convention cannot be changed. *)
    (let$ [x; call_witness; codeid] = ["x"; "call_witness"; "codeid"] in
     [ constructor_rel x
         (Term.constant
            (Field.encode (Code_of_closure Known_arity_code_pointer)))
         call_witness;
       constructor_rel call_witness
         (Term.constant (Field.encode Code_id_of_call_witness))
         codeid;
       cannot_change_calling_convention codeid ]
     ==> cannot_unbox0 x);
    (* An allocation that is one of the results of a function can only be
       unboxed if the function's calling conventation can be changed. *)
    (let$ [alias; allocation_id; relation; call_witness; codeid] =
       ["alias"; "allocation_id"; "relation"; "call_witness"; "codeid"]
     in
     [ sources_rel alias allocation_id;
       rev_constructor_rel alias relation call_witness;
       filter_field
         (fun (f : Field.t) ->
           match[@ocaml.warning "-4"] f with Apply _ -> true | _ -> false)
         relation;
       constructor_rel call_witness
         (Term.constant (Field.encode Code_id_of_call_witness))
         codeid;
       cannot_change_calling_convention codeid ]
     ==> cannot_unbox0 allocation_id);
    (* Likewise, an allocation passed as a parameter of a function can only be
       unboxed if the function's calling convention can be changed. *)
    (* CR ncourant: note that this can fail to trigger if the alias is
       any_source but has no use! This is not a problem but makes it necessary
       to replace unused params in calls with poison values. In the future, we
       could modify this check to ensure it only triggers if the variable is
       indeed used, allowing slightly more unboxing. *)
    (let$ [alias; allocation_id; relation; call_witness; codeid] =
       ["alias"; "allocation_id"; "relation"; "call_witness"; "codeid"]
     in
     [ sources_rel alias allocation_id;
       rev_coconstructor_rel alias relation call_witness;
       constructor_rel call_witness
         (Term.constant (Field.encode Code_id_of_call_witness))
         codeid;
       cannot_change_calling_convention codeid ]
     ==> cannot_unbox0 allocation_id);
    (* Cannot unbox parameters of [Indirect_unknown_arity] calls, even if they
       do not escape. *)
    (* (let$ [usage; allocation_id; relation; _v] = ["usage"; "allocation_id";
       "relation"; "_v"] in [ sources_rel usage allocation_id; coaccessor_rel
       usage relation _v; filter (fun [f] -> match CoField.decode f with | Param
       (Unknown_arity_code_pointer, _) -> true | Param
       (Known_arity_code_pointer, _) -> false) [relation] ] ==> cannot_unbox0
       allocation_id); *)
    (* CR ncourant: I'm not sure this is useful? *)
    (* An allocation that is stored in another can only be unboxed if either the
       representation of the other allocation can be changed, of it the field it
       is stored in is never read, as in that case a poison value will be stored
       instead. *)
    (let$ [alias; allocation_id; relation; to_] =
       ["alias"; "allocation_id"; "relation"; "to_"]
     in
     [ sources_rel alias allocation_id;
       rev_constructor_rel alias relation to_;
       field_of_constructor_is_used to_ relation;
       cannot_change_representation to_;
       cannot_unbox0 to_ ]
     ==> cannot_unbox0 allocation_id);
    (* As previously: if any closure of a set of closures cannot be unboxed,
       then every closure in the set cannot be unboxed. *)
    (let$ [x] = ["x"] in
     [cannot_unbox0 x] ==> cannot_unbox x);
    (let$ [x; field; y] = ["x"; "field"; "y"] in
     [ cannot_unbox0 x;
       constructor_rel x field y;
       filter_field is_function_slot field ]
     ==> cannot_unbox y);
    (* Compute allocations to unbox or to change representation. This requires
       the rules to be executed in order. *)
    (let$ [x] = ["x"] in
     [any_usage_pred x; not (cannot_unbox x)] ==> to_unbox x);
    (let$ [x; _y] = ["x"; "_y"] in
     [usages_rel x _y; not (cannot_unbox x)] ==> to_unbox x);
    (let$ [x] = ["x"] in
     [any_usage_pred x; not (cannot_change_representation x); not (to_unbox x)]
     ==> to_change_representation x);
    (let$ [x; _y] = ["x"; "_y"] in
     [usages_rel x _y; not (cannot_change_representation x); not (to_unbox x)]
     ==> to_change_representation x) ]

let map_from_allocation_points_to_dominated =
  (* let open! Syntax in let map_rule = let$ [x; y; z] = ["x"; "y"; "z"] in [
     sources_rel x y; sources_rel x z; distinct y z ] ==>
     multiple_allocation_points x in let dominator_rule = let$ [x; y] = ["x";
     "y"] in [ sources_rel x y; not (multiple_allocation_points x) ] ==>
     dominator y x in [ map_rule; dominator_rule ] *)
  let open! Syntax in
  let sources_query =
    compile ["x"; "y"] (fun [x; y] -> where [sources_rel x y] (yield [x; y]))
  in
  fun db ->
    let h = Hashtbl.create 17 in
    Cursor.iter
      ~f:(fun [x; y] ->
        if Hashtbl.mem h x
        then Hashtbl.replace h x None
        else Hashtbl.add h x (Some y))
      sources_query db;
    Hashtbl.fold
      (fun id elt acc ->
        match elt with
        | None -> acc
        | Some elt ->
          Code_id_or_name.Map.update elt
            (function
              | None -> Some (Code_id_or_name.Set.singleton id)
              | Some set -> Some (Code_id_or_name.Set.add id set))
            acc)
      h Code_id_or_name.Map.empty

let rec mapi_unboxed_fields (not_unboxed : 'a -> 'b -> 'c)
    (unboxed : Field.t -> 'a -> 'a) (acc : 'a) (uf : 'b unboxed_fields) :
    'c unboxed_fields =
  match uf with
  | Not_unboxed x -> Not_unboxed (not_unboxed acc x)
  | Unboxed f ->
    Unboxed
      (Field.Map.mapi
         (fun field uf ->
           mapi_unboxed_fields not_unboxed unboxed (unboxed field acc) uf)
         f)

let map_unboxed_fields f uf =
  mapi_unboxed_fields (fun () x -> f x) (fun _ () -> ()) () uf

(* Note that this depends crucially on the fact that the poison value is not
   nullable. If it was, we could instead keep the subkind but erase the
   nullability part instead. *)
let[@inline] erase kind =
  Flambda_kind.With_subkind.create
    (Flambda_kind.With_subkind.kind kind)
    Flambda_kind.With_subkind.Non_null_value_subkind.Anything
    (Flambda_kind.With_subkind.nullable kind)

let rec rewrite_kind_with_subkind_not_top_not_bottom db flow_to kind =
  (* CR ncourant: rewrite changed representation, or at least replace with Top.
     Not needed while we don't change representation of blocks. *)
  match Flambda_kind.With_subkind.non_null_value_subkind kind with
  | Anything -> kind
  | Tagged_immediate ->
    kind (* Always correct, since poison is a tagged immediate *)
  | Boxed_float32 | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
  | Boxed_vec128 | Boxed_vec256 | Boxed_vec512 | Float_block _ | Float_array
  | Immediate_array | Value_array | Generic_array | Unboxed_float32_array
  | Unboxed_int32_array | Unboxed_int64_array | Unboxed_nativeint_array
  | Unboxed_vec128_array | Unboxed_vec256_array | Unboxed_vec512_array
  | Unboxed_product_array ->
    (* For all these subkinds, we don't track fields (for now). Thus, being in
       this case without being top or bottom means that we never use this
       particular value, but that it syntactically looks like it could be used.
       We probably could keep the subkind info, but as this value should not be
       used, it is best to delete it. *)
    erase kind
  | Variant { consts; non_consts } ->
    (* CR ncourant: we should make sure poison is in the consts! *)
    (* We don't need to follow indirect code pointers for usage, since functions
       never appear in value_kinds *)
    let usages = get_all_usages ~follow_known_arity_calls:false db flow_to in
    let fields = get_fields db usages in
    let non_consts =
      Tag.Scannable.Map.map
        (fun (shape, kinds) ->
          let kinds =
            List.mapi
              (fun i kind ->
                let field =
                  Global_flow_graph.Field.Block
                    (i, Flambda_kind.With_subkind.kind kind)
                in
                match Field.Map.find_opt field fields with
                | None -> (* maybe poison *) erase kind
                | Some Used_as_top -> (* top *) kind
                | Some (Used_as_vars flow_to) ->
                  rewrite_kind_with_subkind_not_top_not_bottom db flow_to kind)
              kinds
          in
          shape, kinds)
        non_consts
    in
    Flambda_kind.With_subkind.create Flambda_kind.value
      (Flambda_kind.With_subkind.Non_null_value_subkind.Variant
         { consts; non_consts })
      (Flambda_kind.With_subkind.nullable kind)

let rewrite_kind_with_subkind uses var kind =
  let db = uses.db in
  let var = Code_id_or_name.name var in
  if is_top db var
  then kind
  else if not (has_use db var)
  then erase kind
  else
    rewrite_kind_with_subkind_not_top_not_bottom db
      (Code_id_or_name.Map.singleton var ())
      kind

let rec mk_unboxed_fields ~has_to_be_unboxed ~mk db usages name_prefix =
  let fields = get_fields db usages in
  Field.Map.filter_map
    (fun field field_use ->
      match field with
      | Function_slot _ | Code_id_of_call_witness | Apply _ -> assert false
      | Code_of_closure _ -> None
      | Block _ | Value_slot _ | Is_int | Get_tag -> (
        let new_name =
          Flambda_colours.without_colours ~f:(fun () ->
              Format.asprintf "%s_field_%a" name_prefix Field.print field)
        in
        let[@local] default () =
          Some (Not_unboxed (mk (Field.kind field) new_name))
        in
        match field_use with
        | Used_as_top -> default ()
        | Used_as_vars flow_to ->
          if Code_id_or_name.Map.is_empty flow_to
          then Misc.fatal_errorf "Empty set in [get_fields]";
          if Code_id_or_name.Map.for_all
               (fun k () -> has_to_be_unboxed k)
               flow_to
          then
            Some
              (Unboxed
                 (mk_unboxed_fields ~has_to_be_unboxed ~mk db
                    (get_all_usages ~follow_known_arity_calls:true db flow_to)
                    new_name))
          else if Code_id_or_name.Map.exists
                    (fun k () -> has_to_be_unboxed k)
                    flow_to
          then
            Misc.fatal_errorf
              "Field %a of %s flows to both unboxed and non-unboxed variables"
              Field.print field name_prefix
          else default ()))
    fields

let fixpoint (graph : Global_flow_graph.graph) =
  let datalog = Global_flow_graph.to_datalog graph in
  let stats = Datalog.Schedule.create_stats datalog in
  let db = Datalog.Schedule.run ~stats datalog_schedule datalog in
  let db =
    List.fold_left
      (fun db rule ->
        Datalog.Schedule.run ~stats (Datalog.Schedule.saturate [rule]) db)
      db datalog_rules
  in
  if debug then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
  if Sys.getenv_opt "DUMPDB" <> None then Format.eprintf "%a@." Datalog.print db;
  let dominated_by_allocation_points =
    map_from_allocation_points_to_dominated db
  in
  let allocation_point_dominator =
    Code_id_or_name.Map.fold
      (fun alloc_point dominated acc ->
        Code_id_or_name.Set.fold
          (fun dom acc -> Code_id_or_name.Map.add dom alloc_point acc)
          dominated acc)
      dominated_by_allocation_points Code_id_or_name.Map.empty
  in
  let unboxed : unboxed Code_id_or_name.Map.t ref =
    ref Code_id_or_name.Map.empty
  in
  let query_to_unbox =
    Datalog.(compile ["X"] (fun [x] -> where [to_unbox x] (yield [x])))
  in
  let query_to_change_representation =
    Datalog.(
      compile ["X"] (fun [x] -> where [to_change_representation x] (yield [x])))
  in
  let to_unbox = Hashtbl.create 17 in
  let to_change_representation = Hashtbl.create 17 in
  Datalog.Cursor.iter query_to_unbox db ~f:(fun [u] ->
      Hashtbl.replace to_unbox u ());
  Datalog.Cursor.iter query_to_change_representation db ~f:(fun [u] ->
      Hashtbl.replace to_change_representation u ());
  let has_to_be_unboxed code_or_name =
    match
      Code_id_or_name.Map.find_opt code_or_name allocation_point_dominator
    with
    | None -> false
    | Some alloc_point -> Hashtbl.mem to_unbox alloc_point
  in
  Hashtbl.iter
    (fun code_or_name () ->
      (* Format.eprintf "%a@." Code_id_or_name.print code_or_name; *)
      let to_patch =
        match
          Code_id_or_name.Map.find_opt code_or_name
            dominated_by_allocation_points
        with
        | None -> Code_id_or_name.Set.empty
        | Some x -> x
      in
      Code_id_or_name.Set.iter
        (fun to_patch ->
          (* CR-someday ncourant: produce ghost makeblocks/set of closures for
             debugging *)
          let new_name =
            Flambda_colours.without_colours ~f:(fun () ->
                Format.asprintf "%a_into_%a" Code_id_or_name.print code_or_name
                  Code_id_or_name.print to_patch)
          in
          let fields =
            mk_unboxed_fields ~has_to_be_unboxed
              ~mk:(fun kind name -> Variable.create name kind)
              db
              (get_all_usages ~follow_known_arity_calls:true db
                 (Code_id_or_name.Map.singleton to_patch ()))
              new_name
          in
          unboxed := Code_id_or_name.Map.add to_patch fields !unboxed)
        to_patch)
    to_unbox;
  if debug
  then
    Format.printf "new vars: %a"
      (Code_id_or_name.Map.print
         (Field.Map.print (pp_unboxed_elt Variable.print)))
      !unboxed;
  let changed_representation = ref Code_id_or_name.Map.empty in
  Hashtbl.iter
    (fun code_id_or_name () ->
      if Code_id_or_name.Map.mem code_id_or_name !changed_representation
      then ()
      else
        let add_to_s repr c =
          Code_id_or_name.Set.iter
            (fun c ->
              changed_representation
                := Code_id_or_name.Map.add c repr !changed_representation)
            (match
               Code_id_or_name.Map.find_opt c dominated_by_allocation_points
             with
            | None -> Code_id_or_name.Set.empty
            | Some s -> s)
        in
        match get_set_of_closures_def db code_id_or_name with
        | Not_a_set_of_closures ->
          let r = ref ~-1 in
          let mk _kind _name =
            (* XXX fixme, disabled for now *)
            (* TODO depending on the kind, use two counters; then produce a
               mixed block; map_unboxed_fields should help with that *)
            incr r;
            ( !r,
              Flambda_primitive.(
                Block_access_kind.Values
                  { tag = Unknown;
                    size = Unknown;
                    field_kind = Block_access_field_kind.Any_value
                  }) )
          in
          let uses =
            get_all_usages ~follow_known_arity_calls:false db
              (Code_id_or_name.Map.singleton code_id_or_name ())
          in
          let repr = mk_unboxed_fields ~has_to_be_unboxed ~mk db uses "" in
          add_to_s (Block_representation (repr, !r + 1)) code_id_or_name
        | Set_of_closures l ->
          let mk kind name =
            Value_slot.create
              (Compilation_unit.get_current_exn ())
              ~name ~is_always_immediate:false kind
          in
          let uses =
            get_all_usages ~follow_known_arity_calls:false db
              (List.fold_left
                 (fun acc (_, x) -> Code_id_or_name.Map.add x () acc)
                 Code_id_or_name.Map.empty l)
          in
          let repr =
            mk_unboxed_fields ~has_to_be_unboxed ~mk db uses "unboxed"
          in
          let fss =
            List.fold_left
              (fun acc (fs, _) ->
                Function_slot.Map.add fs
                  (Function_slot.create
                     (Compilation_unit.get_current_exn ())
                     ~name:(Function_slot.name fs) ~is_always_immediate:false
                     Flambda_kind.value)
                  acc)
              Function_slot.Map.empty l
          in
          List.iter
            (fun (fs, f) -> add_to_s (Closure_representation (repr, fss, fs)) f)
            l)
    to_change_representation;
  if debug
  then
    Format.eprintf "@.TO_CHG: %a@."
      (Code_id_or_name.Map.print pp_changed_representation)
      !changed_representation;
  { db;
    unboxed_fields = !unboxed;
    changed_representation =
      !changed_representation
      (* unboxed_fields = Code_id_or_name.Map.empty; changed_representation =
         Code_id_or_name.Map.empty *)
  }

let print_color { db; unboxed_fields; changed_representation } v =
  let red =
    if Code_id_or_name.Map.mem v unboxed_fields
    then "22"
    else if Code_id_or_name.Map.mem v changed_representation
    then "88"
    else "ff"
  in
  let green =
    if exists_with_parameters any_usage_pred_query [v] db
    then "22"
    else if has_use db v
    then "88"
    else "ff"
  in
  let blue =
    if exists_with_parameters any_source_query [v] db
    then "22"
    else if has_source db v
    then "88"
    else "ff"
  in
  "#" ^ red ^ green ^ blue

let get_unboxed_fields uses cn =
  Code_id_or_name.Map.find_opt cn uses.unboxed_fields

let get_changed_representation uses cn =
  Code_id_or_name.Map.find_opt cn uses.changed_representation

let has_use uses v = has_use uses.db v

let field_used uses v f = field_used uses.db v f

let cofield_has_use uses v f = cofield_has_use uses.db v f

let has_source uses v = has_source uses.db v

let not_local_field_has_source uses v f = not_local_field_has_source uses.db v f

let cannot_change_calling_convention_query =
  mk_exists_query ["X"] [] (fun [x] [] -> [cannot_change_calling_convention x])

let cannot_change_calling_convention uses v =
  (not (Compilation_unit.is_current (Code_id.get_compilation_unit v)))
  || exists_with_parameters cannot_change_calling_convention_query
       [Code_id_or_name.code_id v]
       uses.db

let unknown_code_id_actually_directly_called_query =
  let open Syntax in
  let open! Global_flow_graph in
  mk_exists_query ["set_of_closures"] ["known_arity_call_witness"]
    (fun [set_of_closures] [known_arity_call_witness] ->
      [ rev_accessor_rel set_of_closures
          (Term.constant
             (Field.encode (Code_of_closure Known_arity_code_pointer)))
          known_arity_call_witness;
        any_source_pred known_arity_call_witness ])

let code_id_actually_directly_called_query =
  let open Syntax in
  let open! Global_flow_graph in
  compile_with_parameters ["set_of_closures"] [] (fun [set_of_closures] [] ->
      foreach ["apply_widget"; "call_witness"; "codeid"]
        (fun [apply_widget; call_witness; codeid] ->
          where
            [ rev_accessor_rel set_of_closures
                (Term.constant
                   (Field.encode (Code_of_closure Known_arity_code_pointer)))
                apply_widget;
              sources_rel apply_widget call_witness;
              constructor_rel call_witness
                (Term.constant (Field.encode Code_id_of_call_witness))
                codeid ]
            (yield [codeid])))

let code_id_actually_directly_called uses v =
  if exists_with_parameters unknown_code_id_actually_directly_called_query
       [Code_id_or_name.name v]
       uses.db
  then Or_unknown.Unknown
  else
    Or_unknown.Known
      (Datalog.Cursor.fold_with_parameters
         code_id_actually_directly_called_query
         [Code_id_or_name.name v]
         uses.db ~init:Code_id.Set.empty
         ~f:(fun [codeid] acc ->
           let codeid =
             Code_id_or_name.pattern_match' codeid
               ~code_id:(fun code_id -> code_id)
               ~name:(fun name ->
                 Misc.fatal_errorf
                   "code_id_actually_directly_called found a name: %a"
                   Name.print name)
           in
           Code_id.Set.add codeid acc))
