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

(* Disable [not-principal] warning in this file. We often write code that looks
   like [let@ [x; y] = a in b] where the list constructor is an [hlist], and [a]
   is used to determine the type of that [hlist]. Unfortunately, due to how
   [let@] is expansed, this is not principal. *)
[@@@ocaml.warning "-not-principal"]

open Global_flow_graph.Relations

let reaperdbg_env = Sys.getenv_opt "REAPERDBG"

let reaperdbg_flags =
  String.split_on_char ',' (Option.value ~default:"" reaperdbg_env)

let debug = reaperdbg_env <> None

(* This needs to be set before creating any datalog rules. *)
let () =
  if debug && List.mem "prov" reaperdbg_flags
  then Datalog.Schedule.enable_provenance_for_debug ()

type 'a unboxed_fields =
  | Not_unboxed of 'a
  | Unboxed of 'a unboxed_fields Field.Map.t

let rec pp_unboxed_elt pp_unboxed ppf = function
  | Not_unboxed x -> pp_unboxed ppf x
  | Unboxed fields -> Field.Map.print (pp_unboxed_elt pp_unboxed) ppf fields

let print_unboxed_fields = pp_unboxed_elt

let is_not_local_field f = not (Field.is_local f)

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
    changed_representation :
      (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t
  }

let pp_result ppf res = Format.fprintf ppf "%a@." Datalog.print res.db

let ( % ) tbl xs = Datalog.atom tbl xs

module Syntax = struct
  include Datalog

  let ( let$ ) xs f =
    let@ xs = variables xs in
    f xs

  let ( let^ ) ps f =
    let@ ps = parameters ps in
    f ps

  let ( let^$ ) (ps, xs) f =
    let@ ps = parameters ps in
    let@ xs = variables xs in
    f (ps, xs)

  let ( ==> ) h c = where h (deduce c)

  let ( =>? ) h l = where h (yield l)

  let ( ?? ) h = where h (yield [])

  let ( !! ) = Term.constant

  let ( ~! ) = Schedule.saturate

  let ( ~> ) = List.map (fun r -> Schedule.saturate [r])

  let ( ~~ ) = not
end

open Syntax

(* Don't shadow [not] *)
let not = Stdlib.not

module Cols = struct
  let n = Code_id_or_name.datalog_column_id

  let f = Field.datalog_column_id

  let cf = Cofield.datalog_column_id
end

let nrel name schema = Datalog.create_relation ~provenance:false ~name schema

let rel1_r ?provenance name schema =
  let tbl = Datalog.create_relation ?provenance ~name schema in
  tbl, fun x -> tbl % [x]

let rel1 ?provenance name schema = snd (rel1_r ?provenance name schema)

let rel2_r ?provenance name schema =
  let tbl = Datalog.create_relation ?provenance ~name schema in
  tbl, fun x y -> tbl % [x; y]

let rel2 ?provenance name schema = snd (rel2_r ?provenance name schema)

let rel3 ?provenance name schema =
  let tbl = Datalog.create_relation ?provenance ~name schema in
  fun x y z -> tbl % [x; y; z]

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
let any_usage_pred = any_usage_pred

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
let any_source_pred = any_source_pred

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

(* Reverse relations *)
let rev_alias_rel =
  let tbl = nrel "rev_alias" Cols.[n; n] in
  fun ~from ~to_ -> tbl % [from; to_]

let rev_use_rel =
  let tbl = nrel "rev_use" Cols.[n; n] in
  fun ~from ~to_ -> tbl % [from; to_]

let rev_constructor_rel =
  let tbl = nrel "rev_constructor" Cols.[n; f; n] in
  fun ~from relation ~base -> tbl % [from; relation; base]

let rev_accessor_rel =
  let tbl = nrel "rev_accessor" Cols.[n; f; n] in
  fun ~base relation ~to_ -> tbl % [base; relation; to_]

let rev_coconstructor_rel =
  let tbl = nrel "rev_coconstructor" Cols.[n; cf; n] in
  fun ~from relation ~base -> tbl % [from; relation; base]

let rev_coaccessor_rel =
  let tbl = nrel "rev_coaccessor" Cols.[n; cf; n] in
  fun ~base relation ~to_ -> tbl % [base; relation; to_]

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

let filter1 f x = filter (fun [x] -> f x) [x]

let filter1_not f x = filter1 (fun x -> not (f x)) x

let datalog_schedule =
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
    [alias_rel ~to_ ~from] ==> rev_alias_rel ~from ~to_
  in
  let rev_use =
    let$ [to_; from] = ["to_"; "from"] in
    [use_rel ~to_ ~from] ==> rev_use_rel ~from ~to_
  in
  let rev_accessor =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [accessor_rel ~to_ relation ~base] ==> rev_accessor_rel ~base relation ~to_
  in
  let rev_constructor =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [constructor_rel ~base relation ~from]
    ==> rev_constructor_rel ~from relation ~base
  in
  let rev_coaccessor =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [coaccessor_rel ~to_ relation ~base]
    ==> rev_coaccessor_rel ~base relation ~to_
  in
  let rev_coconstructor =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [coconstructor_rel ~base relation ~from]
    ==> rev_coconstructor_rel ~from relation ~base
  in
  (* propagate

     The [propagate] relation is part of the input of the solver, with the
     intended meaning of this rule. That is an alias if [is_used] is used. *)
  let alias_from_usage_propagate =
    let$ [if_used; to_; from] = ["if_used"; "to_"; "from"] in
    [any_usage_pred if_used; propagate_rel ~if_used ~to_ ~from]
    ==> alias_rel ~to_ ~from
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
    [alias_rel ~to_ ~from; any_usage_pred to_] ==> any_usage_pred from
  in
  let usages_accessor_1 =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ ~~(any_usage_pred base);
      accessor_rel ~to_ relation ~base;
      usages_rel to_ _var ]
    ==> usages_rel base base
  in
  let usages_accessor_2 =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ ~~(any_usage_pred base);
      accessor_rel ~to_ relation ~base;
      any_usage_pred to_ ]
    ==> usages_rel base base
  in
  let usages_coaccessor_1 =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ ~~(any_usage_pred base);
      sources_rel to_ _var;
      coaccessor_rel ~to_ relation ~base ]
    ==> usages_rel base base
  in
  let usages_coaccessor_2 =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ ~~(any_usage_pred base);
      any_source_pred to_;
      coaccessor_rel ~to_ relation ~base ]
    ==> usages_rel base base
  in
  let usages_alias =
    let$ [to_; from; usage] = ["to_"; "from"; "usage"] in
    [ ~~(any_usage_pred from);
      ~~(any_usage_pred to_);
      usages_rel to_ usage;
      alias_rel ~to_ ~from ]
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
    [ ~~(any_usage_pred base);
      any_usage_pred to_;
      accessor_rel ~to_ relation ~base;
      filter1 is_not_local_field relation ]
    ==> field_usages_top_rel base relation
  in
  let field_usages_from_accessor_field_usages_top_local =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ ~~(any_usage_pred base);
      any_usage_pred to_;
      accessor_rel ~to_ relation ~base;
      filter1 Field.is_local relation ]
    ==> field_usages_rel base relation to_
  in
  let field_usages_from_accessor_field_usages =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ ~~(any_usage_pred base);
      ~~(any_usage_pred to_);
      ~~(field_usages_top_rel base relation);
      accessor_rel ~to_ relation ~base;
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
    [ ~~(any_usage_pred base);
      coaccessor_rel ~to_ relation ~base;
      sources_rel to_ _var ]
    ==> cofield_usages_rel base relation to_
  in
  let cofield_usages_from_coaccessor2 =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [ ~~(any_usage_pred base);
      any_source_pred to_;
      coaccessor_rel ~to_ relation ~base ]
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
    [ ~~(any_usage_pred from);
      ~~(field_usages_top_rel base_use relation);
      ~~(any_usage_pred base);
      constructor_rel ~base relation ~from;
      usages_rel base base_use;
      field_usages_rel base_use relation to_ ]
    ==> alias_rel ~to_ ~from
  in
  let alias_from_accessed_constructor_1_local =
    let$ [base; base_use; relation; from; to_] =
      ["base"; "base_use"; "relation"; "from"; "to_"]
    in
    [ filter1 Field.is_local relation;
      constructor_rel ~base relation ~from;
      usages_rel base base_use;
      field_usages_rel base_use relation to_ ]
    ==> alias_rel ~to_ ~from
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
    [ constructor_rel ~base relation ~from;
      ~~(any_usage_pred base);
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
      constructor_rel ~base relation ~from;
      filter1 is_not_local_field relation ]
    ==> any_usage_pred from
  in
  let escaping_local_field =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [ any_usage_pred base;
      constructor_rel ~base relation ~from;
      filter1 Field.is_local relation ]
    ==> and_
          [ escaping_field_rel relation
              from (*; field_usages_rel base relation from *) ]
  in
  let any_usage_from_coaccessor_any_source =
    let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
    [any_source_pred base; rev_coaccessor_rel ~base relation ~to_]
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
    [rev_alias_rel ~from ~to_; any_source_pred from] ==> any_source_pred to_
  in
  let sources_constructor_1 =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ ~~(any_source_pred base);
      sources_rel from _var;
      rev_constructor_rel ~from relation ~base ]
    ==> sources_rel base base
  in
  let sources_constructor_2 =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ ~~(any_source_pred base);
      any_source_pred from;
      rev_constructor_rel ~from relation ~base ]
    ==> sources_rel base base
  in
  let sources_coconstructor_1 =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ ~~(any_source_pred base);
      usages_rel from _var;
      rev_coconstructor_rel ~from relation ~base ]
    ==> sources_rel base base
  in
  let sources_coconstructor_2 =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ ~~(any_source_pred base);
      any_usage_pred from;
      rev_coconstructor_rel ~from relation ~base ]
    ==> sources_rel base base
  in
  let sources_alias =
    let$ [from; to_; source] = ["from"; "to_"; "source"] in
    [ ~~(any_source_pred from);
      ~~(any_source_pred to_);
      sources_rel from source;
      rev_alias_rel ~from ~to_ ]
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
    [ ~~(any_source_pred base);
      any_source_pred from;
      rev_constructor_rel ~from relation ~base;
      filter1 is_not_local_field relation ]
    ==> field_sources_top_rel base relation
  in
  let field_sources_from_constructor_field_sources_top_local =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ ~~(any_source_pred base);
      any_source_pred from;
      rev_constructor_rel ~from relation ~base;
      filter1 Field.is_local relation ]
    ==> field_sources_rel base relation from
  in
  let field_sources_from_constructor_field_sources =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ ~~(any_source_pred base);
      ~~(any_source_pred from);
      ~~(field_sources_top_rel base relation);
      rev_constructor_rel ~from relation ~base;
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
    [ ~~(any_source_pred base);
      rev_coconstructor_rel ~from relation ~base;
      usages_rel from _var ]
    ==> cofield_sources_rel base relation from
  in
  let cofield_sources_from_coconstrucor2 =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ ~~(any_source_pred base);
      any_usage_pred from;
      rev_coconstructor_rel ~from relation ~base ]
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
    [ ~~(any_usage_pred base);
      coconstructor_rel ~base relation ~from;
      usages_rel base base_use;
      cofield_usages_rel base_use relation to_ ]
    ==> alias_rel ~to_:from ~from:to_
  in
  (*
   any_source_from_coconstructor_any_usage
   * any_usage Base
   * /\ coconstructor Base Rel From
   * => any_source From
   *)
  let any_source_from_coconstructor_any_usage =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [any_usage_pred base; coconstructor_rel ~base relation ~from]
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
    [ ~~(any_source_pred to_);
      ~~(field_sources_top_rel base_source relation);
      ~~(any_source_pred base);
      rev_accessor_rel ~base relation ~to_;
      sources_rel base base_source;
      field_sources_rel base_source relation from ]
    ==> alias_rel ~to_ ~from
  in
  let alias_from_accessed_constructor_2_local =
    let$ [base; base_source; relation; to_; from] =
      ["base"; "base_source"; "relation"; "to_"; "from"]
    in
    [ filter1 Field.is_local relation;
      rev_accessor_rel ~base relation ~to_;
      sources_rel base base_source;
      field_sources_rel base_source relation from ]
    ==> alias_rel ~to_ ~from
  in
  let any_source_from_accessed_constructor =
    let$ [base; base_source; relation; to_] =
      ["base"; "base_source"; "relation"; "to_"]
    in
    [ rev_accessor_rel ~base relation ~to_;
      ~~(any_source_pred base);
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
      rev_accessor_rel ~base relation ~to_;
      filter1 is_not_local_field relation ]
    ==> any_source_pred to_
  in
  let reading_local_field =
    let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
    [ any_source_pred base;
      rev_accessor_rel ~base relation ~to_;
      filter1 Field.is_local relation ]
    ==> and_
          [ reading_field_rel relation
              to_ (*; field_sources_rel base relation to_*) ]
  in
  let reading_escaping =
    let$ [relation; from; to_] = ["relation"; "from"; "to_"] in
    [escaping_field_rel relation from; reading_field_rel relation to_]
    ==> alias_rel ~to_ ~from
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
    [ ~~(any_source_pred base);
      rev_coaccessor_rel ~base relation ~to_;
      sources_rel base base_source;
      cofield_sources_rel base_source relation from ]
    ==> alias_rel ~to_:from ~from:to_
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
    [usages_rel to_ _var; use_rel ~to_ ~from] ==> any_usage_pred from
  in
  let any_usage_from_use_2 =
    let$ [to_; from] = ["to_"; "from"] in
    [any_usage_pred to_; use_rel ~to_ ~from] ==> any_usage_pred from
  in
  let any_source_use_1 =
    let$ [from; to_; _var] = ["from"; "to_"; "_var"] in
    [sources_rel from _var; rev_use_rel ~from ~to_] ==> any_source_pred to_
  in
  let any_source_use_2 =
    let$ [from; to_] = ["from"; "to_"] in
    [any_source_pred from; rev_use_rel ~from ~to_] ==> any_source_pred to_
  in
  Schedule.fixpoint
    [ ~![ rev_accessor;
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
      ~![ alias_from_accessed_constructor_1;
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
          rev_alias ] ]

let mk_exists_query params existentials f =
  let q =
    query
      (let^$ params, existentials = params, existentials in
       ??(f params existentials))
  in
  fun params db ->
    Cursor.fold_with_parameters q params db ~init:false ~f:(fun [] _ -> true)

module Fixit : sig
  type (_, _, _) stmt

  val ( let+ ) : ('a, 'b, 'c) stmt -> ('a -> 'd) -> ('d, 'b, 'c) stmt

  val ( and+ ) :
    ('a, 'b, 'b) stmt -> ('c, 'b, 'b) stmt -> ('a * 'c, 'b, 'b) stmt

  val run : ('a, 'a, 'b) stmt -> Datalog.database -> 'b

  (* Don't try to write to this one ;) *)
  val empty :
    ('t, 'k, unit) Datalog.Column.hlist -> ('t, 'k, unit) Datalog.table

  val param :
    string ->
    ('a, 'b, unit) Datalog.Column.hlist ->
    (('a, 'b, unit) Datalog.table -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'a -> 'd) stmt

  val paramc :
    string ->
    ('a, 'b, unit) Datalog.Column.hlist ->
    ('e -> 'a) ->
    (('a, 'b, unit) Datalog.table -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'e -> 'd) stmt

  val param0 :
    ('a -> 'b) ->
    (('b, 'c, 'c) stmt -> ('d, 'e, 'f) stmt) ->
    ('d, 'e, 'a -> 'f) stmt

  val local0 :
    ('a, 'b, unit) Datalog.Column.hlist ->
    ('a, 'c, 'c) stmt ->
    (('a, 'b, unit) Datalog.table -> ('d, 'c, 'c) stmt) ->
    ('d, 'c, 'c) stmt

  module Table : sig
    type (_, _) hlist =
      | [] : (Datalog.nil, Datalog.nil) hlist
      | ( :: ) :
          ('t, 'k, unit) Datalog.table * ('ts, 'xs) hlist
          -> ('t -> 'ts, ('t, 'k, unit) Datalog.table -> 'xs) hlist
  end

  val return : ('a, 'b, unit) Datalog.table -> ('a, 'c, 'c) stmt

  val fix :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> (Datalog.nil, Datalog.rule) Datalog.program list) ->
    (('a, 'b) Table.hlist -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'd) stmt

  val seq :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> (Datalog.nil, Datalog.rule) Datalog.program list) ->
    (('a, 'b) Table.hlist -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'd) stmt

  val fix1 :
    ('t, 'k, unit) Datalog.table ->
    (('t, 'k, unit) Datalog.table ->
    (Datalog.nil, Datalog.rule) Datalog.program list) ->
    (('t, 'k, unit) Datalog.table -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'd) stmt

  val fix' :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> (Datalog.nil, Datalog.rule) Datalog.program list) ->
    ('a Datalog.Constant.hlist, 'c, 'c) stmt

  val seq' :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> (Datalog.nil, Datalog.rule) Datalog.program list) ->
    ('a Datalog.Constant.hlist, 'c, 'c) stmt

  val fix1' :
    ('t, 'k, unit) Datalog.table ->
    (('t, 'k, unit) Datalog.table ->
    (Datalog.nil, Datalog.rule) Datalog.program list) ->
    ('t, 'c, 'c) stmt

  val ( let@ ) : ('a -> 'b) -> 'a -> 'b
end = struct
  let empty columns =
    Datalog.create_table ~name:"empty" ~default_value:() columns

  let local name columns = Datalog.create_table ~name ~default_value:() columns

  module Table = struct
    type ('t, 'k, 'v) t = ('t, 'k, 'v) Datalog.table

    type (_, _) hlist =
      | [] : (Datalog.nil, Datalog.nil) hlist
      | ( :: ) :
          ('t, 'k, unit) t * ('ts, 'xs) hlist
          -> ('t -> 'ts, ('t, 'k, unit) Datalog.table -> 'xs) hlist

    let rec locals : type a b. (a, b) hlist -> (a, b) hlist = function
      | [] -> []
      | table :: tables ->
        let columns = Datalog.columns table in
        local "fix" columns :: locals tables

    let rec copy :
        type a b.
        (a, b) hlist -> (a, b) hlist -> Datalog.database -> Datalog.database =
     fun from_tables to_tables db ->
      match from_tables, to_tables with
      | [], [] -> db
      | from_table :: from_tables, to_table :: to_tables ->
        let db =
          Datalog.set_table to_table (Datalog.get_table from_table db) db
        in
        copy from_tables to_tables db

    let rec get :
        type a b. (a, b) hlist -> Datalog.database -> a Datalog.Constant.hlist =
     fun tables db ->
      match tables with
      | [] -> []
      | table :: tables -> Datalog.get_table table db :: get tables db
  end

  (* In [('s, 'r, 'f) stmt] the type variables have the following meaning:

     - ['s] is the value associated with the statement (i.e. the value that can
     be inspected using [let+]).

     - ['r] is the global return type of the program this statement is a part
     of. For instance, in [s = let+ x = s1 and+ y = s2 in (x, y)], all of [s1],
     [s2] and [s] have the same value of ['r = 'x * 'y], but have different
     values for ['s] (['x], ['y] and ['x * 'y] respectively).

     - ['f] is the global parametric type of the program this istatement is a
     part of. It is a n-ary function type ultimately returning values of type
     ['r], but with the parameters introduced by the [param*] family of
     functions. *)
  type (_, _, _) stmt =
    | Return : ('t, 'k, unit) Datalog.table -> ('t, 'c, 'c) stmt
    | Value : 'a option ref -> ('a, 'c, 'c) stmt
    | Run : Datalog.Schedule.t -> (unit, 'c, 'c) stmt
    | Seq : (unit, 'c, 'c) stmt * ('a, 'b, 'c) stmt -> ('a, 'b, 'c) stmt
    | Call : ('b, 'c, 'a -> 'c) stmt * ('a, 'c, 'c) stmt -> ('b, 'c, 'c) stmt
    | Map :
        ('a, 'c, 'j) stmt * (Datalog.database -> 'a -> 'b)
        -> ('b, 'c, 'j) stmt
    | Inspect :
        ('a, 'c, 'j) stmt * (Datalog.database -> 'a -> unit)
        -> ('a, 'c, 'j) stmt
    | Conj : ('a, 'c, 'c) stmt * ('b, 'c, 'c) stmt -> ('a * 'b, 'c, 'c) stmt
    | Now :
        ('a, 'b, 'j) stmt * (Datalog.database -> Datalog.database)
        -> ('a, 'b, 'j) stmt
    | Input :
        ('x, 'y, 'j) stmt * (Datalog.database -> 'a -> Datalog.database)
        -> ('x, 'y, 'a -> 'j) stmt

  let rec run :
      type d f e.
      (d, f, e) stmt -> (Datalog.database -> d -> f) -> Datalog.database -> e =
   fun stmt k db ->
    match stmt with
    | Return table -> k db (Datalog.get_table table db)
    | Value v -> k db (Option.get !v)
    | Call (stmt_f, stmt_arg) ->
      run stmt_arg (fun db arg -> run stmt_f k db arg) db
    | Run schedule -> k (Datalog.Schedule.run schedule db) ()
    | Seq (stmt1, stmt2) -> run stmt1 (fun db () -> run stmt2 k db) db
    | Map (stmt, later) -> run stmt (fun db value -> k db (later db value)) db
    | Inspect (stmt, f) ->
      run stmt
        (fun db value ->
          f db value;
          k db value)
        db
    | Conj (stmt1, stmt2) ->
      run stmt1
        (fun db value1 -> run stmt2 (fun db value2 -> k db (value1, value2)) db)
        db
    | Now (stmt, f) -> run stmt k (f db)
    | Input (stmt, set_input) ->
      fun arg ->
        let db = set_input db arg in
        run stmt k db

  let run stmt db = run stmt (fun _ out -> out) db

  let return table = Return table

  let ( let+ ) stmt f = Map (stmt, fun _ value -> f value)

  let ( and+ ) stmt1 stmt2 = Conj (stmt1, stmt2)

  let param0 g f =
    let cell = ref None in
    Inspect
      ( Input
          ( f (Value cell),
            fun db x ->
              cell := Some (g x);
              db ),
        fun _db _value -> cell := None )

  let param name columns f =
    let table = local name columns in
    Input (f table, fun db x -> Datalog.set_table table x db)

  let paramc name columns g f =
    let table = local name columns in
    Input (f table, fun db x -> Datalog.set_table table (g x) db)

  let local0 columns body f =
    let table = local "local" columns in
    Call (Input (f table, fun db x -> Datalog.set_table table x db), body)

  let fix x f g =
    let y = Table.locals x in
    let schedule = Datalog.Schedule.saturate (f y) in
    let body = g y in
    Now (Seq (Run schedule, body), fun db -> Table.copy x y db)

  let seq x f g =
    let y = Table.locals x in
    let rules = f y in
    let body = g y in
    let go =
      List.fold_right
        (fun r acc -> Seq (Run (Datalog.Schedule.saturate [r]), acc))
        rules body
    in
    Now (go, fun db -> Table.copy x y db)

  let fix1 x f g =
    let y = local "fix" (Datalog.columns x) in
    let schedule = Datalog.Schedule.saturate (f y) in
    let body = g y in
    Now
      ( Seq (Run schedule, body),
        fun db -> Datalog.set_table y (Datalog.get_table x db) db )

  let fix' x f =
    let y = Table.locals x in
    let schedule = Datalog.Schedule.saturate (f y) in
    Now
      ( Map (Run schedule, fun db () -> Table.get y db),
        fun db -> Table.copy x y db )

  let seq' x f =
    let y = Table.locals x in
    let rules = f y in
    let go =
      Option.get
        (List.fold_right
           (fun r acc ->
             let r = Run (Datalog.Schedule.saturate [r]) in
             match acc with None -> Some r | Some acc -> Some (Seq (r, acc)))
           rules None)
    in
    Now (Map (go, fun db () -> Table.get y db), fun db -> Table.copy x y db)

  let fix1' x f =
    let y = local "fix" (Datalog.columns x) in
    let schedule = Datalog.Schedule.saturate (f y) in
    Now
      ( Map (Run schedule, fun db () -> Datalog.get_table y db),
        fun db -> Datalog.set_table y (Datalog.get_table x db) db )

  let ( let@ ) f x = f x
end

module One : sig
  type t

  include Datalog.Column.S with type t := t

  val top : t

  val flag :
    (unit Map.t, t -> Datalog.nil, unit) Datalog.table ->
    [> `Atom of Datalog.atom]

  val to_bool : unit Map.t -> bool

  val of_bool : bool -> unit Map.t

  val cols : (unit Map.t, t -> Datalog.nil, unit) Datalog.Column.hlist
end = struct
  include Datalog.Column.Make (struct
    let name = "one"

    let print ppf _ = Format.fprintf ppf "T"
  end)

  let top = 0

  let flag tbl = Datalog.atom tbl [Datalog.Term.constant top]

  let to_bool m = not (Map.is_empty m)

  let of_bool b = if b then Map.singleton top () else Map.empty

  let cols =
    let open! Datalog.Column in
    [datalog_column_id]
end

let () = ignore (One.top, Fixit.fix, Fixit.seq, Fixit.fix1, Fixit.return)

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
  let open! Fixit in
  let stmt =
    let@ follow_known_arity_calls =
      paramc "follow_known_arity_calls" One.cols One.of_bool
    in
    let@ in_ = param "in_" Cols.[n] in
    let@ out = fix1' (empty Cols.[n]) in
    [ (let$ [x; y] = ["x"; "y"] in
       [in_ % [x]; usages_rel x y] ==> out % [y]);
      (let$ [x; apply_witness; call_witness; code_id; my_closure_of_code_id; y]
           =
         [ "x";
           "apply_witness";
           "call_witness";
           "code_id";
           "my_closure_of_code_id";
           "y" ]
       in
       [ One.flag follow_known_arity_calls;
         out % [x];
         rev_accessor_rel ~base:x
           !!(Field.code_of_closure Known_arity_code_pointer)
           ~to_:apply_witness;
         sources_rel apply_witness call_witness;
         constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:code_id;
         code_id_my_closure_rel ~code_id ~my_closure:my_closure_of_code_id;
         usages_rel my_closure_of_code_id y ]
       ==> out % [y]);
      (let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
       [ out % [x];
         field_usages_rel x field y;
         filter1 Field.is_function_slot field;
         usages_rel y z ]
       ==> out % [z]) ]
  in
  fun ~follow_known_arity_calls db s ->
    Usages (run stmt db follow_known_arity_calls s)

let get_direct_usages :
    Datalog.database -> unit Code_id_or_name.Map.t -> unit Code_id_or_name.Map.t
    =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let@ out = fix1' (empty Cols.[n]) in
     [ (let$ [x; y] = ["x"; "y"] in
        [in_ % [x]; usages_rel x y] ==> out % [y]) ])

type field_usage =
  | Used_as_top
  | Used_as_vars of unit Code_id_or_name.Map.t

(** For an usage set (argument s), compute the way its fields are used.
    As function slots are transparent for [get_usages], functions slot
    usages are ignored here.
*)
let get_one_field : Datalog.database -> Field.t -> usages -> field_usage =
  (* CR-someday ncourant: likewise here; I find this function particulartly
     ugly. *)
  let open! Fixit in
  run
    (let@ in_field =
       paramc "in_field" Cols.[f] (fun field -> Field.Map.singleton field ())
     in
     let@ in_ = paramc "in_" Cols.[n] (fun (Usages s) -> s) in
     let+ [used_as_top; used_as_vars] =
       let@ [used_as_top; used_as_vars] =
         seq' [empty One.cols; empty Cols.[n]]
       in
       [ (let$ [x; field] = ["x"; "field"] in
          [in_ % [x]; in_field % [field]; field_usages_top_rel x field]
          ==> One.flag used_as_top);
         (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ ~~(One.flag used_as_top);
            in_ % [x];
            in_field % [field];
            field_usages_rel x field y ]
          ==> used_as_vars % [y]) ]
     in
     if One.to_bool used_as_top then Used_as_top else Used_as_vars used_as_vars)

let get_fields : Datalog.database -> usages -> field_usage Field.Map.t =
  (* CR-someday ncourant: likewise here; I find this function particulartly
     ugly. *)
  let open! Fixit in
  run
    (let@ in_ = paramc "in_" Cols.[n] (fun (Usages s) -> s) in
     let+ [out1; out2] =
       let@ [out1; out2] = seq' [empty Cols.[f]; empty Cols.[f; n]] in
       [ (let$ [x; field] = ["x"; "field"] in
          [ in_ % [x];
            field_usages_top_rel x field;
            filter1_not Field.is_function_slot field ]
          ==> out1 % [field]);
         (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            field_usages_rel x field y;
            ~~(out1 % [field]);
            filter1_not Field.is_function_slot field ]
          ==> out2 % [field; y]) ]
     in
     Field.Map.merge
       (fun k x y ->
         match x, y with
         | None, None -> assert false
         | Some _, Some _ ->
           Misc.fatal_errorf "Got two results for field %a" Field.print k
         | Some (), None -> Some Used_as_top
         | None, Some m -> Some (Used_as_vars m))
       out1 out2)

let field_of_constructor_is_used =
  rel2 "field_of_constructor_is_used" Cols.[n; f]

let field_of_constructor_is_used_top =
  rel2 "field_of_constructor_is_used_top" Cols.[n; f]

let field_of_constructor_is_used_as =
  rel3 "field_of_constructor_is_used" Cols.[n; f; n]

let get_fields_usage_of_constructors :
    Datalog.database -> unit Code_id_or_name.Map.t -> field_usage Field.Map.t =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let+ [out1; out2] =
       let@ [out1; out2] = seq' [empty Cols.[f]; empty Cols.[f; n]] in
       [ (let$ [x; field] = ["x"; "field"] in
          [ in_ % [x];
            field_of_constructor_is_used_top x field;
            filter1_not Field.is_function_slot field ]
          ==> out1 % [field]);
         (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            field_of_constructor_is_used_as x field y;
            ~~(out1 % [field]);
            filter1_not Field.is_function_slot field ]
          ==> out2 % [field; y]) ]
     in
     Field.Map.merge
       (fun k x y ->
         match x, y with
         | None, None -> assert false
         | Some _, Some _ ->
           Misc.fatal_errorf "Got two results for field %a" Field.print k
         | Some (), None -> Some Used_as_top
         | None, Some m -> Some (Used_as_vars m))
       out1 out2)

type set_of_closures_def =
  | Not_a_set_of_closures
  | Set_of_closures of (Function_slot.t * Code_id_or_name.t) list

let get_set_of_closures_def :
    Datalog.database -> Code_id_or_name.t -> set_of_closures_def =
  let q =
    query
      (let^$ [x], [relation; y] = ["x"], ["relation"; "y"] in
       [ constructor_rel ~base:x relation ~from:y;
         filter1 Field.is_function_slot relation ]
       =>? [relation; y])
  in
  fun db v ->
    let l =
      Cursor.fold_with_parameters q [v] db ~init:[] ~f:(fun [f; y] l ->
          (Field.must_be_function_slot f, y) :: l)
    in
    match l with [] -> Not_a_set_of_closures | _ :: _ -> Set_of_closures l

let any_usage_pred_query =
  mk_exists_query ["X"] [] (fun [x] [] -> [any_usage_pred x])

(* CR pchambart: should rename: mutiple potential top is_used_as_top (should be
   obviously different from has use) *)
let is_top db x = any_usage_pred_query [x] db

(* CR pchambart: field_used should rename to mean that this is the specific
   field of a given variable. *)
let has_use, _field_used =
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
  ( (fun db x -> any_usage_pred_query [x] db || usages_query [x] db),
    fun db x field ->
      any_usage_pred_query [x] db
      || used_field_top_query [x; field] db
      || used_field_query [x; field] db )

let field_used =
  let field_of_constructor_is_used_query =
    mk_exists_query ["X"; "F"] [] (fun [x; f] [] ->
        [field_of_constructor_is_used x f])
  in
  fun db x field -> field_of_constructor_is_used_query [x; field] db

let any_source_query =
  mk_exists_query ["X"] [] (fun [x] [] -> [any_source_pred x])

let has_source =
  let has_source_query =
    mk_exists_query ["X"] ["Y"] (fun [x] [y] -> [sources_rel x y])
  in
  fun db x -> any_source_query [x] db || has_source_query [x] db

let cofield_has_use =
  let cofield_query =
    mk_exists_query ["X"; "F"] ["S"; "T"] (fun [x; f] [s; t] ->
        [sources_rel x s; cofield_sources_rel s f t])
  in
  fun db x cofield -> any_source_query [x] db || cofield_query [x; cofield] db

(* CR pchambart: Should rename to remove not local in the name (the notion does
   not exists right now)*)
let not_local_field_has_source =
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
    any_source_query [x] db
    || field_any_source_query [x; field] db
    || field_source_query [x; field] db

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

let multiple_allocation_points = rel1 "multiple_allocations_points" Cols.[n]

let dominated_by_allocation_point =
  rel2 "dominated_by_allocation_point" Cols.[n; n]

let allocation_point_dominator = rel2 "allocation_point_dominator" Cols.[n; n]

let datalog_rules =
  let real_field f =
    match[@ocaml.warning "-4"] Field.view f with
    | Code_of_closure _ | Apply _ | Code_id_of_call_witness -> false
    | _ -> true
  in
  ~>[ (let$ [base; relation; from] = ["base"; "relation"; "from"] in
       [ constructor_rel ~base relation ~from;
         any_usage_pred base;
         filter1 is_not_local_field relation ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (let$ [base; relation; from; usage] =
         ["base"; "relation"; "from"; "usage"]
       in
       [ constructor_rel ~base relation ~from;
         usages_rel base usage;
         field_usages_top_rel usage relation ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (let$ [base; relation; from; usage; v] =
         ["base"; "relation"; "from"; "usage"; "v"]
       in
       [ constructor_rel ~base relation ~from;
         usages_rel base usage;
         field_usages_rel usage relation v ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_as base relation v ]);
      (let$ [base; relation; from; x] = ["base"; "relation"; "from"; "x"] in
       [ constructor_rel ~base relation ~from;
         any_usage_pred base;
         reading_field_rel relation x;
         any_usage_pred x ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (let$ [base; relation; from; x; y] =
         ["base"; "relation"; "from"; "x"; "y"]
       in
       [ constructor_rel ~base relation ~from;
         any_usage_pred base;
         reading_field_rel relation x;
         usages_rel x y ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_as base relation x ]);
      (let$ [usage; base; relation; from; v; u] =
         ["usage"; "base"; "relation"; "from"; "v"; "u"]
       in
       [ constructor_rel ~base relation ~from;
         sources_rel usage base;
         filter1 Field.is_local relation;
         any_usage_pred base;
         rev_accessor_rel ~base:usage relation ~to_:v;
         usages_rel v u ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_as base relation v ]);
      (let$ [usage; base; relation; from; v] =
         ["usage"; "base"; "relation"; "from"; "v"]
       in
       [ constructor_rel ~base relation ~from;
         sources_rel usage base;
         filter1 Field.is_local relation;
         any_usage_pred base;
         (* field_usages_top_rel usage relation *)
         rev_accessor_rel ~base:usage relation ~to_:v;
         any_usage_pred v ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (* CR ncourant: this marks any [Apply] field as
         [field_of_constructor_is_used], as long as the function is called.
         Shouldn't that be gated behind a [cannot_change_calling_convetion]? *)
      (* (let$ [base; relation; from; coderel; call_witness] = ["base";
         "relation"; "from"; "coderel"; "call_witness"] in [ constructor_rel
         base relation from; filter1 is_apply_field relation; constructor_rel
         base coderel call_witness; any_usage_pred indirect_call_witness;
         filter1 is_code_field coderel ] ==> field_of_constructor_is_used base
         relation); *)
      (* CR ncourant: should this be reenabled? I think this is no longer
         necessary because we remove unused arguments of continuations,
         including return continuations. *)
      (* If any usage is possible, do not change the representation. Note that
         this rule will change in the future, when local value slots are
         properly tracked: a closure will only local value slots that has
         any_use will still be able to have its representation changed. *)
      (* (let$ [x] = ["x"] in [any_usage_pred x] ==>
         cannot_change_representation0 x); *)
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ any_usage_pred x;
         filter1 is_not_local_field field;
         filter1 real_field field;
         constructor_rel ~base:x field ~from:y ]
       ==> cannot_change_representation0 x);
      (* If a block with a local field escapes, and that field is read again
         from an [any_source] value, prevent changing the representation. This
         ensures that for a block whose representation is changed, we can know
         the source at each point. *)
      (let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
       [ any_usage_pred x;
         filter1 Field.is_local field;
         reading_field_rel field z;
         constructor_rel ~base:x field ~from:y ]
       ==> cannot_change_representation0 x);
      (* Likewise, if a block with a local field escapes, and that field is read
         again from a value with several sources, prevent changing the
         representation. *)
      (let$ [usage; field; source1; source2; _v] =
         ["usage"; "field"; "source1"; "source2"; "_v"]
       in
       [ field_usages_rel usage field _v;
         filter1 Field.is_local field;
         sources_rel usage source1;
         sources_rel usage source2;
         distinct Cols.n source1 source2 ]
       ==> cannot_change_representation0 source1);
      (let$ [usage; field; source1; source2] =
         ["usage"; "field"; "source1"; "source2"]
       in
       [ field_usages_top_rel usage field;
         filter1 Field.is_local field;
         sources_rel usage source1;
         sources_rel usage source2;
         distinct Cols.n source1 source2 ]
       ==> cannot_change_representation0 source1);
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
         filter1 real_field field;
         field_usages_rel alias field _v ]
       ==> cannot_change_representation0 allocation_id);
      (let$ [allocation_id; alias; alias_source; field] =
         ["allocation_id"; "alias"; "alias_source"; "field"]
       in
       [ usages_rel allocation_id alias;
         sources_rel alias alias_source;
         distinct Cols.n alias_source allocation_id;
         filter1 real_field field;
         field_usages_top_rel alias field ]
       ==> cannot_change_representation0 allocation_id);
      (let$ [allocation_id; alias; field; _v] =
         ["allocation_id"; "alias"; "field"; "_v"]
       in
       [ usages_rel allocation_id alias;
         any_source_pred alias;
         filter1 real_field field;
         field_usages_rel alias field _v ]
       ==> cannot_change_representation0 allocation_id);
      (let$ [allocation_id; alias; field] =
         ["allocation_id"; "alias"; "field"]
       in
       [ usages_rel allocation_id alias;
         any_source_pred alias;
         filter1 real_field field;
         field_usages_top_rel alias field ]
       ==> cannot_change_representation0 allocation_id);
      (* If the allocation has a source distinct from itself, its representation
         cannot be changed (in fact, in that case, it shouldn't even be an
         allocation). *)
      (let$ [allocation_id; source] = ["allocation_id"; "source"] in
       [sources_rel allocation_id source; distinct Cols.n source allocation_id]
       ==> cannot_change_representation0 allocation_id);
      (* Used but not its own source: either from any source, or it has no
         source at all and it is dead code. In either case, do not unbox or
         change the representation. *)
      (let$ [allocation_id; usage] = ["allocation_id"; "usage"] in
       [ usages_rel allocation_id usage;
         ~~(sources_rel allocation_id allocation_id) ]
       ==> cannot_change_representation0 allocation_id);
      (let$ [allocation_id] = ["allocation_id"] in
       [any_source_pred allocation_id]
       ==> cannot_change_representation0 allocation_id);
      (let$ [call_witness; code_id] = ["call_witness"; "code_id"] in
       [ constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:code_id ]
       ==> cannot_change_representation0 call_witness);
      (* Note this rule is here to still allow changing the calling convention
         of symbols /!\ when adding back the local value slots, there should be
         a few more rules here *)
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
         field_usages_rel alias !!Field.code_id_of_call_witness _v ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id; alias; alias_source] =
         ["allocation_id"; "alias"; "alias_source"]
       in
       [ usages_rel allocation_id alias;
         sources_rel alias alias_source;
         distinct Cols.n alias_source allocation_id;
         field_usages_top_rel alias !!Field.code_id_of_call_witness ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id; alias; _v] = ["allocation_id"; "alias"; "_v"] in
       [ usages_rel allocation_id alias;
         any_source_pred alias;
         field_usages_rel alias !!Field.code_id_of_call_witness _v ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id; alias] = ["allocation_id"; "alias"] in
       [ usages_rel allocation_id alias;
         any_source_pred alias;
         field_usages_top_rel alias !!Field.code_id_of_call_witness ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id; source] = ["allocation_id"; "source"] in
       [sources_rel allocation_id source; distinct Cols.n source allocation_id]
       ==> cannot_change_witness_calling_convention allocation_id);
      (* Used but not its own source: either from any source, or it has no
         source at all and it is dead code. In either case, do not unbox *)
      (let$ [allocation_id; usage] = ["allocation_id"; "usage"] in
       [ usages_rel allocation_id usage;
         ~~(sources_rel allocation_id allocation_id) ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id] = ["allocation_id"] in
       [any_source_pred allocation_id]
       ==> cannot_change_witness_calling_convention allocation_id);
      (* If the calling convention of a witness cannot be changed, the calling
         convention of its code_id cannot be either. From now on,
         [cannot_change_witness_calling_convention] should no longer be used. *)
      (let$ [call_witness; code_id; _v] = ["call_witness"; "code_id"; "_v"] in
       [ constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:code_id;
         usages_rel call_witness _v;
         cannot_change_witness_calling_convention call_witness ]
       ==> cannot_change_calling_convention code_id);
      (let$ [call_witness; code_id] = ["call_witness"; "code_id"] in
       [ constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:code_id;
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
       [ rev_constructor_rel ~from:call_witness
           !!(Field.code_of_closure Unknown_arity_code_pointer)
           ~base:set_of_closures;
         any_usage_pred call_witness;
         constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid ]
       ==> cannot_change_calling_convention codeid);
      (let$ [call_witness; codeid; set_of_closures; _v] =
         ["call_witness"; "codeid"; "set_of_closures"; "_v"]
       in
       [ rev_constructor_rel ~from:call_witness
           !!(Field.code_of_closure Unknown_arity_code_pointer)
           ~base:set_of_closures;
         usages_rel call_witness _v;
         constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid ]
       ==> cannot_change_calling_convention codeid);
      (* CR-someday ncourant: we completely prevent changing the representation
         of symbols. While allowing them to be unboxed is difficult, due to
         symbols being always values, we could at least change their
         representation. This would require rewriting in the types, which is not
         done yet. *)
      (let$ [x; _source] = ["x"; "_source"] in
       [ sources_rel x _source;
         filter1
           (fun x ->
             Code_id_or_name.pattern_match x
               ~symbol:(fun _ -> true)
               ~var:(fun _ -> false)
               ~code_id:(fun _ -> false))
           x ]
       ==> cannot_change_representation0 x);
      (* If the representation of any closure in a set of closures cannot be
         changed, the representation of all the closures in the set cannot be
         changed. *)
      (let$ [x] = ["x"] in
       [cannot_change_representation0 x] ==> cannot_change_representation1 x);
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ constructor_rel ~base:x field ~from:y;
         filter1 Field.is_function_slot field;
         cannot_change_representation0 x ]
       ==> cannot_change_representation1 y);
      (let$ [x] = ["x"] in
       [cannot_change_representation1 x] ==> cannot_change_representation x);
      (* Due to value_kinds rewriting not taking representation changes into
         account for now, blocks cannot have their representation changed, so we
         prevent it here. *)
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ constructor_rel ~base:x field ~from:y;
         filter1
           (fun f ->
             match Field.view f with
             | Block _ | Is_int | Get_tag -> true
             | Value_slot _ | Function_slot _ | Code_of_closure _ | Apply _
             | Code_id_of_call_witness ->
               false)
           field ]
       ==> cannot_change_representation x);
      (* The use of [cannot_change_representation1] is here to still allow
         unboxing of blocks, even if we cannot change their representation due
         to the value_kind limitation. *)
      (let$ [x] = ["x"] in
       [cannot_change_representation1 x] ==> cannot_unbox0 x);
      (* This is repeated from the earlier occurrence in
         [cannot_change_representation0]. It is here because in the future, when
         we want to allow the changing of the representation of local value
         slots, it will remain necessary. *)
      (let$ [x] = ["x"] in
       [any_usage_pred x] ==> cannot_unbox0 x);
      (* (let$ [x; field] = ["x"; "field"] in [ field_of_constructor_is_used x
         field; filter1 field_cannot_be_destructured field ] ==> cannot_unbox0
         x); *)
      (* Unboxing a closure requires changing its calling convention, as we must
         pass the value slots as extra arguments. Thus, we prevent unboxing of
         closures if their calling convention cannot be changed. *)
      (let$ [x; call_witness; codeid] = ["x"; "call_witness"; "codeid"] in
       [ constructor_rel ~base:x
           !!(Field.code_of_closure Known_arity_code_pointer)
           ~from:call_witness;
         constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid;
         cannot_change_calling_convention codeid ]
       ==> cannot_unbox0 x);
      (* An allocation that is one of the results of a function can only be
         unboxed if the function's calling conventation can be changed. *)
      (let$ [alias; allocation_id; relation; call_witness; codeid] =
         ["alias"; "allocation_id"; "relation"; "call_witness"; "codeid"]
       in
       [ sources_rel alias allocation_id;
         rev_constructor_rel ~from:alias relation ~base:call_witness;
         filter1
           (fun f ->
             match[@ocaml.warning "-4"] Field.view f with
             | Apply _ -> true
             | _ -> false)
           relation;
         constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid;
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
         rev_coconstructor_rel ~from:alias relation ~base:call_witness;
         constructor_rel ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid;
         cannot_change_calling_convention codeid ]
       ==> cannot_unbox0 allocation_id);
      (* Cannot unbox parameters of [Indirect_unknown_arity] calls, even if they
         do not escape. *)
      (* (let$ [usage; allocation_id; relation; _v] = ["usage"; "allocation_id";
         "relation"; "_v"] in [ sources_rel usage allocation_id; coaccessor_rel
         usage relation _v; filter (fun [f] -> match CoField.decode f with |
         Param (Unknown_arity_code_pointer, _) -> true | Param
         (Known_arity_code_pointer, _) -> false) [relation] ] ==> cannot_unbox0
         allocation_id); *)
      (* CR ncourant: I'm not sure this is useful? *)
      (* An allocation that is stored in another can only be unboxed if either
         the representation of the other allocation can be changed, of it the
         field it is stored in is never read, as in that case a poison value
         will be stored instead. *)
      (let$ [alias; allocation_id; relation; to_] =
         ["alias"; "allocation_id"; "relation"; "to_"]
       in
       [ sources_rel alias allocation_id;
         rev_constructor_rel ~from:alias relation ~base:to_;
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
         constructor_rel ~base:x field ~from:y;
         filter1 Field.is_function_slot field ]
       ==> cannot_unbox y);
      (* Compute allocations to unbox or to change representation. This requires
         the rules to be executed in order. *)
      (let$ [x] = ["x"] in
       [any_usage_pred x; ~~(cannot_unbox x)] ==> to_unbox x);
      (let$ [x; _y] = ["x"; "_y"] in
       [usages_rel x _y; ~~(cannot_unbox x)] ==> to_unbox x);
      (let$ [x] = ["x"] in
       [any_usage_pred x; ~~(cannot_change_representation x); ~~(to_unbox x)]
       ==> to_change_representation x);
      (let$ [x; _y] = ["x"; "_y"] in
       [usages_rel x _y; ~~(cannot_change_representation x); ~~(to_unbox x)]
       ==> to_change_representation x);
      (let$ [x] = ["x"] in
       [any_source_pred x] ==> multiple_allocation_points x);
      (let$ [x; y; z] = ["x"; "y"; "z"] in
       [sources_rel x y; sources_rel x z; distinct Cols.n y z]
       ==> multiple_allocation_points x);
      (* [allocation_point_dominator x y] is the same as
         [dominated_by_allocation_point y x], which is that [y] is the
         allocation point dominator of [x]. *)
      (let$ [x; y] = ["x"; "y"] in
       [sources_rel x y; ~~(multiple_allocation_points x)]
       ==> and_
             [allocation_point_dominator x y; dominated_by_allocation_point y x])
    ]

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

type 'a block_or_closure_fields =
  | Empty
  | Block_fields of
      { is_int : 'a option;
        get_tag : 'a option;
        fields : (Flambda_kind.t * 'a) option list
      }
  | Closure_fields of 'a Value_slot.Map.t * 'a Function_slot.Map.t
  | Could_not_classify

let classify_field_map fields =
  let r =
    Field.Map.fold
      (fun field x acc ->
        match acc with
        | `Could_not_classify -> `Could_not_classify
        | (`Block_fields _ | `Closure_fields _ | `Empty) as acc -> (
          let[@inline] block_fields k =
            let[@local] k is_int get_tag fields =
              (k [@inlined hint]) ~is_int ~get_tag ~fields
            in
            match acc with
            | `Closure_fields _ -> `Could_not_classify
            | `Block_fields (is_int, get_tag, fields) -> k is_int get_tag fields
            | `Empty -> k None None Numeric_types.Int.Map.empty
          in
          let[@inline] closure_fields k =
            let[@local] k value_slots function_slots =
              (k [@inlined hint]) ~value_slots ~function_slots
            in
            match acc with
            | `Closure_fields (value_slots, function_slots) ->
              k value_slots function_slots
            | `Block_fields _ -> `Could_not_classify
            | `Empty -> k Value_slot.Map.empty Function_slot.Map.empty
          in
          match Field.view field with
          | Code_of_closure _ | Apply _ | Code_id_of_call_witness ->
            `Could_not_classify
          | Value_slot vs ->
            closure_fields (fun ~value_slots ~function_slots ->
                `Closure_fields
                  (Value_slot.Map.add vs x value_slots, function_slots))
          | Function_slot fs ->
            closure_fields (fun ~value_slots ~function_slots ->
                `Closure_fields
                  (value_slots, Function_slot.Map.add fs x function_slots))
          | Is_int ->
            block_fields (fun ~is_int ~get_tag ~fields ->
                assert (Option.is_none is_int);
                `Block_fields (Some x, get_tag, fields))
          | Get_tag ->
            block_fields (fun ~is_int ~get_tag ~fields ->
                assert (Option.is_none get_tag);
                `Block_fields (is_int, Some x, fields))
          | Block (i, kind) ->
            block_fields (fun ~is_int ~get_tag ~fields ->
                if Numeric_types.Int.Map.mem i fields
                then `Could_not_classify
                else
                  `Block_fields
                    ( is_int,
                      get_tag,
                      Numeric_types.Int.Map.add i (kind, x) fields ))))
      fields `Empty
  in
  match r with
  | `Empty -> Empty
  | `Could_not_classify -> Could_not_classify
  | `Closure_fields (vs, fs) -> Closure_fields (vs, fs)
  | `Block_fields (is_int, get_tag, fields) ->
    let n =
      if Numeric_types.Int.Map.is_empty fields
      then 0
      else fst (Numeric_types.Int.Map.max_binding fields) + 1
    in
    let fields =
      List.init n (fun i -> Numeric_types.Int.Map.find_opt i fields)
    in
    Block_fields { is_int; get_tag; fields }

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
                  Field.block i (Flambda_kind.With_subkind.kind kind)
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

let forget_all_types = Sys.getenv_opt "FORGETALL" <> None

module Rewriter = struct
  type t0 =
    | Any_usage
    | Usages of unit Code_id_or_name.Map.t

  type t = result * t0

  type set_of_closures = |

  let compare_t0 x y =
    match x, y with
    | Any_usage, Any_usage -> 0
    | Usages usages1, Usages usages2 ->
      Code_id_or_name.Map.compare Unit.compare usages1 usages2
    | Any_usage, Usages _ -> -1
    | Usages _, Any_usage -> 1

  let compare (_result1, t1) (_result2, t2) = compare_t0 t1 t2

  let print_t0 ff t =
    match t with
    | Any_usage -> Format.fprintf ff "Any_usage"
    | Usages usages ->
      Format.fprintf ff "(Usages %a)" Code_id_or_name.Set.print
        (Code_id_or_name.Map.keys usages)

  let print ppf (_, t) = print_t0 ppf t

  module T = Container_types.Make (struct
    type nonrec t = t

    let compare = compare

    let equal t1 t2 = compare t1 t2 = 0

    let hash _t = failwith "hash"

    let print ff (_result, t) = print_t0 ff t
  end)

  module Map = T.Map

  module CNMSet = Stdlib.Set.Make (struct
    type t = unit Code_id_or_name.Map.t

    let compare = Code_id_or_name.Map.compare Unit.compare
  end)

  let identify_set_of_closures_with_one_code_id :
      Datalog.database -> Code_id.t -> unit Code_id_or_name.Map.t list =
    let open! Fixit in
    run
      (let@ in_ =
         paramc "in_"
           Cols.[n]
           (fun code_id ->
             Code_id_or_name.Map.singleton (Code_id_or_name.code_id code_id) ())
       in
       let+ out =
         let@ out = fix1' (empty Cols.[n; n]) in
         [ (let$ [code_id; witness; closure; function_slot; all_closures] =
              ["code_id"; "witness"; "closure"; "function_slot"; "all_closures"]
            in
            [ in_ % [code_id];
              rev_constructor_rel ~from:code_id
                !!Field.code_id_of_call_witness
                ~base:witness;
              rev_constructor_rel ~from:witness
                !!(Field.code_of_closure Known_arity_code_pointer)
                ~base:closure;
              rev_constructor_rel ~from:closure function_slot ~base:all_closures;
              filter1 Field.is_function_slot function_slot ]
            ==> out % [closure; all_closures]) ]
       in
       List.map snd (Code_id_or_name.Map.bindings out))

  let identify_set_of_closures_with_code_ids db code_ids =
    let code_ids =
      List.filter
        (fun code_id ->
          Compilation_unit.is_current (Code_id.get_compilation_unit code_id))
        code_ids
    in
    match code_ids with
    | [] -> None
    | code_id :: code_ids ->
      let r =
        List.fold_left
          (fun r code_id ->
            CNMSet.inter r
              (CNMSet.of_list
                 (identify_set_of_closures_with_one_code_id db code_id)))
          (CNMSet.of_list
             (identify_set_of_closures_with_one_code_id db code_id))
          code_ids
      in
      if CNMSet.cardinal r = 1 then Some (CNMSet.min_elt r) else None

  type use_of_function_slot =
    | Never_called
    | Only_called_with_known_arity
    | Any_call

  let uses_for_set_of_closures :
      Datalog.database ->
      t0 ->
      Function_slot.t ->
      Code_id.t Or_unknown.t Function_slot.Map.t ->
      t0 * (t0 * use_of_function_slot) Function_slot.Map.t =
    let open! Fixit in
    let stmt =
      let@ in_ = param "in_" Cols.[n] in
      let@ in_fs =
        paramc "in_fs"
          Cols.[f]
          (fun fs -> Field.Map.singleton (Field.function_slot fs) ())
      in
      let@ mk_any = param0 (fun mk_any -> mk_any) in
      let@ code_ids_of_function_slots =
        param0 (fun code_ids_of_function_slots ->
            Function_slot.Map.fold
              (fun fs code_id m ->
                Field.Map.add (Field.function_slot fs) code_id m)
              code_ids_of_function_slots Field.Map.empty)
      in
      let@ in_all_fs =
        local0
          Cols.[f]
          (let+ code_ids_of_function_slots = code_ids_of_function_slots in
           Field.Map.map (fun _ -> ()) code_ids_of_function_slots)
      in
      let@ in_code_id =
        local0
          Cols.[f; n]
          (let+ code_ids_of_function_slots = code_ids_of_function_slots in
           Field.Map.filter_map
             (fun _ (code_id : _ Or_unknown.t) ->
               match code_id with
               | Unknown -> None
               | Known code_id ->
                 Some
                   (Code_id_or_name.Map.singleton
                      (Code_id_or_name.code_id code_id)
                      ()))
             code_ids_of_function_slots)
      in
      let@ in_unknown_code_id =
        local0
          Cols.[f]
          (let+ code_ids_of_function_slots = code_ids_of_function_slots in
           Field.Map.filter_map
             (fun _ (code_id : _ Or_unknown.t) ->
               match code_id with Unknown -> Some () | Known _ -> None)
             code_ids_of_function_slots)
      in
      let+ ([out1; out2; known_arity; unknown_arity; any] :
             _ Datalog.Constant.hlist) =
        let@ [out1; out2; known_arity; unknown_arity; any] =
          fix'
            [ empty Cols.[n];
              empty Cols.[f; n];
              empty Cols.[f];
              empty Cols.[f];
              empty One.cols ]
        in
        [ (let$ [x; y] = ["x"; "y"] in
           [in_fs % [x]; in_ % [y]] ==> out2 % [x; y]);
          (let$ [fs; usage; field; field_usage] =
             ["fs"; "usage"; "field"; "field_usage"]
           in
           [ out2 % [fs; usage];
             field_usages_rel usage field field_usage;
             filter1 Field.is_value_slot field ]
           ==> out1 % [usage]);
          (let$ [fs; usage; field] = ["fs"; "usage"; "field"] in
           [ out2 % [fs; usage];
             field_usages_top_rel usage field;
             filter1 Field.is_value_slot field ]
           ==> out1 % [usage]);
          (let$ [fs0; usage; fs; to_; fs_usage] =
             ["fs0"; "usage"; "fs"; "to_"; "fs_usage"]
           in
           [ out2 % [fs0; usage];
             field_usages_rel usage fs to_;
             in_all_fs % [fs];
             usages_rel to_ fs_usage ]
           ==> out2 % [fs; fs_usage]);
          (let$ [fs; usage] = ["fs"; "usage"] in
           [ out2 % [fs; usage];
             field_usages_top_rel usage
               !!(Field.code_of_closure Known_arity_code_pointer) ]
           ==> known_arity % [fs]);
          (let$ [fs; usage; _v] = ["fs"; "usage"; "_v"] in
           [ out2 % [fs; usage];
             field_usages_rel usage
               !!(Field.code_of_closure Known_arity_code_pointer)
               _v ]
           ==> known_arity % [fs]);
          (let$ [fs; usage] = ["fs"; "usage"] in
           [ out2 % [fs; usage];
             field_usages_top_rel usage
               !!(Field.code_of_closure Unknown_arity_code_pointer) ]
           ==> unknown_arity % [fs]);
          (let$ [fs; usage; _v] = ["fs"; "usage"; "_v"] in
           [ out2 % [fs; usage];
             field_usages_rel usage
               !!(Field.code_of_closure Unknown_arity_code_pointer)
               _v ]
           ==> unknown_arity % [fs]);
          (let$ [fs; code_id; my_closure; usage] =
             ["fs"; "code_id"; "my_closure"; "usage"]
           in
           [ known_arity % [fs];
             in_code_id % [fs; code_id];
             code_id_my_closure_rel ~code_id ~my_closure;
             usages_rel my_closure usage ]
           ==> out2 % [fs; usage]);
          (let$ [fs; code_id; my_closure; usage] =
             ["fs"; "code_id"; "my_closure"; "usage"]
           in
           [ unknown_arity % [fs];
             in_code_id % [fs; code_id];
             code_id_my_closure_rel ~code_id ~my_closure;
             usages_rel my_closure usage ]
           ==> out2 % [fs; usage]);
          (let$ [fs0; x; fs] = ["fs0"; "x"; "fs"] in
           [out2 % [fs0; x]; field_usages_top_rel x fs; in_all_fs % [fs]]
           ==> One.flag any);
          (let$ [fs0; x] = ["fs0"; "x"] in
           [out2 % [fs0; x]; any_usage_pred x] ==> One.flag any);
          (let$ [fs] = ["fs"] in
           [known_arity % [fs]; in_unknown_code_id % [fs]] ==> One.flag any);
          (let$ [fs] = ["fs"] in
           [unknown_arity % [fs]; in_unknown_code_id % [fs]] ==> One.flag any)
        ]
      and+ mk_any = mk_any in
      if One.to_bool any
      then mk_any ()
      else
        ( Usages out1,
          Field.Map.fold
            (fun fs uses m ->
              let calls =
                if Field.Map.mem fs unknown_arity
                then Any_call
                else if Field.Map.mem fs known_arity
                then Only_called_with_known_arity
                else Never_called
              in
              Function_slot.Map.add
                (Field.must_be_function_slot fs)
                (Usages uses, calls) m)
            out2 Function_slot.Map.empty )
    in
    fun db usages current_function_slot code_ids_of_function_slots ->
      let any () =
        ( Any_usage,
          Function_slot.Map.map
            (fun _ -> Any_usage, Any_call)
            code_ids_of_function_slots )
      in
      match usages with
      | Any_usage -> any ()
      | Usages s ->
        Fixit.run stmt db s current_function_slot any code_ids_of_function_slots

  let rec patterns_for_unboxed_fields ~machine_width ~bind_function_slots db
      ~var fields unboxed_fields acc =
    let open Flambda2_types.Rewriter in
    if debug
    then
      Format.eprintf "FIELD_USAGE = %a@.unboxed_fields = %a@."
        (Field.Map.print (fun ff -> function
           | Used_as_top -> Format.fprintf ff "Top"
           | Used_as_vars vs -> Code_id_or_name.Map.print Unit.print ff vs))
        fields
        (Field.Map.print (pp_unboxed_elt (fun ff _ -> Format.fprintf ff "_")))
        unboxed_fields;
    let combined =
      Field.Map.merge
        (fun field field_use unboxed_field ->
          match field_use, unboxed_field with
          | None, None -> assert false
          | Some _, None ->
            None
            (* This should only happen for fields like [Code_of_closure _],
               which are ignored when creating unboxed fields. TODO: check
               this? *)
          | None, Some _ ->
            (* This should not happen if we only start
               [patterns_for_unboxed_fields] on the allocation points
               themselves. *)
            Misc.fatal_errorf
              "In [patterns_for_unboxed_fields], field %a existed in \
               [unboxed_fields] but not in [fields]"
              Field.print field
          | Some field_use, Some unboxed_fields ->
            Some (field_use, unboxed_fields))
        fields unboxed_fields
    in
    let rec forget unboxed_fields acc =
      match unboxed_fields with
      | Not_unboxed x -> (None, x) :: acc
      | Unboxed unboxed_fields ->
        Field.Map.fold
          (fun _ unboxed_fields acc -> forget unboxed_fields acc)
          unboxed_fields acc
    in
    let for_one_use (field_use, unboxed_fields) acc =
      match unboxed_fields with
      | Not_unboxed x ->
        let v = Var.create () in
        (Some v, x) :: acc, Pattern.var v (var x field_use)
      | Unboxed unboxed_fields -> (
        match field_use with
        | Used_as_top ->
          Misc.fatal_errorf
            "In [patterns_for_unboxed_fields], field was unboxed but has \
             [Used_as_top] usage"
        | Used_as_vars flow_to ->
          let fields =
            get_fields db
              (get_all_usages ~follow_known_arity_calls:true db flow_to)
          in
          patterns_for_unboxed_fields ~machine_width ~bind_function_slots:None
            db ~var fields unboxed_fields acc)
    in
    let[@local] closure value_slots =
      let value_slots = Value_slot.Map.bindings value_slots in
      let acc, pats =
        List.fold_left_map
          (fun acc (value_slot, use) ->
            let acc, pat = for_one_use use acc in
            acc, Pattern.value_slot value_slot pat)
          acc value_slots
      in
      let pats =
        match bind_function_slots with None -> pats | Some p -> p @ pats
      in
      acc, Pattern.closure pats
    in
    match classify_field_map combined with
    | Empty when Option.is_some bind_function_slots ->
      closure Value_slot.Map.empty
    | Empty | Could_not_classify ->
      ( Field.Map.fold
          (fun _ (_, unboxed_fields) acc -> forget unboxed_fields acc)
          combined acc,
        Pattern.any )
    | Block_fields { is_int; get_tag; fields } ->
      if Option.is_some bind_function_slots
      then
        Misc.fatal_errorf
          "[patterns_for_unboxed_fields] sees a block but needs to bind \
           function slots";
      let acc =
        match is_int with
        | None -> acc
        | Some (_, unboxed_fields) -> forget unboxed_fields acc
      in
      let acc =
        match get_tag with
        | None -> acc
        | Some (_, unboxed_fields) -> forget unboxed_fields acc
      in
      let acc = ref acc in
      let pats = ref [] in
      List.iteri
        (fun i use ->
          match use with
          | None -> ()
          | Some (kind, use) ->
            let nacc, pat = for_one_use use !acc in
            acc := nacc;
            pats
              := Pattern.block_field
                   (Target_ocaml_int.of_int machine_width i)
                   kind pat
                 :: !pats)
        fields;
      !acc, Pattern.block !pats
    | Closure_fields (value_slots, function_slots) ->
      assert (Function_slot.Map.is_empty function_slots);
      closure value_slots

  let rewrite (result, usages) typing_env flambda_type =
    let open Flambda2_types.Rewriter in
    let db = result.db in
    let[@local] forget_type () =
      Rule.rewrite Pattern.any (Expr.unknown (Flambda2_types.kind flambda_type))
    in
    if debug then Format.eprintf "REWRITE usages = %a@." print_t0 usages;
    if forget_all_types
    then forget_type ()
    else if match usages with
            | Any_usage -> false
            | Usages m -> Code_id_or_name.Map.is_empty m
    then
      (* No usages, this might have been deleted: convert to Unknown *)
      forget_type ()
    else
      match
        Flambda2_types.meet_single_closures_entry typing_env flambda_type
      with
      | Invalid ->
        (* Not a closure. For now, we can never change the representation of
           this, so no rewrite is necessary. *)
        Rule.identity
      | Need_meet ->
        (* Multiple closures are possible. We are never able to use this
           information currently; convert to Unknown. *)
        forget_type ()
      | Known_result (function_slot, alloc_mode, closures_entry, _function_type)
        -> (
        let value_slot_types =
          Flambda2_types.Closures_entry.value_slot_types closures_entry
        in
        let function_slot_types =
          Flambda2_types.Closures_entry.function_slot_types closures_entry
        in
        let code_id_of_function_slots =
          Function_slot.Map.mapi
            (fun function_slot _ ->
              let function_type =
                Closures_entry.find_function_type closures_entry function_slot
              in
              Or_unknown.map function_type ~f:Function_type.code_id)
            function_slot_types
        in
        let usages_for_value_slots, usages_of_function_slots =
          uses_for_set_of_closures db usages function_slot
            code_id_of_function_slots
        in
        let[@local] change_representation_of_closures fields value_slots_reprs
            function_slots_reprs =
          let patterns = ref [] in
          if debug
          then (
            Format.eprintf "OLD type: %a@." Flambda2_types.print flambda_type;
            Format.eprintf "OLD->NEW function slots: %a@."
              (Function_slot.Map.print Function_slot.print)
              function_slots_reprs);
          let all_function_slots_in_set =
            Function_slot.Map.fold
              (fun function_slot (_, uses) m ->
                if debug
                then
                  Format.eprintf "OLD function slot: %a@." Function_slot.print
                    function_slot;
                let new_function_slot =
                  Function_slot.Map.find function_slot function_slots_reprs
                in
                let r =
                  match uses with
                  | Never_called -> Or_unknown.Unknown
                  | Only_called_with_known_arity | Any_call ->
                    let v = Var.create () in
                    patterns
                      := Pattern.rec_info function_slot
                           (Pattern.var v (result, Any_usage))
                         :: !patterns;
                    let function_type =
                      Flambda2_types.Closures_entry.find_function_type
                        closures_entry function_slot
                    in
                    Or_unknown.map function_type ~f:(fun function_type ->
                        Expr.Function_type.create
                          (Function_type.code_id function_type)
                          ~rec_info:(Expr.var v))
                in
                Function_slot.Map.add new_function_slot r m)
              usages_of_function_slots Function_slot.Map.empty
          in
          let all_closure_types_in_set =
            Function_slot.Map.fold
              (fun function_slot (metadata, _uses) m ->
                let v = Var.create () in
                patterns
                  := Pattern.function_slot function_slot
                       (Pattern.var v (result, metadata))
                     :: !patterns;
                let new_function_slot =
                  Function_slot.Map.find function_slot function_slots_reprs
                in
                Function_slot.Map.add new_function_slot (Expr.var v) m)
              usages_of_function_slots Function_slot.Map.empty
          in
          let bind_function_slots = Some !patterns in
          let bound, pat =
            patterns_for_unboxed_fields
              ~machine_width:(Typing_env.machine_width typing_env)
              ~bind_function_slots db
              ~var:(fun _ field_use ->
                let metadata =
                  match field_use with
                  | Used_as_top -> Any_usage
                  | Used_as_vars flow_to ->
                    let usages = get_direct_usages result.db flow_to in
                    Usages usages
                in
                result, metadata)
              fields value_slots_reprs []
          in
          let all_value_slots_in_set =
            List.fold_left
              (fun m (var, value_slot) ->
                let e =
                  match var with
                  | None -> Expr.unknown (Value_slot.kind value_slot)
                  | Some var -> Expr.var var
                in
                Value_slot.Map.add value_slot e m)
              Value_slot.Map.empty bound
          in
          let new_function_slot =
            Function_slot.Map.find function_slot function_slots_reprs
          in
          Rule.rewrite pat
            (Expr.exactly_this_closure new_function_slot
               ~all_function_slots_in_set ~all_closure_types_in_set
               ~all_value_slots_in_set alloc_mode)
        in
        let[@local] no_representation_change function_slot value_slots_metadata
            function_slots_metadata_and_uses =
          let all_patterns = ref [] in
          let all_value_slots_in_set =
            Value_slot.Map.mapi
              (fun value_slot metadata ->
                let v = Var.create () in
                all_patterns
                  := Pattern.value_slot value_slot
                       (Pattern.var v (result, metadata))
                     :: !all_patterns;
                Expr.var v)
              value_slots_metadata
          in
          let all_closure_types_in_set =
            Function_slot.Map.mapi
              (fun function_slot (metadata, _uses) ->
                let v = Var.create () in
                all_patterns
                  := Pattern.function_slot function_slot
                       (Pattern.var v (result, metadata))
                     :: !all_patterns;
                Expr.var v)
              function_slots_metadata_and_uses
          in
          let all_function_slots_in_set =
            Function_slot.Map.mapi
              (fun function_slot (_, uses) ->
                match uses with
                | Never_called -> Or_unknown.Unknown
                | Only_called_with_known_arity | Any_call ->
                  let v = Var.create () in
                  all_patterns
                    := Pattern.rec_info function_slot
                         (Pattern.var v (result, Any_usage))
                       :: !all_patterns;
                  let function_type =
                    Flambda2_types.Closures_entry.find_function_type
                      closures_entry function_slot
                  in
                  Or_unknown.map function_type ~f:(fun function_type ->
                      Expr.Function_type.create
                        (Function_type.code_id function_type)
                        ~rec_info:(Expr.var v)))
              function_slots_metadata_and_uses
          in
          Rule.rewrite
            (Pattern.closure !all_patterns)
            (Expr.exactly_this_closure function_slot ~all_function_slots_in_set
               ~all_closure_types_in_set ~all_value_slots_in_set alloc_mode)
        in
        match usages_for_value_slots with
        | Usages usages_for_value_slots ->
          if Code_id_or_name.Map.exists
               (fun clos () ->
                 Code_id_or_name.Map.mem clos result.changed_representation)
               usages_for_value_slots
          then (
            assert (
              Code_id_or_name.Map.for_all
                (fun clos () ->
                  Code_id_or_name.Map.mem clos result.changed_representation)
                usages_for_value_slots);
            if debug
            then
              Format.eprintf "USAGES_FOR_VALUE_SLOTS is: %a@."
                (Code_id_or_name.Map.print Unit.print)
                usages_for_value_slots;
            let changed_representation =
              Code_id_or_name.Map.bindings
                (Code_id_or_name.Map.mapi
                   (fun clos () ->
                     Code_id_or_name.Map.find clos result.changed_representation)
                   usages_for_value_slots)
            in
            let value_slots_reprs, function_slots_reprs, alloc_point =
              match snd (List.hd changed_representation) with
              | ( Closure_representation
                    (value_slots_reprs, function_slots_reprs, _),
                  alloc_point ) ->
                value_slots_reprs, function_slots_reprs, alloc_point
              | Block_representation _, _ ->
                Misc.fatal_errorf
                  "Changed representation of a closure with \
                   [Block_representation]"
            in
            List.iter
              (fun (_, (repr, alloc_point')) ->
                assert (alloc_point == alloc_point');
                match repr with
                | Closure_representation (vs, fs, _) ->
                  if vs != value_slots_reprs || fs != function_slots_reprs
                  then
                    Misc.fatal_errorf
                      "In set of closures, all closures do not have the same \
                       representation changes."
                | Block_representation _ ->
                  Misc.fatal_errorf
                    "Changed representation of a closure with \
                     [Block_representation]")
              changed_representation;
            let fields =
              get_fields_usage_of_constructors db
                (Code_id_or_name.Map.singleton alloc_point ())
            in
            change_representation_of_closures fields value_slots_reprs
              function_slots_reprs)
          else
            let usages_of_value_slots =
              Value_slot.Map.mapi
                (fun value_slot _value_slot_type ->
                  match
                    get_one_field db
                      (Field.value_slot value_slot)
                      (Usages usages_for_value_slots)
                  with
                  | Used_as_top -> Any_usage
                  | Used_as_vars vs -> Usages (get_direct_usages db vs))
                value_slot_types
            in
            no_representation_change function_slot usages_of_value_slots
              usages_of_function_slots
        | Any_usage ->
          let is_local_value_slot vs _ =
            Compilation_unit.is_current (Value_slot.get_compilation_unit vs)
          in
          let is_local_function_slot fs _ =
            Compilation_unit.is_current (Function_slot.get_compilation_unit fs)
          in
          if Value_slot.Map.exists is_local_value_slot value_slot_types
             || Function_slot.Map.exists is_local_function_slot
                  function_slot_types
          then (
            if not
                 (Value_slot.Map.for_all is_local_value_slot value_slot_types
                 && Function_slot.Map.for_all is_local_function_slot
                      function_slot_types)
            then
              Misc.fatal_errorf
                "Some slots in this closure are local while other are not:@\n\
                 Value slots: %a@\n\
                 Function slots: %a@." Value_slot.Set.print
                (Value_slot.Map.keys value_slot_types)
                Function_slot.Set.print
                (Function_slot.Map.keys function_slot_types);
            let code_ids =
              Function_slot.Map.fold
                (fun function_slot _ l ->
                  match
                    Flambda2_types.Closures_entry.find_function_type
                      closures_entry function_slot
                  with
                  | Unknown -> l
                  | Known function_type ->
                    Function_type.code_id function_type :: l)
                function_slot_types []
            in
            let set_of_closures =
              identify_set_of_closures_with_code_ids db code_ids
            in
            match set_of_closures with
            | None -> forget_type ()
            | Some set_of_closures ->
              let fields =
                get_fields_usage_of_constructors db set_of_closures
              in
              if debug
              then
                Format.eprintf "ZZZ: %a@."
                  (Field.Map.print (fun ff t ->
                       match t with
                       | Used_as_top -> Format.fprintf ff "Top"
                       | Used_as_vars m ->
                         Code_id_or_name.Map.print Unit.print ff m))
                  fields;
              if Code_id_or_name.Map.exists
                   (fun clos () ->
                     Code_id_or_name.Map.mem clos result.changed_representation)
                   set_of_closures
              then (
                assert (
                  Code_id_or_name.Map.for_all
                    (fun clos () ->
                      Code_id_or_name.Map.mem clos result.changed_representation)
                    set_of_closures);
                let changed_representation =
                  Code_id_or_name.Map.bindings
                    (Code_id_or_name.Map.mapi
                       (fun clos () ->
                         fst
                           (Code_id_or_name.Map.find clos
                              result.changed_representation))
                       set_of_closures)
                in
                let value_slots_reprs, function_slots_reprs =
                  match snd (List.hd changed_representation) with
                  | Closure_representation
                      (value_slots_reprs, function_slots_reprs, _) ->
                    value_slots_reprs, function_slots_reprs
                  | Block_representation _ ->
                    Misc.fatal_errorf
                      "Changed representation of a closure with \
                       [Block_representation]"
                in
                List.iter
                  (fun (_, repr) ->
                    match repr with
                    | Closure_representation (vs, fs, _) ->
                      if vs != value_slots_reprs || fs != function_slots_reprs
                      then
                        Misc.fatal_errorf
                          "In set of closures, all closures do not have the \
                           same representation changes."
                    | Block_representation _ ->
                      Misc.fatal_errorf
                        "Changed representation of a closure with \
                         [Block_representation]")
                  changed_representation;
                change_representation_of_closures fields value_slots_reprs
                  function_slots_reprs)
              else
                no_representation_change function_slot
                  (Value_slot.Map.mapi
                     (fun value_slot _value_slot_type ->
                       match
                         Field.Map.find_opt (Field.value_slot value_slot) fields
                       with
                       | Some Used_as_top -> Any_usage
                       | Some (Used_as_vars vs) ->
                         Usages (get_direct_usages db vs)
                       | None -> Usages Code_id_or_name.Map.empty)
                     value_slot_types)
                  usages_of_function_slots)
          else
            no_representation_change function_slot
              (Value_slot.Map.map
                 (fun _value_slot_type -> Any_usage)
                 value_slot_types)
              usages_of_function_slots)

  let block_slot ?tag:_ (result, t) index _typing_env flambda_type =
    let r =
      match t with
      | Any_usage -> result, Any_usage
      | Usages usages -> (
        let field_kind = Flambda2_types.kind flambda_type in
        let field = Field.block (Target_ocaml_int.to_int index) field_kind in
        match get_one_field result.db field (Usages usages) with
        | Used_as_top -> result, Any_usage
        | Used_as_vars vs ->
          let usages = get_direct_usages result.db vs in
          result, Usages usages)
    in
    if debug
    then (
      Format.eprintf "%a -[%d]-> %a@." print_t0 t
        (Target_ocaml_int.to_int index)
        print_t0 (snd r);
      Format.eprintf "%a@." Flambda2_types.print flambda_type);
    r

  let array_slot (result, _t) _index _typing_env _flambda_type =
    (* Array primitives are opaque. Thus, anything put inside the array when it
       was created has been treated as escaping, thus giving a [Any_usage]
       result. *)
    result, Any_usage

  let set_of_closures _t _function_slot _typing_env _closures_entry =
    Misc.fatal_error
      "[set_of_closures] should never be called, because all set of closures \
       should be handled by [rewrite]"

  let value_slot (s : set_of_closures) _value_slot _typing_env _flambda_type =
    match s with _ -> .

  let function_slot (s : set_of_closures) _function_slot _typing_env
      _flambda_type =
    match s with _ -> .

  let rec_info _typing_env (s : set_of_closures) _function_slot _code_id
      _flambda_type =
    match s with _ -> .
end

module TypesRewrite = Flambda2_types.Rewriter.Make (Rewriter)

let rewrite_typing_env result ~unit_symbol vars_to_keep typing_env =
  if debug
  then Format.eprintf "OLD typing env: %a@." Typing_env.print typing_env;
  let db = result.db in
  let symbol_metadata sym =
    if Symbol.equal sym unit_symbol
       || (not (Compilation_unit.is_current (Symbol.compilation_unit sym)))
       || is_top db (Code_id_or_name.symbol sym)
    then result, Rewriter.Any_usage
    else
      ( result,
        Rewriter.Usages
          (get_direct_usages db
             (Code_id_or_name.Map.singleton (Code_id_or_name.symbol sym) ())) )
  in
  let variable_metadata var =
    let kind = Variable.kind var in
    let metadata =
      if is_top db (Code_id_or_name.var var)
      then result, Rewriter.Any_usage
      else
        ( result,
          Rewriter.Usages
            (get_direct_usages db
               (Code_id_or_name.Map.singleton (Code_id_or_name.var var) ())) )
    in
    metadata, kind
  in
  let r =
    TypesRewrite.rewrite typing_env symbol_metadata
      (List.fold_left
         (fun m v -> Variable.Map.add v (variable_metadata v) m)
         Variable.Map.empty vars_to_keep)
  in
  if debug then Format.eprintf "NEW typing env: %a@." Typing_env.print r;
  r

let rewrite_result_types result ~old_typing_env func_params func_results
    result_types =
  if debug
  then Format.eprintf "OLD result types: %a@." Result_types.print result_types;
  let params, results, env_extension =
    Result_types.pattern_match result_types ~f:(fun ~params ~results tee ->
        params, results, tee)
  in
  let variable_pattern var =
    let kind = Variable.kind var in
    let name = Variable.name var in
    let var = Code_id_or_name.var var in
    let db = result.db in
    if Code_id_or_name.Map.mem var result.unboxed_fields
    then (
      let unboxed_fields = Code_id_or_name.Map.find var result.unboxed_fields in
      if is_top db var
      then
        Misc.fatal_errorf "In [rewrite_result_types], var %a is unboxed but top"
          Code_id_or_name.print var;
      let fields =
        get_fields db
          (get_all_usages ~follow_known_arity_calls:true db
             (Code_id_or_name.Map.singleton var ()))
      in
      let bound, pat =
        Rewriter.patterns_for_unboxed_fields
          ~machine_width:(Typing_env.machine_width old_typing_env)
          ~bind_function_slots:None db
          ~var:(fun v field_use ->
            let metadata =
              match field_use with
              | Used_as_top -> Rewriter.Any_usage
              | Used_as_vars flow_to ->
                let usages = get_direct_usages db flow_to in
                Rewriter.Usages usages
            in
            Variable.name v, (result, metadata))
          fields unboxed_fields []
      in
      let all_vars =
        List.rev_map
          (fun (pattern_var, v) ->
            if Option.is_none pattern_var
            then
              Format.eprintf
                "In [rewrite_result_types], could not get a pattern variable \
                 for unboxed var %a@."
                Variable.print v;
            Option.get pattern_var)
          bound
      in
      (pat, kind), all_vars)
    else
      let metadata =
        if is_top db var
        then result, Rewriter.Any_usage
        else
          ( result,
            Rewriter.Usages
              (get_direct_usages db (Code_id_or_name.Map.singleton var ())) )
      in
      let v = Flambda2_types.Rewriter.Var.create () in
      let pat = Flambda2_types.Rewriter.Pattern.var v (name, metadata) in
      (pat, kind), [v]
  in
  let patterns_list func_vars type_vars =
    let patterns, vars =
      List.fold_left2
        (fun (patterns, vars) funcv typev ->
          let pat, vs = variable_pattern funcv in
          ( Variable.Map.add (Bound_parameter.var typev) pat patterns,
            List.rev_append vs vars ))
        (Variable.Map.empty, []) func_vars
        (Bound_parameters.to_list type_vars)
    in
    patterns, List.rev vars
  in
  let params_patterns, params_vars = patterns_list func_params params in
  let results_patterns, results_vars = patterns_list func_results results in
  let new_vars, new_env_extension =
    TypesRewrite.rewrite_env_extension_with_extra_variables old_typing_env
      (Variable.Map.disjoint_union params_patterns results_patterns)
      env_extension
      (params_vars @ results_vars)
  in
  let make_bp vars =
    Bound_parameters.create
      (List.map
         (fun v ->
           let var = Flambda2_types.Rewriter.Var.Map.find v new_vars in
           Bound_parameter.create var
             (Flambda_kind.With_subkind.anything (Variable.kind var))
             Flambda_debug_uid.none)
         vars)
  in
  let new_result_types =
    Result_types.create ~params:(make_bp params_vars)
      ~results:(make_bp results_vars) new_env_extension
  in
  if debug
  then
    Format.eprintf "NEW result\n   types: %a@." Result_types.print
      new_result_types;
  new_result_types

type single_field_source =
  | No_source
  | One of Code_id_or_name.t
  | Many

let get_single_field_source =
  let q_any_source1 =
    mk_exists_query ["block"; "field"] [] (fun [block; field] [] ->
        [field_sources_top_rel block field])
  in
  let q_any_source2 =
    mk_exists_query ["block"; "field"] ["source"]
      (fun [block; field] [source] ->
        [field_sources_rel block field source; any_source_pred source])
  in
  let q_source =
    query
      (let^ [block; field] = ["block"; "field"] in
       let$ [field_source; source] = ["field_source"; "source"] in
       [ field_sources_rel block field field_source;
         sources_rel field_source source ]
       =>? [source])
  in
  fun db block field ->
    if q_any_source1 [block; field] db || q_any_source2 [block; field] db
    then Many
    else
      Cursor.fold_with_parameters q_source [block; field] db ~init:No_source
        ~f:(fun [source] acc ->
          match acc with No_source -> One source | One _ | Many -> Many)

let rec mk_unboxed_fields ~has_to_be_unboxed ~mk db unboxed_block fields
    name_prefix =
  Field.Map.filter_map
    (fun field field_use ->
      match Field.view field with
      | Function_slot _ | Code_id_of_call_witness | Apply _ -> assert false
      | Code_of_closure _ -> None
      | Block _ | Value_slot _ | Is_int | Get_tag -> (
        let field_source = get_single_field_source db unboxed_block field in
        (* Format.eprintf "SOURCE OF %a %a : %a@." Code_id_or_name.print
           unboxed_block Field.print field (fun ff -> function | No_source ->
           Format.fprintf ff "No_source" | Many -> Format.fprintf ff "Many" |
           One x -> Format.fprintf ff "One %a" Code_id_or_name.print x)
           field_source; *)
        match field_source with
        | No_source -> None
        | One _ | Many -> (
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
              let new_unboxed_block =
                match field_source with
                | No_source -> assert false
                | Many ->
                  Misc.fatal_errorf
                    "[mk_unboxed_fields]: unboxed fields, but [Many] sources"
                | One v -> v
              in
              let unboxed_fields =
                mk_unboxed_fields ~has_to_be_unboxed ~mk db new_unboxed_block
                  (get_fields db
                     (get_all_usages ~follow_known_arity_calls:true db flow_to))
                  new_name
              in
              if false && Field.Map.is_empty unboxed_fields
              then None
              else Some (Unboxed unboxed_fields)
            else if Code_id_or_name.Map.exists
                      (fun k () -> has_to_be_unboxed k)
                      flow_to
            then
              Misc.fatal_errorf
                "Field %a of %s flows to both unboxed and non-unboxed variables"
                Field.print field name_prefix
            else default ())))
    fields

let has_to_be_unboxed =
  mk_exists_query ["x"] ["alloc_point"] (fun [x] [alloc_point] ->
      [allocation_point_dominator x alloc_point; to_unbox alloc_point])

let query_to_unbox =
  query
    (let$ [x; y] = ["x"; "y"] in
     [to_unbox x; dominated_by_allocation_point x y] =>? [x; y])

let query_to_change_representation =
  query
    (let$ [x] = ["x"] in
     [to_change_representation x] =>? [x])

let query_dominated_by =
  query
    (let^$ [x], [y] = ["x"], ["y"] in
     [dominated_by_allocation_point x y] =>? [y])

let fixpoint (graph : Global_flow_graph.graph) =
  let datalog = Global_flow_graph.to_datalog graph in
  let stats = Datalog.Schedule.create_stats datalog in
  let db = Datalog.Schedule.run ~stats datalog_schedule datalog in
  let db =
    List.fold_left
      (fun db rule -> Datalog.Schedule.run ~stats rule db)
      db datalog_rules
  in
  if debug then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
  if Sys.getenv_opt "DUMPDB" <> None then Format.eprintf "%a@." Datalog.print db;
  let has_to_be_unboxed code_or_name = has_to_be_unboxed [code_or_name] db in
  let unboxed =
    Datalog.Cursor.fold query_to_unbox db ~init:Code_id_or_name.Map.empty
      ~f:(fun [code_or_name; to_patch] unboxed ->
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
            db code_or_name
            (get_fields db
               (get_all_usages ~follow_known_arity_calls:true db
                  (Code_id_or_name.Map.singleton to_patch ())))
            new_name
        in
        Code_id_or_name.Map.add to_patch fields unboxed)
  in
  if debug
  then
    Format.printf "TO UNBOX: %a@."
      (Code_id_or_name.Map.print
         (Field.Map.print (pp_unboxed_elt Variable.print)))
      unboxed;
  let changed_representation = ref Code_id_or_name.Map.empty in
  Datalog.Cursor.iter query_to_change_representation db
    ~f:(fun [code_id_or_name] ->
      (* This can happen because we change the representation of each function
         slot of a set of closures at the same time. *)
      if Code_id_or_name.Map.mem code_id_or_name !changed_representation
      then ()
      else
        let add_to_s repr alloc_point =
          Datalog.Cursor.iter_with_parameters query_dominated_by [alloc_point]
            db ~f:(fun [c] ->
              changed_representation
                := Code_id_or_name.Map.add c (repr, alloc_point)
                     !changed_representation)
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
          let repr =
            mk_unboxed_fields ~has_to_be_unboxed ~mk db code_id_or_name
              (get_fields db uses) ""
          in
          add_to_s (Block_representation (repr, !r + 1)) code_id_or_name
        | Set_of_closures l ->
          let mk kind name =
            Value_slot.create
              (Compilation_unit.get_current_exn ())
              ~name ~is_always_immediate:false kind
          in
          let fields =
            get_fields_usage_of_constructors db
              (List.fold_left
                 (fun acc (_, x) -> Code_id_or_name.Map.add x () acc)
                 Code_id_or_name.Map.empty l)
          in
          let repr =
            mk_unboxed_fields ~has_to_be_unboxed ~mk db code_id_or_name fields
              "unboxed"
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
            l);
  if debug
  then
    Format.eprintf "@.TO_CHG: %a@."
      (Code_id_or_name.Map.print (fun ff (repr, alloc_point) ->
           Format.fprintf ff "[from %a]%a" Code_id_or_name.print alloc_point
             pp_changed_representation repr))
      !changed_representation;
  let no_unbox = Sys.getenv_opt "NOUNBOX" <> None in
  { db;
    unboxed_fields = (if no_unbox then Code_id_or_name.Map.empty else unboxed);
    changed_representation =
      (if no_unbox then Code_id_or_name.Map.empty else !changed_representation)
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
    if any_usage_pred_query [v] db
    then "22"
    else if has_use db v
    then "88"
    else "ff"
  in
  let blue =
    if any_source_query [v] db
    then "22"
    else if has_source db v
    then "88"
    else "ff"
  in
  "#" ^ red ^ green ^ blue

let get_unboxed_fields uses cn =
  Code_id_or_name.Map.find_opt cn uses.unboxed_fields

let get_changed_representation uses cn =
  Option.map fst (Code_id_or_name.Map.find_opt cn uses.changed_representation)

let has_use uses v = has_use uses.db v

let field_used uses v f = field_used uses.db v f

let cofield_has_use uses v f = cofield_has_use uses.db v f

let has_source uses v = has_source uses.db v

let not_local_field_has_source uses v f = not_local_field_has_source uses.db v f

let cannot_change_calling_convention_query =
  mk_exists_query ["X"] [] (fun [x] [] -> [cannot_change_calling_convention x])

let cannot_change_calling_convention uses v =
  (not (Compilation_unit.is_current (Code_id.get_compilation_unit v)))
  || cannot_change_calling_convention_query [Code_id_or_name.code_id v] uses.db

let unknown_code_id_actually_directly_called_query =
  mk_exists_query ["set_of_closures"] ["known_arity_call_witness"]
    (fun [set_of_closures] [known_arity_call_witness] ->
      [ rev_accessor_rel ~base:set_of_closures
          !!(Field.code_of_closure Known_arity_code_pointer)
          ~to_:known_arity_call_witness;
        any_source_pred known_arity_call_witness ])

let code_id_actually_directly_called_query =
  query
    (let^$ [set_of_closures], [apply_widget; call_witness; codeid] =
       ["set_of_closures"], ["apply_widget"; "call_withness"; "codeid"]
     in
     [ rev_accessor_rel ~base:set_of_closures
         !!(Field.code_of_closure Known_arity_code_pointer)
         ~to_:apply_widget;
       sources_rel apply_widget call_witness;
       constructor_rel ~base:call_witness
         !!Field.code_id_of_call_witness
         ~from:codeid ]
     =>? [codeid])

let code_id_actually_directly_called uses v =
  if unknown_code_id_actually_directly_called_query
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
