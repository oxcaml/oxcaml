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

module NN = Datalog.Schema.Relation2 (Code_id_or_name) (Code_id_or_name)
module NFN =
  Datalog.Schema.Relation3 (Code_id_or_name) (Field) (Code_id_or_name)
module NCN =
  Datalog.Schema.Relation3 (Code_id_or_name) (Cofield) (Code_id_or_name)
module NNN =
  Datalog.Schema.Relation3 (Code_id_or_name) (Code_id_or_name) (Code_id_or_name)
module N = Datalog.Schema.Relation1 (Code_id_or_name)

type graph =
  { mutable alias : NN.t;
    mutable use : NN.t;
    mutable accessor : NFN.t;
    mutable constructor : NFN.t;
    mutable argument : NCN.t;
    mutable parameter : NCN.t;
    mutable propagate : NNN.t;
    mutable alias_if_any_source : NNN.t;
    mutable any_usage : N.t;
    mutable any_source : N.t;
    mutable zero_alloc_source : N.t;
    mutable code_id_my_closure : NN.t
  }

let print_iter_edges ~print_edge graph =
  let iter_inner color target m =
    Code_id_or_name.Map.iter
      (fun source () -> print_edge (source, target, color))
      m
  in
  let iter_nn color m = Code_id_or_name.Map.iter (iter_inner color) m in
  let iter_nfn color m =
    Code_id_or_name.Map.iter
      (fun target m -> Field.Map.iter (fun _ m -> iter_inner color target m) m)
      m
  in
  let iter_ncn color m =
    Code_id_or_name.Map.iter
      (fun target m ->
        Cofield.Map.iter (fun _ m -> iter_inner color target m) m)
      m
  in
  iter_nn "black" graph.alias;
  iter_nn "red" graph.use;
  iter_nfn "green" graph.accessor;
  iter_nfn "blue" graph.constructor;
  iter_ncn "darkgreen" graph.argument;
  iter_ncn "darkblue" graph.parameter;
  Code_id_or_name.Map.iter
    (fun _if_used m -> iter_nn "purple" m)
    graph.propagate;
  Code_id_or_name.Map.iter
    (fun _if_any_source m -> iter_nn "orange" m)
    graph.alias_if_any_source

let alias = NN.create ~name:"alias"

let use = NN.create ~name:"use"

let accessor = NFN.create ~name:"accessor"

let constructor = NFN.create ~name:"constructor"

let argument = NCN.create ~name:"argument"

let parameter = NCN.create ~name:"parameter"

let propagate = NNN.create ~name:"propagate"

let alias_if_any_source = NNN.create ~name:"alias_if_any_source"

let any_usage = N.create ~name:"any_usage"

let any_source = N.create ~name:"any_source"

let zero_alloc_source = N.create ~name:"zero_alloc_source"

let code_id_my_closure = NN.create ~name:"code_id_my_closure"

let to_datalog graph =
  Datalog.set_table alias graph.alias
  @@ Datalog.set_table use graph.use
  @@ Datalog.set_table accessor graph.accessor
  @@ Datalog.set_table constructor graph.constructor
  @@ Datalog.set_table argument graph.argument
  @@ Datalog.set_table parameter graph.parameter
  @@ Datalog.set_table propagate graph.propagate
  @@ Datalog.set_table alias_if_any_source graph.alias_if_any_source
  @@ Datalog.set_table any_usage graph.any_usage
  @@ Datalog.set_table any_source graph.any_source
  @@ Datalog.set_table zero_alloc_source graph.zero_alloc_source
  @@ Datalog.set_table code_id_my_closure graph.code_id_my_closure
  @@ Datalog.empty

module Relations = struct
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  (* Naming:
   * to_ = from; (alias)
   * to_ = [...] from (use)
   * to_ = base.relation (accessor)
   * base = Make_block { from_ } (constructor)
   *)

  let alias ~to_ ~from = Datalog.atom alias [to_; from]

  let use ~to_ ~from = Datalog.atom use [to_; from]

  let accessor ~to_ relation ~base = Datalog.atom accessor [to_; relation; base]

  let constructor ~base relation ~from =
    Datalog.atom constructor [base; relation; from]

  let argument ~from relation ~base =
    Datalog.atom argument [from; relation; base]

  let parameter ~base relation ~to_ =
    Datalog.atom parameter [base; relation; to_]

  let propagate ~if_used ~to_ ~from = Datalog.atom propagate [if_used; to_; from]

  let alias_if_any_source ~if_any_source ~to_ ~from =
    Datalog.atom alias_if_any_source [if_any_source; to_; from]

  let any_usage var = Datalog.atom any_usage [var]

  let any_source var = Datalog.atom any_source [var]

  let zero_alloc_source var = Datalog.atom zero_alloc_source [var]

  let code_id_my_closure ~code_id ~my_closure =
    Datalog.atom code_id_my_closure [code_id; my_closure]
end

let create () =
  { alias = NN.empty;
    use = NN.empty;
    accessor = NFN.empty;
    constructor = NFN.empty;
    argument = NCN.empty;
    parameter = NCN.empty;
    propagate = NNN.empty;
    alias_if_any_source = NNN.empty;
    any_usage = N.empty;
    any_source = N.empty;
    zero_alloc_source = N.empty;
    code_id_my_closure = NN.empty
  }

(* CR mvellacott: This is a naive union: nodes with the same identity (such as a
   symbol defined in one unit and used from another) are combined, but the
   conservative facts each unit's traversal records at its boundary (e.g.
   [any_source] on imported symbols) are all kept. This means we can't yet
   determine anything more from analysis of the combined graph than we could
   from analysis of per-unit graphs. *)
let union g1 g2 =
  (* Relations can carry data on each edge, but for these types it is always
     unit. *)
  let keep () () = Some () in
  { alias = NN.union keep g1.alias g2.alias;
    use = NN.union keep g1.use g2.use;
    accessor = NFN.union keep g1.accessor g2.accessor;
    constructor = NFN.union keep g1.constructor g2.constructor;
    argument = NCN.union keep g1.argument g2.argument;
    parameter = NCN.union keep g1.parameter g2.parameter;
    propagate = NNN.union keep g1.propagate g2.propagate;
    alias_if_any_source =
      NNN.union keep g1.alias_if_any_source g2.alias_if_any_source;
    any_usage = N.union keep g1.any_usage g2.any_usage;
    any_source = N.union keep g1.any_source g2.any_source;
    zero_alloc_source = N.union keep g1.zero_alloc_source g2.zero_alloc_source;
    code_id_my_closure =
      NN.union keep g1.code_id_my_closure g2.code_id_my_closure
  }

let add_alias t ~to_ ~from = t.alias <- NN.add_or_replace [to_; from] () t.alias

let add_use_dep t ~to_ ~from = t.use <- NN.add_or_replace [to_; from] () t.use

let add_constructor_dep t ~base relation ~from =
  t.constructor <- NFN.add_or_replace [base; relation; from] () t.constructor

let add_accessor_dep t ~to_ relation ~base =
  t.accessor <- NFN.add_or_replace [to_; relation; base] () t.accessor

let add_argument_dep t ~from relation ~base =
  t.argument <- NCN.add_or_replace [from; relation; base] () t.argument

let add_parameter_dep t ~base relation ~to_ =
  t.parameter <- NCN.add_or_replace [base; relation; to_] () t.parameter

let add_propagate_dep t ~if_used ~to_ ~from =
  t.propagate <- NNN.add_or_replace [if_used; to_; from] () t.propagate

let add_alias_if_any_source_dep t ~if_any_source ~to_ ~from =
  t.alias_if_any_source
    <- NNN.add_or_replace [if_any_source; to_; from] () t.alias_if_any_source

let add_opaque_let_dependency t ~to_ ~from =
  let bound_to = Bound_pattern.free_names to_ in
  let f () bound_to =
    Name_occurrences.fold_names from
      ~f:(fun () var ->
        add_use_dep t
          ~to_:(Code_id_or_name.name bound_to)
          ~from:(Code_id_or_name.name var))
      ~init:()
  in
  Name_occurrences.fold_names bound_to ~f ~init:()

let add_any_usage t (var : Code_id_or_name.t) =
  t.any_usage <- N.add_or_replace [var] () t.any_usage

let add_any_source t (var : Code_id_or_name.t) =
  t.any_source <- N.add_or_replace [var] () t.any_source

let add_zero_alloc_source t var =
  t.zero_alloc_source <- N.add_or_replace [var] () t.zero_alloc_source

let add_code_id_my_closure t code_id my_closure =
  t.code_id_my_closure
    <- NN.add_or_replace
         [Code_id_or_name.code_id code_id; Code_id_or_name.var my_closure]
         () t.code_id_my_closure

let ids_for_export graph =
  let open Datalog_helpers in
  let ids = Ids_for_export.empty in
  let ids = Maps.Nn.add_ids graph.alias ids in
  let ids = Maps.Nn.add_ids graph.use ids in
  let ids = Maps.Nfn.add_ids graph.accessor ids in
  let ids = Maps.Nfn.add_ids graph.constructor ids in
  let ids = Maps.Ncn.add_ids graph.argument ids in
  let ids = Maps.Ncn.add_ids graph.parameter ids in
  let ids = Maps.Nnn.add_ids graph.propagate ids in
  let ids = Maps.Nnn.add_ids graph.alias_if_any_source ids in
  let ids = Maps.N.add_ids graph.any_usage ids in
  let ids = Maps.N.add_ids graph.any_source ids in
  let ids = Maps.N.add_ids graph.zero_alloc_source ids in
  let ids = Maps.Nn.add_ids graph.code_id_my_closure ids in
  ids

let fields_for_export graph =
  let open Datalog_helpers in
  let fields = Field.Set.empty in
  let fields = Maps.Nfn.add_fields graph.accessor fields in
  let fields = Maps.Nfn.add_fields graph.constructor fields in
  fields

let apply_renaming graph renaming ~rename_field =
  let open Datalog_helpers in
  let rename_id = Renaming.apply_code_id_or_name renaming in
  { alias = Maps.Nn.rename graph.alias ~rename_id;
    use = Maps.Nn.rename graph.use ~rename_id;
    accessor = Maps.Nfn.rename graph.accessor ~rename_id ~rename_field;
    constructor = Maps.Nfn.rename graph.constructor ~rename_id ~rename_field;
    argument = Maps.Ncn.rename graph.argument ~rename_id;
    parameter = Maps.Ncn.rename graph.parameter ~rename_id;
    propagate = Maps.Nnn.rename graph.propagate ~rename_id;
    alias_if_any_source = Maps.Nnn.rename graph.alias_if_any_source ~rename_id;
    any_usage = Maps.N.rename graph.any_usage ~rename_id;
    any_source = Maps.N.rename graph.any_source ~rename_id;
    zero_alloc_source = Maps.N.rename graph.zero_alloc_source ~rename_id;
    code_id_my_closure = Maps.Nn.rename graph.code_id_my_closure ~rename_id
  }
