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

module Alias_rel = Datalog.Schema.Relation2 (Code_id_or_name) (Code_id_or_name)
module Use_rel = Datalog.Schema.Relation2 (Code_id_or_name) (Code_id_or_name)
module Accessor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (Field) (Code_id_or_name)
module Constructor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (Field) (Code_id_or_name)
module Propagate_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (Code_id_or_name) (Code_id_or_name)
module CoAccessor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (Cofield) (Code_id_or_name)
module CoConstructor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (Cofield) (Code_id_or_name)
module Any_usage_pred = Datalog.Schema.Relation1 (Code_id_or_name)
module Any_source_pred = Datalog.Schema.Relation1 (Code_id_or_name)
module Code_id_my_closure_rel =
  Datalog.Schema.Relation2 (Code_id_or_name) (Code_id_or_name)

type graph =
  { mutable alias_rel : Alias_rel.t;
    mutable use_rel : Use_rel.t;
    mutable accessor_rel : Accessor_rel.t;
    mutable constructor_rel : Constructor_rel.t;
    mutable coaccessor_rel : CoAccessor_rel.t;
    mutable coconstructor_rel : CoConstructor_rel.t;
    mutable propagate_rel : Propagate_rel.t;
    mutable any_usage_pred : Any_usage_pred.t;
    mutable any_source_pred : Any_source_pred.t;
    mutable code_id_my_closure_rel : Code_id_my_closure_rel.t
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
  iter_nn "black" graph.alias_rel;
  iter_nn "red" graph.use_rel;
  iter_nfn "green" graph.accessor_rel;
  iter_nfn "blue" graph.constructor_rel;
  iter_ncn "darkgreen" graph.coaccessor_rel;
  iter_ncn "darkblue" graph.coconstructor_rel;
  Code_id_or_name.Map.iter
    (fun _if_defined m -> iter_nn "purple" m)
    graph.propagate_rel

let alias_rel = Alias_rel.create ~name:"alias"

let use_rel = Use_rel.create ~name:"use"

let accessor_rel = Accessor_rel.create ~name:"accessor"

let constructor_rel = Constructor_rel.create ~name:"constructor"

let coaccessor_rel = CoAccessor_rel.create ~name:"coaccessor"

let coconstructor_rel = CoConstructor_rel.create ~name:"coconstructor"

let propagate_rel = Propagate_rel.create ~name:"propagate"

let any_usage_pred = Any_usage_pred.create ~name:"any_usage"

let any_source_pred = Any_source_pred.create ~name:"any_source"

let code_id_my_closure_rel =
  Code_id_my_closure_rel.create ~name:"code_id_my_closure"

let to_datalog graph =
  Datalog.set_table alias_rel graph.alias_rel
  @@ Datalog.set_table use_rel graph.use_rel
  @@ Datalog.set_table accessor_rel graph.accessor_rel
  @@ Datalog.set_table constructor_rel graph.constructor_rel
  @@ Datalog.set_table coaccessor_rel graph.coaccessor_rel
  @@ Datalog.set_table coconstructor_rel graph.coconstructor_rel
  @@ Datalog.set_table propagate_rel graph.propagate_rel
  @@ Datalog.set_table any_usage_pred graph.any_usage_pred
  @@ Datalog.set_table any_source_pred graph.any_source_pred
  @@ Datalog.set_table code_id_my_closure_rel graph.code_id_my_closure_rel
  @@ Datalog.empty

type 'a rel0 = [> `Atom of Datalog.atom] as 'a

type ('a, 'b) rel1 = 'a Datalog.Term.t -> 'b rel0

type ('a, 'b, 'c) rel2 = 'a Datalog.Term.t -> ('b, 'c) rel1

type ('a, 'b, 'c, 'd) rel3 = 'a Datalog.Term.t -> ('b, 'c, 'd) rel2

(* Naming:
 * to_ = from; (alias)
 * to_ = [...] from (use)
 * to_ = base.relation (accessor)
 * base = Make_block { from_ } (constructor)
 * *)

let alias_rel to_ from = Datalog.atom alias_rel [to_; from]

let use_rel to_ from = Datalog.atom use_rel [to_; from]

let accessor_rel to_ relation base =
  Datalog.atom accessor_rel [to_; relation; base]

let constructor_rel base relation from =
  Datalog.atom constructor_rel [base; relation; from]

let coaccessor_rel to_ relation base =
  Datalog.atom coaccessor_rel [to_; relation; base]

let coconstructor_rel base relation from =
  Datalog.atom coconstructor_rel [base; relation; from]

let propagate_rel if_used to_ from =
  Datalog.atom propagate_rel [if_used; to_; from]

let any_usage_pred var = Datalog.atom any_usage_pred [var]

let any_source_pred var = Datalog.atom any_source_pred [var]

let code_id_my_closure_rel code_id var =
  Datalog.atom code_id_my_closure_rel [code_id; var]

let create () =
  { alias_rel = Alias_rel.empty;
    use_rel = Use_rel.empty;
    accessor_rel = Accessor_rel.empty;
    constructor_rel = Constructor_rel.empty;
    coaccessor_rel = CoAccessor_rel.empty;
    coconstructor_rel = CoConstructor_rel.empty;
    propagate_rel = Propagate_rel.empty;
    any_usage_pred = Any_usage_pred.empty;
    any_source_pred = Any_source_pred.empty;
    code_id_my_closure_rel = Code_id_my_closure_rel.empty
  }

let add_alias t ~to_ ~from =
  t.alias_rel <- Alias_rel.add_or_replace [to_; from] () t.alias_rel

let add_use_dep t ~to_ ~from =
  t.use_rel <- Use_rel.add_or_replace [to_; from] () t.use_rel

let add_constructor_dep t ~base relation ~from =
  t.constructor_rel
    <- Constructor_rel.add_or_replace [base; relation; from] ()
         t.constructor_rel

let add_accessor_dep t ~to_ relation ~base =
  t.accessor_rel
    <- Accessor_rel.add_or_replace [to_; relation; base] () t.accessor_rel

let add_coaccessor_dep t ~to_ relation ~base =
  t.coaccessor_rel
    <- CoAccessor_rel.add_or_replace [to_; relation; base] () t.coaccessor_rel

let add_coconstructor_dep t ~base relation ~from =
  t.coconstructor_rel
    <- CoConstructor_rel.add_or_replace [base; relation; from] ()
         t.coconstructor_rel

let add_propagate_dep t ~if_used ~to_ ~from =
  t.propagate_rel
    <- Propagate_rel.add_or_replace [if_used; to_; from] () t.propagate_rel

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

let add_use t (var : Code_id_or_name.t) =
  t.any_usage_pred <- Any_usage_pred.add_or_replace [var] () t.any_usage_pred

let add_any_source t (var : Code_id_or_name.t) =
  t.any_source_pred <- Any_source_pred.add_or_replace [var] () t.any_source_pred

let add_code_id_my_closure t code_id my_closure =
  t.code_id_my_closure_rel
    <- Code_id_my_closure_rel.add_or_replace
         [Code_id_or_name.code_id code_id; Code_id_or_name.var my_closure]
         () t.code_id_my_closure_rel
