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
  { mutable alias_rel : NN.t;
    mutable use_rel : NN.t;
    mutable accessor_rel : NFN.t;
    mutable constructor_rel : NFN.t;
    mutable coaccessor_rel : NCN.t;
    mutable coconstructor_rel : NCN.t;
    mutable propagate_rel : NNN.t;
    mutable any_usage_pred : N.t;
    mutable any_source_pred : N.t;
    mutable code_id_my_closure_rel : NN.t
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

let alias_rel = NN.create ~name:"alias"

let use_rel = NN.create ~name:"use"

let accessor_rel = NFN.create ~name:"accessor"

let constructor_rel = NFN.create ~name:"constructor"

let coaccessor_rel = NCN.create ~name:"coaccessor"

let coconstructor_rel = NCN.create ~name:"coconstructor"

let propagate_rel = NNN.create ~name:"propagate"

let any_usage_pred = N.create ~name:"any_usage"

let any_source_pred = N.create ~name:"any_source"

let code_id_my_closure_rel = NN.create ~name:"code_id_my_closure"

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

module Relations = struct
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  (* Naming:
   * to_ = from; (alias)
   * to_ = [...] from (use)
   * to_ = base.relation (accessor)
   * base = Make_block { from_ } (constructor)
   * *)

  let alias_rel ~to_ ~from = Datalog.atom alias_rel [to_; from]

  let use_rel ~to_ ~from = Datalog.atom use_rel [to_; from]

  let accessor_rel ~to_ relation ~base =
    Datalog.atom accessor_rel [to_; relation; base]

  let constructor_rel ~base relation ~from =
    Datalog.atom constructor_rel [base; relation; from]

  let coaccessor_rel ~to_ relation ~base =
    Datalog.atom coaccessor_rel [to_; relation; base]

  let coconstructor_rel ~base relation ~from =
    Datalog.atom coconstructor_rel [base; relation; from]

  let propagate_rel ~if_used ~to_ ~from =
    Datalog.atom propagate_rel [if_used; to_; from]

  let any_usage_pred var = Datalog.atom any_usage_pred [var]

  let any_source_pred var = Datalog.atom any_source_pred [var]

  let code_id_my_closure_rel ~code_id ~my_closure =
    Datalog.atom code_id_my_closure_rel [code_id; my_closure]
end

let create () =
  { alias_rel = NN.empty;
    use_rel = NN.empty;
    accessor_rel = NFN.empty;
    constructor_rel = NFN.empty;
    coaccessor_rel = NCN.empty;
    coconstructor_rel = NCN.empty;
    propagate_rel = NNN.empty;
    any_usage_pred = N.empty;
    any_source_pred = N.empty;
    code_id_my_closure_rel = NN.empty
  }

let add_alias t ~to_ ~from =
  t.alias_rel <- NN.add_or_replace [to_; from] () t.alias_rel

let add_use_dep t ~to_ ~from =
  t.use_rel <- NN.add_or_replace [to_; from] () t.use_rel

let add_constructor_dep t ~base relation ~from =
  t.constructor_rel
    <- NFN.add_or_replace [base; relation; from] () t.constructor_rel

let add_accessor_dep t ~to_ relation ~base =
  t.accessor_rel <- NFN.add_or_replace [to_; relation; base] () t.accessor_rel

let add_coaccessor_dep t ~to_ relation ~base =
  t.coaccessor_rel
    <- NCN.add_or_replace [to_; relation; base] () t.coaccessor_rel

let add_coconstructor_dep t ~base relation ~from =
  t.coconstructor_rel
    <- NCN.add_or_replace [base; relation; from] () t.coconstructor_rel

let add_propagate_dep t ~if_used ~to_ ~from =
  t.propagate_rel <- NNN.add_or_replace [if_used; to_; from] () t.propagate_rel

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
  t.any_usage_pred <- N.add_or_replace [var] () t.any_usage_pred

let add_any_source t (var : Code_id_or_name.t) =
  t.any_source_pred <- N.add_or_replace [var] () t.any_source_pred

let add_code_id_my_closure t code_id my_closure =
  t.code_id_my_closure_rel
    <- NN.add_or_replace
         [Code_id_or_name.code_id code_id; Code_id_or_name.var my_closure]
         () t.code_id_my_closure_rel
