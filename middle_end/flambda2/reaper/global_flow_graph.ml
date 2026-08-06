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

module NMap = Code_id_or_name.Map

let ids_for_export graph =
  let add_id = Ids_for_export.add_code_id_or_name in
  (* Edges are stored in nested maps which come in a few different types. These
     are given short names like [ncn] which means [unit NMap.t Cofield.Map.t
     NMap.t]. *)
  let add_ids_from_n (n : N.t) ids =
    NMap.fold (fun id () ids -> add_id ids id) n ids
  in
  let add_ids_from_nn (nn : NN.t) ids =
    NMap.fold (fun id n ids -> add_ids_from_n n (add_id ids id)) nn ids
  in
  let add_ids_from_nnn (nnn : NNN.t) ids =
    NMap.fold (fun id nn ids -> add_ids_from_nn nn (add_id ids id)) nnn ids
  in
  let add_ids_from_ncn (ncn : NCN.t) ids =
    NMap.fold
      (fun id (cn : N.t Cofield.Map.t) ids ->
        Cofield.Map.fold
          (fun (_ : Cofield.t) n ids -> add_ids_from_n n ids)
          cn (add_id ids id))
      ncn ids
  in
  let add_ids_from_nfn (nfn : NFN.t) ids =
    NMap.fold
      (fun id (fn : N.t Field.Map.t) ids ->
        Field.Map.fold
          (fun (_ : Field.t) n ids -> add_ids_from_n n ids)
          fn (add_id ids id))
      nfn ids
  in
  let ids = Ids_for_export.empty in
  let ids = add_ids_from_nn graph.alias ids in
  let ids = add_ids_from_nn graph.use ids in
  let ids = add_ids_from_nfn graph.accessor ids in
  let ids = add_ids_from_nfn graph.constructor ids in
  let ids = add_ids_from_ncn graph.argument ids in
  let ids = add_ids_from_ncn graph.parameter ids in
  let ids = add_ids_from_nnn graph.propagate ids in
  let ids = add_ids_from_nnn graph.alias_if_any_source ids in
  let ids = add_ids_from_n graph.any_usage ids in
  let ids = add_ids_from_n graph.any_source ids in
  let ids = add_ids_from_n graph.zero_alloc_source ids in
  let ids = add_ids_from_nn graph.code_id_my_closure ids in
  ids

let fields_for_export graph =
  (* Here [fn] means [unit NMap.t Field.Map.t], and similarly [nfn] is an [fn]
     inside a [NMap.t]. *)
  let add_fields_from_nfn (nfn : NFN.t) fields =
    NMap.fold
      (fun (_ : Code_id_or_name.t) (fn : N.t Field.Map.t) fields ->
        Field.Map.fold
          (fun field (_ : N.t) fields -> Field.Set.add field fields)
          fn fields)
      nfn fields
  in
  let fields = Field.Set.empty in
  let fields = add_fields_from_nfn graph.accessor fields in
  let fields = add_fields_from_nfn graph.constructor fields in
  fields

let apply_renaming graph renaming ~rename_field =
  let rename_id = Renaming.apply_code_id_or_name renaming in
  (* Edges are stored in nested maps which come in a few different types. These
     are given short names like [ncn] which means [unit NMap.t Cofield.Map.t
     NMap.t]. Within this function, the prefix [old_] means pre-renaming and
     [new_] means post-renaming. *)
  let rename_n (old_n : N.t) =
    NMap.fold
      (fun id () new_n -> NMap.add (rename_id id) () new_n)
      old_n NMap.empty
  in
  let rename_nn (old_nn : NN.t) =
    NMap.fold
      (fun id old_n new_nn -> NMap.add (rename_id id) (rename_n old_n) new_nn)
      old_nn NMap.empty
  in
  let rename_nnn (old_nnn : NNN.t) =
    NMap.fold
      (fun id old_nn new_nnn ->
        NMap.add (rename_id id) (rename_nn old_nn) new_nnn)
      old_nnn NMap.empty
  in
  let rename_ncn (old_ncn : NCN.t) =
    (* Cofields are not renamed because they are stable across processes. *)
    NMap.fold
      (fun id (old_cn : N.t Cofield.Map.t) new_ncn ->
        let new_cn : N.t Cofield.Map.t = Cofield.Map.map rename_n old_cn in
        NMap.add (rename_id id) new_cn new_ncn)
      old_ncn NMap.empty
  in
  let rename_nfn old_nfn =
    NMap.fold
      (fun id (old_fn : N.t Field.Map.t) new_nfn ->
        let new_fn : N.t Field.Map.t =
          Field.Map.fold
            (fun field old_n new_fn ->
              Field.Map.add (rename_field field) (rename_n old_n) new_fn)
            old_fn Field.Map.empty
        in
        NMap.add (rename_id id) new_fn new_nfn)
      old_nfn NMap.empty
  in
  { alias = rename_nn graph.alias;
    use = rename_nn graph.use;
    accessor = rename_nfn graph.accessor;
    constructor = rename_nfn graph.constructor;
    argument = rename_ncn graph.argument;
    parameter = rename_ncn graph.parameter;
    propagate = rename_nnn graph.propagate;
    alias_if_any_source = rename_nnn graph.alias_if_any_source;
    any_usage = rename_n graph.any_usage;
    any_source = rename_n graph.any_source;
    zero_alloc_source = rename_n graph.zero_alloc_source;
    code_id_my_closure = rename_nn graph.code_id_my_closure
  }
