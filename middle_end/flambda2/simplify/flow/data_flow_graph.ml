(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module T = Flow_types

type t =
  { code_age_relation : Code_age_relation.t;
    name_to_name : Name.Set.t Name.Map.t;
    name_to_code_id : Code_id.Set.t Name.Map.t;
    code_id_to_name : Name.Set.t Code_id.Map.t;
    code_id_to_code_id : Code_id.Set.t Code_id.Map.t;
    normal_code_id_to_name : Name.Set.t Code_id.Map.t;
    normal_code_id_to_code_id : Code_id.Set.t Code_id.Map.t;
    normal_code_id_unconditionally_used : Code_id.Set.t;
    unconditionally_used : Name.Set.t;
    non_normal_only_roots : Name.Set.t;
    has_specialisation_sites : bool;
    code_id_unconditionally_used : Code_id.Set.t;
    traverse_code_dependencies : bool
  }

module Reachable = struct
  module Edge (Src_map : Container_types.Map) (Dst_set : Container_types.Set) =
  struct
    type src = Src_map.key

    type dst = Dst_set.elt

    let push ~(src : src) (enqueued : Dst_set.t) (queue : dst Queue.t)
        (graph : Dst_set.t Src_map.t) : Dst_set.t =
      let neighbours =
        match Src_map.find src graph with
        | exception Not_found -> Dst_set.empty
        | set -> set
      in
      let new_neighbours = Dst_set.diff neighbours enqueued in
      Dst_set.iter (fun dst -> Queue.push dst queue) new_neighbours;
      Dst_set.union enqueued new_neighbours
  end
  [@@inline]
  (* TODO check that this applied here *)

  module Name_Name_Edge = Edge (Name.Map) (Name.Set)
  module Name_Code_id_Edge = Edge (Name.Map) (Code_id.Set)
  module Code_id_Name_Edge = Edge (Code_id.Map) (Name.Set)
  module Code_id_Code_id_Edge = Edge (Code_id.Map) (Code_id.Set)

  (* breadth-first reachability analysis. *)
  let rec reachable_names t code_id_queue code_id_enqueued older_enqueued
      name_queue name_enqueued =
    match Queue.take name_queue with
    | exception Queue.Empty ->
      if t.traverse_code_dependencies
      then
        if Queue.is_empty code_id_queue
        then
          T.Data_flow_result.
            { required_names = name_enqueued;
              reachable_code_ids =
                Known
                  T.Reachable_code_ids.
                    { live_code_ids = code_id_enqueued;
                      ancestors_of_live_code_ids = older_enqueued
                    }
            }
        else
          reachable_code_ids t code_id_queue code_id_enqueued (Queue.create ())
            older_enqueued name_queue name_enqueued
      else
        T.Data_flow_result.
          { required_names = name_enqueued; reachable_code_ids = Unknown }
    | src ->
      let name_enqueued =
        Name_Name_Edge.push ~src name_enqueued name_queue t.name_to_name
      in
      let code_id_enqueued =
        Name_Code_id_Edge.push ~src code_id_enqueued code_id_queue
          t.name_to_code_id
      in
      reachable_names t code_id_queue code_id_enqueued older_enqueued name_queue
        name_enqueued

  and reachable_code_ids t code_id_queue code_id_enqueued older_queue
      older_enqueued name_queue name_enqueued =
    match Queue.take code_id_queue with
    | exception Queue.Empty ->
      if Queue.is_empty older_queue
      then
        reachable_names t code_id_queue code_id_enqueued older_enqueued
          name_queue name_enqueued
      else
        reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
          older_enqueued name_queue name_enqueued
    | src ->
      let name_enqueued =
        Code_id_Name_Edge.push ~src name_enqueued name_queue t.code_id_to_name
      in
      let code_id_enqueued =
        Code_id_Code_id_Edge.push ~src code_id_enqueued code_id_queue
          t.code_id_to_code_id
      in
      let older_enqueued =
        if Code_id.Set.mem src older_enqueued
        then older_enqueued
        else (
          Queue.push src older_queue;
          Code_id.Set.add src older_enqueued)
      in
      reachable_code_ids t code_id_queue code_id_enqueued older_queue
        older_enqueued name_queue name_enqueued

  and reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
      older_enqueued name_queue name_enqueued =
    match Queue.take older_queue with
    | exception Queue.Empty ->
      reachable_code_ids t code_id_queue code_id_enqueued older_queue
        older_enqueued name_queue name_enqueued
    | src -> (
      match Code_age_relation.get_older_version_of t.code_age_relation src with
      | None ->
        reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
          older_enqueued name_queue name_enqueued
      | Some dst ->
        if Code_id.Set.mem dst older_enqueued
        then (
          if Code_id.Set.mem dst code_id_enqueued
          then
            reachable_older_code_ids t code_id_queue code_id_enqueued
              older_queue older_enqueued name_queue name_enqueued
          else
            let code_id_enqueued = Code_id.Set.add dst code_id_enqueued in
            Queue.push dst code_id_queue;
            reachable_older_code_ids t code_id_queue code_id_enqueued
              older_queue older_enqueued name_queue name_enqueued)
        else
          let older_enqueued = Code_id.Set.add dst older_enqueued in
          Queue.push dst older_queue;
          reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
            older_enqueued name_queue name_enqueued)
end

let empty code_age_relation traverse_code_dependencies ~code_ids_to_never_delete
    ~has_specialisation_sites =
  { code_age_relation;
    traverse_code_dependencies;
    name_to_name = Name.Map.empty;
    name_to_code_id = Name.Map.empty;
    code_id_to_name = Code_id.Map.empty;
    code_id_to_code_id = Code_id.Map.empty;
    normal_code_id_to_name = Code_id.Map.empty;
    normal_code_id_to_code_id = Code_id.Map.empty;
    normal_code_id_unconditionally_used = code_ids_to_never_delete;
    unconditionally_used = Name.Set.empty;
    non_normal_only_roots = Name.Set.empty;
    has_specialisation_sites;
    code_id_unconditionally_used = code_ids_to_never_delete
  }

let [@ocamlformat "disable"] print ppf
    { traverse_code_dependencies;
      name_to_name;
      name_to_code_id;
      code_id_to_name;
      code_id_to_code_id;
      normal_code_id_to_name;
      normal_code_id_to_code_id;
      normal_code_id_unconditionally_used;
      code_age_relation;
      unconditionally_used;
      non_normal_only_roots;
      has_specialisation_sites;
      code_id_unconditionally_used
    } =
  Format.fprintf ppf
    "@[<hov 1>(\
       @[<hov 1>(traverse_code_dependencies %b)@]@ \
       @[<hov 1>(code_age_relation@ %a)@]@ \
       @[<hov 1>(name_to_name@ %a)@]@ \
       @[<hov 1>(name_to_code_id@ %a)@]@ \
       @[<hov 1>(code_id_to_name@ %a)@]@ \
       @[<hov 1>(code_id_to_code_id@ %a)@]@ \
       @[<hov 1>(unconditionally_used@ %a)@]@ \
       @[<hov 1>(code_id_unconditionally_used@ %a)@]@ \
       @[<hov 1>(has_specialisation_sites %b)@]@ \
       @[<hov 1>(normal_code_id_to_name@ %a)@]@ \
       @[<hov 1>(normal_code_id_to_code_id@ %a)@]@ \
       @[<hov 1>(normal_code_id_unconditionally_used@ %a)@]@ \
       @[<hov 1>(non_normal_only_roots@ %a)@]\
     )@]"
    traverse_code_dependencies
    Code_age_relation.print code_age_relation
    (Name.Map.print Name.Set.print) name_to_name
    (Name.Map.print Code_id.Set.print) name_to_code_id
    (Code_id.Map.print Name.Set.print) code_id_to_name
    (Code_id.Map.print Code_id.Set.print) code_id_to_code_id
    Name.Set.print unconditionally_used
    Code_id.Set.print code_id_unconditionally_used
    has_specialisation_sites
    (Code_id.Map.print Name.Set.print) normal_code_id_to_name
    (Code_id.Map.print Code_id.Set.print) normal_code_id_to_code_id
    Code_id.Set.print normal_code_id_unconditionally_used
    Name.Set.print non_normal_only_roots

(* *)
let fold_name_occurrences name_occurrences ~init ~names ~code_ids =
  Name_occurrences.fold_names name_occurrences ~f:names
    ~init:(code_ids init (Name_occurrences.code_ids name_occurrences))

(* Some auxiliary functions *)
let add_code_id_dep ~src ~(dst : Code_id.Set.t) ({ name_to_code_id; _ } as t) =
  let name_to_code_id =
    Name.Map.update src
      (function
        | None -> if Code_id.Set.is_empty dst then None else Some dst
        | Some old ->
          Misc.fatal_errorf "Same name bound multiple times: %a -> %a, %a"
            Name.print src Code_id.Set.print old Code_id.Set.print dst)
      name_to_code_id
  in
  { t with name_to_code_id }

let add_dependency ~src ~dst ({ name_to_name; _ } as t) =
  let name_to_name =
    Name.Map.update src
      (function
        | None -> Some (Name.Set.singleton dst)
        | Some set -> Some (Name.Set.add dst set))
      name_to_name
  in
  { t with name_to_name }

let add_code_id_dependency ~src ~dst ({ code_id_to_name; _ } as t) =
  let code_id_to_name =
    Code_id.Map.update src
      (function
        | None -> Some (Name.Set.singleton dst)
        | Some set -> Some (Name.Set.add dst set))
      code_id_to_name
  in
  { t with code_id_to_name }

let add_code_id_to_code_id ~src ~dst ({ code_id_to_code_id; _ } as t) =
  let code_id_to_code_id =
    Code_id.Map.update src
      (function
        | None -> if Code_id.Set.is_empty dst then None else Some dst
        | Some old ->
          Misc.fatal_errorf "Same code_id bound multiple times: %a -> %a, %a"
            Code_id.print src Code_id.Set.print old Code_id.Set.print dst)
      code_id_to_code_id
  in
  { t with code_id_to_code_id }

let name_occurs_normally name_occurrences name =
  match Name_occurrences.greatest_name_mode_name name_occurrences name with
  | Absent -> false
  | Present mode -> Name_mode.is_normal mode

let add_name_occurrences name_occurrences
    ({ unconditionally_used;
       non_normal_only_roots;
       has_specialisation_sites;
       code_id_unconditionally_used;
       _
     } as t) =
  let unconditionally_used, non_normal_only_roots =
    if not has_specialisation_sites
    then
      ( Name_occurrences.fold_names name_occurrences
          ~f:(fun used name -> Name.Set.add name used)
          ~init:unconditionally_used,
        non_normal_only_roots )
    else
      Name_occurrences.fold_names name_occurrences
        ~f:(fun (used, phantom_only) name ->
          let is_normal = name_occurs_normally name_occurrences name in
          let phantom_only =
            if is_normal
            then Name.Set.remove name phantom_only
            else if Name.Set.mem name used
            then phantom_only
            else Name.Set.add name phantom_only
          in
          Name.Set.add name used, phantom_only)
        ~init:(unconditionally_used, non_normal_only_roots)
  in
  let code_id_unconditionally_used =
    Code_id.Set.union
      (Name_occurrences.code_ids name_occurrences)
      code_id_unconditionally_used
  in
  let normal_code_id_unconditionally_used =
    if not has_specialisation_sites
    then t.normal_code_id_unconditionally_used
    else
      Code_id.Set.union t.normal_code_id_unconditionally_used
        (Name_occurrences.code_ids_in_normal_mode name_occurrences)
  in
  { t with
    unconditionally_used;
    non_normal_only_roots;
    code_id_unconditionally_used;
    normal_code_id_unconditionally_used
  }

let add_continuation_info map ~return_continuation ~exn_continuation
    ~used_value_slots _
    T.Continuation_info.
      { apply_cont_args;
        (* CR pchambart: properly follow dependencies in exception extra args.
           They are currently marked as always used, so it is correct, but not
           optimal *)
        used_in_handler;
        bindings;
        direct_aliases;
        mutable_let_prims_rev;
        defined = _;
        code_ids;
        value_slots;
        continuation = _;
        recursive = _;
        is_exn_handler = _;
        parent_continuation = _;
        params = _
      } t =
  (* Add the vars used in the handler *)
  let t = add_name_occurrences used_in_handler t in
  (* Add the dependencies created by closures vars in envs *)
  let is_value_slot_used =
    match (used_value_slots : _ Or_unknown.t) with
    | Unknown -> fun _ -> true
    | Known used_value_slots ->
      Name_occurrences.value_slot_is_used_or_imported used_value_slots
  in
  let t =
    Value_slot.Map.fold
      (fun value_slot map t ->
        if not (is_value_slot_used value_slot)
        then t
        else
          Name.Map.fold
            (fun closure_name values_in_env t ->
              Name_occurrences.fold_names
                ~f:(fun t value_in_env ->
                  add_dependency ~src:closure_name ~dst:value_in_env t)
                values_in_env ~init:t)
            map t)
      value_slots t
  in
  (* Build the graph of dependencies between names *)
  let t =
    Name.Map.fold
      (fun src name_occurrences graph ->
        fold_name_occurrences name_occurrences ~init:graph
          ~names:(fun t dst -> add_dependency ~src ~dst t)
          ~code_ids:(fun t dst -> add_code_id_dep ~src ~dst t))
      bindings t
  in
  let t =
    Variable.Map.fold
      (fun src simple graph ->
        let src = Name.var src in
        let name_occurrences = Simple.free_names simple in
        fold_name_occurrences name_occurrences ~init:graph
          ~names:(fun t dst -> add_dependency ~src ~dst t)
          ~code_ids:(fun t dst -> add_code_id_dep ~src ~dst t))
      direct_aliases t
  in
  let t =
    List.fold_left
      (fun t
           T.Mutable_let_prim.
             { bound_var; prim; original_prim; named_rewrite_id = _ } ->
        let src = Name.var bound_var in
        (* This is an over-aproximation of the the dependencies after mutable
           unboxing, but: if no unboxing happen this is the correct
           dependencies, if some unboxing happens, then we will run a second
           round of optimisation on the current function (if this is the code of
           a function) that will actually remove those spurious dependencies *)
        match prim with
        | Is_int _ | Get_tag _ | Make_block _ | Block_load _ ->
          Name_occurrences.fold_names
            ~f:(fun t dst -> add_dependency ~src ~dst t)
            (Flambda_primitive.free_names original_prim)
            ~init:t
        | Block_set _ ->
          add_name_occurrences (Flambda_primitive.free_names original_prim) t)
      t mutable_let_prims_rev
  in
  let t =
    Code_id.Map.fold
      (fun src name_occurrences graph ->
        let graph =
          fold_name_occurrences name_occurrences ~init:graph
            ~names:(fun t dst -> add_code_id_dependency ~src ~dst t)
            ~code_ids:(fun t dst -> add_code_id_to_code_id ~src ~dst t)
        in
        if not graph.has_specialisation_sites
        then graph
        else
          (* Non-normal code dependencies must not keep sites or their synthetic
             value slots alive. *)
          let names =
            Name_occurrences.fold_names name_occurrences ~init:Name.Set.empty
              ~f:(fun names name ->
                if name_occurs_normally name_occurrences name
                then Name.Set.add name names
                else names)
          in
          let code_ids =
            Name_occurrences.code_ids_in_normal_mode name_occurrences
          in
          { graph with
            normal_code_id_to_name =
              Code_id.Map.add src names graph.normal_code_id_to_name;
            normal_code_id_to_code_id =
              Code_id.Map.add src code_ids graph.normal_code_id_to_code_id
          })
      code_ids t
  in
  (* Build the graph of dependencies between continuation parameters and
     arguments. *)
  Continuation.Map.fold
    (fun k rewrite_ids t ->
      if
        Continuation.equal return_continuation k
        || Continuation.equal exn_continuation k
      then
        Apply_cont_rewrite_id.Map.fold
          (fun _rewrite_id args t ->
            Numeric_types.Int.Map.fold
              (fun _ (cont_arg : T.Cont_arg.t) t ->
                match cont_arg with
                | Simple simple ->
                  add_name_occurrences (Simple.free_names simple) t
                | New_let_binding (var, prim_free_names) ->
                  add_name_occurrences
                    (Name_occurrences.union prim_free_names
                       (Name_occurrences.singleton_variable var Name_mode.normal))
                    t
                | Function_result -> t)
              args t)
          rewrite_ids t
      else
        let params =
          match Continuation.Map.find k map with
          | elt ->
            Array.of_list (Bound_parameters.vars elt.T.Continuation_info.params)
          | exception Not_found ->
            Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
              Continuation.print k
        in
        Apply_cont_rewrite_id.Map.fold
          (fun rewrite_id args t ->
            let correct_number_of_arguments =
              match Numeric_types.Int.Map.max_binding args with
              | exception Not_found -> Array.length params = 0
              | max_arg, _ -> max_arg = Array.length params - 1
            in
            if not correct_number_of_arguments
            then
              Misc.fatal_errorf
                "Mismatched number of argument and params for %a at rewrite_id \
                 %a"
                Continuation.print k Apply_cont_rewrite_id.print rewrite_id;
            Numeric_types.Int.Map.fold
              (fun i (cont_arg : T.Cont_arg.t) t ->
                (* Note on the direction of the edge:

                   We later do a reachability analysis to compute the transitive
                   closure of the used variables.

                   Therefore an edge from src to dst means: if src is used, then
                   dst is also used.

                   Applied here, this means : if the param of a continuation is
                   used, then any argument provided for that param is also used.
                   The other way wouldn't make much sense. *)
                let src = Name.var params.(i) in
                match cont_arg with
                | Simple simple ->
                  Name_occurrences.fold_names (Simple.free_names simple) ~init:t
                    ~f:(fun t dst -> add_dependency ~src ~dst t)
                | New_let_binding (var, prim_free_names) ->
                  let t = add_dependency ~src ~dst:(Name.var var) t in
                  Name_occurrences.fold_names prim_free_names ~init:t
                    ~f:(fun t dst -> add_dependency ~src:(Name.var var) ~dst t)
                | Function_result -> t)
              args t)
          rewrite_ids t)
    apply_cont_args t

let create ~return_continuation ~exn_continuation ~code_age_relation
    ~used_value_slots ~code_ids_to_never_delete ~has_specialisation_sites map =
  (* Build the dependencies using the regular params and args of continuations,
     and the let-bindings in continuations handlers. *)
  let traverse_code_dependencies =
    match (used_value_slots : _ Or_unknown.t) with
    | Known _ -> true
    | Unknown -> false
  in
  let t =
    Continuation.Map.fold
      (add_continuation_info map ~return_continuation ~exn_continuation
         ~used_value_slots)
      map
      (empty code_age_relation traverse_code_dependencies
         ~code_ids_to_never_delete ~has_specialisation_sites)
  in
  t

let compute_reachability
    ({ code_age_relation = _;
       name_to_name = _;
       name_to_code_id = _;
       code_id_to_name = _;
       code_id_to_code_id = _;
       normal_code_id_to_name = _;
       normal_code_id_to_code_id = _;
       normal_code_id_unconditionally_used = _;
       unconditionally_used;
       non_normal_only_roots = _;
       has_specialisation_sites = _;
       code_id_unconditionally_used;
       traverse_code_dependencies
     } as t) =
  let name_queue = Queue.create () in
  Name.Set.iter (fun v -> Queue.push v name_queue) unconditionally_used;
  let code_id_queue = Queue.create () in
  if traverse_code_dependencies
  then
    Code_id.Set.iter
      (fun v -> Queue.push v code_id_queue)
      code_id_unconditionally_used;
  Reachable.reachable_names t code_id_queue code_id_unconditionally_used
    Code_id.Set.empty name_queue unconditionally_used

let required_names t =
  let all_uses = compute_reachability t in
  let specialisation_site_info : T.Specialisation_site_info.t =
    if not t.has_specialisation_sites
    then T.Specialisation_site_info.empty
    else
      let t =
        { t with
          unconditionally_used =
            Name.Set.diff t.unconditionally_used t.non_normal_only_roots;
          code_id_to_name = t.normal_code_id_to_name;
          code_id_to_code_id = t.normal_code_id_to_code_id;
          code_id_unconditionally_used = t.normal_code_id_unconditionally_used
        }
      in
      let without_non_normal_roots = compute_reachability t in
      let names_available_for_hints = without_non_normal_roots.required_names in
      let live_code_ids =
        match without_non_normal_roots.reachable_code_ids with
        | Known { live_code_ids; ancestors_of_live_code_ids = _ } ->
          live_code_ids
        | Unknown -> (
          (* Follow the recorded code and symbol dependencies for site liveness,
             without widening the names available for hints. *)
          let result =
            compute_reachability { t with traverse_code_dependencies = true }
          in
          match result.reachable_code_ids with
          | Known { live_code_ids; ancestors_of_live_code_ids = _ } ->
            live_code_ids
          | Unknown ->
            Misc.fatal_error
              "Expected code reachability for specialisation sites")
      in
      { names_available_for_hints; live_code_ids }
  in
  all_uses, specialisation_site_info
