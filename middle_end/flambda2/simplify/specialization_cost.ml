(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type reason =
  | Specialization_disabled
  | At_toplevel
  | Contains_static_consts
  | Contains_set_of_closures

type cost = { primitives : (Bound_var.t * Flambda_primitive.t) list }

type t =
  | Can_specialize of cost
  | Cannot_specialize of { reason : reason }

let [@ocamlformat "disable"] print_reason ppf reason =
  match reason with
  | Specialization_disabled ->
    Format.fprintf ppf "specialization_disabled"
  | At_toplevel ->
    Format.fprintf ppf "at_top_level"
  | Contains_static_consts ->
    Format.fprintf ppf "contains_static_consts"
  | Contains_set_of_closures ->
    Format.fprintf ppf "contains_set_of_closures"

let [@ocamlformat "disable"] print_prim ppf (bound_var, prim) =
  Format.fprintf ppf "@[<hov 1>(%a %a)@]"
    Bound_var.print bound_var Flambda_primitive.print prim

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Can_specialize { primitives } ->
    Format.fprintf ppf "@[<hv>(can_specialize@ \
        @[<hv 1>(primitives@ %a)@]\
      )@]"
      (Format.pp_print_list print_prim) primitives
  | Cannot_specialize { reason } ->
    Format.fprintf ppf "@[<hv>(cannot_specialize@ \
        @[<hv 1>(reason@ %a)@]\
      )@]"
    print_reason reason

(* Creations *)

let cannot_specialize reason = Cannot_specialize { reason }

let can_specialize () =
  if Flambda_features.match_in_match ()
  then Can_specialize { primitives = [] }
  else cannot_specialize Specialization_disabled

(* Updating costs *)

let update_cost ~f = function
  | Can_specialize cost -> Can_specialize (f cost)
  | Cannot_specialize _ as res -> res

let add_set_of_closures _soc _t =
  Cannot_specialize { reason = Contains_set_of_closures }

let add_lifted_set_of_closures _soc _t =
  Cannot_specialize { reason = Contains_set_of_closures }

let add_prim bound_var prim t =
  update_cost t ~f:(fun { primitives } ->
      (* CR gbury: we might want to limit the number of primitives we store, so
         that e.g. if there are more than <n> primitives, we stop storing them
         and decide that no specialization should take place *)
      let primitives = (bound_var, prim) :: primitives in
      { primitives })

(* Computing cost and benefits *)

let rec repeat n acc ~f = if n <= 0 then acc else repeat (n - 1) (f acc) ~f

let cost_metrics typing_env ~switch ~join_analysis ~specialized ~generic
    (cost : cost) =
  let machine_width = Typing_env.machine_width typing_env in
  (* If there is exactly 1 "generic" call site, then that call site is the same
     as a specialized one. *)
  let n_specialized = Apply_cont_rewrite_id.Set.cardinal specialized in
  let n_generic = Apply_cont_rewrite_id.Set.cardinal generic in
  let n_total = n_specialized + if n_generic = 0 then 0 else 1 in
  (* Cost metrics for a generic call site *)
  let metrics =
    if n_generic = 0
    then
      (* CR gbury: in this case, we actually remove the code size of the
         handler, since it disappears in favor of the specialized calls; we'll
         take into account the size of each specialized handler later, so here
         we need to count the negative of the code size of the generic
         handler *)
      let code_size =
        List.fold_left
          (fun acc (_, primitive) ->
            Code_size.( + ) acc (Code_size.prim ~machine_width primitive))
          (Code_size.switch switch) cost.primitives
      in
      Cost_metrics.from_size (Code_size.( - ) Code_size.zero code_size)
    else
      (* The regular case, where the generic continuation handler stays, and the
         code size is not changed (at least for the generic case *)
      Cost_metrics.zero
  in
  (* Cost metrics for specialized call sites *)
  let metrics =
    (* Each specialized handler eliminates the switch *)
    repeat n_specialized metrics
      ~f:(Cost_metrics.notify_removed ~operation:Removed_operations.branch)
  in
  (* For each primitive, and each specialized call site: 1) notify it removed if
     its value is known, 2) else add the prim to the code size *)
  List.fold_left
    (fun metrics (bound_var, prim) ->
      let simple = Simple.var (Bound_var.var bound_var) in
      let disappears, stays =
        match
          Join_analysis.simple_refined_at_join join_analysis typing_env simple
        with
        | Not_refined_at_join -> 0, n_total
        | Invariant_in_all_uses _ ->
          (* if the canonical is a simple already, then specialization does not
             change much *)
          0, 0
        | Variable_refined_at_these_uses var_analysis ->
          Join_analysis.Variable_refined_at_join.fold_values_at_uses
            (fun id value (disappears, stays) ->
              if Apply_cont_rewrite_id.Set.mem id specialized
              then
                match value with
                | Known _ -> disappears + 1, stays
                | Unknown -> disappears, stays + 1
              else disappears, stays + 1)
            var_analysis (0, 0)
      in
      let metrics =
        repeat disappears metrics
          ~f:
            (Cost_metrics.notify_removed
               ~operation:(Removed_operations.prim prim))
      in
      let metrics =
        repeat stays metrics
          ~f:
            (Cost_metrics.notify_added
               ~code_size:(Code_size.prim ~machine_width prim))
      in
      metrics)
    metrics cost.primitives
