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
  | At_toplevel
  | Contains_static_consts
  | Contains_set_of_closures

type cost = { primitives : (Bound_var.t * Flambda_primitive.t) list }

type t =
  | Can_specialize of cost
  | Cannot_specialize of { reason : reason }

let [@ocamlformat "disable"] print_reason ppf reason =
  match reason with
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
    Format.fprintf ppf "@[<hov>(can_specialize@ \
        @[<hov 1>(primitives@ %a)@]\
      )@]"
      (Format.pp_print_list print_prim) primitives
  | Cannot_specialize { reason } ->
    Format.fprintf ppf "@[<hov>(cannot_specialize@ \
        @[<hov 1>(reason@ %a)@]\
      )@]"
    print_reason reason

(* Creations *)

let can_specialize = Can_specialize { primitives = [] }

let cannot_specialize reason = Cannot_specialize { reason }

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

let cost_metrics typing_env ~switch ~join_info ~specialized ~generic
    (cost : cost) =
  let machine_width = Typing_env.machine_width typing_env in
  (* If there is exactly 1 "generic" call site, then that call site is the same
     as a specialized one. *)
  let specialized_call_sites, generic_call_sites =
    match generic with
    | [id] -> id :: specialized, []
    | _ -> specialized, generic
  in
  (* Cost metrics for a generic call site *)
  let metrics =
    match generic_call_sites with
    | [] ->
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
    | _ :: _ ->
      (* The regular case, where the generic continuaion handler stays, and the
         code size is not changed (at least for the generic case *)
      Cost_metrics.zero
  in
  (* Cost metrics for specialized call sites *)
  let metrics =
    (* Each specialized handler eliminates the switch *)
    List.fold_left
      (fun metrics _ ->
        Cost_metrics.notify_removed ~operation:Removed_operations.branch metrics)
      metrics specialized_call_sites
  in
  (* For each primitive, and each specialized call site: - notify it removed if
     its value is known - else add the prim to the code size *)
  List.fold_left
    (fun metrics (bound_var, prim) ->
      let simple = Simple.var (Bound_var.var bound_var) in
      let canonical_simple =
        Typing_env.get_canonical_simple_exn typing_env
          ~min_name_mode:Name_mode.in_types simple
      in
      let disappears, stays =
        Simple.pattern_match canonical_simple
          ~name:(fun name ~coercion:_ ->
            match Join_analysis.known_values_at_uses name join_info with
            | Unknown -> [], specialized_call_sites
            | Known { known_at_uses; unknown_at_uses } ->
              known_at_uses, unknown_at_uses)
          ~const:(fun _ ->
            (* if the canonical is a simple already, then specialization does
               not change much *)
            [], [])
      in
      let metrics =
        List.fold_left
          (fun acc _ ->
            Cost_metrics.notify_removed
              ~operation:(Removed_operations.prim prim)
              acc)
          metrics disappears
      in
      let metrics =
        List.fold_left
          (fun acc _ ->
            Cost_metrics.notify_added ~code_size:(Code_size.prim prim) acc)
          metrics stays
      in
      metrics)
    metrics cost.primitives
