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

type cost =
  { primitives : (Bound_var.t * Flambda_primitive.t) list;
    non_lifted_continuations : Original_handlers.t list
  }

type t =
  | Can_specialize of cost
  | Cannot_specialize of { reason : reason }

let flambda2_profile_mim =
  Oxcaml_args.Extra_options.bool __LOC__ "flambda2-profile-mim"

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
  | Can_specialize { primitives; non_lifted_continuations; } ->
    Format.fprintf ppf "@[<hv>(can_specialize@ \
        @[<hv 1>(primitives@ %a)@]@ \
        @[<hv 1>(non_lifted_continuations@ %a)@]\
      )@]"
      (Format.pp_print_list print_prim) primitives
      (Format.pp_print_list Original_handlers.print) non_lifted_continuations
  | Cannot_specialize { reason } ->
    Format.fprintf ppf "@[<hv>(cannot_specialize@ \
        @[<hv 1>(reason@ %a)@]\
      )@]"
    print_reason reason

(* Creations *)

let cannot_specialize reason = Cannot_specialize { reason }

let can_specialize () =
  if Flambda_features.match_in_match ()
  then Can_specialize { primitives = []; non_lifted_continuations = [] }
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
  update_cost t ~f:(fun ({ primitives; _ } as cost) ->
      (* CR gbury: we might want to limit the number of primitives we store, so
         that e.g. if there are more than <n> primitives, we stop storing them
         and decide that no specialization should take place *)
      let primitives = (bound_var, prim) :: primitives in
      { cost with primitives })

let add_continuations ~can_be_lifted handlers t =
  if can_be_lifted
  then t
  else
    update_cost t ~f:(fun ({ non_lifted_continuations; _ } as cost) ->
        let non_lifted_continuations = handlers :: non_lifted_continuations in
        { cost with non_lifted_continuations })

(* Computing cost and benefits *)

let rec expr_size ~machine_width acc e =
  let open! Code_size in
  match Flambda.Expr.descr e with
  | Flambda.Let l ->
    let defining_expr_size =
      let defining_expr = Flambda.Let_expr.defining_expr l in
      match defining_expr with
      | Flambda.Simple s -> simple s
      | Flambda.Prim (p, _) -> prim ~machine_width p
      | Flambda.Set_of_closures (_, _) -> assert false
      | Flambda.Static_consts group ->
        List.fold_left
          (fun acc const_or_code ->
            match (const_or_code : Flambda.static_const_or_code) with
            | Code code0 ->
              let params_and_body = Code0.params_and_body code0 in
              Flambda.Function_params_and_body.pattern_match params_and_body
                ~f:(fun
                    ~return_continuation:_
                    ~exn_continuation:_
                    _params
                    ~body
                    ~my_closure:_
                    ~is_my_closure_used:_
                    ~my_alloc_mode:_
                    ~my_depth:_
                    ~free_names_of_body:_
                  -> expr_size ~machine_width acc body)
            | Deleted_code -> acc
            | Static_const _c ->
              (* CR gbury: better represent the size of the static_const, since
                 this function is mostly interested in the size of an expr as it
                 relates to the amount of work we might spend on simplifying
                 it *)
              acc + static_consts ())
          zero
          (Flambda.Static_const_group.to_list group)
      | Flambda.Rec_info _ -> zero
    in
    let acc = acc + defining_expr_size in
    Flambda.Let_expr.pattern_match l ~f:(fun _ ~body ->
        expr_size ~machine_width acc body)
  | Flambda.Let_cont _ -> assert false
  | Flambda.Apply a -> acc + apply a
  | Flambda.Apply_cont ac -> acc + apply_cont ac
  | Flambda.Switch s -> acc + switch s
  | Flambda.Invalid _ -> acc + invalid

let print_cont_sizes ~machine_width ~inlining_arguments ppf t =
  let l = t.non_lifted_continuations in
  let n = List.length l in
  let pp_dash ppf () = Format.fprintf ppf "-" in
  let pp_comma ppf () = Format.fprintf ppf ", " in
  let pp_handlers_sizes ~can_be_lifted ppf l =
    let pp ppf handler =
      let size = expr_size ~machine_width Code_size.zero handler in
      let float_size = Code_size.evaluate ~args:inlining_arguments size in
      Format.fprintf ppf "%f" float_size
    in
    Format.fprintf ppf "%s%a"
      (if can_be_lifted then "!" else "")
      (Format.pp_print_list ~pp_sep:pp_dash pp)
      l
  in
  let pp ppf handlers =
    match (handlers : Original_handlers.t) with
    | Original_handlers.Recursive { continuation_handlers; can_be_lifted; _ } ->
      let l = Continuation.Lmap.data continuation_handlers in
      let l =
        List.map (fun { One_recursive_handler.handler; _ } -> handler) l
      in
      pp_handlers_sizes ~can_be_lifted ppf l
    | Original_handlers.Non_recursive { handler; can_be_lifted; _ } ->
      pp_handlers_sizes ~can_be_lifted ppf [handler]
  in
  if n = 0
  then Format.fprintf ppf "N/A"
  else
    Format.fprintf ppf "n=%d, sizes: %a" n
      (Format.pp_print_list ~pp_sep:pp_comma pp)
      l

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
  let metrics =
    List.fold_left
      (fun metrics (bound_var, prim) ->
        let simple = Simple.var (Bound_var.var bound_var) in
        let disappears, stays =
          match
            Join_analysis.simple_refined_at_join join_analysis typing_env simple
          with
          | Not_refined_at_join -> 0, n_total
          | Invariant_in_all_uses _ ->
            (* if the canonical is a simple already, then specialization does
               not change much *)
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
  in
  (* Count the non lifted continuations. We currently cannot easily access the
     size of handlers, since we have not yet done a downwards traversal on them,
     so instead we just use an arbitrary size.

     CR gbury: find a way to get an estimate of the code size of
     handlers/exprs *)
  let handler_metrics (_handler : Flambda.Expr.t) =
    Cost_metrics.from_size (Code_size.of_int 5)
  in
  let metrics =
    List.fold_left
      (fun metrics handlers ->
        match (handlers : Original_handlers.t) with
        | Non_recursive { handler; _ } ->
          Cost_metrics.( + ) metrics (handler_metrics handler)
        | Recursive { continuation_handlers; _ } ->
          Continuation.Lmap.fold
            (fun _cont one_rec_handler metrics ->
              let handler = one_rec_handler.One_recursive_handler.handler in
              Cost_metrics.( + ) metrics (handler_metrics handler))
            continuation_handlers metrics)
      metrics cost.non_lifted_continuations
  in
  (* Return *)
  metrics
