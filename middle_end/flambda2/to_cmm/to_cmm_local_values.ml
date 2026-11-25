(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street Europe                *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module P = Flambda_primitive

(* Directed graphs over variables, as adjacency maps from a variable to the
   variables its value may flow to. *)
type graph = Variable.Set.t Variable.Map.t

let add_edge (graph : graph) ~src ~dst : graph =
  Variable.Map.update src
    (function
      | None -> Some (Variable.Set.singleton dst)
      | Some dsts -> Some (Variable.Set.add dst dsts))
    graph

let reachable (graph : graph) seeds =
  let rec loop seen = function
    | [] -> seen
    | var :: todo ->
      let dsts =
        match Variable.Map.find_opt var graph with
        | None -> Variable.Set.empty
        | Some dsts -> dsts
      in
      let new_vars = Variable.Set.diff dsts seen in
      loop
        (Variable.Set.union seen new_vars)
        (Variable.Set.elements new_vars @ todo)
  in
  loop seeds (Variable.Set.elements seeds)

type acc =
  { mutable region_graph : graph;
    mutable begun_regions : Variable.Set.t;
    mutable ended_regions : Variable.Set.t;
    mutable value_graph : graph;
    mutable local_allocs : (Variable.t * Variable.t) list;
        (* Variables bound to local allocations, with their regions. *)
    mutable local_returns : (Continuation.t * Variable.t) list;
        (* Return continuations of calls that may allocate their results in a
           region, with those regions. *)
    mutable jumps : (Continuation.t * Simple.t list) list;
        (* The [Apply_cont]s of the body. *)
    mutable exn_jumps : (Continuation.t * Simple.t list) list;
        (* The exception continuations of the [Apply]s of the body, with their
           extra arguments. *)
    mutable cont_params : Bound_parameter.t list Continuation.Map.t
        (* The parameters of the continuations bound in the body, as they are
           expected at [Apply_cont] (invariant parameters first). *)
  }

(* The shape of the body, retained by the traversal so that the body can be
   rebuilt, once the analysis is complete, with the variables it found marked on
   their binders. The binders are those opened by the traversal; the leaves bind
   nothing and are reused as they are. *)
type skeleton =
  | Leaf of
      { expr : Expr.t;
        free_names : Name_occurrences.t
      }
  | Let of
      { bound_pattern : Bound_pattern.t;
        defining_expr : Named.t;
        body : skeleton
      }
  | Let_cont_non_recursive of
      { k : Continuation.t;
        handler : handler;
        can_be_lifted : bool;
        body : skeleton
      }
  | Let_cont_recursive of
      { invariant_params : Bound_parameters.t;
        handlers : handler Continuation.Lmap.t;
        body : skeleton
      }

and handler =
  { params : Bound_parameters.t;
    handler_body : skeleton;
    is_exn_handler : bool;
    is_cold : bool
  }

let var_of_simple simple =
  match Simple.must_be_var simple with
  | None -> None
  | Some (var, _coercion) -> Some var

let is_region kind =
  Flambda_kind.equal (Flambda_kind.With_subkind.kind kind) Flambda_kind.region

let is_value kind = Flambda_kind.is_value (Flambda_kind.With_subkind.kind kind)

let add_flow acc ~arg ~(param : Bound_parameter.t) =
  match var_of_simple arg with
  | None -> ()
  | Some src ->
    let dst = Bound_parameter.var param in
    let kind = Bound_parameter.kind param in
    if is_region kind
    then acc.region_graph <- add_edge acc.region_graph ~src ~dst
    else if is_value kind
    then acc.value_graph <- add_edge acc.value_graph ~src ~dst

let add_flows acc k args =
  match Continuation.Map.find_opt k acc.cont_params with
  | None ->
    (* The return or exception continuation of the body itself: its handler is
       not part of the body. *)
    ()
  | Some params ->
    if List.compare_lengths params args <> 0
    then
      Misc.fatal_errorf
        "To_cmm_local_values: wrong number of arguments for continuation %a"
        Continuation.print k;
    List.iter2 (fun param arg -> add_flow acc ~arg ~param) params args

let add_exn_flows acc k extra_args =
  match Continuation.Map.find_opt k acc.cont_params with
  | None -> ()
  | Some [] ->
    Misc.fatal_errorf
      "To_cmm_local_values: exception continuation %a has no parameter"
      Continuation.print k
  | Some (_exn_param :: params) ->
    (* The exception value is always on the heap. *)
    if List.compare_lengths params extra_args <> 0
    then
      Misc.fatal_errorf
        "To_cmm_local_values: wrong number of extra arguments for exception \
         continuation %a"
        Continuation.print k;
    List.iter2 (fun param arg -> add_flow acc ~arg ~param) params extra_args

let record_alloc acc var (alloc_mode : Alloc_mode.For_allocations.t) =
  match alloc_mode with
  | Heap { alloc_region = _ } -> ()
  | Local { alloc_region = _; region } ->
    acc.local_allocs <- (var, region) :: acc.local_allocs

let record_prim acc var (prim : P.t) =
  match[@ocaml.warning "-4"] prim with
  | Variadic ((Begin_region _ | Begin_try_region _), _) ->
    acc.begun_regions <- Variable.Set.add var acc.begun_regions
  | Unary ((End_region _ | End_try_region _), region) -> (
    match var_of_simple region with
    | None -> ()
    | Some region ->
      acc.ended_regions <- Variable.Set.add region acc.ended_regions)
  | Variadic (Make_block (_, _, alloc_mode), _)
  | Variadic (Make_array (_, _, alloc_mode), _) ->
    (* A block on the heap, or in a region that outlives this body, cannot point
       into a region ended in this body; so there is no flow from the fields to
       the block. *)
    record_alloc acc var alloc_mode
  | Unary (Box_number (_, alloc_mode), _) -> record_alloc acc var alloc_mode
  | Nullary _ | Unary _ | Binary _ | Ternary _ | Quaternary _ ->
    if Flambda_kind.is_value (P.result_kind' prim)
    then
      List.iter
        (fun arg ->
          match var_of_simple arg with
          | None -> ()
          | Some src ->
            acc.value_graph <- add_edge acc.value_graph ~src ~dst:var)
        (P.args prim)

let record_let acc (bound_pattern : Bound_pattern.t) (defining_expr : Named.t) =
  match bound_pattern, defining_expr with
  | Singleton var, Simple simple -> (
    match var_of_simple simple with
    | None -> ()
    | Some src ->
      let dst = Bound_var.var var in
      acc.region_graph <- add_edge acc.region_graph ~src ~dst;
      acc.value_graph <- add_edge acc.value_graph ~src ~dst)
  | Singleton var, Prim (prim, _dbg) -> record_prim acc (Bound_var.var var) prim
  | Set_of_closures vars, Set_of_closures (_, alloc_mode) ->
    List.iter (fun var -> record_alloc acc (Bound_var.var var) alloc_mode) vars
  | Static _, Static_consts _ | Singleton _, Rec_info _ -> ()
  | ( (Singleton _ | Set_of_closures _ | Static _),
      (Simple _ | Prim _ | Set_of_closures _ | Static_consts _ | Rec_info _) )
    ->
    (* Combinations which do not occur in well-formed terms. *)
    ()

let record_params acc k params =
  acc.cont_params
    <- Continuation.Map.add k (Bound_parameters.to_list params) acc.cont_params

(* The flows into continuation parameters are only recorded once the whole body
   has been traversed, so that the order of the traversal does not matter: a
   handler may be reached before its continuation is bound (in a recursive
   group) and each binder is opened exactly once. *)
let record_jump acc apply_cont =
  acc.jumps
    <- (Apply_cont.continuation apply_cont, Apply_cont.args apply_cont)
       :: acc.jumps

let record_apply acc apply =
  (match Apply.continuation apply with
  | Never_returns -> ()
  | Return k -> (
    match Apply.return_mode apply with
    | Not_alloc_stack { alloc_region = _ } -> ()
    | Maybe_alloc_stack { alloc_region = _; region; ghost_region = _ } ->
      acc.local_returns <- (k, region) :: acc.local_returns));
  let exn_continuation = Apply.exn_continuation apply in
  acc.exn_jumps
    <- ( Exn_continuation.exn_handler exn_continuation,
         List.map fst (Exn_continuation.extra_args exn_continuation) )
       :: acc.exn_jumps

(* The traversal opens every binder of the body, under a fresh name as any
   traversal does, and records the flows between the names it sees. The skeleton
   it returns is built from those same names, so the variables found by the
   analysis are the binders of the term rebuilt from the skeleton. *)
let rec traverse acc (e : Expr.t) : skeleton =
  match Expr.descr e with
  | Let let_expr ->
    Let.pattern_match let_expr ~f:(fun bound_pattern ~body ->
        let defining_expr = Let.defining_expr let_expr in
        record_let acc bound_pattern defining_expr;
        Let { bound_pattern; defining_expr; body = traverse acc body })
  | Let_cont
      (Non_recursive
         { handler;
           num_free_occurrences = _;
           is_applied_with_traps = _;
           can_be_lifted
         }) ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
        let handler =
          traverse_handler acc k ~invariant_params:Bound_parameters.empty
            (Non_recursive_let_cont_handler.handler handler)
        in
        Let_cont_non_recursive
          { k; handler; can_be_lifted; body = traverse acc body })
  | Let_cont (Recursive handlers) ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params ~body conts ->
        let handlers =
          Continuation.Lmap.mapi
            (fun k handler -> traverse_handler acc k ~invariant_params handler)
            (Continuation_handlers.to_map conts)
        in
        Let_cont_recursive
          { invariant_params; handlers; body = traverse acc body })
  | Apply apply ->
    record_apply acc apply;
    Leaf { expr = e; free_names = Apply.free_names apply }
  | Apply_cont apply_cont ->
    record_jump acc apply_cont;
    Leaf { expr = e; free_names = Apply_cont.free_names apply_cont }
  | Switch switch ->
    Target_ocaml_int.Map.iter
      (fun _ arm -> record_jump acc (Switch_arm.action arm))
      (Switch.arms switch);
    Leaf { expr = e; free_names = Switch.free_names switch }
  | Invalid _ -> Leaf { expr = e; free_names = Name_occurrences.empty }

and traverse_handler acc k ~invariant_params handler =
  Continuation_handler.pattern_match handler ~f:(fun params ~handler:body ->
      record_params acc k (Bound_parameters.append invariant_params params);
      { params;
        handler_body = traverse acc body;
        is_exn_handler = Continuation_handler.is_exn_handler handler;
        is_cold = Continuation_handler.is_cold handler
      })

let mark_bound_var ~is_marked bound_var =
  if is_marked (Bound_var.var bound_var)
  then Bound_var.mark_maybe_local bound_var
  else bound_var

let mark_bound_pattern ~is_marked (bound_pattern : Bound_pattern.t) :
    Bound_pattern.t =
  match bound_pattern with
  | Singleton bound_var ->
    Bound_pattern.singleton (mark_bound_var ~is_marked bound_var)
  | Set_of_closures bound_vars ->
    Bound_pattern.set_of_closures
      (List.map (mark_bound_var ~is_marked) bound_vars)
  | Static _ -> bound_pattern

let mark_params ~is_marked params =
  Bound_parameters.create
    (List.map
       (fun param ->
         if is_marked (Bound_parameter.var param)
         then Bound_parameter.mark_maybe_local param
         else param)
       (Bound_parameters.to_list params))

let remove_params free_names params =
  List.fold_left
    (fun free_names param ->
      Name_occurrences.remove_var free_names ~var:(Bound_parameter.var param))
    free_names
    (Bound_parameters.to_list params)

(* Rebuilds the term from its skeleton, marking the binders of the variables
   found by the analysis. The free names of each rebuilt subterm are computed as
   [Simplify] computes them when it rebuilds terms, so that the occurrence
   counts stored in the rebuilt binders (on which [To_cmm] bases its inlining
   decisions) are unchanged. *)
let rec materialise ~is_marked (skeleton : skeleton) :
    Expr.t * Name_occurrences.t =
  match skeleton with
  | Leaf { expr; free_names } -> expr, free_names
  | Let { bound_pattern; defining_expr; body } ->
    let body, free_names_of_body = materialise ~is_marked body in
    let bound_pattern = mark_bound_pattern ~is_marked bound_pattern in
    let defining_expr_free_names = Named.free_names defining_expr in
    let name_mode = Bound_pattern.name_mode bound_pattern in
    let defining_expr_free_names =
      if Name_mode.is_phantom name_mode
      then
        Name_occurrences.downgrade_occurrences_at_strictly_greater_name_mode
          defining_expr_free_names name_mode
      else defining_expr_free_names
    in
    let free_names =
      match bound_pattern with
      | Singleton _ | Set_of_closures _ ->
        Name_occurrences.union defining_expr_free_names
          (Name_occurrences.diff free_names_of_body
             ~without:(Bound_pattern.free_names bound_pattern))
      | Static bound_static ->
        Name_occurrences.diff
          (Name_occurrences.union free_names_of_body defining_expr_free_names)
          ~without:(Bound_static.free_names bound_static)
    in
    let let_expr =
      Let.create bound_pattern defining_expr ~body
        ~free_names_of_body:(Known free_names_of_body)
    in
    Expr.create_let let_expr, free_names
  | Let_cont_non_recursive { k; handler; can_be_lifted; body } ->
    let handler, free_names_of_handler =
      materialise_handler ~is_marked ~invariant_params:Bound_parameters.empty
        handler
    in
    let body, free_names_of_body = materialise ~is_marked body in
    let free_names =
      Name_occurrences.remove_continuation ~continuation:k
        (Name_occurrences.union free_names_of_body free_names_of_handler)
    in
    let create =
      if can_be_lifted
      then Let_cont.create_non_recursive
      else Let_cont.create_non_liftable
    in
    ( create k handler ~body ~free_names_of_body:(Known free_names_of_body),
      free_names )
  | Let_cont_recursive { invariant_params; handlers; body } ->
    let handlers =
      Continuation.Lmap.map
        (materialise_handler ~is_marked ~invariant_params)
        handlers
    in
    let body, free_names_of_body = materialise ~is_marked body in
    let free_names =
      Continuation.Lmap.fold
        (fun _ (_, free_names_of_handler) free_names ->
          Name_occurrences.union free_names
            (Name_occurrences.increase_counts free_names_of_handler))
        handlers free_names_of_body
    in
    let free_names =
      Continuation.Lmap.fold
        (fun k _ free_names ->
          Name_occurrences.remove_continuation free_names ~continuation:k)
        handlers free_names
    in
    let handlers = Continuation.Lmap.map fst handlers in
    let invariant_params = mark_params ~is_marked invariant_params in
    Let_cont.create_recursive ~invariant_params handlers ~body, free_names

(* Returns the rebuilt handler with its free names, less its own parameters and
   the invariant parameters of its group. *)
and materialise_handler ~is_marked ~invariant_params
    { params; handler_body; is_exn_handler; is_cold } =
  let body, free_names_of_handler = materialise ~is_marked handler_body in
  let free_names =
    remove_params (remove_params free_names_of_handler params) invariant_params
  in
  let params = mark_params ~is_marked params in
  ( Continuation_handler.create params ~handler:body
      ~free_names_of_handler:(Known free_names_of_handler) ~is_exn_handler
      ~is_cold,
    free_names )

(* Whether the body contains a region primitive at all, in which case the
   analysis is needed (if only to check that the regions ended are begun in the
   body). Most function bodies do not. *)
let rec has_region_prim (e : Expr.t) =
  match Expr.descr e with
  | Let let_expr -> (
    match[@ocaml.warning "-4"] Let.defining_expr let_expr with
    | Prim
        ( ( Variadic ((Begin_region _ | Begin_try_region _), _)
          | Unary ((End_region _ | End_try_region _), _) ),
          _ ) ->
      true
    | Prim _ | Simple _ | Set_of_closures _ | Static_consts _ | Rec_info _ ->
      Let.pattern_match let_expr ~f:(fun _bound_pattern ~body ->
          has_region_prim body))
  | Let_cont (Non_recursive { handler; _ }) ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun _k ~body ->
        has_region_prim body
        || Continuation_handler.pattern_match
             (Non_recursive_let_cont_handler.handler handler)
             ~f:(fun _params ~handler -> has_region_prim handler))
  | Let_cont (Recursive handlers) ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params:_ ~body conts ->
        has_region_prim body
        || Continuation.Lmap.exists
             (fun _k handler ->
               Continuation_handler.pattern_match handler
                 ~f:(fun _params ~handler -> has_region_prim handler))
             (Continuation_handlers.to_map conts))
  | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> false

let foreign_regions ~params ~my_region ~my_ghost_region =
  List.fold_left
    (fun foreign (param : Bound_parameter.t) ->
      if is_region (Bound_parameter.kind param)
      then Variable.Set.add (Bound_parameter.var param) foreign
      else foreign)
    (Variable.Set.of_list
       (Option.to_list my_region @ Option.to_list my_ghost_region))
    (Bound_parameters.to_list params)

let rewrite ~params ~my_region ~my_ghost_region body =
  if
    (not (Flambda_features.stack_allocation_enabled ()))
    || not (has_region_prim body)
  then body
  else
    let acc =
      { region_graph = Variable.Map.empty;
        begun_regions = Variable.Set.empty;
        ended_regions = Variable.Set.empty;
        value_graph = Variable.Map.empty;
        local_allocs = [];
        local_returns = [];
        jumps = [];
        exn_jumps = [];
        cont_params = Continuation.Map.empty
      }
    in
    let skeleton = traverse acc body in
    List.iter (fun (k, args) -> add_flows acc k args) acc.jumps;
    List.iter
      (fun (k, extra_args) -> add_exn_flows acc k extra_args)
      acc.exn_jumps;
    let local_regions = reachable acc.region_graph acc.begun_regions in
    (* Check that every region ended in the body was begun in the body, and
       cannot be a region passed in from outside. *)
    let foreign_regions =
      reachable acc.region_graph
        (foreign_regions ~params ~my_region ~my_ghost_region)
    in
    Variable.Set.iter
      (fun region ->
        if
          Variable.Set.mem region foreign_regions
          || not (Variable.Set.mem region local_regions)
        then
          Misc.fatal_errorf
            "To_cmm_local_values: region %a is ended but was not begun in the \
             same function body"
            Variable.print region)
      acc.ended_regions;
    let seeds =
      List.fold_left
        (fun seeds (var, region) ->
          if Variable.Set.mem region local_regions
          then Variable.Set.add var seeds
          else seeds)
        Variable.Set.empty acc.local_allocs
    in
    let seeds =
      List.fold_left
        (fun seeds (k, region) ->
          if not (Variable.Set.mem region local_regions)
          then seeds
          else
            match Continuation.Map.find_opt k acc.cont_params with
            | None -> seeds
            | Some params ->
              List.fold_left
                (fun seeds (param : Bound_parameter.t) ->
                  if is_value (Bound_parameter.kind param)
                  then Variable.Set.add (Bound_parameter.var param) seeds
                  else seeds)
                seeds params)
        seeds acc.local_returns
    in
    let maybe_local = reachable acc.value_graph seeds in
    (* The parameters of the body (and its regions) are bound outside the body;
       a local value cannot reach them. *)
    let bound_outside =
      Variable.Set.of_list
        (Bound_parameters.vars params
        @ Option.to_list my_region
        @ Option.to_list my_ghost_region)
    in
    let escaping = Variable.Set.inter maybe_local bound_outside in
    if not (Variable.Set.is_empty escaping)
    then
      Misc.fatal_errorf
        "To_cmm_local_values: variables bound outside the body may hold local \
         values: %a"
        Variable.Set.print escaping;
    if Variable.Set.is_empty maybe_local
    then body
    else
      let is_marked var = Variable.Set.mem var maybe_local in
      fst (materialise ~is_marked skeleton)
