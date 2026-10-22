(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module K = Flambda_kind
module P = Flambda_primitive
module RE = Rebuilt_expr

type region =
  { region : Variable.t;
    ghost_region : Variable.t;
    inner_conts : Continuation.Set.t;
    (* [false] for the region of the caller of a function returning at mode
       local ([my_region]), which the function must not close. *)
    closable : bool
  }

type t =
  { solution : Escape_solution.t;
    code_ids_directly_called : Name.t -> Code_id.Set.t Or_unknown.t;
    params_of_continuation : Continuation.t -> Bound_parameter.t list;
    (* For each region scope, the continuations defined inside it. *)
    conts_per_scope : Continuation.Set.t Code_id_or_name.Map.t;
    continuation_info : Traverse_acc.continuation_info Continuation.Map.t;
    machine_width : Target_system.Machine_width.t;
    (* The return continuation of the enclosing function, or of the unit:
       applications returning to it are tail calls. *)
    return_continuation : Continuation.t;
    (* The regions of the enclosing region scopes being rebuilt, innermost
       first. *)
    regions : region list
  }

let create ~solution ~code_ids_directly_called ~params_of_continuation
    ~conts_per_scope ~continuation_info ~machine_width ~return_continuation =
  { solution;
    code_ids_directly_called;
    params_of_continuation;
    conts_per_scope;
    continuation_info;
    machine_width;
    return_continuation;
    regions = []
  }

(* Turn a heap allocation into a stack allocation in the region of the innermost
   region scope when the escape analysis has decided it is local. *)
let localize_alloc_mode0 t ~is_local (alloc_mode : Alloc_mode.For_allocations.t)
    =
  match alloc_mode, t.regions with
  | Heap { alloc_region }, { region; _ } :: _ when is_local ->
    Alloc_mode.For_allocations.local ~alloc_region ~region
  | (Heap _ | Local _), _ -> alloc_mode

let localize_alloc_mode t var alloc_mode =
  localize_alloc_mode0 t
    ~is_local:(Escape_solution.is_local_allocation t.solution var)
    alloc_mode

let localize_alloc_mode_of_pattern t (bp : Bound_pattern.t) alloc_mode =
  match bp with
  | Singleton bv -> localize_alloc_mode t (Bound_var.var bv) alloc_mode
  | Set_of_closures _ | Static _ -> alloc_mode

let localize_set_of_closures_alloc_mode t bvs alloc_mode =
  localize_alloc_mode0 t
    ~is_local:
      (List.for_all
         (fun bv ->
           Escape_solution.is_local_allocation t.solution (Bound_var.var bv))
         bvs)
    alloc_mode

let localize_box_number t bv (defining_expr : Named.t) : Named.t =
  match[@ocaml.warning "-fragile-match"] defining_expr with
  | Prim (Unary (Box_number (bn, alloc_mode), contents), dbg)
    when Escape_solution.is_local_allocation t.solution (Bound_var.var bv) ->
    let alloc_mode = localize_alloc_mode0 t ~is_local:true alloc_mode in
    Named.create_prim (Unary (Box_number (bn, alloc_mode), contents)) dbg
  | Prim _ | Simple _ | Set_of_closures _ | Static_consts _ | Rec_info _ ->
    defining_expr

(* The regions to close when jumping to continuation [k] from the current point:
   those of the enclosing region scopes in which [k] is not defined. Nothing is
   closed on the exceptional path (see [Escape_analysis]). *)
let regions_exited_by t k =
  match t.regions with
  | [] -> []
  | _ :: _ ->
    let is_exn_handler =
      match Continuation.Map.find_opt k t.continuation_info with
      | Some { Traverse_acc.is_exn_handler; _ } -> is_exn_handler
      | None ->
        Misc.fatal_errorf
          "[regions_exited_by]: no information about continuation %a"
          Continuation.print k
    in
    if is_exn_handler
    then []
    else
      List.filter
        (fun { inner_conts; closable; _ } ->
          closable && not (Continuation.Set.mem k inner_conts))
        t.regions

let end_regions t regions (body : RE.t) : RE.t =
  let end_region ~ghost region body =
    let end_region : P.t = Unary (End_region { ghost }, Simple.var region) in
    RE.create_let
      (Bound_pattern.singleton
         (Bound_var.create
            (Variable.create "region_end" K.value)
            Flambda_debug_uid.none Name_mode.normal))
      (Named.create_prim end_region Debuginfo.none)
      ~size_of_defining_expr:
        (Code_size.prim ~machine_width:t.machine_width end_region)
      ~body
  in
  List.fold_left
    (fun body { region; ghost_region; _ } ->
      end_region ~ghost:false region (end_region ~ghost:true ghost_region body))
    body regions

let begin_regions t { region; ghost_region; _ } (body : RE.t) : RE.t =
  let begin_region ~ghost region body =
    let begin_region : P.t = Variadic (Begin_region { ghost }, []) in
    RE.create_let
      (Bound_pattern.singleton
         (Bound_var.create region Flambda_debug_uid.none Name_mode.normal))
      (Named.create_prim begin_region Debuginfo.none)
      ~size_of_defining_expr:
        (Code_size.prim ~machine_width:t.machine_width begin_region)
      ~body
  in
  begin_region ~ghost:false region (begin_region ~ghost:true ghost_region body)

let end_regions_before_jump t k body =
  end_regions t (regions_exited_by t k) body

(* Calls to functions made to return at mode local (see [Escape_analysis])
   receive their result in the region of the innermost enclosing scope. *)
let set_local_return_mode t apply =
  let returns_local code_id =
    Escape_solution.returns_local t.solution code_id
  in
  (* The callee may be given by the call kind, or be one of the functions the
     callee expression is known to evaluate to (which is how [Rebuild] changes
     the calling convention of known-arity indirect calls). *)
  let callee_code_ids_return_local () =
    match Apply.callee apply with
    | None -> false
    | Some callee ->
      Simple.pattern_match callee
        ~const:(fun _ -> false)
        ~name:(fun name ~coercion:_ ->
          match t.code_ids_directly_called name with
          | Unknown -> false
          | Known code_ids -> Code_id.Set.exists returns_local code_ids)
  in
  let callee_returns_local =
    match Apply.call_kind apply with
    | Function { function_call = Direct code_id; _ } ->
      returns_local code_id || callee_code_ids_return_local ()
    | Function { function_call = Indirect_known_arity _; _ } ->
      callee_code_ids_return_local ()
    | Function { function_call = Indirect_unknown_arity; _ }
    | Method _ | C_call _ | Effect _ ->
      false
  in
  if not callee_returns_local
  then apply
  else
    match t.regions with
    | [] ->
      Misc.fatal_errorf "No region for the local result of application %a"
        Apply.print apply
    | { region; ghost_region; _ } :: _ ->
      let alloc_region =
        Alloc_mode.For_applications.alloc_region (Apply.return_mode apply)
      in
      Apply.with_return_mode apply
        (Alloc_mode.For_applications.maybe_alloc_stack ~alloc_region ~region
           ~ghost_region)

(* A continuation that closes the regions left when jumping to [k], then jumps
   to [k]. Used where an [End_region] cannot simply be inserted before the jump:
   [Switch] arms, and non-tail applications returning to [k]. Such closing
   continuations are bound locally, around the [Switch] or the application. *)
type closers = (Continuation.t * RE.continuation_handler) list

let closer t k =
  let k_close = Continuation.create () in
  let params = t.params_of_continuation k in
  let apply_cont =
    Apply_cont_expr.create k
      ~args:(List.map (fun p -> Simple.var (Bound_parameter.var p)) params)
      ~dbg:Debuginfo.none
  in
  let handler =
    RE.from_expr
      ~expr:(Expr.create_apply_cont apply_cont)
      ~free_names:(Apply_cont_expr.free_names apply_cont)
      ~code_size:(Code_size.apply_cont apply_cont)
  in
  let handler = end_regions t (regions_exited_by t k) handler in
  let handler =
    RE.create_continuation_handler
      (Bound_parameters.create params)
      ~handler ~is_exn_handler:false ~is_cold:false
  in
  k_close, handler

let bind_closers (closers : closers) body =
  List.fold_left
    (fun body (k_close, handler) ->
      RE.create_non_recursive_let_cont k_close handler ~body)
    body closers

let redirect_switch_arms t arms =
  let arms, closers =
    Target_ocaml_int.Map.fold
      (fun discriminant arm (arms, closers) ->
        let action = Switch_arm.action arm in
        let k = Apply_cont_expr.continuation action in
        match regions_exited_by t k with
        | [] -> Target_ocaml_int.Map.add discriminant arm arms, closers
        | _ :: _ ->
          let k_close, closers =
            match Continuation.Map.find_opt k closers with
            | Some (k_close, _) -> k_close, closers
            | None ->
              let k_close, handler = closer t k in
              k_close, Continuation.Map.add k (k_close, handler) closers
          in
          let arm =
            Switch_arm.create
              ~annotations:(Switch_arm.annotations arm)
              (Apply_cont_expr.with_continuation action k_close)
          in
          Target_ocaml_int.Map.add discriminant arm arms, closers)
      arms
      (Target_ocaml_int.Map.empty, Continuation.Map.empty)
  in
  arms, List.map snd (Continuation.Map.bindings closers)

let rebuild_apply t apply ~rebuild =
  let apply = set_local_return_mode t apply in
  let expr : RE.t = rebuild apply in
  match Apply.continuation apply with
  | Never_returns -> expr
  | Return k -> (
    match regions_exited_by t k with
    | [] -> expr
    | regions ->
      let is_tail_call =
        Continuation.equal k t.return_continuation
        && match Apply.position apply with Normal -> true | Nontail -> false
      in
      if is_tail_call
      then
        (* Tail call: close the regions beforehand so that it remains a tail
           call; no local value is passed to it (see
           [Escape_traverse.record_apply]). *)
        end_regions t regions expr
      else
        let k_close, handler = closer t k in
        let renaming = Renaming.add_continuation Renaming.empty k k_close in
        RE.create_non_recursive_let_cont k_close handler
          ~body:
            (RE.from_expr
               ~expr:(Expr.apply_renaming expr.expr renaming)
               ~free_names:
                 (Name_occurrences.apply_renaming expr.free_names renaming)
               ~code_size:expr.code_size))

(* Rebuild the body of a region scope (a function body or a recursive
   continuation handler), adding a region around it if the escape analysis found
   local allocations in it: [Begin_region] at the start, and [End_region] on
   every exit, which [Rebuild.rebuild_expr] inserts when rebuilding the jumps to
   continuations defined outside the scope. Tail calls are preserved by closing
   the regions before them (no local value may be passed to them, see
   [Escape_traverse.record_apply]). Nothing is done on the exceptional path: the
   exception handler that catches the exception closes the region (as for
   regions introduced by the type checker), or, failing that, an enclosing
   region. *)
let rebuild_region_scope t ~scope ~rebuild_body =
  if not (Escape_solution.scope_needs_region t.solution scope)
  then rebuild_body t
  else
    let region =
      { region = Variable.create "region" K.region;
        ghost_region = Variable.create "ghost_region" K.region;
        inner_conts =
          (match Code_id_or_name.Map.find_opt scope t.conts_per_scope with
          | Some conts -> conts
          | None -> Continuation.Set.empty);
        closable = true
      }
    in
    let body, res = rebuild_body { t with regions = region :: t.regions } in
    begin_regions t region body, res

(* A function returning at mode local allocates in its caller's region, which it
   must not close (see [Escape_analysis]). *)
let enter_function t ~code_id ~return_continuation
    ~(my_alloc_mode : Alloc_mode.For_applications.t) =
  let t = { t with return_continuation; regions = [] } in
  if not (Escape_solution.returns_local t.solution code_id)
  then my_alloc_mode, t
  else
    match my_alloc_mode with
    | Maybe_alloc_stack _ ->
      Misc.fatal_errorf
        "Function %a already returns at mode local, but the escape analysis \
         decided to make it do so"
        Code_id.print code_id
    | Not_alloc_stack { alloc_region } ->
      let region = Variable.create "my_region" K.region in
      let ghost_region = Variable.create "my_ghost_region" K.region in
      ( Alloc_mode.For_applications.maybe_alloc_stack ~alloc_region ~region
          ~ghost_region,
        { t with
          regions =
            [ { region;
                ghost_region;
                inner_conts = Continuation.Set.empty;
                closable = false
              } ]
        } )

let rebuild_function_body t ~code_id ~rebuild_body =
  if Escape_solution.returns_local t.solution code_id
  then rebuild_body t
  else
    rebuild_region_scope t
      ~scope:(Code_id_or_name.code_id code_id)
      ~rebuild_body
