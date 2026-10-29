(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)
open! Flambda.Import
module String = Misc.Stdlib.String
module FC = Fdo_counter

type state =
  { function_ : FC.Function.t;
    body_hash : FC.Body_hash.t;
    mutable next_index : int; (* counter discriminators *)
    module_call_indices :
      int String.Tbl.t (* for callsites that bind a module *)
  }

let create_state function_ body_hash =
  { function_;
    body_hash;
    next_index = 0;
    module_call_indices = String.Tbl.create 3
  }

let fresh_index state =
  let index = state.next_index in
  state.next_index <- index + 1;
  index

(* Insert annotations to each arm *)
let switch_arms_with_counters ({ function_; body_hash; _ } as state) arms =
  let index = fresh_index state in
  Target_ocaml_int.Map.mapi
    (fun value arm ->
      let fdo_annotation =
        FC.Position.create_branch ~function_ ~body_hash ~index
          ~arm:(Target_ocaml_int.to_int value)
        |> FC.of_position |> Fdo_annotation.singleton
      in
      Switch_arm.map_annotations
        (fun branch_annotations ->
          Branch_annotations.with_fdo_annotation branch_annotations
            fdo_annotation)
        arm)
    arms

(* [Some path] if [dbg] corresponds to a call that binds a module-level result
   with scope path [path] (see [Fdo_counter_identity] for why we care). *)
let module_level_call_binding dbg =
  let path (scopes : Debuginfo.Scoped_location.scopes) =
    match scopes with
    | Cons { item = Sc_module_definition; name; prev; _ } ->
      let path =
        Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
          scopes
      in
      let is_compilation_unit =
        match prev with Empty -> true | Cons _ -> false
      in
      let anonymous = String.equal name "" || is_compilation_unit in
      (* If a call has no binding of its own, append ".(module)" to its path.
         This can happen, for instance, in a toplevel [include F (X)] or [module
         _ = F (X)]. *)
      Some (if anonymous then path ^ ".(module)" else path)
    | Empty
    | Cons
        { item =
            ( Sc_anonymous_function | Sc_value_definition | Sc_class_definition
            | Sc_method_definition | Sc_partial_or_eta_wrapper | Sc_lazy );
          _
        } ->
      None
  in
  match Debuginfo.to_items dbg with
  | [] -> None
  | [item] -> path item.dinfo_scopes
  | _ :: _ :: _ ->
    (* Only inlining adds items, and this pass runs before Simplify. *)
    Misc.fatal_errorf
      "Fdo_instrumentation: unexpected inlined debuginfo %a before Simplify"
      Debuginfo.print_compact dbg

let callsite_position
    ({ function_; body_hash; module_call_indices; _ } as state)
    (apply : Apply_expr.t) =
  match Apply_expr.call_kind apply with
  | Function _ | Method _ ->
    let index = fresh_index state in
    let position =
      match module_level_call_binding (Apply_expr.dbg apply) with
      | None -> FC.Position.create_call ~function_ ~body_hash ~index
      | Some path ->
        (* Multiple calls can have the same binder, e.g. [module M = F (G
           (X))] *)
        let index =
          Option.value (String.Tbl.find_opt module_call_indices path) ~default:0
        in
        String.Tbl.replace module_call_indices path (index + 1);
        let binder = FC.Name.create ~name_str:path ~discriminator:0 in
        FC.Position.create_module_level_call ~function_ ~binder ~index
    in
    Some position
  | C_call _ | Effect _ -> None

(* Since this pass reconstructs the flambda terms as counters are added, it
   loses the cached data in [free_names_of_body] (we pass [Unknown] below). This
   is fine since we run Simplify right after this which repopulates them. *)

let rec expr state (e : Expr.t) : Expr.t =
  match Expr.descr e with
  | Let let_expr ->
    Let.pattern_match let_expr ~f:(fun bound_pattern ~body ->
        let defining_expr = named (Let.defining_expr let_expr) in
        let body = expr state body in
        Expr.create_let
          (Let.create bound_pattern defining_expr ~body
             ~free_names_of_body:Unknown))
  | Let_cont
      (Non_recursive
         { handler; num_free_occurrences; is_applied_with_traps; can_be_lifted })
    ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun cont ~body ->
        let body = expr state body in
        let handler =
          continuation_handler state
            (Non_recursive_let_cont_handler.handler handler)
        in
        Let_cont.create_non_recursive' ~can_be_lifted ~cont handler ~body
          ~num_free_occurrences_of_cont_in_body:num_free_occurrences
          ~is_applied_with_traps)
  | Let_cont (Recursive handlers) ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params ~body handlers ->
        let body = expr state body in
        let handlers =
          Continuation.Lmap.map
            (continuation_handler state)
            (Continuation_handlers.to_map handlers)
        in
        Let_cont.create_recursive ~invariant_params handlers ~body)
  | Apply apply -> (
    match callsite_position state apply with
    | None -> e
    | Some position ->
      Expr.create_apply
        (Apply_expr.with_callsite_counter apply
           (Some (FC.of_position position))))
  | Switch switch ->
    let arms = switch_arms_with_counters state (Switch.arms switch) in
    Expr.create_switch
      (Switch.create
         ~condition_dbg:(Switch.condition_dbg switch)
         ~scrutinee:(Switch.scrutinee switch) ~arms)
  | Apply_cont _ | Invalid _ -> e

and continuation_handler state handler =
  Continuation_handler.pattern_match handler ~f:(fun params ~handler:body ->
      Continuation_handler.create params ~handler:(expr state body)
        ~free_names_of_handler:Unknown
        ~is_exn_handler:(Continuation_handler.is_exn_handler handler)
        ~is_cold:(Continuation_handler.is_cold handler))

and named (n : Named.t) : Named.t =
  match n with
  | Static_consts group ->
    Named.create_static_consts
      (Static_const_group.map group ~f:(fun const ->
           match Static_const_or_code.to_code const with
           | Some c -> Static_const_or_code.create_code (code c)
           | None -> const))
  | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ -> n

and code (c : Code.t) : Code.t =
  let entry_annotation = Code.fdo_entry_annotation c in
  match Fdo_annotation.With_body_hash.body_hash entry_annotation with
  | None -> c (* Skip *)
  | Some body_hash -> (
    let entry_annotation =
      Fdo_annotation.With_body_hash.annotation entry_annotation
    in
    match Fdo_annotation.to_list entry_annotation with
    | [counter] when List.is_empty (FC.inlining_stack counter) ->
      let function_ = FC.Position.function_ (FC.position counter) in
      let state = create_state function_ body_hash in
      let params_and_body =
        Function_params_and_body.pattern_match (Code.params_and_body c)
          ~f:(fun
              ~return_continuation
              ~exn_continuation
              params
              ~body
              ~my_closure
              ~is_my_closure_used:_
              ~my_alloc_mode
              ~my_depth
              ~free_names_of_body:_
            ->
            Function_params_and_body.create ~return_continuation
              ~exn_continuation params ~body:(expr state body)
              ~free_names_of_body:Unknown ~my_closure ~my_alloc_mode ~my_depth)
      in
      Code.with_params_and_body ~params_and_body
        ~free_names_of_params_and_body:(Code.free_names_of_params_and_body c)
        ~cost_metrics:(Code.cost_metrics c) c
    | [] | _ :: _ ->
      Misc.fatal_errorf "Fdo_instrumentation: unexpected entry counters %a"
        Fdo_annotation.print entry_annotation)

let add_to_unit ~toplevel_fingerprint unit =
  let function_ = Fdo_fingerprint.function_ toplevel_fingerprint in
  let body_hash = Fdo_fingerprint.body_hash toplevel_fingerprint in
  let state = create_state function_ body_hash in
  Flambda_unit.with_body unit (expr state (Flambda_unit.body unit))
