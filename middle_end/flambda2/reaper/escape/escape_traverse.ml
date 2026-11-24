(******************************************************************************
 *                                  OxCaml                                    *
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

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

open! Flambda.Import
module Acc = Traverse_acc
module Env = Traverse_env
module K = Flambda_kind

let graph acc = Acc.escape acc

let enabled acc = Escape_graph.enabled (graph acc)

let toplevel_env acc ~return_continuation ~exn_continuation =
  Escape_env.create
    ~region_scope:(if enabled acc then Toplevel else No_region_scope)
    ~return_continuation ~exn_continuation

let function_env acc ~code_id ~is_opaque
    ~(my_alloc_mode : Alloc_mode.For_applications.t) ~return ~exn
    ~return_continuation ~exn_continuation =
  let graph = graph acc in
  let scope = Code_id_or_name.code_id code_id in
  (match Acc.find_code_dep acc code_id with
  | None -> ()
  | Some { code_metadata; _ } ->
    Escape_graph.add_code_arity graph code_id
      ~num_params:
        (if Code_metadata.is_tupled code_metadata
         then 1
         else
           Flambda_arity.num_params (Code_metadata.params_arity code_metadata)));
  Escape_graph.add_scope_exit graph ~scope ~param:exn;
  List.iter
    (fun result -> Escape_graph.add_return_of graph ~scope ~result)
    return;
  let region_scope : Escape_env.region_scope =
    if enabled acc && not is_opaque
    then
      match my_alloc_mode with
      | Not_alloc_stack _ ->
        Escape_graph.add_scope graph ~scope
          ~outer_conts:
            (Continuation.Set.of_list [return_continuation; exn_continuation]);
        Scope scope
      | Maybe_alloc_stack _ ->
        (* The body may allocate its result directly in the caller's region;
           such allocations must not happen while an inner region is open. *)
        No_region_scope
    else No_region_scope
  in
  Escape_env.create ~region_scope ~return_continuation ~exn_continuation

let is_exn_handler acc cont =
  match Continuation.Map.find_opt cont (Acc.get_continuation_info acc) with
  | Some { is_exn_handler; _ } -> is_exn_handler
  | None ->
    Misc.fatal_errorf
      "[Escape_traverse.is_exn_handler]: no information about continuation %a"
      Continuation.print cont

(* Regions are not closed on exceptional paths (see [Escape_analysis]). A region
   scope from which an exception can escape to a handler defined outside of it
   (other than the exception continuation of the function, for which the caller
   is responsible) cannot be given a region: an enclosing loop could otherwise
   accumulate unclosed regions. *)
let note_jump_to denv acc cont =
  if enabled acc
  then
    let escape = Env.escape denv in
    if
      is_exn_handler acc cont
      && not (Continuation.equal cont (Escape_env.exn_continuation escape))
    then
      let graph = graph acc in
      List.iter
        (fun scope ->
          if not (Escape_graph.scope_contains_continuation graph ~scope cont)
          then Escape_graph.add_unsafe_scope graph scope)
        (Escape_env.enclosing_region_scopes escape)

(* Record where the results of an application land; for an unknown-arity
   application, this is also where a partial application would be allocated (see
   [Escape_analysis]). *)
let record_call_results denv acc apply ~return_args =
  let graph = graph acc in
  let region_scope = Escape_env.region_scope (Env.escape denv) in
  match Apply.call_kind apply with
  | Function { function_call = Direct _ | Indirect_known_arity _; _ } -> (
    match region_scope with
    | Some scope ->
      List.iter
        (fun result -> Escape_graph.add_call_result graph ~result ~scope)
        return_args
    | None ->
      List.iter (Escape_graph.add_unscoped_call_result graph) return_args)
  | Function { function_call = Indirect_unknown_arity; _ } -> (
    List.iter (Escape_graph.add_indirect_call_result graph) return_args;
    match region_scope with
    | Some scope ->
      List.iter
        (fun result ->
          Escape_graph.add_scoped_indirect_call_result graph ~result ~scope)
        return_args
    | None ->
      List.iter
        (Escape_graph.add_unscoped_indirect_call_result graph)
        return_args)
  | Method _ | C_call _ | Effect _ -> ()

let record_tail_call denv acc apply =
  let escape = Env.escape denv in
  let is_tail_call =
    match Apply.continuation apply, Apply.position apply with
    | Return k, Normal ->
      Continuation.equal k (Escape_env.return_continuation escape)
    | Never_returns, Normal ->
      (* An application that does not return is a tail call too, unless an
         exception it raises is handled in the function. *)
      Continuation.equal
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        (Escape_env.exn_continuation escape)
    | (Return _ | Never_returns), Nontail -> false
  in
  if is_tail_call
  then (
    (* Tail call: the regions of the enclosing scopes are closed before it, so
       that it remains a tail call (see [Escape_rebuild]); hence no local value
       may be passed to it. This is the same restriction as the one imposed by
       the type checker on local values; as there, an application marked
       [@nontail] is not subject to it.

       CR mshinwell: this is conservative. In the future we could consider e.g.
       automatically making such applications non-tail when the callee is known
       not to be recursive, or letting the callee allocate its result in the
       caller's region (as done by [exclave_], see [Lexclave] in Lambda), which
       would also allow allocations in a callee that only escape through its
       result to become local. *)
    let graph = graph acc in
    let add_sink scope simple =
      Simple.pattern_match' simple
        ~const:(fun _ -> ())
        ~symbol:(fun _ ~coercion:_ -> ())
        ~var:(fun param ~coercion:_ ->
          Escape_graph.add_scope_exit graph ~scope ~param)
    in
    List.iter
      (fun scope ->
        List.iter (add_sink scope) (Apply.args apply);
        Option.iter (add_sink scope) (Apply.callee apply))
      (Escape_env.enclosing_region_scopes escape);
    (* See [Escape_analysis.tail_over_applied_code]. *)
    match Apply.call_kind apply, Apply.callee apply with
    | Function { function_call = Indirect_unknown_arity; _ }, Some callee ->
      Simple.pattern_match' callee
        ~const:(fun _ -> ())
        ~symbol:(fun _ ~coercion:_ -> ())
        ~var:(fun callee ~coercion:_ ->
          Escape_graph.add_tail_callee graph callee
            ~num_args:(Flambda_arity.num_params (Apply.args_arity apply)))
    | Function { function_call = Indirect_unknown_arity; _ }, None
    | Function { function_call = Direct _ | Indirect_known_arity _; _ }, _
    | (Method _ | C_call _ | Effect _), _ ->
      ())

let record_apply denv acc apply ~return_args =
  if enabled acc
  then (
    note_jump_to denv acc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply));
    record_tail_call denv acc apply;
    record_call_results denv acc apply ~return_args)

(* Record heap allocations that the escape analysis may turn into stack
   allocations. Sets of closures are only candidates when the code of every
   function is known, since their code metadata is adjusted when they become
   local (see [Escape_solution.adjust_code_metadata]). *)
let record_candidate_allocation denv acc ~(bound_pattern : Bound_pattern.t)
    (defining_expr : Named.t) =
  match Escape_env.region_scope (Env.escape denv) with
  | None -> ()
  | Some scope -> (
    let graph = graph acc in
    let add bv =
      Escape_graph.add_allocated_in graph
        ~alloc:(Code_id_or_name.var (Bound_var.var bv))
        ~scope
    in
    match[@ocaml.warning "-fragile-match"] defining_expr, bound_pattern with
    | ( Prim
          ( ( Variadic (Make_block (_, _, Alloc_mode.For_allocations.Heap _), _)
            | Unary (Box_number (_, Alloc_mode.For_allocations.Heap _), _) ),
            _ ),
        Singleton bv ) ->
      add bv
    | ( Set_of_closures (set_of_closures, Alloc_mode.For_allocations.Heap _),
        Set_of_closures bvs ) ->
      let funs =
        Function_declarations.funs
          (Set_of_closures.function_decls set_of_closures)
      in
      let can_be_local =
        Function_slot.Map.for_all
          (fun _ (decl : Function_declarations.code_id_in_function_declaration)
             ->
            match decl with
            | Deleted _ -> false
            | Code_id { code_id; only_full_applications = _ } ->
              Option.is_some (Acc.find_code_dep acc code_id))
          funs
      in
      if can_be_local then List.iter add bvs
    | _ -> ())

(* Keep track of the regions pre-existing in the term (see
   [Escape_analysis]). *)
let enter_let denv acc ~(bound_pattern : Bound_pattern.t)
    (defining_expr : Named.t) =
  if not (enabled acc)
  then denv
  else (
    record_candidate_allocation denv acc ~bound_pattern defining_expr;
    let escape = Env.escape denv in
    let with_region_var region f =
      Simple.pattern_match' region
        ~const:(fun _ -> escape)
        ~symbol:(fun _ ~coercion:_ -> escape)
        ~var:(fun region ~coercion:_ -> f region)
    in
    let escape =
      match[@ocaml.warning "-fragile-match"] defining_expr, bound_pattern with
      | Prim (Variadic (Begin_region _, _), _), Singleton bv ->
        Escape_env.enter_foreign_region escape (Bound_var.var bv)
      | Prim (Unary (End_region _, region), _), _ ->
        with_region_var region (Escape_env.exit_foreign_region escape)
      | Prim (Variadic (Begin_try_region _, _), _), Singleton bv ->
        (* Try-regions are only closed by the handlers of the corresponding
           [try], so allocations in its body belong to the enclosing region (see
           [Lambda_to_flambda]). *)
        Escape_env.enter_try_region escape (Bound_var.var bv)
      | Prim (Unary (End_try_region _, region), _), _ ->
        with_region_var region (fun region ->
            (* Closing the try-region frees everything allocated since it was
               opened, including any value passed to the handler closing it (the
               exception, or mutable variables turned into extra parameters). *)
            let graph = graph acc in
            List.iter
              (fun scope ->
                List.iter
                  (fun param -> Escape_graph.add_scope_exit graph ~scope ~param)
                  (Escape_env.handler_params_freed_by_end_of_try_region escape
                     region))
              (Escape_env.enclosing_region_scopes escape);
            Escape_env.exit_try_region escape region)
      | _ -> escape
    in
    Env.with_escape denv escape)

let enter_handler denv acc ~params =
  if not (enabled acc)
  then denv
  else Env.with_escape denv (Escape_env.enter_handler (Env.escape denv) ~params)

(* The handlers of a recursive [Let_cont] form a region scope of their own (see
   [Escape_analysis]): every continuation currently in scope, including the
   recursive ones themselves, is outside of it. *)
let enter_recursive_handlers denv acc ~conts ~params =
  if not (enabled acc)
  then denv
  else
    let region_scope = Variable.create "region_scope" K.region in
    let escape =
      Escape_env.enter_recursive_handler (Env.escape denv) ~scope:region_scope
    in
    (match Escape_env.region_scope escape with
    | Some scope
      when Code_id_or_name.equal scope (Code_id_or_name.var region_scope) ->
      let graph = graph acc in
      let outer_conts = Env.conts denv in
      Escape_graph.add_scope graph ~scope
        ~outer_conts:(Continuation.Map.keys outer_conts);
      Continuation.Map.iter
        (fun _ (Env.Normal params) ->
          List.iter
            (fun param -> Escape_graph.add_scope_exit graph ~scope ~param)
            params)
        outer_conts;
      List.iter
        (fun cont ->
          Escape_graph.add_recursive_scope graph cont ~scope:region_scope)
        conts
    | Some _ | None -> ());
    enter_handler (Env.with_escape denv escape) acc ~params
