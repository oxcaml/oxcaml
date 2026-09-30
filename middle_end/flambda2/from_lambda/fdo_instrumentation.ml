open! Flambda

(* The constructs of one function body (or of the unit's toplevel code) are
   numbered in traversal order, starting from the function's id. *)
type state =
  { function_id : Fdo_counter.function_id;
    function_body_hash : Fdo_counter.Function_body_hash.t;
    mutable next_index : int;
    module_call_indices : int Misc.Stdlib.String.Tbl.t
  }

let create_state function_id function_body_hash =
  { function_id;
    function_body_hash;
    next_index = 0;
    module_call_indices = Misc.Stdlib.String.Tbl.create 4
  }

let fresh_index state =
  let index = state.next_index in
  state.next_index <- index + 1;
  index

(* The edge counters of a switch go onto its arms, one per scrutinee value. A
   switch on 0/1 alone is an if-then-else (or a match on a two-constructor type,
   or the [Is_int] dispatch of a mixed match, which are the same thing), whose
   edges read better as [else] and [then]. *)
let arm_counters state (switch : Switch_expr.t) =
  let arms = Switch_expr.arms switch in
  let max_value, _ = Target_ocaml_int.Map.max_binding arms in
  let edge =
    if Target_ocaml_int.to_int max_value + 1 <= 2
    then fun i -> if i = 0 then Fdo_counter.Else else Fdo_counter.Then
    else fun i -> Fdo_counter.Switch_case i
  in
  let index = fresh_index state in
  Target_ocaml_int.Map.mapi
    (fun value action ->
      Apply_cont_expr.with_fdo_counters action
        [ { Fdo_counter.position =
              Fdo_counter.position ~function_id:state.function_id
                ~function_body_hash:state.function_body_hash ~ast_pos:index
                ~edge:(edge (Target_ocaml_int.to_int value));
            inlining_stack = []
          } ])
    arms

(* Functor applications carry the scope of the module they produce. Use that
   path, with a counter per scope, so inserting another module binding does not
   rename every subsequent specialization. *)
let module_binding_path dbg =
  let rec path (scopes : Debuginfo.Scoped_location.scopes) =
    match scopes with
    | Cons { item = Sc_module_definition; name = ""; prev; _ } -> path prev
    | Cons { item = Sc_module_definition; prev = Cons _; _ } ->
      Some
        (Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
           scopes)
    | Empty
    | Cons { item = Sc_module_definition; prev = Empty; _ }
    | Cons
        { item =
            ( Sc_anonymous_function | Sc_value_definition | Sc_class_definition
            | Sc_method_definition | Sc_partial_or_eta_wrapper | Sc_lazy );
          _
        } ->
      None
  in
  match Debuginfo.to_items dbg with
  | item :: _ -> path item.dinfo_scopes
  | [] -> None

(* The counter of a call site; C calls and effect operations are not calls of
   the call graph. *)
let callsite_position state (apply : Apply_expr.t) =
  match Apply_expr.call_kind apply with
  | Function _ | Method _ ->
    let index = fresh_index state in
    let position =
      match module_binding_path (Apply_expr.dbg apply) with
      | None ->
        Fdo_counter.position ~function_id:state.function_id
          ~function_body_hash:state.function_body_hash ~ast_pos:index
          ~edge:Callsite
      | Some path ->
        let index =
          Option.value
            (Misc.Stdlib.String.Tbl.find_opt state.module_call_indices path)
            ~default:0
        in
        Misc.Stdlib.String.Tbl.replace state.module_call_indices path (index + 1);
        Fdo_counter.instantiation_site
          (Fdo_counter.function_id ~unmangled_name:path ~discriminator:index)
    in
    Some position
  | C_call _ | Effect _ -> None

let rec expr state (e : Expr.t) : Expr.t =
  match Expr.descr e with
  | Let let_expr ->
    Let_expr.pattern_match let_expr ~f:(fun bound_pattern ~body ->
        let defining_expr = named (Let_expr.defining_expr let_expr) in
        let body = expr state body in
        Expr.create_let
          (Let_expr.create bound_pattern defining_expr ~body
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
        Let_cont_expr.create_non_recursive' ~can_be_lifted ~cont handler ~body
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
        Let_cont_expr.create_recursive ~invariant_params handlers ~body)
  | Apply apply -> (
    match callsite_position state apply with
    | None -> e
    | Some position ->
      Expr.create_apply
        (Apply_expr.with_callsite_counter apply
           (Some { Fdo_counter.position; inlining_stack = [] })))
  | Switch switch ->
    Expr.create_switch
      (Switch_expr.create
         ~condition_dbg:(Switch_expr.condition_dbg switch)
         ~scrutinee:(Switch_expr.scrutinee switch)
         ~arms:(arm_counters state switch))
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

(* A function's constructs are numbered from its own id, that of the entry
   counter of its code. *)
and code (c : Code.t) : Code.t =
  let malformed () =
    Misc.fatal_errorf "Malformed entry counters on the code of %a" Code_id.print
      (Code.code_id c)
  in
  match Code.fdo_entry_counters c, Code.function_body_hash c with
  | [], None -> c
  | [], Some _ | _ :: _, None | _ :: _ :: _, Some _ -> malformed ()
  | [{ Fdo_counter.position = Position _ | Instantiation_site _; _ }], Some _
  | [{ position = Function_entry _; inlining_stack = _ :: _ }], Some _ ->
    malformed ()
  | ( [{ position = Function_entry function_id; inlining_stack = [] }],
      Some function_body_hash ) ->
    let state = create_state function_id function_body_hash in
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
          Function_params_and_body.create ~return_continuation ~exn_continuation
            params ~body:(expr state body) ~free_names_of_body:Unknown
            ~my_closure ~my_alloc_mode ~my_depth)
    in
    (* Counters change no names, so the free names and cost are unchanged. *)
    Code.with_params_and_body ~params_and_body
      ~free_names_of_params_and_body:(Code.free_names_of_params_and_body c)
      ~cost_metrics:(Code.cost_metrics c) c

let add_to_unit ~compilation_unit ~function_body_hash unit =
  let state =
    create_state
      (Fdo_counter.function_id
         ~unmangled_name:(Compilation_unit.full_path_as_string compilation_unit)
         ~discriminator:0)
      function_body_hash
  in
  Flambda_unit.with_body unit (expr state (Flambda_unit.body unit))
