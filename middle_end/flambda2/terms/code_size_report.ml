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

module V1 = Code_size_v1
module V2 = Code_size_v2

(* A traversal of Flambda terms that is only used to visit each construct once;
   nothing is rebuilt. *)
type visitor =
  { named : Flambda.Named.t -> unit;
    let_cont : Continuation.t -> Flambda.Continuation_handler.t -> unit;
        (** Called for non-recursive continuation bindings before their scope is
            visited. *)
    apply : Apply_expr.t -> unit;
    apply_cont : Apply_cont_expr.t -> unit;
    switch : Switch_expr.t -> unit;
    invalid : unit -> unit
  }

let rec iter_expr visitor (expr : Flambda.Expr.t) =
  match Flambda.Expr.descr expr with
  | Let let_expr ->
    Flambda.Let.pattern_match let_expr ~f:(fun _bound_pattern ~body ->
        visitor.named (Flambda.Let.defining_expr let_expr);
        iter_expr visitor body)
  | Let_cont (Non_recursive { handler; _ }) ->
    Flambda.Non_recursive_let_cont_handler.pattern_match handler
      ~f:(fun cont ~body ->
        let handler = Flambda.Non_recursive_let_cont_handler.handler handler in
        visitor.let_cont cont handler;
        iter_expr visitor body;
        iter_continuation_handler visitor handler)
  | Let_cont (Recursive handlers) ->
    Flambda.Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params:_ ~body conts ->
        iter_expr visitor body;
        Continuation.Lmap.iter
          (fun _cont handler -> iter_continuation_handler visitor handler)
          (Flambda.Continuation_handlers.to_map conts))
  | Apply apply -> visitor.apply apply
  | Apply_cont apply_cont -> visitor.apply_cont apply_cont
  | Switch switch -> visitor.switch switch
  | Invalid _ -> visitor.invalid ()

and iter_continuation_handler visitor handler =
  Flambda.Continuation_handler.pattern_match handler ~f:(fun _params ~handler ->
      iter_expr visitor handler)

let iter_function_body code ~f =
  Flambda.Function_params_and_body.pattern_match (Code.params_and_body code)
    ~f:(fun
        ~return_continuation
        ~exn_continuation
        _params
        ~body
        ~my_closure:_
        ~is_my_closure_used:_
        ~my_alloc_mode:_
        ~my_depth:_
        ~free_names_of_body:_
      -> f ~return_continuation ~exn_continuation body)

(* All pieces of code in the unit, including any bound inside function
   bodies. *)
let collect_code unit =
  let codes = ref Code_id.Map.empty in
  let rec visitor =
    { named =
        (fun named ->
          match named with
          | Static_consts group ->
            List.iter
              (fun code ->
                codes := Code_id.Map.add (Code.code_id code) code !codes;
                iter_function_body code
                  ~f:(fun ~return_continuation:_ ~exn_continuation:_ body ->
                    iter_expr visitor body))
              (Flambda.Static_const_group.pieces_of_code' group)
          | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ -> ());
      let_cont = (fun _cont _handler -> ());
      apply = ignore;
      apply_cont = ignore;
      switch = ignore;
      invalid = ignore
    }
  in
  iter_expr visitor (Flambda_unit.body unit);
  !codes

(* Sizes in both models. *)
type sizes =
  { v1 : int;
    v2 : V2.t
  }

let zero_sizes = { v1 = 0; v2 = V2.zero }

(* See [Code_size.seq] and [Code_size.with_out_of_line]. *)
let seq a b = { v1 = a.v1 + b.v1; v2 = V2.seq a.v2 b.v2 }

let with_out_of_line t ~out_of_line =
  { v1 = t.v1 + out_of_line.v1;
    v2 = V2.with_out_of_line t.v2 ~out_of_line:out_of_line.v2
  }

let seq_list f l = List.fold_left (fun size x -> seq size (f x)) zero_sizes l

(* The size of the code of a function body, in both models. Unlike the cost
   metrics stored in terms (see [Cost_metrics.set_of_closures]), the bodies of
   closures defined in the function are not included, only the allocation of the
   set of closures, so that the result is comparable with the code actually
   emitted for the function. *)
let size_if_inlined ~(code_metadata : Code_metadata.t)
    ~(inlined : Inlined_attribute.t) =
  let size () =
    Some (Cost_metrics.size (Code_metadata.cost_metrics code_metadata))
  in
  match inlined with
  | Never_inlined -> None
  | Always_inlined _ | Unroll _ -> size ()
  | Hint_inlined | Default_inlined ->
    if
      Function_decl_inlining_decision_type.must_be_inlined
        (Code_metadata.inlining_decision code_metadata)
    then size ()
    else None

let measure ~machine_width ~function_slot_size ~inlined_callee_size
    ~return_continuation ~exn_continuation body =
  (* Continuations whose handler just jumps to the return continuation with the
     parameters it received. [To_cmm] inlines these, so a call whose
     continuation is one of them is compiled as a tail call. *)
  let return_aliases = ref Continuation.Set.empty in
  let is_return k =
    Continuation.equal k return_continuation
    || Continuation.Set.mem k !return_aliases
  in
  let let_cont cont handler =
    (* Bindings that generate no code ([let x = y] and identity coercions) are
       looked through; [subst] maps the variables they bind to what they stand
       for. *)
    let resolve subst simple =
      Simple.pattern_match simple
        ~const:(fun _ -> simple)
        ~name:(fun name ~coercion:_ ->
          Name.pattern_match name
            ~var:(fun var ->
              Option.value (Variable.Map.find_opt var subst) ~default:simple)
            ~symbol:(fun _ -> simple))
    in
    let rec is_trivial_return subst params expr =
      match Flambda.Expr.descr expr with
      | Apply_cont apply_cont ->
        Option.is_none (Apply_cont_expr.trap_action apply_cont)
        && is_return (Apply_cont_expr.continuation apply_cont)
        && List.equal Simple.equal
             (List.map (resolve subst) (Apply_cont_expr.args apply_cont))
             params
      | Let let_expr ->
        Flambda.Let.pattern_match let_expr ~f:(fun bound_pattern ~body ->
            match bound_pattern, Flambda.Let.defining_expr let_expr with
            | Singleton bound_var, Simple simple ->
              let subst =
                Variable.Map.add (Bound_var.var bound_var)
                  (resolve subst simple) subst
              in
              is_trivial_return subst params body
            | ( (Singleton _ | Set_of_closures _ | Static _),
                ( Simple _ | Prim _ | Set_of_closures _ | Static_consts _
                | Rec_info _ ) ) ->
              false)
      | Let_cont _ | Apply _ | Switch _ | Invalid _ -> false
    in
    Flambda.Continuation_handler.pattern_match handler
      ~f:(fun params ~handler ->
        if
          is_trivial_return Variable.Map.empty
            (Bound_parameters.simples params)
            handler
        then return_aliases := Continuation.Set.add cont !return_aliases)
  in
  let is_tail apply =
    match Apply_expr.position apply with
    | Nontail -> false
    | Normal -> (
      match Apply_expr.continuation apply with
      | Never_returns -> false
      | Return k ->
        let exn_continuation' = Apply_expr.exn_continuation apply in
        is_return k
        && Continuation.equal
             (Exn_continuation.exn_handler exn_continuation')
             exn_continuation
        && Misc.Stdlib.List.is_empty
             (Exn_continuation.extra_args exn_continuation'))
  in
  (* Fields of statically allocated constants whose values are variables are
     initialised at runtime (in the module initialiser, with the v1 model not
     counting them at all). *)
  let static_field ~pointer simple =
    Simple.pattern_match simple
      ~const:(fun _ -> zero_sizes)
      ~name:(fun name ~coercion:_ ->
        Name.pattern_match name
          ~var:(fun _ ->
            { v1 = 0; v2 = V2.static_field_initialization ~pointer })
          ~symbol:(fun _ -> zero_sizes))
  in
  let or_variable (or_variable : _ Or_variable.t) =
    match or_variable with
    | Const _ -> zero_sizes
    | Var _ -> { v1 = 0; v2 = V2.static_field_initialization ~pointer:false }
  in
  let fields l =
    seq_list
      (fun field ->
        static_field ~pointer:true (Simple.With_debuginfo.simple field))
      l
  in
  let static_const (const : Static_const.t) =
    match const with
    | Set_of_closures set ->
      seq_list
        (static_field ~pointer:true)
        (Value_slot.Map.data (Set_of_closures.value_slots set))
    | Block (_tag, _mut, _shape, l) -> fields l
    | Immutable_value_array l -> fields l
    | Boxed_float32 v -> or_variable v
    | Boxed_float v -> or_variable v
    | Boxed_int32 v -> or_variable v
    | Boxed_int64 v -> or_variable v
    | Boxed_nativeint v -> or_variable v
    | Boxed_vec128 v -> or_variable v
    | Boxed_vec256 v -> or_variable v
    | Boxed_vec512 v -> or_variable v
    | Boxed_mask v -> or_variable v
    | Immutable_float_block l -> seq_list or_variable l
    | Immutable_float_array l -> seq_list or_variable l
    | Immutable_float32_array l -> seq_list or_variable l
    | Immutable_int_array l -> seq_list or_variable l
    | Immutable_int8_array l -> seq_list or_variable l
    | Immutable_int16_array l -> seq_list or_variable l
    | Immutable_int32_array l -> seq_list or_variable l
    | Immutable_int64_array l -> seq_list or_variable l
    | Immutable_nativeint_array l -> seq_list or_variable l
    | Immutable_vec128_array l -> seq_list or_variable l
    | Immutable_vec256_array l -> seq_list or_variable l
    | Immutable_vec512_array l -> seq_list or_variable l
    | Immutable_mask_array l -> seq_list or_variable l
    | Empty_array _ | Immutable_string _ -> zero_sizes
  in
  let set_of_closures set =
    let num_value_slots =
      Value_slot.Map.cardinal (Set_of_closures.value_slots set)
    in
    let funs =
      Function_declarations.funs (Set_of_closures.function_decls set)
    in
    (* [words] follows the v1 accounting, one per word of the block; [stores]
       the v2 one, where the constant words of the function slots count double
       (see [Cost_metrics.set_of_closures]). *)
    let words, stores =
      Function_slot.Map.fold
        (fun _slot
             (decl : Function_declarations.code_id_in_function_declaration)
             (words, stores) ->
          let size =
            match decl with
            | Deleted { function_slot_size; _ } -> function_slot_size
            | Code_id { code_id; only_full_applications = _ } ->
              function_slot_size code_id
          in
          words + size + 1, stores + (2 * size) + 1)
        funs
        (num_value_slots, num_value_slots)
    in
    { v1 = V1.alloc_size + words - 1;
      v2 = V2.set_of_closures_allocation ~num_stores:stores
    }
  in
  let named (named : Flambda.Named.t) =
    match named with
    | Simple simple -> { v1 = V1.simple simple; v2 = V2.simple simple }
    | Prim (prim, _dbg) ->
      { v1 = V1.prim ~machine_width prim; v2 = V2.prim ~machine_width prim }
    | Set_of_closures (set, _alloc_mode) -> set_of_closures set
    (* Static data is not code (the code it contains is measured on its own) but
       its runtime initialisation is. *)
    | Static_consts group ->
      seq_list
        (fun (const : Flambda.Static_const_or_code.t) ->
          match const with
          | Static_const const -> static_const const
          | Code _ | Deleted_code -> zero_sizes)
        (Flambda.Static_const_group.to_list group)
    | Rec_info _ -> zero_sizes
  in
  (* This follows the way the sizes are combined when terms are built (see e.g.
     [Expr_builder.create_let] and [Simplify_let_cont_expr]). *)
  let rec expr (e : Flambda.Expr.t) =
    match Flambda.Expr.descr e with
    | Let let_expr ->
      Flambda.Let.pattern_match let_expr ~f:(fun _bound_pattern ~body ->
          let defining_expr = named (Flambda.Let.defining_expr let_expr) in
          seq defining_expr (expr body))
    | Let_cont (Non_recursive { handler; _ }) ->
      Flambda.Non_recursive_let_cont_handler.pattern_match handler
        ~f:(fun cont ~body ->
          let handler =
            Flambda.Non_recursive_let_cont_handler.handler handler
          in
          (* Before the body is measured, see [is_tail]. *)
          let_cont cont handler;
          let body = expr body in
          with_out_of_line body ~out_of_line:(continuation_handler handler))
    | Let_cont (Recursive handlers) ->
      Flambda.Recursive_let_cont_handlers.pattern_match handlers
        ~f:(fun ~invariant_params:_ ~body conts ->
          let body = expr body in
          let handlers =
            Continuation.Lmap.fold
              (fun _cont handler size ->
                let handler = continuation_handler handler in
                { v1 = size.v1 + handler.v1; v2 = V2.( + ) size.v2 handler.v2 })
              (Flambda.Continuation_handlers.to_map conts)
              zero_sizes
          in
          with_out_of_line body ~out_of_line:handlers)
    | Apply apply -> (
      (* A call that is sure to be inlined is measured as its callee's body. *)
      let inlined_callee_size =
        match Apply_expr.call_kind apply with
        | Function { function_call = Direct code_id } ->
          inlined_callee_size code_id ~inlined:(Apply_expr.inlined apply)
        | Function
            { function_call = Indirect_unknown_arity | Indirect_known_arity _ }
        | Method _ | C_call _ | Effect _ ->
          None
      in
      match inlined_callee_size with
      | Some size ->
        { v1 = Code_size.to_int size;
          v2 =
            Code_size_v2.create ~x86_64:(Code_size.x86_64 size)
              ~arm64:(Code_size.arm64 size)
        }
      | None ->
        { v1 = V1.apply apply; v2 = V2.apply ~is_tail:(is_tail apply) apply })
    | Apply_cont apply_cont ->
      { v1 = V1.apply_cont apply_cont; v2 = V2.apply_cont apply_cont }
    | Switch switch -> { v1 = V1.switch switch; v2 = V2.switch switch }
    | Invalid _ -> { v1 = V1.invalid; v2 = V2.invalid }
  and continuation_handler handler =
    Flambda.Continuation_handler.pattern_match handler
      ~f:(fun _params ~handler -> expr handler)
  in
  let { v1; v2 } = expr body in
  v1, V2.add_function_frame v2

let dump ~prefixname ~machine_width unit =
  let codes = collect_code unit in
  let function_slot_size code_id =
    match Code_id.Map.find_opt code_id codes with
    | Some code -> Code.function_slot_size code
    | None -> 2
  in
  let inlined_callee_size code_id ~inlined =
    match Code_id.Map.find_opt code_id codes with
    | Some code ->
      size_if_inlined ~code_metadata:(Code.code_metadata code) ~inlined
    | None -> None
  in
  Misc.protect_output_to_file (prefixname ^ ".code_sizes.csv") (fun out ->
      output_string out "symbol,debuginfo,v1,v2_x86_64,v2_arm64\n";
      let line symbol dbg (v1, v2) =
        (* Symbols of anonymous functions contain commas, hence the quotes. *)
        Printf.fprintf out "\"%s\",\"%s\",%d,%d,%d\n" symbol
          (Format.asprintf "%a" Debuginfo.print_compact dbg)
          v1 (V2.x86_64 v2) (V2.arm64 v2)
      in
      Code_id.Map.iter
        (fun code_id code ->
          iter_function_body code
            ~f:(fun ~return_continuation ~exn_continuation body ->
              line
                (Linkage_name.to_string (Code_id.linkage_name code_id))
                (Code.dbg code)
                (measure ~machine_width ~function_slot_size ~inlined_callee_size
                   ~return_continuation ~exn_continuation body)))
        codes;
      let entry =
        Linkage_name.to_string
          (Symbol.linkage_name (Flambda_unit.module_symbol unit))
        ^ "__entry"
      in
      line entry Debuginfo.none
        (measure ~machine_width ~function_slot_size ~inlined_callee_size
           ~return_continuation:(Flambda_unit.return_continuation unit)
           ~exn_continuation:(Flambda_unit.exn_continuation unit)
           (Flambda_unit.body unit)))
