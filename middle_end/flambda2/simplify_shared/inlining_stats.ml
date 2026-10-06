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

let enabled = Inlining_stats_table.enabled

let add = Inlining_stats_table.add

let add_float = Inlining_stats_table.add_float

let incr = Inlining_stats_table.incr

let set_max = Inlining_stats_table.set_max

(* Sizes are recorded for both architectures so that the output does not depend
   on the host. *)
let add_size key size =
  add (key ^ ".x86_64") (Code_size.x86_64 size);
  add (key ^ ".arm64") (Code_size.arm64 size)

let add_removed key (removed : Removed_operations.t) =
  add (key ^ ".call") removed.call;
  add (key ^ ".alloc") removed.alloc;
  add (key ^ ".prim") removed.prim;
  add (key ^ ".branch") removed.branch;
  add (key ^ ".direct_call_of_indirect") removed.direct_call_of_indirect;
  add (key ^ ".poly_compare") removed.specialized_poly_compare;
  add (key ^ ".requested_inline") removed.requested_inline

type pass =
  | Simplify
  | Closure_conversion

let call_site_prefix ~pass ~in_speculation =
  match pass with
  | Closure_conversion -> "closure_conversion.call_site"
  | Simplify ->
    if in_speculation then "in_speculation.call_site" else "call_site"

let call_site_decision_name (decision : Call_site_inlining_decision_type.t) =
  match decision with
  | Missing_code -> "missing_code"
  | Definition_says_not_to_inline -> "definition_says_not_to_inline"
  | In_a_stub -> "in_a_stub"
  | Doing_speculative_inlining _ -> "nested_speculation_not_performed"
  | Argument_types_not_useful -> "argument_types_not_useful"
  | Unrolling_depth_exceeded -> "unrolling_depth_exceeded"
  | Max_inlining_depth_exceeded -> "max_inlining_depth_exceeded"
  | Recursion_depth_exceeded -> "recursion_depth_exceeded"
  | Never_inlined_attribute -> "never_inlined_attribute"
  | Speculative_inlining_budget_exhausted _ -> "speculation_budget_exhausted"
  | Speculative_inlining_aborted _ -> "speculation_aborted"
  | Speculatively_not_inline _ -> "speculation_not_inline"
  | Attribute_always -> "attribute_always"
  | Replay_history_says_must_inline _ -> "replay_history_says_must_inline"
  | Begin_unrolling _ -> "begin_unrolling"
  | Continue_unrolling -> "continue_unrolling"
  | Definition_says_inline { was_inline_always = true } ->
    "definition_says_inline_attribute"
  | Definition_says_inline { was_inline_always = false } ->
    "definition_says_inline"
  | Speculatively_inline _ -> "speculation_inline"
  | Jsir_inlining_disabled -> "jsir_inlining_disabled"

let attribute_name (attribute : Inlined_attribute.t) =
  match attribute with
  | Always_inlined _ -> "always"
  | Hint_inlined -> "hint"
  | Never_inlined -> "never"
  | Unroll _ -> "unroll"
  | Default_inlined -> "default"

let speculation_scope ~in_speculation ~threshold_is_remaining_budget =
  if in_speculation
  then "in_speculation"
  else if threshold_is_remaining_budget
  then "in_region"
  else "outermost"

let speculation ~scope ~outcome ~is_a_functor ~original_size =
  let key = "speculation." ^ scope ^ "." ^ outcome in
  incr (key ^ ".count");
  if is_a_functor then incr (key ^ ".functor.count");
  add_size (key ^ ".original_size") original_size;
  key

let speculation_completed ~scope ~outcome ~is_a_functor ~original_size
    ~cost_metrics ~cost_metrics_of_lifted_constants ~call_site_credit ~threshold
    =
  let key = speculation ~scope ~outcome ~is_a_functor ~original_size in
  add_size (key ^ ".simplified_size") (Cost_metrics.size cost_metrics);
  add_size
    (key ^ ".lifted_constants_size")
    (Cost_metrics.size cost_metrics_of_lifted_constants);
  add_removed (key ^ ".removed") (Cost_metrics.removed cost_metrics);
  add_float (key ^ ".call_site_credit") call_site_credit;
  add_float (key ^ ".budget") threshold

let record_call_site_decision ~pass ~in_speculation ~is_a_functor ~callee_size
    ~apply (decision : Call_site_inlining_decision_type.t) =
  if enabled ()
  then (
    let key =
      call_site_prefix ~pass ~in_speculation
      ^ if is_a_functor then ".functor" else ".function"
    in
    incr (key ^ ".count");
    incr (key ^ "." ^ call_site_decision_name decision);
    (match Call_site_inlining_decision_type.can_inline decision with
    | Inline _ ->
      incr (key ^ ".inlined");
      add
        (key ^ ".inlined.inlining_depth_sum")
        (Inlining_state.depth (Apply_expr.inlining_state apply))
    | Do_not_inline _ -> incr (key ^ ".not_inlined"));
    incr (key ^ ".attribute." ^ attribute_name (Apply_expr.inlined apply));
    match decision with
    | Speculatively_inline
        { cost_metrics;
          cost_metrics_of_lifted_constants;
          original_size;
          call_site_credit;
          criterion = _;
          threshold;
          threshold_is_remaining_budget;
          is_a_functor
        } ->
      speculation_completed
        ~scope:
          (speculation_scope ~in_speculation ~threshold_is_remaining_budget)
        ~outcome:"inline" ~is_a_functor ~original_size ~cost_metrics
        ~cost_metrics_of_lifted_constants ~call_site_credit ~threshold
    | Speculatively_not_inline
        { cost_metrics;
          cost_metrics_of_lifted_constants;
          original_size;
          call_site_credit;
          criterion = _;
          threshold;
          threshold_is_remaining_budget;
          is_a_functor
        } ->
      speculation_completed
        ~scope:
          (speculation_scope ~in_speculation ~threshold_is_remaining_budget)
        ~outcome:"not_inline" ~is_a_functor ~original_size ~cost_metrics
        ~cost_metrics_of_lifted_constants ~call_site_credit ~threshold
    | Speculative_inlining_aborted { budget; threshold_is_remaining_budget } ->
      let key =
        speculation
          ~scope:
            (speculation_scope ~in_speculation ~threshold_is_remaining_budget)
          ~outcome:"aborted" ~is_a_functor ~original_size:callee_size
      in
      add_float (key ^ ".budget") budget
    | Speculative_inlining_budget_exhausted
        { remaining_budget; code_size; max_code_size } ->
      let key =
        speculation
          ~scope:
            (speculation_scope ~in_speculation
               ~threshold_is_remaining_budget:true)
          ~outcome:"budget_exhausted" ~is_a_functor ~original_size:code_size
      in
      add_float (key ^ ".remaining_budget") remaining_budget;
      add_float (key ^ ".max_code_size") max_code_size
    | Missing_code | Definition_says_not_to_inline | In_a_stub
    | Doing_speculative_inlining _ | Argument_types_not_useful
    | Unrolling_depth_exceeded | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded | Never_inlined_attribute | Attribute_always
    | Replay_history_says_must_inline _ | Begin_unrolling _ | Continue_unrolling
    | Definition_says_inline _ | Jsir_inlining_disabled ->
      ())

let call_kind_name (call_kind : Call_kind.t) =
  match call_kind with
  | Function { function_call = Direct _ } -> "direct"
  | Function { function_call = Indirect_unknown_arity } ->
    "indirect_unknown_arity"
  | Function { function_call = Indirect_known_arity _ } ->
    "indirect_known_arity"
  | Method _ -> "method"
  | C_call _ -> "c_call"
  | Effect _ -> "effect"

let record_unknown_callee ~pass ~in_speculation apply =
  if enabled ()
  then (
    let key = call_site_prefix ~pass ~in_speculation ^ ".unknown_callee" in
    incr (key ^ ".count");
    incr (key ^ "." ^ call_kind_name (Apply_expr.call_kind apply)))

let definition_decision_name (decision : Function_decl_inlining_decision_type.t)
    =
  match decision with
  | Not_yet_decided -> "not_yet_decided"
  | Never_inline_attribute -> "never_inline_attribute"
  | Function_body_too_large _ -> "function_body_too_large"
  | Functor_body_too_large _ -> "functor_body_too_large"
  | Stub -> "stub"
  | Attribute_inline -> "attribute_inline"
  | Small_function _ -> "small_function"
  | Small_functor _ -> "small_functor"
  | Speculatively_inlinable _ -> "speculatively_inlinable"
  | Speculatively_inlinable_functor _ -> "speculatively_inlinable_functor"
  | Recursive -> "recursive"
  | Jsir_inlining_disabled -> "jsir_inlining_disabled"

let record_function_definition ~pass ~in_speculation ~code_metadata decision =
  if enabled ()
  then (
    let key =
      match pass with
      | Closure_conversion -> "closure_conversion.definition"
      | Simplify ->
        if in_speculation then "in_speculation.definition" else "definition"
    in
    let cost_metrics = Code_metadata.cost_metrics code_metadata in
    let size = Cost_metrics.size cost_metrics in
    let record key =
      incr (key ^ ".count");
      add_size (key ^ ".size") size;
      add_removed (key ^ ".removed") (Cost_metrics.removed cost_metrics)
    in
    record key;
    if Code_metadata.is_a_functor code_metadata then record (key ^ ".functor");
    record (key ^ "." ^ definition_decision_name decision))

let time_speculation ~outermost f =
  if not (enabled ())
  then f ()
  else
    let start = Sys.time () in
    let result = f () in
    let key =
      if outermost then "speculation.outermost" else "speculation.nested"
    in
    add_float (key ^ ".seconds") (Sys.time () -. start);
    result

let budget_prefix ~in_region =
  if in_region then "budget.region" else "budget.speculation"

let record_budget_opened ~in_region ~budget =
  if enabled ()
  then (
    let key = budget_prefix ~in_region in
    incr (key ^ ".opened");
    add_float (key ^ ".initial") budget)

let record_budget_charge ~in_region ~charge ~credit_granted ~credit_capped
    ~credit_used ~exhausted =
  if enabled ()
  then (
    let key = budget_prefix ~in_region in
    add_float (key ^ ".charged") charge;
    add_float (key ^ ".credit_granted") credit_granted;
    add_float (key ^ ".credit_capped") credit_capped;
    add_float (key ^ ".credit_used") credit_used;
    if exhausted then incr (key ^ ".exhausted"))

let record_final_unit ~machine_width unit =
  if enabled ()
  then (
    let codes = Code_size_report.collect_code unit in
    let function_slot_size code_id =
      match Code_id.Map.find_opt code_id codes with
      | Some code -> Code.function_slot_size code
      | None -> 2
    in
    let measure ~return_continuation ~exn_continuation body =
      let v1, v2 =
        Code_size_report.measure ~machine_width ~function_slot_size
          ~return_continuation ~exn_continuation body
      in
      add "code.size.v1" v1;
      add "code.size.v2.x86_64" (Code_size_v2.x86_64 v2);
      add "code.size.v2.arm64" (Code_size_v2.arm64 v2);
      v2
    in
    let num_closures set =
      Function_slot.Map.cardinal
        (Function_declarations.funs (Set_of_closures.function_decls set))
    in
    let visitor =
      { Code_size_report.named =
          (fun named ->
            match named with
            | Set_of_closures (set, _alloc_mode) ->
              incr "code.set_of_closures.dynamic";
              add "code.closures.dynamic" (num_closures set)
            | Static_consts group ->
              List.iter
                (fun (const : Flambda.Static_const_or_code.t) ->
                  match const with
                  | Static_const const ->
                    if Static_const.is_set_of_closures const
                    then (
                      incr "code.set_of_closures.static";
                      add "code.closures.static"
                        (num_closures
                           (Static_const.must_be_set_of_closures const)))
                  | Code _ | Deleted_code -> ())
                (Flambda.Static_const_group.to_list group)
            | Simple _ | Prim _ | Rec_info _ -> ());
        let_cont = (fun _cont _handler -> ());
        apply =
          (fun apply ->
            incr "code.apply.count";
            incr ("code.apply." ^ call_kind_name (Apply_expr.call_kind apply));
            match Apply_expr.inlined apply with
            | Default_inlined -> ()
            | (Always_inlined _ | Hint_inlined | Never_inlined | Unroll _) as
              attribute ->
              incr ("code.apply.attribute." ^ attribute_name attribute));
        apply_cont = (fun _ -> ());
        switch = (fun _ -> incr "code.switch");
        invalid = (fun () -> ())
      }
    in
    (* Copies of a function made by inlining (specialisations) keep the name of
       the original code ID and receive a fresh stamp, so the functions of the
       output are grouped by name. *)
    let copies : (string, int * int * int * int * int) Hashtbl.t =
      Hashtbl.create 64
    in
    Code_id.Map.iter
      (fun code_id code ->
        incr "code.functions";
        if Code.is_a_functor code then incr "code.functors";
        if Code.stub code then incr "code.stubs";
        Code_size_report.iter_function_body code
          ~f:(fun ~return_continuation ~exn_continuation body ->
            let v2 = measure ~return_continuation ~exn_continuation body in
            let x86_64 = Code_size_v2.x86_64 v2 in
            let arm64 = Code_size_v2.arm64 v2 in
            let name = Code_id.name code_id in
            let count, sum_x86_64, sum_arm64, max_x86_64, max_arm64 =
              Option.value
                (Hashtbl.find_opt copies name)
                ~default:(0, 0, 0, 0, 0)
            in
            Hashtbl.replace copies name
              ( count + 1,
                sum_x86_64 + x86_64,
                sum_arm64 + arm64,
                Int.max max_x86_64 x86_64,
                Int.max max_arm64 arm64 );
            Code_size_report.iter_expr visitor body))
      codes;
    Hashtbl.iter
      (fun _name (count, sum_x86_64, sum_arm64, max_x86_64, max_arm64) ->
        incr "code.specialisation.names";
        set_max "code.specialisation.max_copies" count;
        if count > 1
        then (
          incr "code.specialisation.names_with_copies";
          add "code.specialisation.copies" count;
          add "code.specialisation.extra_copies" (count - 1);
          add "code.specialisation.copies_size.x86_64" sum_x86_64;
          add "code.specialisation.copies_size.arm64" sum_arm64;
          (* The size of all copies but the largest one. *)
          add "code.specialisation.extra_copies_size.x86_64"
            (sum_x86_64 - max_x86_64);
          add "code.specialisation.extra_copies_size.arm64"
            (sum_arm64 - max_arm64)))
      copies;
    let body = Flambda_unit.body unit in
    let (_ : Code_size_v2.t) =
      measure
        ~return_continuation:(Flambda_unit.return_continuation unit)
        ~exn_continuation:(Flambda_unit.exn_continuation unit)
        body
    in
    Code_size_report.iter_expr visitor body)
