open Flambda2_bound_identifiers
open Flambda2_identifiers
open Flambda2_kinds
open Flambda2_nominal
open Flambda2_term_basics
open Flambda2_terms
open Flambda2_reaper
module Acc = Traverse_acc
module Graph = Global_flow_graph
module Solve_inputs = Reaper.Staged.Solve_inputs
module Queries = Rebuild_queries
module Requests = Queries.Requests
module PTA = Points_to_analysis

let unit_a = Compilation_unit.of_string "Reaper_boundary_a"

let unit_b = Compilation_unit.of_string "Reaper_boundary_b"

let unit_c = Compilation_unit.of_string "Reaper_boundary_c"

let lto_participants = Compilation_unit.Set.of_list [unit_a; unit_b]

let set_current_unit unit =
  Env.set_current_unit (Unit_info.make_dummy ~input_name:"reaper_boundary" unit)

let var name = Variable.create name Flambda_kind.value

let node name = Code_id_or_name.var (var name)

let code_id unit name = Code_id.create unit ~name ~debug:Debuginfo.none

let symbol unit name = Symbol.create unit (Linkage_name.of_string name)

let environment acc current_code_id =
  let external_world = Name.var (var "external_world") in
  let all_constants = Name.var (var "all_constants") in
  Acc.add_any_source acc (Code_id_or_name.name external_world);
  Acc.add_any_source acc (Code_id_or_name.name all_constants);
  Traverse_env.create ~parent:Rev_expr.Hole ~conts:Continuation.Map.empty
    ~current_code_id ~should_preserve_direct_calls:Yes
    ~le_monde_exterieur:external_world ~all_constants

let graph acc = Acc.deps acc ~all_constants:(Name.var (var "all_constants"))

let link_and_solve graph ~code_deps ~code_references ~analysis_scope =
  let solve_inputs =
    Solve_inputs.
      { code_deps;
        code_references;
        rebuild_queries = Requests.empty;
        all_sets_of_closures = []
      }
  in
  let solution, _ =
    Reaper.Staged.solve ~slot_offsets_inputs:Slot_offsets_analysis.Inputs.empty
      ~analysis_scope ~solve_inputs:[solve_inputs] graph
  in
  solution.uses

let interface acc code_id =
  let param = var "param" in
  let return = var "return" in
  let exn = var "exn" in
  let my_closure = var "my_closure" in
  let params = [param] and returns = [return] in
  let arity =
    Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value]
  in
  let known_arity_call_witness =
    Acc.create_known_arity_call_witness acc code_id ~params ~returns ~exn
  in
  let unknown_arity_call_witnesses =
    Acc.create_unknown_arity_call_witnesses acc code_id ~is_tupled:false ~arity
      ~params ~returns ~exn
  in
  let code_metadata =
    Code_metadata.create code_id ~newer_version_of:None ~params_arity:arity
      ~param_modes:[Alloc_mode.For_types.heap]
      ~first_complex_local_param:(Index 1)
      ~result_arity:(Flambda_arity.unarize_t arity)
      ~result_types:Unknown ~result_mode:Lambda.not_alloc_stack ~stub:false
      ~inline:Never_inline ~zero_alloc_attribute:Default_zero_alloc
      ~poll_attribute:Default ~regalloc_attribute:Default_regalloc
      ~regalloc_param_attribute:Default_regalloc_params ~cold:false
      ~is_a_functor:false ~is_opaque:false ~recursive:Non_recursive
      ~cost_metrics:Cost_metrics.zero
      ~inlining_arguments:(Inlining_arguments.create ~round:0)
      ~dbg:Debuginfo.none ~is_tupled:false ~is_my_closure_used:true
      ~inlining_decision:Never_inline_attribute
      ~absolute_history:
        (Inlining_history.Absolute.empty (Code_id.get_compilation_unit code_id))
      ~relative_history:Inlining_history.Relative.empty ~loopify:Never_loopify
  in
  let dep : Acc.code_dep =
    { arity;
      code_metadata;
      function_slot_size = Code_metadata.function_slot_size code_metadata;
      params;
      my_closure;
      return = returns;
      exn;
      is_tupled = false;
      known_arity_call_witness;
      unknown_arity_call_witnesses
    }
  in
  Acc.add_code_dep acc code_id dep;
  let body_closure = var "body_closure" in
  Acc.add_code_id_my_closure acc code_id body_closure;
  Acc.add_alias_vars acc ~to_:body_closure ~from:my_closure;
  Acc.add_alias_vars acc ~to_:return ~from:param;
  dep, Code_id_or_name.var param

(* The graph part of a unary call, without constructing a Flambda term. *)
let call acc =
  let witness = node "apply_witness" in
  let argument = node "argument" in
  let result = node "result" in
  let called = node "called" in
  Acc.add_any_source acc argument;
  Acc.add_argument_dep acc ~base:witness (Cofield.param 0) ~from:argument;
  Acc.add_accessor_dep acc ~base:witness
    (Field.normal_return_of_call 0)
    ~to_:result;
  Acc.add_accessor_dep acc ~base:witness Field.code_id_of_call_witness
    ~to_:called;
  Acc.add_any_usage acc result;
  Acc.add_any_usage acc called;
  witness, argument, result

let test_module_root () =
  let open Flambda in
  set_current_unit unit_a;
  let root = symbol unit_a "root" in
  let root_node = Code_id_or_name.symbol root in
  let used = symbol unit_c "used_field"
  and unused = symbol unit_c "unused_field" in
  let first = Field.block 0 Flambda_kind.value in
  let second = Field.block 1 Flambda_kind.value in
  let return_continuation = Continuation.create ~sort:Toplevel_return () in
  let body =
    Expr.create_apply_cont
      (Apply_cont.create return_continuation
         ~args:[Simple.symbol root]
         ~dbg:Debuginfo.none)
  in
  let block =
    Static_const.block Tag.Scannable.zero Immutable Value_only
      (List.map
         (fun symbol ->
           Simple.With_debuginfo.create (Simple.symbol symbol) Debuginfo.none)
         [used; unused])
  in
  let body =
    Expr.create_let
      (Let.create
         (Bound_pattern.static
            (Bound_static.singleton (Bound_static.Pattern.block_like root)))
         (Named.create_static_consts
            (Static_const_group.create
               [Static_const_or_code.create_static_const block]))
         ~body ~free_names_of_body:Unknown)
  in
  let unit =
    Flambda_unit.create ~return_continuation
      ~exn_continuation:(Continuation.create ())
      ~toplevel_my_alloc_region:
        (Variable.create "alloc_region" Flambda_kind.region)
      ~body ~module_symbol:root
  in
  let graph = (Traverse.run ~closed_world:true unit).deps in
  let solve () =
    Analysis.fixpoint graph ~analysis_scope:(Lto_participants lto_participants)
  in
  let closed = solve () in
  assert (not (Analysis.has_use closed root_node));
  assert (not (Analysis.field_used closed root_node first));
  assert (not (Analysis.has_use closed (Code_id_or_name.symbol unused)));
  let exported =
    Analysis.fixpoint (Traverse.run ~closed_world:false unit).deps
      ~analysis_scope:Current_unit
  in
  assert (Analysis.any_usage exported root_node);
  assert (Analysis.field_used exported root_node first);
  assert (Analysis.field_used exported root_node second);
  assert (not (Analysis.any_usage (solve ()) root_node));
  let projected = node "projected" in
  Graph.add_accessor_dep graph ~base:root_node first ~to_:projected;
  Graph.add_any_usage graph projected;
  let projected = solve () in
  assert (Analysis.has_use projected (Code_id_or_name.symbol used));
  assert (Analysis.field_used projected root_node first);
  assert (not (Analysis.field_used projected root_node second));
  assert (not (Analysis.has_use projected (Code_id_or_name.symbol unused)))

let test_imported_symbols () =
  set_current_unit unit_a;
  let acc = Acc.create () in
  let denv = environment acc None in
  let imported = symbol unit_b "imported" in
  let genuinely_unknown = symbol unit_b "genuinely_unknown" in
  let outside = symbol unit_c "outside" in
  let import symbol = Acc.simple_to_node acc ~denv (Simple.symbol symbol) in
  let imported = import imported in
  let genuinely_unknown = import genuinely_unknown in
  let outside = import outside in
  Acc.add_any_source acc genuinely_unknown;
  let graph = graph acc in
  let solve lto_participants =
    Analysis.fixpoint graph ~analysis_scope:(Lto_participants lto_participants)
  in
  let separate = solve (Compilation_unit.Set.singleton unit_a) in
  assert (Analysis.any_source separate imported);
  let linked = solve lto_participants in
  assert (not (Analysis.any_source linked imported));
  assert (Analysis.any_source linked genuinely_unknown);
  assert (Analysis.any_source linked outside);
  assert (
    not
      (Analysis.any_source
         (Analysis.fixpoint graph
            ~analysis_scope:(Lto_participants lto_participants))
         imported))

let test_direct_call caller_live =
  set_current_unit unit_b;
  let callee_acc = Acc.create () in
  let callee = code_id unit_b "callee" in
  let _, param = interface callee_acc callee in
  let callee_graph = graph callee_acc in
  let code_deps = Acc.code_deps callee_acc in
  set_current_unit unit_a;
  let caller_acc = Acc.create () in
  let caller = Option.map (fun _ -> code_id unit_a "caller") caller_live in
  let denv = environment caller_acc caller in
  let live = Option.value caller_live ~default:true in
  Option.iter
    (fun caller ->
      if live then Acc.add_any_usage caller_acc (Code_id_or_name.code_id caller))
    caller;
  let witness, argument, result = call caller_acc in
  Acc.add_external_apply caller_acc ~participant_call:(witness, None) ~denv
    ~code_id:callee ~witness ~closure:None;
  let caller_graph = graph caller_acc in
  let make_graph () = Graph.union callee_graph caller_graph in
  let code_references = Acc.code_references caller_acc in
  let linked =
    link_and_solve (make_graph ()) ~code_deps ~code_references
      ~analysis_scope:(Lto_participants lto_participants)
  in
  assert (Analysis.has_use linked (Code_id_or_name.code_id callee) = live);
  assert (Analysis.has_use linked param = live);
  assert (Analysis.any_source linked param = live);
  assert (Analysis.has_use linked argument = live);
  assert (Analysis.any_source linked result = live);
  assert (not (Analysis.any_source linked witness));
  Option.iter
    (fun caller ->
      assert (Analysis.has_use linked (Code_id_or_name.code_id caller) = live))
    caller;
  let separate =
    link_and_solve (make_graph ()) ~code_deps ~code_references
      ~analysis_scope:(Lto_participants (Compilation_unit.Set.singleton unit_a))
  in
  assert (Analysis.any_source separate witness = live);
  assert (Analysis.any_source separate result = live);
  assert (Analysis.has_use separate argument = live);
  assert (not (Analysis.has_use separate (Code_id_or_name.code_id callee)));
  let linked_again =
    link_and_solve (make_graph ()) ~code_deps ~code_references
      ~analysis_scope:(Lto_participants lto_participants)
  in
  assert (not (Analysis.any_source linked_again witness))

let test_guarded_direct_call () =
  set_current_unit unit_b;
  let callee_acc = Acc.create () in
  let callee = code_id unit_b "specialized_target" in
  let _, param = interface callee_acc callee in
  let callee_graph = graph callee_acc in
  let code_deps = Acc.code_deps callee_acc in
  set_current_unit unit_a;
  let acc = Acc.create () in
  let denv = environment acc None in
  let witness, _, _ = call acc in
  let closure = var "known_closure" in
  let closure_node = Code_id_or_name.var closure in
  let unknown_witness = node "unknown_code_pointer" in
  Acc.add_any_source acc unknown_witness;
  Acc.add_constructor_dep acc ~base:closure_node Field.known_arity_call_witness
    ~from:unknown_witness;
  Acc.add_accessor_dep acc ~base:closure_node Field.known_arity_call_witness
    ~to_:witness;
  let guarded_witness = node "guarded_witness" in
  let guarded_closure = var "guarded_closure" in
  Acc.add_alias_if_any_source_dep acc ~if_any_source:closure_node
    ~from:closure_node
    ~to_:(Code_id_or_name.var guarded_closure);
  Acc.add_alias_if_any_source_dep acc ~if_any_source:closure_node
    ~from:guarded_witness ~to_:witness;
  Acc.add_external_apply acc
    ~participant_call:(witness, Some (Simple.var closure))
    ~denv ~code_id:callee ~witness:guarded_witness
    ~closure:(Some (Simple.var guarded_closure));
  let caller_graph = graph acc in
  let make_graph () = Graph.union callee_graph caller_graph in
  let code_references = Acc.code_references acc in
  let solve lto_participants =
    link_and_solve (make_graph ()) ~code_deps ~code_references
      ~analysis_scope:(Lto_participants lto_participants)
  in
  let separate = solve (Compilation_unit.Set.singleton unit_a) in
  assert (not (Analysis.any_source separate closure_node));
  assert (Analysis.any_source separate witness);
  assert (not (Analysis.has_use separate (Code_id_or_name.code_id callee)));
  let linked = solve lto_participants in
  assert (Analysis.has_use linked (Code_id_or_name.code_id callee));
  assert (Analysis.has_use linked param)

let test_foreign_closure entry_point =
  set_current_unit unit_b;
  let callee_acc = Acc.create () in
  let callee = code_id unit_b "foreign_closure_code" in
  let dep, param = interface callee_acc callee in
  let callee_graph = graph callee_acc in
  let code_deps = Acc.code_deps callee_acc in
  set_current_unit unit_a;
  let caller_acc = Acc.create () in
  let closure = var "locally_allocated_closure" in
  let closure_node = Code_id_or_name.var closure in
  Acc.add_set_of_closures_dep caller_acc (Name.var closure)
    ~closure_code_id:callee ~only_full_applications:false
    ~defined_in_code_id:None;
  let witness, argument, result = call caller_acc in
  Acc.add_accessor_dep caller_acc ~base:closure_node entry_point ~to_:witness;
  (* [deps] records the foreign-code closure reference. *)
  let caller_graph = graph caller_acc in
  let code_references = Acc.code_references caller_acc in
  let ids = Acc.ids_for_export_code_references code_references in
  assert (Code_id.Set.mem callee ids.code_ids);
  assert (Variable.Set.mem closure ids.variables);
  assert (
    not (Code_id.Set.mem callee (Graph.ids_for_export caller_graph).code_ids));
  let inputs =
    Solve_inputs.
      { code_deps = Code_id.Map.empty;
        code_references;
        rebuild_queries = Requests.empty;
        all_sets_of_closures = []
      }
  in
  assert (
    Compilation_unit.Set.mem unit_b
      (Solve_inputs.referenced_compilation_units inputs));
  let make_graph () = Graph.union callee_graph caller_graph in
  let linked =
    link_and_solve (make_graph ()) ~code_deps ~code_references
      ~analysis_scope:(Lto_participants lto_participants)
  in
  assert (Analysis.any_usage linked (Code_id_or_name.code_id callee));
  assert (Analysis.any_source linked param);
  assert (Analysis.has_use linked argument);
  assert (Analysis.any_source linked result);
  assert (Analysis.has_source linked (Code_id_or_name.var dep.my_closure));
  assert (not (Analysis.any_source linked witness));
  assert (not (Analysis.any_source linked (Code_id_or_name.var dep.my_closure)));
  let separate =
    link_and_solve (make_graph ()) ~code_deps ~code_references
      ~analysis_scope:Current_unit
  in
  assert (Analysis.any_source separate witness);
  assert (not (Analysis.has_use separate (Code_id_or_name.code_id callee)));
  let linked_again =
    link_and_solve (make_graph ()) ~code_deps ~code_references
      ~analysis_scope:(Lto_participants lto_participants)
  in
  assert (not (Analysis.any_source linked_again witness))

let test_escaped_slots make_field =
  set_current_unit unit_a;
  Oxcaml_flags.Flambda2.reaper_local_fields := Oxcaml_flags.Set true;
  let acc = Acc.create () in
  let closure = node "escaped_closure" in
  let callback_closure = node "callback_closure" in
  let read = node "read_slot" and unread = node "unread_slot" in
  let foreign = node "foreign_slot" and projected = node "projection" in
  let read_field = make_field unit_b "read" in
  let unread_field = make_field unit_b "unread" in
  let foreign_field = make_field unit_c "foreign" in
  let analysis_scope = Analysis.Scope.Lto_participants lto_participants in
  assert (not (Analysis.Scope.is_closed Current_unit));
  assert (Analysis.Scope.is_closed analysis_scope);
  assert (Analysis.Scope.contains_unit Current_unit unit_a);
  assert (not (Analysis.Scope.contains_unit Current_unit unit_b));
  assert (Analysis.Scope.contains_unit analysis_scope unit_b);
  assert (not (Analysis.Scope.contains_unit analysis_scope unit_c));
  assert (not (Field.is_local read_field ~analysis_scope:Current_unit));
  assert (Field.is_local read_field ~analysis_scope);
  assert (not (Field.is_local foreign_field ~analysis_scope));
  assert (not (Field.is_local read_field ~analysis_scope:Current_unit));
  Acc.add_any_usage acc closure;
  Acc.add_any_source acc callback_closure;
  Acc.add_constructor_dep acc ~base:closure read_field ~from:read;
  Acc.add_constructor_dep acc ~base:closure unread_field ~from:unread;
  Acc.add_constructor_dep acc ~base:closure foreign_field ~from:foreign;
  Acc.add_accessor_dep acc ~base:callback_closure read_field ~to_:projected;
  Acc.add_any_usage acc projected;
  let graph = graph acc in
  let linked =
    Analysis.fixpoint graph ~analysis_scope:(Lto_participants lto_participants)
  in
  assert (Analysis.has_use linked read);
  assert (not (Analysis.has_use linked unread));
  assert (Analysis.has_use linked foreign);
  assert (Analysis.field_used linked closure read_field);
  assert (not (Analysis.field_used linked closure unread_field));
  assert (Analysis.field_used linked closure foreign_field);
  assert (Analysis.get_unboxed_fields linked closure = None);
  assert (Analysis.get_changed_representation linked closure = None);
  let separate = Analysis.fixpoint graph ~analysis_scope:Current_unit in
  assert (Analysis.has_use separate unread);
  assert (Analysis.field_used separate closure unread_field);
  Oxcaml_flags.Flambda2.reaper_local_fields := Oxcaml_flags.Set false;
  let disabled =
    Analysis.fixpoint graph ~analysis_scope:(Lto_participants lto_participants)
  in
  assert (Analysis.has_use disabled unread);
  assert (Analysis.field_used disabled closure unread_field)

let test_graph_renaming_and_union () =
  set_current_unit unit_a;
  let roots = Acc.create () and calls = Acc.create () in
  let root = symbol unit_a "renamed_root" in
  let returned = var "returned" in
  Acc.add_alias roots
    ~to_:(Code_id_or_name.var returned)
    ~from:(Code_id_or_name.symbol root);
  Acc.add_any_usage roots (Code_id_or_name.var returned);
  let denv = environment calls None in
  let imported = symbol unit_b "renamed_import" in
  ignore (Acc.simple_to_node calls ~denv (Simple.symbol imported));
  let callee = code_id unit_b "renamed_call" in
  let witness = var "witness" and closure = var "closure" in
  Acc.add_external_apply calls
    ~participant_call:(Code_id_or_name.var witness, Some (Simple.var closure))
    ~denv ~code_id:callee
    ~witness:(Code_id_or_name.var witness)
    ~closure:(Some (Simple.var closure));
  let graph =
    Graph.union
      (Graph.union (Graph.create ()) (graph roots))
      (Graph.union (graph calls) (Graph.create ()))
  in
  let closure_code_witness = var "closure_code_witness" in
  let code_references =
    Acc.Closure
      { closure = Code_id_or_name.var closure;
        code_id = callee;
        external_witness = Code_id_or_name.var closure_code_witness
      }
    :: Acc.code_references calls
  in
  let inputs =
    Solve_inputs.
      { code_deps = Code_id.Map.empty;
        code_references;
        rebuild_queries = Requests.empty;
        all_sets_of_closures = []
      }
  in
  let ids =
    Ids_for_export.union
      (Graph.ids_for_export graph)
      (Solve_inputs.ids_for_export inputs)
  in
  assert (
    Compilation_unit.Set.mem unit_b
      (Solve_inputs.referenced_compilation_units inputs));
  assert (Symbol.Set.mem root ids.symbols);
  assert (Symbol.Set.mem imported ids.symbols);
  assert (Code_id.Set.mem callee ids.code_ids);
  List.iter
    (fun v -> assert (Variable.Set.mem v ids.variables))
    [returned; witness; closure; closure_code_witness];
  let renaming =
    Variable.Set.fold
      (fun v renaming ->
        Renaming.add_fresh_variable renaming v ~guaranteed_fresh:(var "renamed"))
      ids.variables Renaming.empty
  in
  let renamed_graph =
    Graph.apply_renaming graph renaming ~rename_field:Fun.id
  in
  let renamed_inputs = Solve_inputs.apply_renaming inputs renaming in
  let renamed_ids =
    Ids_for_export.union
      (Graph.ids_for_export renamed_graph)
      (Solve_inputs.ids_for_export renamed_inputs)
  in
  assert (
    Compilation_unit.Set.equal
      (Solve_inputs.referenced_compilation_units inputs)
      (Solve_inputs.referenced_compilation_units renamed_inputs));
  assert (Symbol.Set.equal ids.symbols renamed_ids.symbols);
  assert (Code_id.Set.equal ids.code_ids renamed_ids.code_ids);
  assert (
    Variable.Set.equal
      (Renaming.apply_variable_set renaming ids.variables)
      renamed_ids.variables);
  let renamed_node v =
    Code_id_or_name.var (Renaming.apply_variable renaming v)
  in
  let result =
    link_and_solve renamed_graph ~code_deps:renamed_inputs.code_deps
      ~code_references:renamed_inputs.code_references
      ~analysis_scope:Current_unit
  in
  assert (Analysis.any_usage result (renamed_node returned));
  assert (Analysis.any_source result (renamed_node witness));
  assert (Analysis.any_source result (renamed_node closure_code_witness));
  assert (Analysis.any_usage result (renamed_node closure));
  assert (Analysis.any_source result (Code_id_or_name.symbol imported));
  assert (not (Analysis.has_use result (Code_id_or_name.var returned)));
  assert (not (Analysis.any_source result (Code_id_or_name.var witness)));
  assert (
    not (Analysis.any_source result (Code_id_or_name.var closure_code_witness)));
  assert (not (Analysis.has_use result (Code_id_or_name.var closure)))

let rebuild_apply callee call_kind widths =
  let args_arity =
    let open Flambda_arity.Component_for_creation in
    Flambda_arity.create
      (List.map
         (fun width ->
           Unboxed_product
             (List.init width (fun _ ->
                  Singleton Flambda_kind.With_subkind.any_value)))
         widths)
  in
  Flambda.Apply.create ~callee ~continuation:Never_returns
    (Exn_continuation.create ~exn_handler:(Continuation.create ())
       ~extra_args:[])
    ~args:
      (List.init (Flambda_arity.cardinal_unarized args_arity) (fun _ ->
           Simple.var (var "arg")))
    ~args_arity ~return_arity:Flambda_arity.nullary ~call_kind
    ~return_mode:
      (Alloc_mode.For_applications.not_alloc_stack
         ~alloc_region:(Variable.create "alloc_region" Flambda_kind.region))
    Debuginfo.none ~inlined:Default_inlined
    ~inlining_state:(Inlining_state.default ~round:0)
    ~probe:None ~position:Normal
    ~relative_history:Inlining_history.Relative.empty

let test_rebuild_queries () =
  set_current_unit unit_b;
  let absent = Name.var (var "absent_callee") in
  set_current_unit unit_a;
  let top = Name.var (var "top_callee") in
  let precise = Name.var (var "precise_callee") in
  let callees = [absent; top; precise] in
  let acc = Acc.create () in
  let target = code_id unit_a "query_target" in
  ignore (interface acc target);
  Acc.add_set_of_closures_dep acc precise ~closure_code_id:target
    ~only_full_applications:false ~defined_in_code_id:None;
  List.iter
    (fun (callee, entry_point) ->
      let witness, _, _ = call acc in
      Acc.add_accessor_dep acc
        ~base:(Code_id_or_name.name callee)
        entry_point ~to_:witness)
    [ precise, Field.known_arity_call_witness;
      precise, Field.unknown_arity_call_witness;
      top, Field.known_arity_call_witness ];
  Acc.add_any_source acc (Code_id_or_name.name top);
  let known = Call_kind.indirect_function_call_known_arity ~code_ids:Unknown in
  let unknown = Call_kind.indirect_function_call_unknown_arity in
  let add requests callee kind widths =
    Requests.add_apply requests
      (rebuild_apply (Some (Simple.name callee)) kind widths)
  in
  let requests =
    List.fold_left
      (fun requests callee ->
        let left = add Requests.empty callee known [3] in
        let left = add left callee unknown [1; 0; 2] in
        let right = add Requests.empty callee known [1] in
        let right = add right callee unknown [2; 0; 1; 1] in
        Requests.union requests (Requests.union left right))
      Requests.empty callees
  in
  let graph = graph acc in
  let solve graph =
    Analysis.fixpoint graph ~analysis_scope:(Lto_participants lto_participants)
  in
  let analysis = solve graph in
  let summary = Queries.create analysis.db ~requests in
  let known_args = [0; 1; 2] in
  let unknown_args = [[0; 1]; []; [2; 3]; [4]] in
  let check summary db renaming =
    List.iter
      (fun callee ->
        let callee = Renaming.apply_name renaming callee in
        let node = Code_id_or_name.name callee in
        assert (
          match
            ( Queries.code_id_actually_directly_called summary callee,
              PTA.code_id_actually_directly_called db callee )
          with
          | Unknown, Unknown -> true
          | Known a, Known b -> Code_id.Set.equal a b
          | Unknown, Known _ | Known _, Unknown -> false);
        List.iter
          (fun args ->
            assert (
              Queries.arguments_used_by_known_arity_call summary node args
              = PTA.arguments_used_by_known_arity_call db node args))
          [[]; [0]; known_args];
        List.iter
          (fun args ->
            assert (
              Queries.arguments_used_by_unknown_arity_call summary node args
              = PTA.arguments_used_by_unknown_arity_call db node args))
          [[]; [[0]]; unknown_args])
      callees;
    let ids =
      Ids_for_export.union
        (Graph.ids_for_export graph)
        (Requests.ids_for_export requests)
    in
    Variable.Set.iter
      (fun var ->
        let node = Code_id_or_name.var (Renaming.apply_variable renaming var) in
        assert (Queries.has_use summary node = PTA.has_use db node);
        assert (Queries.has_source summary node = PTA.has_source_query db node);
        List.iter
          (fun field ->
            assert (
              Queries.field_used summary node field
              = PTA.field_used db node field))
          [ Field.known_arity_call_witness;
            Field.unknown_arity_call_witness;
            Field.normal_return_of_call 0 ])
      ids.variables
  in
  check summary analysis.db Renaming.empty;
  assert (
    Queries.arguments_used_by_known_arity_call summary
      (Code_id_or_name.name precise)
      known_args
    = [0, PTA.Keep; 1, PTA.Delete; 2, PTA.Delete]);
  assert (
    Queries.arguments_used_by_unknown_arity_call summary
      (Code_id_or_name.name precise)
      unknown_args
    = [ [0, PTA.Keep; 1, PTA.Delete];
        [];
        [2, PTA.Keep; 3, PTA.Keep];
        [4, PTA.Keep] ]);
  assert (Code_id.Set.mem target (Queries.ids_for_export summary).code_ids);
  let inputs =
    Solve_inputs.
      { code_deps = Code_id.Map.empty;
        code_references = [];
        rebuild_queries = requests;
        all_sets_of_closures = []
      }
  in
  let request_ids = Requests.ids_for_export requests in
  assert (
    Variable.Set.equal request_ids.variables
      (Solve_inputs.ids_for_export inputs).variables);
  let ids = Queries.ids_for_export summary in
  let renaming =
    Variable.Set.fold
      (fun v renaming ->
        Renaming.add_fresh_variable renaming v ~guaranteed_fresh:(var "fresh"))
      ids.variables Renaming.empty
  in
  let inputs : Solve_inputs.t =
    Marshal.from_string (Marshal.to_string inputs []) 0
  in
  let renamed_inputs = Solve_inputs.apply_renaming inputs renaming in
  assert (
    Variable.Set.equal
      (Renaming.apply_variable_set renaming request_ids.variables)
      (Solve_inputs.ids_for_export renamed_inputs).variables);
  let renamed_graph =
    Graph.apply_renaming graph renaming ~rename_field:Fun.id
  in
  let renamed_analysis = solve renamed_graph in
  let renamed_summary =
    Queries.apply_renaming summary renaming ~rename_field:Fun.id
  in
  check renamed_summary renamed_analysis.db renaming;
  check
    (Queries.create renamed_analysis.db ~requests:renamed_inputs.rebuild_queries)
    renamed_analysis.db renaming;
  let partitions = Queries.partition_by_compilation_unit summary in
  assert (Compilation_unit.Map.mem unit_a partitions);
  assert (Compilation_unit.Map.mem unit_b partitions);
  let reunited =
    Compilation_unit.Map.fold
      (fun _ part acc -> Queries.disjoint_union acc part)
      partitions Queries.empty
  in
  check reunited analysis.db Renaming.empty;
  (* Nullary requests must not disappear, even though their masks are empty. *)
  let nullary = symbol unit_b "nullary" in
  let apply = rebuild_apply (Some (Simple.symbol nullary)) known [] in
  let nullary_acc = Acc.create () in
  Acc.record_apply_for_rebuild nullary_acc apply;
  let requests =
    Requests.union Requests.empty (Acc.rebuild_queries nullary_acc)
  in
  let requests = Requests.union requests Requests.empty in
  assert (Symbol.Set.mem nullary (Requests.ids_for_export requests).symbols);
  let unit =
    Flambda_unit.create
      ~return_continuation:(Continuation.create ~sort:Toplevel_return ())
      ~exn_continuation:
        (Exn_continuation.exn_handler (Flambda.Apply.exn_continuation apply))
      ~toplevel_my_alloc_region:
        (Alloc_mode.For_applications.alloc_region
           (Flambda.Apply.return_mode apply))
      ~body:(Flambda.Expr.create_apply apply)
      ~module_symbol:(symbol unit_a "query_module")
  in
  let traversed = Traverse.run ~closed_world:true unit in
  assert (
    Symbol.Set.mem nullary
      (Requests.ids_for_export traversed.rebuild_queries).symbols);
  let summary = Queries.create analysis.db ~requests in
  assert (
    Queries.arguments_used_by_known_arity_call summary
      (Code_id_or_name.symbol nullary)
      []
    = []);
  assert (not (Queries.has_use Queries.empty (Code_id_or_name.symbol nullary)))

let () =
  Oxcaml_flags.Flambda2.reaper_unbox := Oxcaml_flags.Set false;
  Oxcaml_flags.Flambda2.reaper_change_calling_conventions
    := Oxcaml_flags.Set false;
  Oxcaml_flags.Flambda2.reaper_local_fields := Oxcaml_flags.Set false;
  test_module_root ();
  test_imported_symbols ();
  List.iter test_direct_call [None; Some true; Some false];
  test_guarded_direct_call ();
  List.iter test_foreign_closure
    [Field.known_arity_call_witness; Field.unknown_arity_call_witness];
  test_escaped_slots (fun unit name ->
      Field.value_slot
        (Value_slot.create unit ~name ~is_always_immediate:false
           Flambda_kind.value));
  test_escaped_slots (fun unit name ->
      Field.function_slot
        (Function_slot.create unit ~name ~is_always_immediate:false
           Flambda_kind.value));
  test_graph_renaming_and_union ();
  test_rebuild_queries ()
