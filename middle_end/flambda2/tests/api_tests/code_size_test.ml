open Flambda2_bound_identifiers
open Flambda2_identifiers
open Flambda2_kinds
open Flambda2_numbers
open Flambda2_term_basics
open Flambda2_terms
module V2 = Code_size_v2
module P = Flambda_primitive

let check name size ~x86_64 ~arm64 =
  if V2.x86_64 size <> x86_64 || V2.arm64 size <> arm64
  then
    Misc.fatal_errorf "%s: expected %d / %d, got %a" name x86_64 arm64 V2.print
      size

let check_sum name actual a b =
  check name actual
    ~x86_64:(V2.x86_64 a + V2.x86_64 b)
    ~arm64:(V2.arm64 a + V2.arm64 b)

let () =
  let comp_unit =
    Compilation_unit.create Compilation_unit.Prefix.empty
      (Compilation_unit.Name.of_string "Code_size_test")
  in
  Env.set_current_unit
    (Unit_info.make_dummy ~input_name:"Code_size_test" comp_unit);
  let region = Variable.create "region" Flambda_kind.region in
  let heap = Alloc_mode.For_allocations.heap ~alloc_region:region in
  let value = Simple.var (Variable.create "value" Flambda_kind.value) in
  let zero =
    Simple.const
      (Reg_width_const.const_int (Target_ocaml_int.of_int Sixty_four 0))
  in
  let size prim = V2.prim ~machine_width:Sixty_four prim in
  let make_array kind mode n =
    let field_kind = List.hd (P.Array_kind.element_kinds kind) in
    let field =
      Simple.var
        (Variable.create "field" (Flambda_kind.With_subkind.kind field_kind))
    in
    size
      (P.Variadic
         (Make_array (kind, Mutable, mode), List.init n (fun _ -> field)))
  in
  let alloc = make_array Values heap 1 in
  let atomic =
    size
      (P.Ternary
         ( Atomic_exchange
             (Field_index, Immediate, Alloc_mode.For_assignments.heap),
           value,
           zero,
           value ))
  in
  (* A native atomic needs neither a frame nor an allocation barrier on
     amd64. *)
  check "atomic frame" (V2.add_function_frame atomic) ~x86_64:1 ~arm64:10;
  let allocations_around_atomic = V2.seq alloc (V2.seq atomic alloc) in
  check "atomic allocation barrier" allocations_around_atomic ~x86_64:13
    ~arm64:26;
  if
    not
      (V2.equal allocations_around_atomic (V2.seq (V2.seq alloc atomic) alloc))
  then Misc.fatal_error "Allocation sequencing is not associative";
  (* An ordinary C call remains a barrier on both targets. *)
  let modify =
    size
      (P.Ternary
         ( Atomic_exchange
             (Field_index, Any_value, Alloc_mode.For_assignments.heap),
           value,
           zero,
           value ))
  in
  check "runtime atomic allocation barrier"
    (V2.seq alloc (V2.seq modify alloc))
    ~x86_64:22 ~arm64:26;
  let exn =
    Exn_continuation.create ~exn_handler:(Continuation.create ()) ~extra_args:[]
  in
  let call =
    Apply_expr.create ~callee:(Some value)
      ~continuation:(Return (Continuation.create ()))
      exn ~args:[value]
      ~args_arity:
        (Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value])
      ~return_arity:
        (Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value])
      ~call_kind:Call_kind.indirect_function_call_unknown_arity
      ~return_mode:
        (Alloc_mode.For_applications.not_alloc_stack ~alloc_region:region)
      Debuginfo.none ~inlined:Default_inlined
      ~inlining_state:(Inlining_state.default ~round:0)
      ~probe:None ~position:Normal
      ~relative_history:Inlining_history.Relative.empty
    |> V2.apply ~is_tail:false
  in
  let nested = V2.add_function_frame call in
  check_sum "nested function frame"
    (V2.add_function_frame (V2.with_out_of_line alloc ~out_of_line:nested))
    (V2.add_function_frame alloc)
    nested;
  if not (V2.equal nested (V2.add_function_frame nested))
  then Misc.fatal_error "A completed function retained frame requirements";
  (* Continuation handlers, unlike nested functions, still contribute calls. *)
  check_sum "continuation frame"
    (V2.add_function_frame (V2.with_out_of_line alloc ~out_of_line:call))
    alloc
    (V2.add_function_frame call);
  let int64 = Simple.var (Variable.create "int64" Flambda_kind.naked_int64) in
  check "signed division"
    (size (P.Binary (Int_arith (Naked_int64, Div Signed), int64, int64)))
    ~x86_64:6 ~arm64:1;
  let nativeint =
    Simple.var (Variable.create "nativeint" Flambda_kind.naked_nativeint)
  in
  check "signed remainder"
    (size
       (P.Binary (Int_arith (Naked_nativeint, Mod Signed), nativeint, nativeint)))
    ~x86_64:6 ~arm64:2;
  let limit = Config.max_young_wosize in
  let small = make_array Values heap limit in
  check "minor allocation boundary" small ~x86_64:(7 + limit) ~arm64:(9 + limit);
  let large = make_array Values heap (limit + 1) in
  let initialize_x86_64 = if Config.no_stack_checks then 3 else 6 in
  check "major allocation initializers" large
    ~x86_64:(8 + ((limit + 1) * initialize_x86_64))
    ~arm64:(8 + ((limit + 1) * 7));
  check_sum "major allocation barrier"
    (V2.seq alloc (V2.seq large alloc))
    large (V2.( + ) alloc alloc);
  let floats = make_array Naked_floats heap (limit + 1) in
  check "major float allocation" floats
    ~x86_64:(8 + limit + 1)
    ~arm64:(8 + limit + 1);
  (* The major-allocation boundary is in words, not in primitive arguments. Only
     the value prefix of this mixed block needs [caml_initialize]. *)
  let num_vectors = limit / 8 in
  let shape =
    Flambda_kind.Mixed_block_shape.from_prefix_size_and_suffix_elements 1
      (List.init num_vectors (fun _ -> Flambda_kind.Naked_vec512))
  in
  let vector =
    Simple.var (Variable.create "vector" Flambda_kind.naked_vec512)
  in
  let mixed =
    size
      (P.Variadic
         ( Make_block (Mixed (Tag.Scannable.zero, shape), Mutable, heap),
           value :: List.init num_vectors (fun _ -> vector) ))
  in
  check "major mixed allocation" mixed
    ~x86_64:(9 + initialize_x86_64 + num_vectors)
    ~arm64:(9 + 7 + num_vectors);
  let local = Alloc_mode.For_allocations.local ~alloc_region:region ~region in
  (match local with
  | Heap _ -> ()
  | Local _ ->
    check "large local allocation"
      (make_array Values local (limit + 1))
      ~x86_64:(10 + limit + 1)
      ~arm64:(13 + limit + 1));
  let function_slot =
    Function_slot.create comp_unit ~name:"f" ~is_always_immediate:false
      Flambda_kind.value
  in
  let value_slot =
    Value_slot.create comp_unit ~name:"x" ~is_always_immediate:false
      Flambda_kind.value
  in
  let code_id = Code_id.create comp_unit ~name:"f" ~debug:Debuginfo.none in
  let set =
    Set_of_closures.create
      ~value_slots:(Value_slot.Map.singleton value_slot value)
      (Function_declarations.create
         (Function_slot.Lmap.of_list
            [ ( function_slot,
                Function_declarations.Code_id
                  { code_id; only_full_applications = false } ) ]))
  in
  let old_model = !Oxcaml_flags.Flambda2.code_size_model in
  Fun.protect
    ~finally:(fun () -> Oxcaml_flags.Flambda2.code_size_model := old_model)
    (fun () ->
      Oxcaml_flags.Flambda2.code_size_model := Oxcaml_flags.Flambda2.V1;
      let metrics =
        Cost_metrics.set_of_closures set ~find_code_characteristics:(fun _ ->
            { cost_metrics = Cost_metrics.zero; function_slot_size = 2 })
      in
      if Code_size.to_int (Cost_metrics.size metrics) <> 8
      then
        Misc.fatal_error
          "v1 closure allocation no longer uses the original word count")
