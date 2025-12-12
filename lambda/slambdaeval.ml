(* Helpers for asserting that slambda is trivial. *)

exception Found_a_splice

let rec assert_layout_contains_no_splices : Lambda.layout -> unit = function
  | Psplicevar _ -> raise Found_a_splice
  | Ptop | Pbottom | Pvalue _ | Punboxed_float _
  | Punboxed_or_untagged_integer _ | Punboxed_vector _ ->
    ()
  | Punboxed_product layouts ->
    List.iter assert_layout_contains_no_splices layouts

let rec assert_mixed_block_element_contains_no_splices :
    type a. a Lambda.mixed_block_element -> unit = function
  | Splice_variable _ -> raise Found_a_splice
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
  | Bits64 | Vec128 | Vec256 | Vec512 | Word | Untagged_immediate ->
    ()
  | Product elements ->
    Array.iter assert_mixed_block_element_contains_no_splices elements

let assert_mixed_block_shape_contains_no_splices shape =
  Array.iter assert_mixed_block_element_contains_no_splices shape

let assert_primitive_contains_no_splices (prim : Lambda.primitive) =
  match prim with
  | Popaque layout | Pobj_magic layout ->
    assert_layout_contains_no_splices layout
  | Pget_idx (layout, _)
  | Pset_idx (layout, _)
  | Pget_ptr (layout, _)
  | Pset_ptr (layout, _) ->
    assert_layout_contains_no_splices layout
  | Pmake_unboxed_product layouts | Punboxed_product_field (_, layouts) ->
    List.iter assert_layout_contains_no_splices layouts
  | Pmakemixedblock (_, _, shape, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmixedfield (_, shape, _) ->
    Array.iter assert_mixed_block_element_contains_no_splices shape
  | Psetmixedfield (_, shape, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmake_idx_mixed_field (shape, _, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmake_idx_array (_, _, element, _) | Pidx_deepen (element, _) ->
    assert_mixed_block_element_contains_no_splices element
  | _ -> ()

let assert_function_contains_no_splices { Lambda.params; return; _ } =
  List.iter
    (fun { Lambda.layout; _ } -> assert_layout_contains_no_splices layout)
    params;
  assert_layout_contains_no_splices return

let rec assert_no_splices (lam : Lambda.lambda) =
  (match lam with
  | Lvar _ | Lmutvar _ | Lconst _ -> ()
  | Lapply { ap_result_layout; _ } ->
    assert_layout_contains_no_splices ap_result_layout
  | Lfunction func -> assert_function_contains_no_splices func
  | Llet (_, layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Lmutlet (layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Ldelayedletrec (_, _) | Lletrec (_, _) -> ()
  | Lprim (prim, _, _) -> assert_primitive_contains_no_splices prim
  | Lswitch (_, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lstringswitch (_, _, _, _, layout) ->
    assert_layout_contains_no_splices layout
  | Lstaticraise _ -> ()
  | Lstaticcatch (_, (_, bindings), _, _, layout) ->
    List.iter
      (fun (_, _, layout) -> assert_layout_contains_no_splices layout)
      bindings;
    assert_layout_contains_no_splices layout
  | Ltrywith (_, _, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lifthenelse (_, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lsequence _ | Lwhile _ | Lfor _ | Lassign _ -> ()
  | Lsend (_, _, _, _, _, _, _, layout) ->
    assert_layout_contains_no_splices layout
  | Levent _ | Lifused _ -> ()
  | Lregion (_, layout) -> assert_layout_contains_no_splices layout
  | Lexclave _ -> ()
  | Lsplice _ -> raise Found_a_splice);
  Lambda.iter_head_constructor assert_no_splices lam

(* Check that slambda is trivial (a quote and contains no splices) *)
let assert_slambda_is_trivial (slam : Lambda.slambda) =
  match slam with
  | SLquote lam -> (
    try assert_no_splices lam
    with Found_a_splice ->
      Misc.fatal_error
        "Slambda contains splices but layout_poly extension is disabled")
  | _ ->
    Misc.fatal_error
      "Encountered non-trivial slambda but layout_poly extension is disabled"

(* Introduce dependencies on modules referenced only by "external". *)

let scan_used_globals lam =
  let globals = ref Compilation_unit.Set.empty in
  let rec scan (lam : Lambda.lambda) =
    Lambda.iter_head_constructor scan lam;
    match lam with
    | Lprim (Pgetglobal cu, _, _) ->
      globals := Compilation_unit.Set.add cu !globals
    | _ -> ()
  in
  scan lam;
  !globals

let required_globals ~flambda body =
  let globals = scan_used_globals body in
  let add_global comp_unit req =
    if (not flambda) && Compilation_unit.Set.mem comp_unit globals
    then req
    else Compilation_unit.Set.add comp_unit req
  in
  let required =
    List.fold_left
      (fun acc cu -> add_global cu acc)
      (if flambda then globals else Compilation_unit.Set.empty)
      (Translprim.get_units_with_used_primitives ())
  in
  let required =
    List.fold_right add_global (Env.get_required_globals ()) required
  in
  Env.reset_required_globals ();
  Translprim.clear_used_primitives ();
  required

let do_eval ({ Slambda.code = slam } as p) =
  if not Language_extension.(is_enabled Layout_poly)
  then assert_slambda_is_trivial slam;
  let lam =
    match slam with
    | SLquote lam -> lam
    | _ -> Misc.fatal_error "slambda eval not yet implemented"
  in
  { Lambda.compilation_unit = p.compilation_unit;
    main_module_block_format = p.main_module_block_format;
    arg_block_idx = p.arg_block_idx;
    required_globals = required_globals ~flambda:true lam;
    code = lam
  }

let eval p = Profile.(record static_eval) do_eval p
