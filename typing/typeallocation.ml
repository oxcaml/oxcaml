(* Registration and settling of the modes of allocations. *)

open Mode

type t =
  { locality_mode : Locality.r;
    (** The mode of the allocation. *)
    closures : (Hint.pinpoint * Allocation.r) list;
    (** Closures enclosing the allocation, from the innermost
        to the outermost one. *)
    pp : Hint.pinpoint
  }

let allocations : t list ref = Local_store.s_ref []

let reset_allocations () = allocations := []

let register_mode_for_optimisation pp ?(closures=[]) locality_mode =
  let locality_mode = Locality.disallow_left locality_mode in
  allocations := {locality_mode; closures; pp} :: !allocations

let register_allocation_mode ~env ~loc locality_mode =
  let pp : Hint.pinpoint = (loc, Allocation) in
  let closures = Env.walk_locks_for_allocation ~env pp in
  register_mode_for_optimisation pp ~closures locality_mode

let register_allocation_value_mode ~env ~loc
    ?(desc  = (Unknown : Hint.allocation_desc)) mode =
  let locality =
    with_regionality_to_locality_r2g mode
    |> With_locality.proj_comonadic Areality
  in
  let locality_mode : Locality.r =
    if Language_extension.(is_at_least Mode_polymorphism Alpha)
    then Locality.newvar_below 0 locality |> fst
    else locality
  in
  register_allocation_mode ~env ~loc locality_mode;
  (* We must apply each morphism separately so that their hints correspond to
     the correct morphism *)
  let mode =
    with_regionality_to_locality_r2g ~allocation:({loc; txt = desc})
      (Mode.With_regionality.disallow_left mode)
  in
  let mode =
    with_locality_as_regionality ~allocation:({loc; txt = desc}) mode
  in
  locality_mode, mode

(* Unlike most allocations, which can be the highest mode allowed by
   [expected_mode], functions have more constraints. For example, a two
   parameter function needs to be made global if its partial application
   to one argument must be global. As a result, a function gets an
   [With_locality.lr] allocation mode that can be further constrained. *)
let register_closure_allocation ~env (expected_mode : With_regionality.r) ~loc
    : Locality.lr * Allocation.lr * With_locality.lr * With_regionality.r =
  let allocation : Hint.allocation = { loc; txt = Unknown } in
  let closure_mode, _ =
    With_locality.newvar_below (Ctype.get_current_level ())
      (with_regionality_to_locality_r2g ~allocation expected_mode)
  in
  let locality_mode : Locality.lr =
    With_locality.proj_comonadic Areality closure_mode
    |> Locality.newvar_below 0
    |> fst
  in
  let allocation_mode : Allocation.lr =
    With_locality.proj_comonadic Allocation closure_mode
    |> Allocation.newvar_below 0
    |> fst
  in
  let closed_over_mode =
    With_regionality.meet
      [ with_locality_as_regionality
          ~allocation
          (With_locality.disallow_left closure_mode);
        With_regionality.max_with_comonadic
          Allocation
          (Allocation.disallow_left allocation_mode) ]
  in
  register_allocation_mode ~env ~loc locality_mode;
  locality_mode, allocation_mode, closure_mode, closed_over_mode

(* Module is always allocated on the heap, so every enclosing closure
   is forced to be [alloc]. *)
let register_mod_allocation ~env ~loc ~desc =
  let closures = Env.walk_locks_for_allocation ~env (loc, Hint.Allocation) in
  List.iter
    (fun (_, closure_mode) ->
      Allocation.submode_err (loc, desc)
        (Allocation.of_const ~hint:Allocated_on_heap Alloc) closure_mode)
    closures

let register_zero_alloc_application_allocation ~env ~pos
    (funct : Typedtree.expression) args (mode_ret : With_regionality.l) =
  match funct.exp_desc with
  | Typedtree.Texp_ident
      { desc = { Types.val_kind = Types.Val_prim prim; _ };
        kind = Typedtree.Id_prim (poly_mode, _, _); lid; _ } ->
      let args = List.map (fun (lbl, arg, _) -> (lbl, arg)) args in
      begin match
        Translprim.application_allocation env lid.loc prim pos args
          ~poly_mode ~ty:funct.exp_type
      with
      | Translprim.No_allocation -> ()
      | Translprim.Allocation_at_locality mode ->
          register_allocation_mode ~env ~loc:lid.loc mode
      end
  | Typedtree.Texp_ident { desc; lid; _ } ->
    begin match Type_zero_alloc.val_zero_alloc desc.Types.val_zero_alloc with
    | Type_zero_alloc.Zero_alloc { arity; _ } ->
      if List.length args < arity then
        register_allocation_mode ~env ~loc:lid.loc Locality.legacy
      else
        Env.walk_locks_for_zero_alloc_return ~env ~loc:lid.loc mode_ret
    | Type_zero_alloc.Default -> ()
    end
  | _ -> ()

let relax_alloc (desc : Types.value_description) ~is_applied mode =
  if not is_applied then mode
  else
    match desc.val_kind with
    | Types.Val_prim _ ->
      With_regionality.meet_const_with
        Allocation
        Allocation.Const.Noalloc_strict
        mode
    | _ ->
      begin match Type_zero_alloc.val_zero_alloc desc.val_zero_alloc with
      | Type_zero_alloc.Zero_alloc { strict = true; _ } ->
        With_regionality.meet_const_with
          Allocation
          Allocation.Const.Noalloc_strict
          mode
      | Type_zero_alloc.Zero_alloc { strict = false; _ } ->
        With_regionality.meet_const_with
          Allocation
          Allocation.Const.Noalloc
          mode
      | Type_zero_alloc.Default -> mode
      end

let constrain_enclosing_closures pp closures =
  List.iter
    (fun (_, closure_mode) ->
      Allocation.submode_err pp
        (Allocation.of_const ~hint:Allocated_on_heap Alloc)
        closure_mode)
    closures

let enclosing_noalloc_closure closures =
  List.find_map
    (fun (closure_pp, closure_mode) ->
      match Allocation.Guts.get_ceil closure_mode with
      | Noalloc -> Some (closure_pp, Hint.Noalloc)
      | Noalloc_strict -> Some (closure_pp, Hint.Noalloc_strict)
      | Alloc -> None)
    closures

let constrain_closures () =
  let heap, pending =
    !allocations
    |> List.partition (fun {locality_mode; _} ->
      match
        Locality.Guts.get_ceil locality_mode
      with
      | Global -> true
      | Local -> false)
  in
  allocations := pending;
  (* Visited in registration (i.e. source) order, so that the first offending
     allocation is the one reported. *)
  List.iter (fun {closures; pp; _} -> constrain_enclosing_closures pp closures)
    (List.rev heap)

let constrain_allocations () =
  let local, pending =
    !allocations
    |> List.partition_map (fun ({closures; _} as allocation) ->
      match enclosing_noalloc_closure closures with
      | Some closure -> Left (allocation, closure)
      | None -> Right allocation)
  in
  allocations := pending;
  (* Visited in registration (i.e. source) order, so that the first offending
     allocation is the one reported. *)
  List.iter
    (fun ({locality_mode; pp; _}, (closure_pp, closure_mode)) ->
      let stack_allocated =
        Locality.of_const
          ~hint:(Allocated_in_noalloc_closure (closure_pp, closure_mode))
          Local
      in
      Locality.submode_err pp stack_allocated locality_mode)
    (List.rev local)

let optimise_allocations () =
  (* CR zqian: Ideally we want to optimise all axes relavant to allocation. For
  example, pushing an allocation to [contended] is useful to the middle-end.
  However, a [contended] value in a module causes extra modality in printing.
  Therefore, here we only optimise allocation for stack/heap. Proper solutions:
  - Remove [Contention] axis from [With_locality].
  - Add it back when middle-end can really utilize this information. *)
  (* Allocations are visited in registration (i.e. source) order, so that the
     first offending allocation is the one reported. *)
  let allocations = List.rev !allocations in
  (* Reset first: the loop below can raise. *)
  reset_allocations ();
  List.iter
    (fun {locality_mode; closures; pp} ->
      match Locality.zap_to_ceil_exn locality_mode with
      | Local -> ()
      | Global -> constrain_enclosing_closures pp closures)
    allocations
