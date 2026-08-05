(* Registration and settling of the modes of allocations. *)

open Mode

type t =
  { alloc_mode : Alloc.r;
    (** The mode of the allocation. *)
    closures : (Hint.pinpoint * Allocation.r) list;
    (** Closures enclosing the allocation, from the innermost
        to the outermost one. *)
    pp : Hint.pinpoint
  }

let allocations : t list ref = Local_store.s_ref []

let reset_allocations () = allocations := []

let register_mode_for_optimisation pp ?(closures=[]) alloc_mode =
  let alloc_mode = Alloc.disallow_left alloc_mode in
  allocations := {alloc_mode; closures; pp} :: !allocations

let register_allocation_mode ~env ~loc alloc_mode =
  let pp : Hint.pinpoint = (loc, Allocation) in
  let closures = Env.walk_locks_for_allocation ~env pp in
  register_mode_for_optimisation pp ~closures alloc_mode

let register_allocation_value_mode ~env ~loc
    ?(desc  = (Unknown : Hint.allocation_desc)) mode =
  let alloc_mode = value_to_alloc_r2g mode in
  register_allocation_mode ~env ~loc alloc_mode;
  (* We must apply each morphism separately so that their hints correspond to
     the correct morphism *)
  let mode =
    value_to_alloc_r2g ~allocation:({loc; txt = desc})
      (Mode.Value.disallow_left mode)
  in
  let mode = alloc_as_value ~allocation:({loc; txt = desc}) mode in
  alloc_mode, mode

(* Unlike most allocations, which can be the highest mode allowed by
   [expected_mode], functions have more constraints. For example, a two
   parameter function needs to be made global if its partial application
   to one argument must be global. As a result, a function gets an
   [Alloc.lr] allocation mode that can be further constrained. *)
let register_closure_allocation ~env (mode : Value.r) ~loc
    : Alloc.lr * Value.r =
  let allocation : Hint.allocation = {loc; txt = Unknown} in
  let (alloc_mode : Alloc.lr), _ =
    Alloc.newvar_below (value_to_alloc_r2g ~allocation mode)
  in
  register_allocation_mode ~env ~loc (Alloc.disallow_left alloc_mode);
  let closed_over_mode =
    alloc_as_value ~allocation (Alloc.disallow_left alloc_mode)
  in
  alloc_mode, closed_over_mode

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
    |> List.partition (fun {alloc_mode; _} ->
      match
        Locality.Guts.get_ceil (Alloc.proj_comonadic Areality alloc_mode)
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
    (fun ({alloc_mode; pp; _}, (closure_pp, closure_mode)) ->
      let stack_allocated =
        Locality.of_const
          ~hint:(Allocated_in_noalloc_closure (closure_pp, closure_mode))
          Local
      in
      Locality.submode_err pp stack_allocated
        (Alloc.proj_comonadic Areality alloc_mode))
    (List.rev local)

let optimise_allocations () =
  (* CR zqian: Ideally we want to optimise all axes relavant to allocation. For
  example, pushing an allocation to [contended] is useful to the middle-end.
  However, a [contended] value in a module causes extra modality in printing.
  Therefore, here we only optimise allocation for stack/heap. Proper solutions:
  - Remove [Contention] axis from [Alloc].
  - Add it back when middle-end can really utilize this information. *)
  (* Allocations are visited in registration (i.e. source) order, so that the
     first offending allocation is the one reported. *)
  let allocations = List.rev !allocations in
  (* Reset first: the loop below can raise. *)
  reset_allocations ();
  List.iter
    (fun {alloc_mode; closures; pp} ->
      match Locality.zap_to_ceil (Alloc.proj_comonadic Areality alloc_mode)
      with
      | Local -> ()
      | Global -> constrain_enclosing_closures pp closures)
    allocations
