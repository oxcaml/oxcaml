(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module For_types = struct
  type t =
    | Heap
    | Local
    | Unknown
  [@@warning "-37"]

  let print ppf t =
    match t with
    | Heap -> Format.pp_print_string ppf "Heap"
    | Local -> Format.pp_print_string ppf "Local"
    | Unknown -> Format.pp_print_string ppf "Unknown"

  let compare t1 t2 =
    match t1, t2 with
    | Heap, Heap | Local, Local | Unknown, Unknown -> 0
    | Heap, (Local | Unknown) -> -1
    | (Local | Unknown), Heap -> 1
    | Local, Unknown -> -1
    | Unknown, Local -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let heap = Heap

  let local () =
    if not (Flambda_features.stack_allocation_enabled ()) then Heap else Unknown

  let unknown () =
    if not (Flambda_features.stack_allocation_enabled ()) then Heap else Unknown

  let from_lambda (mode : Lambda.allocation_mode) =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap
    else
      match mode with
      | Alloc_heap -> Heap
      | Alloc_local | Alloc_external -> Unknown

  let to_lambda t =
    match t with
    | Heap -> Lambda.alloc_heap
    | Local | Unknown ->
      assert (Flambda_features.stack_allocation_enabled ());
      Lambda.alloc_local
end

module For_applications = struct
  type t =
    | Heap
    | Local of
        { region : Variable.t;
          ghost_region : Variable.t
        }

  let print ppf t =
    match t with
    | Heap -> Format.pp_print_string ppf "Heap"
    | Local { region; ghost_region } ->
      Format.fprintf ppf "@[<hov 1>(Local (region@ %a)@ (ghost_region@ %a))@]"
        Variable.print region Variable.print ghost_region

  let compare t1 t2 =
    match t1, t2 with
    | Heap, Heap -> 0
    | ( Local { region = region1; ghost_region = ghost_region1 },
        Local { region = region2; ghost_region = ghost_region2 } ) ->
      let c = Variable.compare region1 region2 in
      if c <> 0 then c else Variable.compare ghost_region1 ghost_region2
    | Heap, Local _ -> -1
    | Local _, Heap -> 1

  let heap = Heap

  let local ~region ~ghost_region =
    if Flambda_features.stack_allocation_enabled ()
    then Local { region; ghost_region }
    else Heap

  let as_type t : For_types.t = match t with Heap -> Heap | Local _ -> Unknown

  let from_lambda (mode : Lambda.allocation_mode) ~current_region
      ~current_ghost_region =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap
    else
      match mode with
      | Alloc_heap -> Heap
      | Alloc_external ->
        Misc.fatal_error
          "Impossible, externally-allocated functions are not supported"
      | Alloc_local -> (
        match current_region, current_ghost_region with
        | Some current_region, Some current_ghost_region ->
          Local { region = current_region; ghost_region = current_ghost_region }
        | None, _ | _, None ->
          Misc.fatal_error "Local application without a region")

  let free_names t =
    match t with
    | Heap -> Name_occurrences.empty
    | Local { region; ghost_region } ->
      Name_occurrences.add_variable
        (Name_occurrences.singleton_variable region Name_mode.normal)
        ghost_region Name_mode.normal

  let apply_renaming t renaming =
    match t with
    | Heap -> Heap
    | Local { region; ghost_region } ->
      let region' = Renaming.apply_variable renaming region in
      let ghost_region' = Renaming.apply_variable renaming ghost_region in
      if region == region' && ghost_region == ghost_region'
      then t
      else Local { region = region'; ghost_region = ghost_region' }

  let ids_for_export t =
    match t with
    | Heap -> Ids_for_export.empty
    | Local { region; ghost_region } ->
      Ids_for_export.add_variable
        (Ids_for_export.singleton_variable region)
        ghost_region
end

module For_allocations = struct
  type t =
    | Heap
    | Local of { region : Variable.t }
    | External

  let print ppf t =
    match t with
    | Heap -> Format.pp_print_string ppf "Heap"
    | Local { region } ->
      Format.fprintf ppf "@[<hov 1>(Local (region@ %a))@]" Variable.print region
    | External -> Format.pp_print_string ppf "External"

  let compare t1 t2 =
    match t1, t2 with
    | Heap, Heap -> 0
    | Local { region = region1 }, Local { region = region2 } ->
      Variable.compare region1 region2
    | External, External -> 0
    | Heap, (Local _ | External) -> -1
    | Local _, Heap -> 1
    | Local _, External -> -1
    | External, (Local _ | Heap) -> 1

  let heap = Heap

  let external_ = External

  let local ~region =
    if Flambda_features.stack_allocation_enabled ()
    then Local { region }
    else Heap

  let as_type t : For_types.t =
    match t with Heap -> Heap | Local _ | External -> Unknown

  let from_lambda (mode : Lambda.allocation_mode) ~current_region =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap
    else
      match mode with
      | Alloc_heap -> Heap
      | Alloc_local -> (
        match current_region with
        | Some region -> Local { region }
        | None -> Misc.fatal_error "Local allocation without a region")
      | Alloc_external -> External

  let free_names t =
    match t with
    | Heap -> Name_occurrences.empty
    | Local { region } ->
      Name_occurrences.singleton_variable region Name_mode.normal
    | External -> Name_occurrences.empty

  let apply_renaming t renaming =
    match t with
    | Heap -> Heap
    | Local { region } ->
      let region' = Renaming.apply_variable renaming region in
      if region == region' then t else Local { region = region' }
    | External -> External

  let ids_for_export t =
    match t with
    | Heap -> Ids_for_export.empty
    | Local { region } -> Ids_for_export.singleton_variable region
    | External -> Ids_for_export.empty
end

module For_assignments = struct
  type t =
    | Heap
    | Local

  let print ppf t =
    match t with
    | Heap -> Format.pp_print_string ppf "Heap"
    | Local -> Format.pp_print_string ppf "Local"

  let compare t1 t2 =
    match t1, t2 with
    | Heap, Heap -> 0
    | Local, Local -> 0
    | Heap, Local -> -1
    | Local, Heap -> 1

  let heap = Heap

  let local () =
    if Flambda_features.stack_allocation_enabled () then Local else Heap

  let from_lambda (mode : Lambda.modify_mode) =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap
    else match mode with Modify_heap -> Heap | Modify_maybe_stack -> Local

  let to_lambda t =
    match t with
    | Heap -> Lambda.modify_heap
    | Local ->
      assert (Flambda_features.stack_allocation_enabled ());
      Lambda.modify_maybe_stack
end
