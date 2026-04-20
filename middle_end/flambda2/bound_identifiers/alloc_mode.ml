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
    | Heap_or_local
  [@@warning "-37"]

  let print ppf t =
    match t with
    | Heap -> Format.pp_print_string ppf "Heap"
    | Local -> Format.pp_print_string ppf "Local"
    | Heap_or_local -> Format.pp_print_string ppf "Heap_or_local"

  let compare t1 t2 =
    match t1, t2 with
    | Heap, Heap | Local, Local | Heap_or_local, Heap_or_local -> 0
    | Heap, (Local | Heap_or_local) -> -1
    | (Local | Heap_or_local), Heap -> 1
    | Local, Heap_or_local -> -1
    | Heap_or_local, Local -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let heap = Heap

  let local () =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap
    else Heap_or_local

  let unknown () =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap
    else Heap_or_local

  let from_lambda (mode : Lambda.locality_mode) =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap
    else match mode with Alloc_heap -> Heap | Alloc_local -> Heap_or_local

  let to_lambda t =
    match t with
    | Heap -> Lambda.alloc_heap
    | Local | Heap_or_local ->
      assert (Flambda_features.stack_allocation_enabled ());
      Lambda.alloc_local
end

module For_applications = struct
  type t =
    | Not_alloc_stack of { alloc_region : Variable.t }
    | Maybe_alloc_stack of
        { alloc_region : Variable.t;
          region : Variable.t;
          ghost_region : Variable.t
        }

  let print ppf t =
    match t with
    | Not_alloc_stack { alloc_region } ->
      Format.fprintf ppf "@[<hov 1>(Heap (alloc@ %a))@]" Variable.print
        alloc_region
    | Maybe_alloc_stack { region; ghost_region; alloc_region } ->
      Format.fprintf ppf
        "@[<hov 1>(Local (region@ %a)@ (ghost_region@ %a)@ (alloc@ %a))@]"
        Variable.print region Variable.print ghost_region Variable.print
        alloc_region

  let compare t1 t2 =
    match t1, t2 with
    | ( Not_alloc_stack { alloc_region = alloc_region1 },
        Not_alloc_stack { alloc_region = alloc_region2 } ) ->
      Variable.compare alloc_region1 alloc_region2
    | ( Maybe_alloc_stack
          { region = region1;
            ghost_region = ghost_region1;
            alloc_region = alloc_region1
          },
        Maybe_alloc_stack
          { region = region2;
            ghost_region = ghost_region2;
            alloc_region = alloc_region2
          } ) ->
      let c = Variable.compare region1 region2 in
      if c <> 0
      then c
      else
        let c = Variable.compare ghost_region1 ghost_region2 in
        if c <> 0 then c else Variable.compare alloc_region1 alloc_region2
    | Not_alloc_stack _, Maybe_alloc_stack _ -> -1
    | Maybe_alloc_stack _, Not_alloc_stack _ -> 1

  let not_alloc_stack ~alloc_region = Not_alloc_stack { alloc_region }

  let maybe_alloc_stack ~alloc_region ~region ~ghost_region =
    if Flambda_features.stack_allocation_enabled ()
    then Maybe_alloc_stack { alloc_region; region; ghost_region }
    else Not_alloc_stack { alloc_region }

  let as_type t : For_types.t =
    match t with
    | Not_alloc_stack _ -> Heap
    | Maybe_alloc_stack _ -> Heap_or_local

  let from_lambda (mode : Lambda.return_mode) ~current_alloc_region
      ~current_region ~current_ghost_region =
    if not (Flambda_features.stack_allocation_enabled ())
    then Not_alloc_stack { alloc_region = current_alloc_region }
    else
      match mode with
      | Not_alloc_stack ->
        Not_alloc_stack { alloc_region = current_alloc_region }
      | Maybe_alloc_stack -> (
        match current_region, current_ghost_region with
        | Some current_region, Some current_ghost_region ->
          Maybe_alloc_stack
            { alloc_region = current_alloc_region;
              region = current_region;
              ghost_region = current_ghost_region
            }
        | None, _ | _, None ->
          Misc.fatal_error "Local application without a region")

  let free_names t =
    match t with
    | Not_alloc_stack { alloc_region } ->
      Name_occurrences.singleton_variable alloc_region Name_mode.normal
    | Maybe_alloc_stack { alloc_region; region; ghost_region } ->
      Name_occurrences.add_variable
        (Name_occurrences.add_variable
           (Name_occurrences.singleton_variable region Name_mode.normal)
           ghost_region Name_mode.normal)
        alloc_region Name_mode.normal

  let rename = function
    | Not_alloc_stack { alloc_region } ->
      Not_alloc_stack { alloc_region = Variable.rename alloc_region }
    | Maybe_alloc_stack { alloc_region; region; ghost_region } ->
      Maybe_alloc_stack
        { alloc_region = Variable.rename alloc_region;
          region = Variable.rename region;
          ghost_region = Variable.rename ghost_region
        }

  let is_renamed_version_of t t' =
    match t, t' with
    | ( Not_alloc_stack { alloc_region },
        Not_alloc_stack { alloc_region = alloc_region' } ) ->
      Variable.is_renamed_version_of alloc_region alloc_region'
    | Not_alloc_stack _, Maybe_alloc_stack _
    | Maybe_alloc_stack _, Not_alloc_stack _ ->
      false
    | ( Maybe_alloc_stack { alloc_region; region; ghost_region },
        Maybe_alloc_stack
          { alloc_region = alloc_region';
            region = region';
            ghost_region = ghost_region'
          } ) ->
      Variable.is_renamed_version_of alloc_region alloc_region'
      && Variable.is_renamed_version_of region region'
      && Variable.is_renamed_version_of ghost_region ghost_region'

  let renaming t ~guaranteed_fresh =
    match t, guaranteed_fresh with
    | ( Not_alloc_stack { alloc_region },
        Not_alloc_stack { alloc_region = alloc_region' } ) ->
      Renaming.add_fresh_variable Renaming.empty alloc_region
        ~guaranteed_fresh:alloc_region'
    | ( Maybe_alloc_stack { alloc_region; region; ghost_region },
        Maybe_alloc_stack
          { alloc_region = alloc_region';
            region = region';
            ghost_region = ghost_region'
          } ) ->
      let renaming =
        Renaming.add_fresh_variable Renaming.empty alloc_region
          ~guaranteed_fresh:alloc_region'
      in
      let renaming =
        Renaming.add_fresh_variable renaming region ~guaranteed_fresh:region'
      in
      Renaming.add_fresh_variable renaming ghost_region
        ~guaranteed_fresh:ghost_region'
    | Not_alloc_stack _, Maybe_alloc_stack _
    | Maybe_alloc_stack _, Not_alloc_stack _ ->
      Misc.fatal_error "Mismatched alloc_mode in renaming"

  let apply_renaming t renaming =
    match t with
    | Not_alloc_stack { alloc_region } ->
      let alloc_region' = Renaming.apply_variable renaming alloc_region in
      if alloc_region == alloc_region'
      then t
      else Not_alloc_stack { alloc_region = alloc_region' }
    | Maybe_alloc_stack { alloc_region; region; ghost_region } ->
      let alloc_region' = Renaming.apply_variable renaming alloc_region in
      let region' = Renaming.apply_variable renaming region in
      let ghost_region' = Renaming.apply_variable renaming ghost_region in
      if
        alloc_region == alloc_region'
        && region == region'
        && ghost_region == ghost_region'
      then t
      else
        Maybe_alloc_stack
          { alloc_region = alloc_region';
            region = region';
            ghost_region = ghost_region'
          }

  let ids_for_export t =
    match t with
    | Not_alloc_stack { alloc_region } ->
      Ids_for_export.singleton_variable alloc_region
    | Maybe_alloc_stack { alloc_region; region; ghost_region } ->
      Ids_for_export.add_variable
        (Ids_for_export.add_variable
           (Ids_for_export.singleton_variable region)
           ghost_region)
        alloc_region

  let alloc_region t =
    match t with
    | Not_alloc_stack { alloc_region } | Maybe_alloc_stack { alloc_region; _ }
      ->
      alloc_region
end

module For_allocations = struct
  type t =
    | Heap of { alloc_region : Variable.t }
    | Local of
        { alloc_region : Variable.t;
          region : Variable.t
        }

  let print ppf t =
    match t with
    | Heap { alloc_region } ->
      Format.fprintf ppf "@[<hov 1>(Heap (alloc_region@ %a))@]" Variable.print
        alloc_region
    | Local { alloc_region; region } ->
      Format.fprintf ppf "@[<hov 1>(Local (alloc_region@ %a) (region@ %a))@]"
        Variable.print alloc_region Variable.print region

  let compare t1 t2 =
    match t1, t2 with
    | ( Heap { alloc_region = alloc_region1 },
        Heap { alloc_region = alloc_region2 } ) ->
      Variable.compare alloc_region1 alloc_region2
    | ( Local { alloc_region = alloc_region1; region = region1 },
        Local { alloc_region = alloc_region2; region = region2 } ) ->
      let c = Variable.compare region1 region2 in
      if c <> 0 then c else Variable.compare alloc_region1 alloc_region2
    | Heap _, Local _ -> -1
    | Local _, Heap _ -> 1

  let heap ~alloc_region = Heap { alloc_region }

  let local ~alloc_region ~region =
    if Flambda_features.stack_allocation_enabled ()
    then Local { alloc_region; region }
    else Heap { alloc_region }

  let as_type t : For_types.t =
    match t with Heap _ -> Heap | Local _ -> Heap_or_local

  let from_lambda (mode : Lambda.locality_mode) ~current_alloc_region
      ~current_region =
    if not (Flambda_features.stack_allocation_enabled ())
    then Heap { alloc_region = current_alloc_region }
    else
      match mode with
      | Alloc_heap -> Heap { alloc_region = current_alloc_region }
      | Alloc_local -> (
        match current_region with
        | Some region -> Local { alloc_region = current_alloc_region; region }
        | None -> Misc.fatal_error "Local allocation without a region")

  let free_names t =
    match t with
    | Heap { alloc_region } ->
      Name_occurrences.singleton_variable alloc_region Name_mode.normal
    | Local { alloc_region; region } ->
      Name_occurrences.add_variable
        (Name_occurrences.singleton_variable region Name_mode.normal)
        alloc_region Name_mode.normal

  let apply_renaming t renaming =
    match t with
    | Heap { alloc_region } ->
      let alloc_region' = Renaming.apply_variable renaming alloc_region in
      if alloc_region == alloc_region'
      then t
      else Heap { alloc_region = alloc_region' }
    | Local { alloc_region; region } ->
      let alloc_region' = Renaming.apply_variable renaming alloc_region in
      let region' = Renaming.apply_variable renaming region in
      if alloc_region == alloc_region' && region == region'
      then t
      else Local { alloc_region = alloc_region'; region = region' }

  let ids_for_export t =
    match t with
    | Heap { alloc_region } -> Ids_for_export.singleton_variable alloc_region
    | Local { alloc_region; region } ->
      Ids_for_export.add_variable
        (Ids_for_export.singleton_variable region)
        alloc_region
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
