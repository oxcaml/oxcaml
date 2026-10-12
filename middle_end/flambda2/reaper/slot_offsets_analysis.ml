(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module PTA = Points_to_analysis
module Unboxed_fields = Unboxing_analysis.Unboxed_fields

let function_slots_to_be_built ~function_slot_rewrites ~function_slots =
  List.fold_left
    (fun new_slots (slot, _closure_name) ->
      let slot' =
        match function_slot_rewrites with
        | None -> slot
        | Some function_slot_rewrites -> (
          match Function_slot.Map.find_opt slot function_slot_rewrites with
          | Some function_slot -> function_slot
          | None ->
            Misc.fatal_errorf "Could not find rewritten function slot for %a"
              Function_slot.print slot)
      in
      Function_slot.Set.add slot' new_slots)
    Function_slot.Set.empty function_slots

let value_slots_to_be_built ~db ~unboxed_value_slots
    ({ function_slots; value_slots } : PTA.function_and_value_slots) =
  match unboxed_value_slots with
  | None ->
    (* A value slot is kept iff it is used through some member of the set. *)
    List.fold_left
      (fun surviving_value_slots (value_slot, _) ->
        if
          List.exists
            (fun (_, member) ->
              PTA.field_used db member (Field.value_slot value_slot))
            function_slots
        then Value_slot.Set.add value_slot surviving_value_slots
        else surviving_value_slots)
      Value_slot.Set.empty value_slots
  | Some unboxed_value_slots ->
    Unboxed_fields.fold_with_kind
      (fun _kind value_slot acc -> Value_slot.Set.add value_slot acc)
      unboxed_value_slots Value_slot.Set.empty

(* Get the list of value and function slots that will be built for a set of
   closures after rewriting. Returns [None] if the set of closures will not get
   built at all, e.g. if it has no usages. [closure_name] should be the name of
   any one of the closures in the set. *)
let slots_to_be_built_for_set_of_closures ~(uses : Unboxing_analysis.result)
    ~unboxed_fields
    ~(changed_representation :
       (Unboxing_analysis.changed_representation * Code_id_or_name.t)
       Code_id_or_name.Map.t) ~closure_name (set : PTA.function_and_value_slots)
    =
  let db = uses.db in
  let any_member_has_usage =
    List.exists (fun (_, member) -> PTA.has_use db member) set.function_slots
  in
  let any_member_is_unboxed =
    List.exists
      (fun (_, member) -> Code_id_or_name.Map.mem member unboxed_fields)
      set.function_slots
  in
  if (not any_member_has_usage) || any_member_is_unboxed
  then None
  else
    let unboxed_value_slots, function_slot_rewrites =
      match
        Code_id_or_name.Map.find_opt closure_name changed_representation
      with
      | None -> None, None
      | Some (Block_representation _, _) ->
        Misc.fatal_error
          "Set of closures is represented as a block rather than a closure"
      | Some
          ( Closure_representation
              (unboxed_value_slots, function_slot_rewrites, _),
            _ ) ->
        Some unboxed_value_slots, Some function_slot_rewrites
    in
    Some
      ( function_slots_to_be_built ~function_slot_rewrites
          ~function_slots:set.function_slots,
        value_slots_to_be_built ~db ~unboxed_value_slots set )

let compute ~free_names ~analysis_scope
    ({ db; unboxed_fields; changed_representation; _ } as uses :
      Unboxing_analysis.result) =
  (* The query gives us the name of every closure, but we want one entry per set
     of closures. [seen_closure_names] tracks the closures of the sets already
     handled, so that the rest of them are skipped. *)
  let _seen, set_slots_to_be_built =
    Code_id_or_name.Set.fold
      (fun closure_name (seen_closure_names, set_slots) ->
        if Code_id_or_name.Set.mem closure_name seen_closure_names
        then seen_closure_names, set_slots
        else
          match
            PTA.get_set_of_closures_def_with_value_slots db closure_name
          with
          | Not_a_set_of_closures ->
            Misc.fatal_errorf "%a is not bound to a set of closures"
              Code_id_or_name.print closure_name
          | Set_of_closures set ->
            let seen_closure_names' =
              Code_id_or_name.Set.union seen_closure_names
                (Code_id_or_name.Set.of_list (List.map snd set.function_slots))
            in
            let set_slots' =
              match
                slots_to_be_built_for_set_of_closures ~uses ~unboxed_fields
                  ~changed_representation ~closure_name set
              with
              | None -> set_slots
              | Some slots -> slots :: set_slots
            in
            seen_closure_names', set_slots')
      (PTA.all_closure_names db)
      (Code_id_or_name.Set.empty, [])
  in
  let slot_offsets =
    List.fold_left
      (fun slot_offsets (function_slots, value_slots) ->
        (* Phantom sets are already excluded by [any_member_has_usage]. *)
        Slot_offsets.add_set_of_closures_slots slot_offsets ~is_phantom:false
          ~function_slots ~value_slots)
      Slot_offsets.empty set_slots_to_be_built
  in
  let built_function_slots =
    Function_slot.Set.union_list (List.map fst set_slots_to_be_built)
  in
  let built_value_slots =
    Value_slot.Set.union_list (List.map snd set_slots_to_be_built)
  in
  (* [free_names] comes from simplify, so it doesn't know about fresh value
     slots introduced by representation changes. Collect the fresh slots so we
     can include them as used. *)
  let fresh_value_slots =
    Code_id_or_name.Map.fold
      (fun _ (repr, _) fresh_value_slots ->
        match (repr : Unboxing_analysis.changed_representation) with
        | Block_representation _ -> fresh_value_slots
        | Closure_representation (unboxed_value_slots, _, _) ->
          Unboxed_fields.fold_with_kind
            (fun _kind value_slot acc -> Value_slot.Set.add value_slot acc)
            unboxed_value_slots fresh_value_slots)
      changed_representation Value_slot.Set.empty
  in
  (* Projections have an accessed slot and a slot used as a base for accessing.
     [To_cmm] needs offsets for both slots, but the graph records only the
     accessed one, so liveness is taken as simplify's free names plus the fresh
     value slots. *)
  let used_slots : Slot_offsets.used_slots =
    { function_slots_in_normal_projections =
        Function_slot.Set.union
          (Name_occurrences.function_slots_in_normal_projections free_names)
          built_function_slots;
      all_function_slots =
        Function_slot.Set.union
          (Name_occurrences.all_function_slots_at_normal_mode free_names)
          built_function_slots;
      value_slots_in_normal_projections =
        Value_slot.Set.union
          (Name_occurrences.value_slots_in_normal_projections free_names)
          fresh_value_slots;
      all_value_slots =
        Value_slot.Set.union
          (Name_occurrences.all_value_slots_at_normal_mode free_names)
          built_value_slots
    }
  in
  Slot_offsets.finalize_offsets slot_offsets
    ~is_local_compilation_unit:(Analysis_scope.contains_unit analysis_scope)
    ~used_slots
