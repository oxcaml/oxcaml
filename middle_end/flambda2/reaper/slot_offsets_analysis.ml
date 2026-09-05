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

let function_slots_to_be_built ~db ~closure_function_decls
    ~function_slot_rewrites ~function_slots =
  List.fold_left
    (fun new_slots (slot, closure_name) ->
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
      let code_id' : Function_declarations.code_id_in_function_declaration =
        match
          Code_id_or_name.Map.find_opt closure_name closure_function_decls
        with
        | Some (Function_declarations.Deleted _ as decl) -> decl
        | Some
            (Function_declarations.Code_id
               { code_id; only_full_applications = _ }) ->
          (* Escaping closures use this field too. *)
          let only_full_applications =
            not
              (PTA.field_used db closure_name Field.unknown_arity_call_witness)
          in
          Code_id { code_id; only_full_applications }
        | None ->
          Misc.fatal_errorf "No function declaration found for closure %a"
            Code_id_or_name.print closure_name
      in
      Function_slot.Map.add slot' code_id' new_slots)
    Function_slot.Map.empty function_slots

let value_slots_to_be_built ~db ~value_slot_rewrites
    ({ function_slots; value_slots } : PTA.function_and_value_slots) =
  match value_slot_rewrites with
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
  | Some value_slot_rewrites ->
    Unboxed_fields.fold_with_kind
      (fun _kind value_slot acc -> Value_slot.Set.add value_slot acc)
      value_slot_rewrites Value_slot.Set.empty

(* Get the list of value and function slots that will be built for a set of
   closures after rewriting. Returns [None] if the set of closures will not get
   built at all, e.g. if it has no usages. [closure_name] should be the name of
   any one of the closures in the set. *)
let slots_to_be_built_for_set_of_closures ~db ~closure_function_decls
    ~unboxed_fields
    ~(changed_representation :
       (Unboxing_analysis.changed_representation * Code_id_or_name.t)
       Code_id_or_name.Map.t) ~closure_name (set : PTA.function_and_value_slots)
    =
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
    let value_slot_rewrites, function_slot_rewrites =
      match
        Code_id_or_name.Map.find_opt closure_name changed_representation
      with
      | None -> None, None
      | Some (Block_representation _, _) ->
        Misc.fatal_error
          "Set of closures is represented as a block rather than a closure"
      | Some
          ( Closure_representation
              (value_slot_rewrites, function_slot_rewrites, _),
            _ ) ->
        Some value_slot_rewrites, Some function_slot_rewrites
    in
    Some
      ( function_slots_to_be_built ~db ~closure_function_decls
          ~function_slot_rewrites ~function_slots:set.function_slots,
        value_slots_to_be_built ~db ~value_slot_rewrites set )

let compute ~free_names ~code_metadata ~closure_function_decls
    ~get_code_metadata
    ({ db; unboxed_fields; changed_representation; _ } :
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
                slots_to_be_built_for_set_of_closures ~db
                  ~closure_function_decls ~unboxed_fields
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
    Function_slot.Set.union_list
      (List.map
         (fun (function_slots, _) -> Function_slot.Map.keys function_slots)
         set_slots_to_be_built)
  in
  let built_value_slots =
    Value_slot.Set.union_list (List.map snd set_slots_to_be_built)
  in
  (* [To_cmm] needs offsets for both slots of a projection but the graph records
     only the accessed one, so use simplify's output free names: a safe
     over-approximation, missing only the fresh slots, which the unions add. *)
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
          built_value_slots;
      all_value_slots =
        Value_slot.Set.union
          (Name_occurrences.all_value_slots_at_normal_mode free_names)
          built_value_slots
    }
  in
  (* [To_cmm] checks the layout against the rewritten metadata, so take the
     sizes from there. (Calling convention changes preserve the number of
     parameter groups, so a slot never actually changes size.) *)
  let get_function_slot_size code_id =
    match Code_id.Map.find_opt code_id code_metadata with
    | Some metadata -> Code_metadata.function_slot_size metadata
    | None ->
      (* Imported code, which is only reached through its cmx. *)
      Code_metadata.function_slot_size (get_code_metadata code_id)
  in
  Slot_offsets.finalize_offsets ~get_function_slot_size ~used_slots slot_offsets
