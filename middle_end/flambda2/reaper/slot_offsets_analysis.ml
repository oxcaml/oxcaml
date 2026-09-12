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

module Inputs = struct
  type code_info =
    { function_slot_size : int;
      dbg : Debuginfo.t
    }

  type t =
    { free_names : Name_occurrences.t;
      closure_function_decls :
        Function_declarations.code_id_in_function_declaration
        Code_id_or_name.Map.t;
      code_info : code_info Code_id.Map.t
    }

  let create ~free_names ~closure_function_decls ~code_deps ~get_code_metadata =
    (* [code_info] covers every code ID a function slot can be bound to, so the
       solve-time computation can fall back to traversal metadata without
       loading .cmx files. *)
    let code_info =
      Code_id_or_name.Map.fold
        (fun _closure_name
             (decl : Function_declarations.code_id_in_function_declaration)
             code_info ->
          match decl with
          | Deleted _ -> code_info
          | Code_id { code_id; only_full_applications = _ } ->
            if Code_id.Map.mem code_id code_info
            then code_info
            else
              let function_slot_size =
                match Code_id.Map.find_opt code_id code_deps with
                | Some ({ function_slot_size; _ } : Traverse_acc.code_dep) ->
                  function_slot_size
                | None ->
                  (* Imported code, which is only reached through its cmx. *)
                  Code_metadata.function_slot_size (get_code_metadata code_id)
              in
              let dbg = Code_metadata.dbg (get_code_metadata code_id) in
              Code_id.Map.add code_id { function_slot_size; dbg } code_info)
        closure_function_decls Code_id.Map.empty
    in
    { free_names; closure_function_decls; code_info }

  let empty =
    { free_names = Name_occurrences.empty;
      closure_function_decls = Code_id_or_name.Map.empty;
      code_info = Code_id.Map.empty
    }

  let union t1 t2 =
    { free_names = Name_occurrences.union t1.free_names t2.free_names;
      (* Closures are defined by a single compilation unit, so the maps of
         different units are disjoint. *)
      closure_function_decls =
        Code_id_or_name.Map.disjoint_union t1.closure_function_decls
          t2.closure_function_decls;
      (* Several units can record info for the same imported code ID; the
         entries are equal, so keep either. *)
      code_info =
        Code_id.Map.union
          (fun _code_id info _info -> Some info)
          t1.code_info t2.code_info
    }

  let ids_for_export { free_names; closure_function_decls; code_info } =
    let ids = Name_occurrences.ids_for_export free_names in
    let ids =
      Code_id_or_name.Map.fold
        (fun closure_name decl ids ->
          let ids = Ids_for_export.add_code_id_or_name ids closure_name in
          match
            (decl : Function_declarations.code_id_in_function_declaration)
          with
          | Deleted _ -> ids
          | Code_id { code_id; only_full_applications = _ } ->
            Ids_for_export.add_code_id ids code_id)
        closure_function_decls ids
    in
    Code_id.Map.fold
      (fun code_id _info ids -> Ids_for_export.add_code_id ids code_id)
      code_info ids

  let apply_renaming { free_names; closure_function_decls; code_info } renaming
      =
    let free_names = Name_occurrences.apply_renaming free_names renaming in
    let closure_function_decls =
      Code_id_or_name.Map.fold
        (fun closure_name
             (decl : Function_declarations.code_id_in_function_declaration)
             decls ->
          let decl : Function_declarations.code_id_in_function_declaration =
            match decl with
            | Deleted _ -> decl
            | Code_id { code_id; only_full_applications } ->
              Code_id
                { code_id = Renaming.apply_code_id renaming code_id;
                  only_full_applications
                }
          in
          Code_id_or_name.Map.add
            (Renaming.apply_code_id_or_name renaming closure_name)
            decl decls)
        closure_function_decls Code_id_or_name.Map.empty
    in
    let code_info =
      Code_id.Map.fold
        (fun code_id info code_info ->
          Code_id.Map.add
            (Renaming.apply_code_id renaming code_id)
            info code_info)
        code_info Code_id.Map.empty
    in
    { free_names; closure_function_decls; code_info }
end

let function_slots_to_be_built ~db ~code_changes ~analysis_scope ~get_code_info
    ~closure_function_decls ~function_slot_rewrites ~function_slots =
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
            (Function_declarations.Code_id { code_id; only_full_applications })
          ->
          (* CR mvellacott: The following logic is duplicated from
             [Rebuild.rewrite_set_of_closures] and must be kept in sync. In the
             future it would be nice to avoid this duplication. *)
          if
            PTA.field_used db closure_name Field.known_arity_call_witness
            || PTA.field_used db closure_name Field.unknown_arity_call_witness
          then
            (* Only participating units can change calling convention. Use the
               solved decision, not eligibility, to detect a change. *)
            let changed_calling_convention =
              Analysis_scope.contains_unit analysis_scope
                (Code_id.get_compilation_unit code_id)
              &&
              match
                Unboxing_analysis.get_calling_convention_change code_changes
                  code_id
              with
              | Not_changing_calling_convention -> false
              | Changing_calling_convention _ -> true
            in
            Code_id
              { code_id;
                only_full_applications =
                  only_full_applications || changed_calling_convention
              }
          else
            let ({ function_slot_size; dbg } : Inputs.code_info) =
              get_code_info code_id
            in
            Deleted { function_slot_size; dbg }
        | None ->
          Misc.fatal_errorf "No function declaration found for closure %a"
            Code_id_or_name.print closure_name
      in
      Function_slot.Map.add slot' code_id' new_slots)
    Function_slot.Map.empty function_slots

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
let slots_to_be_built_for_set_of_closures ~db ~code_changes ~analysis_scope
    ~get_code_info ~closure_function_decls ~unboxed_fields
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
      ( function_slots_to_be_built ~db ~code_changes ~analysis_scope
          ~get_code_info ~closure_function_decls ~function_slot_rewrites
          ~function_slots:set.function_slots,
        value_slots_to_be_built ~db ~unboxed_value_slots set )

let compute ~(inputs : Inputs.t) ~analysis_scope ~code_changes ~db
    ({ unboxed_fields; changed_representation; _ } : Unboxing_analysis.result) =
  let { Inputs.free_names; closure_function_decls; code_info } = inputs in
  let get_code_info code_id : Inputs.code_info =
    match Unboxing_analysis.find_code_metadata code_changes code_id with
    | Some code_metadata ->
      { function_slot_size = Code_metadata.function_slot_size code_metadata;
        dbg = Code_metadata.dbg code_metadata
      }
    | None -> (
      match Code_id.Map.find_opt code_id code_info with
      | Some info -> info
      | None ->
        Misc.fatal_errorf "No code info was recorded for code ID %a"
          Code_id.print code_id)
  in
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
                slots_to_be_built_for_set_of_closures ~db ~code_changes
                  ~analysis_scope ~get_code_info ~closure_function_decls
                  ~unboxed_fields ~changed_representation ~closure_name set
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
  let get_function_slot_size code_id =
    let ({ function_slot_size; dbg = _ } : Inputs.code_info) =
      get_code_info code_id
    in
    function_slot_size
  in
  Slot_offsets.finalize_offsets
    ~is_local_compilation_unit:(Analysis_scope.contains_unit analysis_scope)
    ~get_function_slot_size ~used_slots slot_offsets
