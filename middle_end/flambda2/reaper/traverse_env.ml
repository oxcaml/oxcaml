(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaelle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type cont_kind = Normal of Variable.t list

type should_preserve_direct_calls =
  | Yes
  | No
  | Auto

type t =
  { parent : Rev_expr.rev_expr_holed;
    conts : cont_kind Continuation.Map.t;
    current_code_id : Code_id.t option;
    should_preserve_direct_calls : should_preserve_direct_calls;
    all_constants : Name.t;
    function_slots_to_keep : Function_slot.Set.t;
    value_slots_to_keep : Value_slot.Set.t;
    region_scope : region_scope;
    enclosing_region_scopes : Code_id_or_name.t list;
    foreign_regions : Variable.Set.t;
    try_regions : Variable.Set.t;
    handlers : (Variable.t list * Variable.Set.t) list;
    return_continuation : Continuation.t;
    exn_continuation : Continuation.t
  }

and region_scope =
  | No_region_scope
  | Toplevel
  | Scope of Code_id_or_name.t

let create ~parent ~conts ~current_code_id ~should_preserve_direct_calls
    ~all_constants ~function_slots_to_keep ~value_slots_to_keep ~region_scope
    ~return_continuation ~exn_continuation =
  { parent;
    conts;
    current_code_id;
    should_preserve_direct_calls;
    all_constants;
    function_slots_to_keep;
    value_slots_to_keep;
    region_scope;
    enclosing_region_scopes =
      (match region_scope with
      | No_region_scope | Toplevel -> []
      | Scope scope -> [scope]);
    foreign_regions = Variable.Set.empty;
    try_regions = Variable.Set.empty;
    handlers = [];
    return_continuation;
    exn_continuation
  }

let region_scope t =
  match t.region_scope with
  | No_region_scope | Toplevel -> None
  | Scope scope ->
    (* An allocation always goes to the innermost region open at runtime, so
       none may be made local in [scope] while a pre-existing region is open. *)
    if Variable.Set.is_empty t.foreign_regions then Some scope else None

let enclosing_region_scopes t = t.enclosing_region_scopes

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let enter_recursive_handler t ~scope =
  match t.region_scope with
  | No_region_scope -> t
  | (Toplevel | Scope _) when not (Variable.Set.is_empty t.foreign_regions) ->
    (* The region of the handlers would be nested inside the pre-existing
       region, which may be closed inside the handlers. *)
    t
  | Toplevel | Scope _ ->
    let scope = Code_id_or_name.var scope in
    { t with
      region_scope = Scope scope;
      enclosing_region_scopes = scope :: t.enclosing_region_scopes
    }

let enter_foreign_region t region =
  { t with foreign_regions = Variable.Set.add region t.foreign_regions }

let exit_foreign_region t region =
  { t with foreign_regions = Variable.Set.remove region t.foreign_regions }

let enter_try_region t region =
  { t with try_regions = Variable.Set.add region t.try_regions }

let exit_try_region t region =
  { t with try_regions = Variable.Set.remove region t.try_regions }

let enter_handler t ~params =
  { t with handlers = (params, t.try_regions) :: t.handlers }

let handler_params_freed_by_end_of_try_region t region =
  List.concat_map
    (fun (params, try_regions) ->
      if Variable.Set.mem region try_regions then params else [])
    t.handlers

let all_continuation_params t =
  Continuation.Map.fold (fun _ (Normal params) acc -> params @ acc) t.conts []

let parent t = t.parent

let current_code_id t = t.current_code_id

let should_preserve_direct_calls t = t.should_preserve_direct_calls

let all_constants t = t.all_constants

let with_parent t parent = { t with parent }

let find_cont t cont =
  match Continuation.Map.find_opt cont t.conts with
  | Some cont_kind -> cont_kind
  | None ->
    Misc.fatal_errorf "[Env.find_cont]: continuation %a not found in env"
      Continuation.print cont

let add_cont t cont cont_kind =
  { t with conts = Continuation.Map.add cont cont_kind t.conts }

let function_slots_to_keep t = t.function_slots_to_keep

let should_keep_function_slot _t _function_slot =
  (* CR chambart/gbury: we currently do not track the used function slots
     precisely enough in simplify/data_flow, see similar comment in
     [slot_offsets.ml] *)
  (* not (Current_unit.is_current (Function_slot.get_compilation_unit
     function_slot)) || Function_slot.Set.mem function_slot
     t.function_slots_to_keep *)
  true

let value_slots_to_keep t = t.value_slots_to_keep

let should_keep_value_slot t value_slot =
  (not (Current_unit.is_current (Value_slot.get_compilation_unit value_slot)))
  || Value_slot.Set.mem value_slot t.value_slots_to_keep
