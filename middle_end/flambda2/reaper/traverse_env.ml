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
    value_slots_to_keep : Value_slot.Set.t
  }

let create ~parent ~conts ~current_code_id ~should_preserve_direct_calls
    ~all_constants ~function_slots_to_keep ~value_slots_to_keep =
  { parent;
    conts;
    current_code_id;
    should_preserve_direct_calls;
    all_constants;
    function_slots_to_keep;
    value_slots_to_keep
  }

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
