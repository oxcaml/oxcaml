(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { function_decls : Function_declarations.t;
    value_slots : Simple.t Value_slot.Map.t
  }

let [@ocamlformat "disable"] print ppf
      { function_decls;
        value_slots
      } =
  Format.fprintf ppf "@[<hov 1>(%tset_of_closures%t@ \
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(value_slots@ %a)@]\
      )@]"
    Flambda_colours.prim_constructive
    Flambda_colours.pop
    (Function_declarations.print) function_decls
    (Value_slot.Map.print Simple.print) value_slots

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let hash _ = Misc.fatal_error "Not yet implemented"

  let compare { function_decls = function_decls1; value_slots = value_slots1 }
      { function_decls = function_decls2; value_slots = value_slots2 } =
    let c = Function_declarations.compare function_decls1 function_decls2 in
    if c <> 0
    then c
    else Value_slot.Map.compare Simple.compare value_slots1 value_slots2

  let equal t1 t2 = compare t1 t2 = 0
end)

let is_empty { function_decls; value_slots } =
  Function_declarations.is_empty function_decls
  && Value_slot.Map.is_empty value_slots

let create ~value_slots function_decls = { function_decls; value_slots }

let function_decls t = t.function_decls

let value_slots t = t.value_slots

let is_closed t = Value_slot.Map.is_empty t.value_slots

let [@ocamlformat "disable"] print ppf
      { function_decls;
        value_slots;
      } =
  if Value_slot.Map.is_empty value_slots then
    Format.fprintf ppf "@[<hov 1>(%tset_of_closures%t@ \
        @[<hov 1>%a@]\
        )@]"
      Flambda_colours.prim_constructive
      Flambda_colours.pop
      (Function_declarations.print) function_decls
  else
    Format.fprintf ppf "@[<hov 1>(%tset_of_closures%t@ \
        @[<hov 1>%a@]@ \
        @[<hov 1>(env@ %a)@]\
        )@]"
      Flambda_colours.prim_constructive
      Flambda_colours.pop
      Function_declarations.print function_decls
      (Value_slot.Map.print Simple.print) value_slots

let free_names { function_decls; value_slots } =
  let free_names_of_value_slots =
    Value_slot.Map.fold
      (fun value_slot simple free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.add_value_slot_in_declaration
             (Simple.free_names simple) value_slot Name_mode.normal))
      value_slots Name_occurrences.empty
  in
  Name_occurrences.union
    (Function_declarations.free_names function_decls)
    free_names_of_value_slots

let apply_renaming ({ function_decls; value_slots } as t) renaming =
  let function_decls' =
    Function_declarations.apply_renaming function_decls renaming
  in
  let changed = ref false in
  let value_slots' =
    Value_slot.Map.filter_map
      (fun var simple ->
        if Renaming.value_slot_is_used renaming var
        then (
          let simple' = Simple.apply_renaming simple renaming in
          if not (simple == simple') then changed := true;
          Some simple')
        else (
          changed := true;
          None))
      value_slots
  in
  if function_decls == function_decls' && not !changed
  then t
  else { function_decls = function_decls'; value_slots = value_slots' }

let ids_for_export { function_decls; value_slots } =
  let function_decls_ids =
    Function_declarations.ids_for_export function_decls
  in
  Value_slot.Map.fold
    (fun _value_slot simple ids -> Ids_for_export.add_simple ids simple)
    value_slots function_decls_ids

let filter_function_declarations t ~f =
  let function_decls = Function_declarations.filter t.function_decls ~f in
  { t with function_decls }
