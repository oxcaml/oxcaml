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
    value_slots : Simple.t Value_slot.Map.t;
    dbg : Debuginfo.t
  }

let sexp_fields { function_decls; value_slots; dbg } =
  let open! Misc.Sexp in
  [ fmt ~colour:Flambda_colours.prim_constructive "set_of_closures";
    a "function_decls" function_decls Function_declarations.print;
    a "value_slots" value_slots (Value_slot.Map.print Simple.print) ]
  @ if Debuginfo.is_none dbg then [] else [a "dbg" dbg Debuginfo.print_compact]

let print ppf t = Misc.Sexp.print ppf (sexp_fields t)

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let hash _ = Misc.fatal_error "Not yet implemented"

  let compare
      { function_decls = function_decls1;
        value_slots = value_slots1;
        dbg = dbg1
      }
      { function_decls = function_decls2;
        value_slots = value_slots2;
        dbg = dbg2
      } =
    let c = Function_declarations.compare function_decls1 function_decls2 in
    if c <> 0
    then c
    else
      let c = Value_slot.Map.compare Simple.compare value_slots1 value_slots2 in
      if c <> 0 then c else Debuginfo.compare dbg1 dbg2

  let equal t1 t2 = compare t1 t2 = 0
end)

let is_empty { function_decls; value_slots; dbg = _ } =
  Function_declarations.is_empty function_decls
  && Value_slot.Map.is_empty value_slots

let create ~value_slots ~dbg function_decls =
  { function_decls; value_slots; dbg }

let function_decls t = t.function_decls

let value_slots t = t.value_slots

let debuginfo t = t.dbg

let is_closed t = Value_slot.Map.is_empty t.value_slots

let print ppf { function_decls; value_slots; dbg } =
  let open! Misc.Sexp in
  let value_slots =
    if Value_slot.Map.is_empty value_slots
    then []
    else [a "env" value_slots (Value_slot.Map.print Simple.print)]
  in
  let dbg =
    if Debuginfo.is_none dbg then [] else [a "dbg" dbg Debuginfo.print_compact]
  in
  print ppf
    (fmt "%tset_of_closures%t" Flambda_colours.prim_constructive
       Flambda_colours.pop
    :: fmt "%a" Function_declarations.print function_decls
    :: (value_slots @ dbg))

let free_names { function_decls; value_slots; dbg = _ } =
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

let apply_renaming ({ function_decls; value_slots; dbg } as t) renaming =
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
  else { function_decls = function_decls'; value_slots = value_slots'; dbg }

let ids_for_export { function_decls; value_slots; dbg = _ } =
  let function_decls_ids =
    Function_declarations.ids_for_export function_decls
  in
  Value_slot.Map.fold
    (fun _value_slot simple ids -> Ids_for_export.add_simple ids simple)
    value_slots function_decls_ids

let filter_function_declarations t ~f =
  let function_decls = Function_declarations.filter t.function_decls ~f in
  { t with function_decls }
