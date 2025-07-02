(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Pattern = struct
  type t =
    | Code of Code_id.t
    | Set_of_closures of Symbol.t Function_slot.Lmap.t
    | Block_like of Symbol.t

  let code code_id = Code code_id

  let set_of_closures closure_symbols = Set_of_closures closure_symbols

  let block_like symbol = Block_like symbol

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Code code_id ->
      Format.fprintf ppf "@[<hov 1>(Code@ %a)@]" Code_id.print code_id
    | Set_of_closures closure_symbols ->
      Format.fprintf ppf "@[<hov 1>(Set_of_closures@ %a)@]"
        (Function_slot.Lmap.print Symbol.print) closure_symbols
    | Block_like symbol ->
      Format.fprintf ppf "@[<hov 1>(Block_like@ %a)@]" Symbol.print symbol

  let apply_renaming t renaming =
    match t with
    | Code code_id -> Code (Renaming.apply_code_id renaming code_id)
    | Set_of_closures map ->
      Set_of_closures
        (Function_slot.Lmap.map (Renaming.apply_symbol renaming) map)
    | Block_like symbol -> Block_like (Renaming.apply_symbol renaming symbol)

  let free_names t =
    match t with
    | Code code_id ->
      Name_occurrences.singleton_code_id code_id Name_mode.normal
    | Set_of_closures closure_symbols ->
      Function_slot.Lmap.fold
        (fun _ symbol free_names ->
          Name_occurrences.add_symbol free_names symbol Name_mode.normal)
        closure_symbols Name_occurrences.empty
    | Block_like symbol ->
      Name_occurrences.singleton_symbol symbol Name_mode.normal

  let symbols_being_defined t =
    match t with
    | Code _ -> Symbol.Set.empty
    | Set_of_closures closure_symbols ->
      closure_symbols |> Function_slot.Lmap.data |> Symbol.Set.of_list
    | Block_like symbol -> Symbol.Set.singleton symbol

  let code_being_defined t =
    match t with
    | Code code_id -> Code_id.Set.singleton code_id
    | Set_of_closures _ | Block_like _ -> Code_id.Set.empty

  let binds_code t =
    match t with Code _ -> true | Set_of_closures _ | Block_like _ -> false

  let binds_symbols t =
    match t with Code _ -> false | Set_of_closures _ | Block_like _ -> true

  let everything_being_defined t =
    match t with
    | Code code_id ->
      Code_id_or_symbol.Set.singleton (Code_id_or_symbol.create_code_id code_id)
    | Set_of_closures closure_symbols ->
      closure_symbols |> Function_slot.Lmap.data |> Symbol.Set.of_list
      |> Code_id_or_symbol.set_of_symbol_set
    | Block_like symbol ->
      Code_id_or_symbol.Set.singleton (Code_id_or_symbol.create_symbol symbol)

  let everything_being_defined_as_list t =
    match t with
    | Code code_id -> [Code_id_or_symbol.create_code_id code_id]
    | Set_of_closures closure_symbols ->
      closure_symbols |> Function_slot.Lmap.data
      |> List.map Code_id_or_symbol.create_symbol
    | Block_like symbol -> [Code_id_or_symbol.create_symbol symbol]

  let ids_for_export t =
    match t with
    | Code code_id -> Ids_for_export.singleton_code_id code_id
    | Set_of_closures closure_symbols ->
      let symbols =
        closure_symbols |> Function_slot.Lmap.data |> Symbol.Set.of_list
      in
      Ids_for_export.create ~symbols ()
    | Block_like symbol -> Ids_for_export.singleton_symbol symbol

  let gc_roots t =
    match t with
    | Code _ -> []
    | Set_of_closures closure_symbols ->
      [List.hd (Function_slot.Lmap.data closure_symbols)]
    | Block_like s -> [s]
end

type t = Pattern.t list

let empty = []

let check_pattern_list_invariant pattern_list =
  (* Check that there are no repeated bindings of symbols or code IDs. *)
  let everything_being_defined =
    List.map Pattern.everything_being_defined_as_list pattern_list
    |> List.concat
  in
  let everything_being_defined_as_set =
    Code_id_or_symbol.Set.of_list everything_being_defined
  in
  if List.compare_length_with everything_being_defined
       (Code_id_or_symbol.Set.cardinal everything_being_defined_as_set)
     <> 0
  then
    Misc.fatal_errorf
      "Illegal pattern list (duplicate code IDs or symbols):@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Pattern.print)
      pattern_list

let [@ocamlformat "disable"] print ppf t =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Pattern.print) t

let create pattern_list =
  if Flambda_features.check_invariants ()
  then check_pattern_list_invariant pattern_list;
  pattern_list

let singleton pattern = [pattern]

let to_list t = t

let symbols_being_defined t =
  List.map Pattern.symbols_being_defined t |> Symbol.Set.union_list

let code_being_defined t =
  List.map Pattern.code_being_defined t |> Code_id.Set.union_list

let binds_code t = List.exists Pattern.binds_code t

let binds_symbols t = List.exists Pattern.binds_symbols t

let everything_being_defined t =
  List.map Pattern.everything_being_defined t
  |> Code_id_or_symbol.Set.union_list

let everything_being_defined_as_list t =
  List.concat_map Pattern.everything_being_defined_as_list t

let apply_renaming t renaming =
  List.map (fun pattern -> Pattern.apply_renaming pattern renaming) t

let free_names t = List.map Pattern.free_names t |> Name_occurrences.union_list

let ids_for_export t =
  List.map Pattern.ids_for_export t |> Ids_for_export.union_list

let concat t1 t2 = t1 @ t2

let gc_roots t = List.concat_map Pattern.gc_roots t
