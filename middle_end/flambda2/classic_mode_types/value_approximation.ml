(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Pierre Chambart and Vincent Laviron, OCamlPro               *)
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

(** Approximations used for cross-module inlining in Closure_conversion *)

type 'code t =
  | Unknown of Flambda_kind.t
  | Value_symbol of Symbol.t
  | Value_const of Reg_width_const.t
  | Closure_approximation of
      { code_id : Code_id.t;
        function_slot : Function_slot.t;
        code : 'code;
        symbol : Symbol.t option
      }
  | Block_approximation of
      Tag.Scannable.t
      * Flambda_kind.Scannable_block_shape.t
      * 'code t array
      * Alloc_mode.For_types.t

let rec print fmt = function
  | Unknown _ -> Format.fprintf fmt "?"
  | Value_symbol sym -> Symbol.print fmt sym
  | Value_const i -> Reg_width_const.print fmt i
  | Closure_approximation { code_id; _ } ->
    Format.fprintf fmt "[%a]" Code_id.print code_id
  | Block_approximation (tag, _shape, fields, _) ->
    let len = Array.length fields in
    if len < 1
    then Format.fprintf fmt "{}"
    else (
      Format.fprintf fmt "@[<hov 2>{%a:%a" Tag.Scannable.print tag print
        fields.(0);
      for i = 1 to len - 1 do
        Format.fprintf fmt "@ %a" print fields.(i)
      done;
      Format.fprintf fmt "}@]")

let is_unknown = function
  | Unknown _ -> true
  | Value_symbol _ | Value_const _ | Closure_approximation _
  | Block_approximation _ ->
    false

let rec free_names ~code_free_names approx =
  match approx with
  | Unknown _ | Value_const _ -> Name_occurrences.empty
  | Value_symbol sym -> Name_occurrences.singleton_symbol sym Name_mode.normal
  | Block_approximation (_tag, _shape, approxs, _) ->
    Array.fold_left
      (fun names approx ->
        Name_occurrences.union names (free_names ~code_free_names approx))
      Name_occurrences.empty approxs
  | Closure_approximation { code_id; function_slot; code; symbol } ->
    let free_names = code_free_names code in
    let free_names =
      match symbol with
      | None -> free_names
      | Some sym -> Name_occurrences.add_symbol free_names sym Name_mode.normal
    in
    let free_names =
      Name_occurrences.add_code_id free_names code_id Name_mode.normal
    in
    Name_occurrences.add_function_slot_in_types free_names function_slot

let compilation_units_of_free_names free_names =
  (* The units of the predefined-exception and external symbols have no cmx
     data, so are not returned. *)
  let add_unit unit units =
    if
      Compilation_unit.equal unit (Symbol.external_symbols_compilation_unit ())
      || Compilation_unit.Name.equal
           (Compilation_unit.name unit)
           Compilation_unit.Name.predef_exn
    then units
    else Compilation_unit.Set.add unit units
  in
  let units =
    Symbol.Set.fold
      (fun sym units -> add_unit (Symbol.compilation_unit sym) units)
      (Name_occurrences.symbols free_names)
      Compilation_unit.Set.empty
  in
  Code_id.Set.fold
    (fun code_id units -> add_unit (Code_id.get_compilation_unit code_id) units)
    (Name_occurrences.code_ids free_names)
    units

module Standalone = struct
  type 'code approx = 'code t

  type compilation_unit =
    { pack_prefix : string list;
      name : string
    }

  type symbol =
    { symbol_compilation_unit : compilation_unit;
      linkage_name : string
    }

  type code_id =
    { code_id_compilation_unit : compilation_unit;
      code_id_name : string;
      code_id_linkage_name : string
          (* The full linkage name, which includes the name stamp. Unlike the
             stamp itself this is meaningful across compilations, so it can be
             used to look up exported code at demarshalling time. *)
    }

  type t =
    | Unknown of Flambda_kind.t
    | Value_symbol of symbol
    | Value_const of Reg_width_const.Descr.t
    | Closure_approximation of
        { code_id : code_id;
          function_slot : string;
          symbol : symbol option;
          lookup_symbol : symbol option
              (* A symbol under which the closure's full approximation was
                 registered in its unit's cmx data (manufactured by
                 [Closure_conversion] for closures that have no symbol of their
                 own). Looking it up at demarshalling time recovers the original
                 code ID and function slot, whose stamps are not preserved in
                 this standalone form. *)
        }
    | Block_approximation of
        Tag.Scannable.t
        * Flambda_kind.Scannable_block_shape.t
        * t array
        * Alloc_mode.For_types.t

  let create_compilation_unit compilation_unit : compilation_unit =
    { pack_prefix =
        Compilation_unit.for_pack_prefix compilation_unit
        |> Compilation_unit.Prefix.to_list
        |> List.map Compilation_unit.Name.to_string;
      name = Compilation_unit.name_as_string compilation_unit
    }

  let create_symbol symbol : symbol =
    { symbol_compilation_unit =
        create_compilation_unit (Symbol.compilation_unit symbol);
      linkage_name = Symbol.linkage_name_as_string symbol
    }

  let create_code_id code_id : code_id =
    { code_id_compilation_unit =
        create_compilation_unit (Code_id.get_compilation_unit code_id);
      code_id_name = Code_id.name code_id;
      code_id_linkage_name =
        Code_id.linkage_name code_id |> Linkage_name.to_string
    }

  let rec create ~closure_lookup_symbols (approx : _ approx) : t =
    match approx with
    | Unknown kind -> Unknown kind
    | Value_symbol symbol -> Value_symbol (create_symbol symbol)
    | Value_const const -> Value_const (Reg_width_const.descr const)
    | Closure_approximation { code_id; function_slot; code = _; symbol } ->
      (* The code is not saved: at demarshalling time it can be looked up via
         the symbol or the lookup symbol, when present (see
         [to_approximation]). *)
      Closure_approximation
        { code_id = create_code_id code_id;
          function_slot = Function_slot.name function_slot;
          symbol = Option.map create_symbol symbol;
          lookup_symbol =
            Option.map create_symbol
              (Code_id.Map.find_opt code_id closure_lookup_symbols)
        }
    | Block_approximation (tag, shape, fields, alloc_mode) ->
      Block_approximation
        ( tag,
          shape,
          Array.map (create ~closure_lookup_symbols) fields,
          alloc_mode )

  let to_compilation_unit { pack_prefix; name } =
    let prefix =
      match pack_prefix with
      | [] -> Compilation_unit.Prefix.empty
      | _ :: _ ->
        Compilation_unit.Prefix.parse_for_pack (String.concat "." pack_prefix)
    in
    Compilation_unit.create prefix (Compilation_unit.Name.of_string name)

  let to_symbol { symbol_compilation_unit; linkage_name } =
    (* [Symbol.create] would prefix the compilation unit onto the linkage name,
       but we saved the full linkage name, so [unsafe_create] is the faithful
       reconstruction. *)
    Symbol.unsafe_create
      (to_compilation_unit symbol_compilation_unit)
      (Linkage_name.of_string linkage_name)

  let to_code_id
      { code_id_compilation_unit; code_id_name; code_id_linkage_name = _ } =
    Code_id.create ~name:code_id_name ~debug:Debuginfo.none
      (to_compilation_unit code_id_compilation_unit)

  type 'code closure_resolution =
    | Resolved of 'code approx
      (* A full replacement approximation, e.g. recovered wholesale from the
         original unit's cmx data via [lookup_symbol] or [symbol]; the code IDs
         and function slots therein are the original ones. *)
    | Code of Code_id.t * 'code
      (* Code (typically found by linkage name); the code ID given is used in
         place of the freshly created one, but the function slot remains freshly
         created (with a new stamp). *)
    | Unknown_code

  let rec to_approximation t
      ~(find_code :
         code_id:Code_id.t ->
         code_id_linkage_name:Linkage_name.t ->
         function_slot:Function_slot.t ->
         symbol:Symbol.t option ->
         lookup_symbol:Symbol.t option ->
         'code closure_resolution) : 'code approx =
    match t with
    | Unknown kind -> Unknown kind
    | Value_symbol symbol -> Value_symbol (to_symbol symbol)
    | Value_const descr -> Value_const (Reg_width_const.of_descr descr)
    | Closure_approximation { code_id; function_slot; symbol; lookup_symbol }
      -> (
      let code_id_linkage_name =
        Linkage_name.of_string code_id.code_id_linkage_name
      in
      let code_id = to_code_id code_id in
      let function_slot =
        (* The function slot's own compilation unit was not saved; use the code
           ID's. *)
        Function_slot.create
          (Code_id.get_compilation_unit code_id)
          ~name:function_slot ~is_always_immediate:false Flambda_kind.value
      in
      let symbol = Option.map to_symbol symbol in
      let lookup_symbol = Option.map to_symbol lookup_symbol in
      match
        find_code ~code_id ~code_id_linkage_name ~function_slot ~symbol
          ~lookup_symbol
      with
      | Resolved approx -> approx
      | Code (code_id, code) ->
        Closure_approximation { code_id; function_slot; code; symbol }
      | Unknown_code -> Unknown Flambda_kind.value)
    | Block_approximation (tag, shape, fields, alloc_mode) ->
      Block_approximation
        ( tag,
          shape,
          Array.map (fun field -> to_approximation field ~find_code) fields,
          alloc_mode )

  let to_marshalled_string t = Marshal.to_string t []

  let of_marshalled_string s : t = Marshal.from_string s 0
end
