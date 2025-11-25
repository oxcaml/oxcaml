(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
type closure_entry_point =
  | Unknown_arity_code_pointer
  | Known_arity_code_pointer

let closure_entry_point_to_int = function
  | Unknown_arity_code_pointer -> 0
  | Known_arity_code_pointer -> 1

let closure_entry_point_to_string = function
  | Unknown_arity_code_pointer -> "Unknown_arity_code_pointer"
  | Known_arity_code_pointer -> "Known_arity_code_pointer"

type return_kind =
  | Normal of int
  | Exn

let hash_seed =
  let seed = Random.bits () in
  if seed mod 2 = 0 then seed + 1 else seed

let hash2 a b =
  let r = (a * hash_seed) + b in
  r lxor (r lsr 17)

let hash3 a b c =
  let r = (((a * hash_seed) + b) * hash_seed) + c in
  r lxor (r lsr 17)

type view =
  | Block of int * Flambda_kind.t
  | Value_slot of Value_slot.t
  | Function_slot of Function_slot.t
  | Code_of_closure of closure_entry_point
  | Is_int
  | Get_tag
  | Apply of return_kind
  | Code_id_of_call_witness

let hash_view = function
  | Block (i, kind) -> hash3 0 i (Flambda_kind.hash kind)
  | Value_slot vs -> hash2 1 (Value_slot.hash vs)
  | Function_slot fs -> hash2 2 (Function_slot.hash fs)
  | Code_of_closure ep -> hash2 3 (closure_entry_point_to_int ep)
  | Is_int -> 4
  | Get_tag -> 5
  | Apply Exn -> 6
  | Apply (Normal i) -> hash2 7 i
  | Code_id_of_call_witness -> 8

let equal_view v1 v2 =
  match v1, v2 with
  | Block (i1, kind1), Block (i2, kind2) ->
    i1 = i2 && Flambda_kind.equal kind1 kind2
  | Value_slot vs1, Value_slot vs2 -> Value_slot.equal vs1 vs2
  | Function_slot fs1, Function_slot fs2 -> Function_slot.equal fs1 fs2
  | Code_of_closure ep1, Code_of_closure ep2 ->
    closure_entry_point_to_int ep1 = closure_entry_point_to_int ep2
  | Is_int, Is_int
  | Get_tag, Get_tag
  | Code_id_of_call_witness, Code_id_of_call_witness ->
    true
  | Apply Exn, Apply Exn -> true
  | Apply (Normal i1), Apply (Normal i2) -> i1 = i2
  | ( ( Block _ | Value_slot _ | Function_slot _ | Code_of_closure _ | Is_int
      | Get_tag
      | Apply Exn
      | Apply (Normal _)
      | Code_id_of_call_witness ),
      _ ) ->
    false

let print_view ppf = function
  | Block (i, k) -> Format.fprintf ppf "%i_%a" i Flambda_kind.print k
  | Value_slot s -> Format.fprintf ppf "%a" Value_slot.print s
  | Function_slot f -> Format.fprintf ppf "%a" Function_slot.print f
  | Code_of_closure ep ->
    Format.fprintf ppf "Code %s" (closure_entry_point_to_string ep)
  | Is_int -> Format.fprintf ppf "Is_int"
  | Get_tag -> Format.fprintf ppf "Get_tag"
  | Apply (Normal i) -> Format.fprintf ppf "Apply (Normal %i)" i
  | Apply Exn -> Format.fprintf ppf "Apply Exn"
  | Code_id_of_call_witness -> Format.fprintf ppf "Code_id_of_call_witness"

module Table = Table_by_int_id.Make (struct
  type t = view

  let flags = 0

  let print = print_view

  let hash = hash_view

  let equal = equal_view
end)

let grand_table_of_fields = Table.create ()

let create view = Table.add grand_table_of_fields view

let view t = Table.find grand_table_of_fields t

include Datalog.Column.Make (struct
  let name = "field"

  let print ppf t = print_view ppf (view t)
end)

let block i k = create (Block (i, k))

let value_slot vs = create (Value_slot vs)

let function_slot fs = create (Function_slot fs)

let code_of_closure ep = create (Code_of_closure ep)

let is_int = create Is_int

let get_tag = create Get_tag

let apply return_kind = create (Apply return_kind)

let code_id_of_call_witness = create Code_id_of_call_witness

let kind t =
  match view t with
  | Block (_, kind) -> kind
  | Value_slot vs -> Value_slot.kind vs
  | Function_slot _ -> Flambda_kind.value
  | Is_int | Get_tag -> Flambda_kind.naked_immediate
  | (Code_of_closure _ | Apply _ | Code_id_of_call_witness) as view ->
    Misc.fatal_errorf "[field_kind] for virtual field %a" print_view view

let is_value_slot t =
  match view t with
  | Value_slot _ -> true
  | Block _ | Function_slot _ | Is_int | Get_tag | Code_of_closure _ | Apply _
  | Code_id_of_call_witness ->
    false

let is_function_slot t =
  match view t with
  | Function_slot _ -> true
  | Block _ | Value_slot _ | Is_int | Get_tag | Code_of_closure _ | Apply _
  | Code_id_of_call_witness ->
    false

let must_be_function_slot t =
  match view t with
  | Function_slot fs -> fs
  | ( Block _ | Value_slot _ | Is_int | Get_tag | Code_of_closure _ | Apply _
    | Code_id_of_call_witness ) as view ->
    Misc.fatal_errorf "[must_be_function_slot] got %a instead" print_view view

let is_local f =
  Flambda_features.reaper_local_fields ()
  &&
  match view f with
  | Value_slot vs ->
    Compilation_unit.is_current (Value_slot.get_compilation_unit vs)
  | Function_slot fs ->
    Compilation_unit.is_current (Function_slot.get_compilation_unit fs)
  | Block _ | Code_of_closure _ | Apply _ | Code_id_of_call_witness | Is_int
  | Get_tag ->
    false
