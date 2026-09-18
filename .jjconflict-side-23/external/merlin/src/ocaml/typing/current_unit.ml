(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The compilation unit currently being compiled. *)

let current_unit : Unit_info.t option ref =
  ref None

let get () =
  !current_unit

let get_cu () =
  Option.map Unit_info.modname (get ())

let get_cu_or_dummy () =
  Option.value (get_cu ()) ~default:Compilation_unit.dummy

let get_cu_exn () =
  match get_cu () with
  | Some t -> t
  | None -> Misc.fatal_error "No compilation unit set"

let is_current t =
  match get_cu () with
  | None -> false
  | Some t' -> Compilation_unit.equal t t'

let set cu =
  current_unit := Some cu

let unset () =
  current_unit := None

module Name = struct
  let get () =
    match !current_unit with
    | None -> ""
    | Some cu ->
      Compilation_unit.Name.to_string
        (Compilation_unit.name (Unit_info.modname cu))
  let is name =
    get () = name
  let is_ident id =
    Ident.is_global id && is (Ident.name id)
  let is_path = function
  | Path.Pident id -> is_ident id
  | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
end

(* Merlin: the below functions are commented out because they are unused by the frontend

let symbol_for_local_ident id =
  assert (not (Ident.is_global_or_predef id));
  let compilation_unit = get_cu_exn () in
  Symbol.for_name compilation_unit (Ident.unique_name id)

let symbol () =
  Symbol.for_compilation_unit (get_cu_exn ())

let const_label = ref 0

let symbol_for_new_const () =
  incr const_label;
  Symbol.for_name (get_cu_exn ()) (Int.to_string !const_label)
*)
