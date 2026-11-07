(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019--2020 OCamlPro SAS                                    *)
(*   Copyright 2019--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Zero
  | One
  | More_than_one

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Zero -> Format.fprintf ppf "Zero"
  | One -> Format.fprintf ppf "One"
  | More_than_one -> Format.fprintf ppf "More_than_one"

module By_name_mode = struct
  type num_occurrences = t

  type t =
    { normal : num_occurrences;
      phantom : num_occurrences
    }

  let print ppf { normal; phantom } =
    match phantom with
    | Zero -> Format.fprintf ppf "#%a" print normal
    | One | More_than_one ->
      Format.fprintf ppf "#%a(phantom #%a)" print normal print phantom

  let needed_by_phantom_let { normal = _; phantom } var =
    match phantom with
    | Zero -> false
    | One | More_than_one -> (
      match Variable.kind var with
      | Region | Rec_info -> false
      | Value | Naked_number _ -> true)

  let needed_by_phantom_let_in_map num_occurrences var =
    match Variable.Map.find var num_occurrences with
    | exception Not_found -> false
    | num_occurrences -> needed_by_phantom_let num_occurrences var
end
