(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Immutable of string
  | Mutable of { length : Target_ocaml_int.t }

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Immutable str1, Immutable str2 -> String.compare str1 str2
    | Mutable { length = length1 }, Mutable { length = length2 } ->
      Target_ocaml_int.compare length1 length2
    | Immutable _, Mutable _ -> -1
    | Mutable _, Immutable _ -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let hash = Hashtbl.hash

  let print ppf t =
    match t with
    | Immutable str ->
      let size = String.length str in
      let s, dots =
        let max_size = 10 in
        let long = size > max_size in
        if long then String.sub str 0 8, "..." else str, ""
      in
      Format.fprintf ppf "(size %d) (contents \"%S\"%s)" size s dots
    | Mutable { length } ->
      Format.fprintf ppf "(mutable (size %a))" Target_ocaml_int.print length
end)
