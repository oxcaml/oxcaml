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

type t = string list

let print ppf t =
  match t with
  | [] -> Format.pp_print_string ppf "[]"
  | params ->
    Format.fprintf ppf "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         (fun ppf s -> Format.fprintf ppf "%S" s))
      params

let equal t1 t2 = List.equal String.equal t1 t2

let is_default t = match t with [] -> true | _ -> false

let from_lambda (attr : Lambda.regalloc_param_attribute) = attr

let to_lambda t = t
