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

type t = Lambda.regalloc_param_attribute

let print ppf t =
  match (t : t) with
  | Default_regalloc_params -> Format.pp_print_string ppf "[]"
  | Regalloc_params params ->
    Format.fprintf ppf "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         (fun ppf s -> Format.fprintf ppf "%S" s))
      params

let equal (t1 : t) (t2 : t) =
  match t1, t2 with
  | Default_regalloc_params, Default_regalloc_params -> true
  | Regalloc_params l1, Regalloc_params l2 -> List.equal String.equal l1 l2
  | (Default_regalloc_params | Regalloc_params _), _ -> false

let is_default (t : t) =
  match t with Default_regalloc_params -> true | Regalloc_params _ -> false

let from_lambda (attr : Lambda.regalloc_param_attribute) = attr

let to_lambda t = t
