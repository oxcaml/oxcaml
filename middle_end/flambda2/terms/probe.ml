(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type desc =
  | Optimized of
      { name : string;
        enabled_at_init : bool
      }
  | Behaves_like_direct_call of
      { name : string;
        enabled_at_init : bool
      }

type t = desc option

let from_lambda (p : Lambda.probe) =
  match p with
  | None -> None
  | Some (Lambda.Optimized { name; enabled_at_init }) ->
    Some (Optimized { name; enabled_at_init })
  | Some (Lambda.Behaves_like_direct_call { name; enabled_at_init }) ->
    Some (Behaves_like_direct_call { name; enabled_at_init })

let print ppf t =
  match t with
  | None -> Format.pp_print_string ppf "()"
  | Some (Optimized { name; enabled_at_init }) ->
    Format.pp_print_string ppf name;
    if enabled_at_init then Format.pp_print_string ppf " enabled_at_init"
  | Some (Behaves_like_direct_call { name; enabled_at_init }) ->
    Format.pp_print_string ppf name;
    Format.pp_print_string ppf " behaves_like_direct_call";
    if enabled_at_init then Format.pp_print_string ppf " enabled_at_init"
