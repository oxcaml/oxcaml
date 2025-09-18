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

type t = Lambda.regalloc_attribute

let print ppf (t : t) =
  match t with
  | Default_regalloc -> Format.pp_print_string ppf "default"
  | Regalloc regalloc -> Clflags.Register_allocator.format ppf regalloc

let equal (t1 : t) (t2 : t) =
  match t1, t2 with
  | Default_regalloc, Default_regalloc -> true
  | Regalloc r1, Regalloc r2 -> Clflags.Register_allocator.equal r1 r2
  | (Default_regalloc | Regalloc _), _ -> false

let is_default (t : t) =
  match t with Default_regalloc -> true | Regalloc _ -> false

let from_lambda (attr : Lambda.regalloc_attribute) = attr

let to_lambda t : Lambda.regalloc_attribute = t
