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

let print ppf t =
  match t with
  | None -> Format.pp_print_string ppf "default"
  | Some regalloc -> Clflags.Register_allocator.format ppf regalloc

let equal t1 t2 = Option.equal Clflags.Register_allocator.equal t1 t2

let is_default t = Option.is_none t

let from_lambda (attr : Lambda.regalloc_attribute) = attr

let to_lambda t : Lambda.regalloc_attribute = t
