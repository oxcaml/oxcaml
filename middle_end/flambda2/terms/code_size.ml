(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The representation of sizes and the arithmetic on them live here; the
   estimates for individual terms are computed by [Code_size_v1]. *)

type t = int

let zero = 0

let equal (a : t) (b : t) = a = b

let ( + ) (a : t) (b : t) : t = a + b

let ( - ) (a : t) (b : t) : t = a - b

let ( <= ) (a : t) (b : t) = a <= b

let alloc_size = Code_size_v1.alloc_size

let box_number = Code_size_v1.box_number

let block = Code_size_v1.block

let array = Code_size_v1.array

let prim = Code_size_v1.prim

let simple = Code_size_v1.simple

let static_consts = Code_size_v1.static_consts

let apply = Code_size_v1.apply

let apply_cont = Code_size_v1.apply_cont

let switch = Code_size_v1.switch

let invalid = Code_size_v1.invalid

let [@ocamlformat "disable"] print ppf t = Format.fprintf ppf "%d" t

let of_int t = t

let to_int t = t

let evaluate ~args:_ t = float_of_int t
