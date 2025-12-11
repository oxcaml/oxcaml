(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Effects.t * Coeffects.t * Placement.t * Validity.t

let print fmt (eff, coeff, dup, validity) =
  Format.fprintf fmt "%a * %a * %a * %a" Effects.print eff Coeffects.print coeff
    Placement.print dup Validity.print validity

let compare (e1, c1, d1, v1) (e2, c2, d2, v2) =
  match Effects.compare e1 e2 with
  | 0 -> (
    match Coeffects.compare c1 c2 with
    | 0 -> (
      match Placement.compare d1 d2 with
      | 0 -> Validity.compare v1 v2
      | res -> res)
    | res -> res)
  | res -> res

(* projections *)
let validity ((_, _, _, v) : t) = v

(* Some useful constants *)
let pure : t = No_effects, No_coeffects, Strict, Valid_after_some_branch

let pure_can_be_duplicated : t =
  No_effects, No_coeffects, Delay, Valid_after_some_branch

let all : t = Arbitrary_effects, Has_coeffects, Strict, Control_flow_point

let read : t = No_effects, Has_coeffects, Strict, Valid_after_some_branch

(* Joining effects, coeffects and placement *)
let join (eff1, coeff1, dup1, val1) (eff2, coeff2, dup2, val2) =
  ( Effects.join eff1 eff2,
    Coeffects.join coeff1 coeff2,
    Placement.join dup1 dup2,
    Validity.join val1 val2 )
