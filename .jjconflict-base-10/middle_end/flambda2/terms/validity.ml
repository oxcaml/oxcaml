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

type t =
  | Can_move_anywhere
  | Can't_move_before_any_branch
  | Control_flow_point

let print fmt = function
  | Can_move_anywhere -> Format.fprintf fmt "Can_move_anywhere"
  | Can't_move_before_any_branch ->
    Format.fprintf fmt "Can't_move_before_any_branch"
  | Control_flow_point -> Format.fprintf fmt "Control_flow_point"

let discr = function
  | Can_move_anywhere -> 0
  | Can't_move_before_any_branch -> 1
  | Control_flow_point -> 2

let compare t1 t2 = discr t1 - discr t2

let join t1 t2 =
  match t1, t2 with
  | Can_move_anywhere, Can_move_anywhere -> Can_move_anywhere
  | ( (Can_move_anywhere | Can't_move_before_any_branch),
      Can't_move_before_any_branch )
  | Can't_move_before_any_branch, Can_move_anywhere ->
    Can't_move_before_any_branch
  | ( (Can_move_anywhere | Can't_move_before_any_branch | Control_flow_point),
      Control_flow_point )
  | Control_flow_point, (Can_move_anywhere | Can't_move_before_any_branch) ->
    Control_flow_point
