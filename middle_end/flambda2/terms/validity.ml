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
  | Always_valid
  | Valid_after_some_branch
  | Control_flow_point

let print fmt = function
  | Always_valid -> Format.fprintf fmt "Always_valid"
  | Valid_after_some_branch -> Format.fprintf fmt "Valid_after_some_branch"
  | Control_flow_point -> Format.fprintf fmt "Control_flow_point"

let discr = function
  | Always_valid -> 0
  | Valid_after_some_branch -> 1
  | Control_flow_point -> 2

let compare t1 t2 = discr t1 - discr t2

let join t1 t2 =
  match t1, t2 with
  | Always_valid, Always_valid -> Always_valid
  | (Always_valid | Valid_after_some_branch), Valid_after_some_branch
  | Valid_after_some_branch, Always_valid ->
    Valid_after_some_branch
  | ( (Always_valid | Valid_after_some_branch | Control_flow_point),
      Control_flow_point )
  | Control_flow_point, (Always_valid | Valid_after_some_branch) ->
    Control_flow_point
