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

(** Simplification of primitives taking two arguments. *)

(** Constant-fold an integer binary primitive. *)
val fold_binary_int_primitive :
  machine_width:Target_system.Machine_width.t ->
  Flambda_primitive.binary_primitive ->
  Reg_width_const.t ->
  Reg_width_const.t ->
  Reg_width_const.t option

val simplify_binary_primitive :
  Downwards_acc.t ->
  Flambda_primitive.t ->
  Flambda_primitive.binary_primitive ->
  arg1:Simple.t ->
  arg1_ty:Flambda2_types.t ->
  arg2:Simple.t ->
  arg2_ty:Flambda2_types.t ->
  Debuginfo.t ->
  result_var:Bound_var.t ->
  Simplify_primitive_result.t
