(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Translation of the defining expressions of Flambda phantom lets to Cmm
    phantom defining expressions.

    [None] results indicate that the value cannot be described to the debugger;
    the corresponding variable will be presented as optimised out. *)

val simple :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t ->
  Cmm.phantom_defining_expr option
  * To_cmm_env.t
  * To_cmm_result.t
  * To_cmm_env.free_vars

val prim :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Flambda_primitive.t ->
  Cmm.phantom_defining_expr option
  * To_cmm_env.t
  * To_cmm_result.t
  * To_cmm_env.free_vars
