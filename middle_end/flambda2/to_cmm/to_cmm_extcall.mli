(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street Europe                *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Translation of external (C) calls to Cmm, including the implementation of
    the calling conventions for unboxed products (which are passed and returned
    as if they were values of C struct type). *)

val translate_external_call :
  To_cmm_env.t ->
  To_cmm_result.t ->
  free_vars:To_cmm_env.free_vars ->
  Apply_expr.t ->
  callee_simple:Simple.t option ->
  args:Cmm.expression list ->
  return_arity:[`Complex] Flambda_arity.t ->
  return_ty:Cmm_helpers.Extended_machtype.t ->
  Debuginfo.t ->
  needs_caml_c_call:bool ->
  is_c_builtin:bool ->
  effects:Effects.t ->
  coeffects:Coeffects.t ->
  Cmm.expression
  * To_cmm_env.free_vars
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t
