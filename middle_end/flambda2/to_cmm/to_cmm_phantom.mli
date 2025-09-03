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

(** Translation of Flambda primitives to Cmm phantom expressions *)

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
