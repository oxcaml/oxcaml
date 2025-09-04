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

(** Translation of Flambda expressions to Cmm. *)

val expr :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Flambda.Expr.t ->
  Cmm.expression
  * To_cmm_env.free_vars
  * To_cmm_env.Symbol_inits.t
  * To_cmm_result.t
