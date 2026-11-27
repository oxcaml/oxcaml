(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = private
  { cont : Continuation.t;
    params : Bound_parameters.t;
        (** The params of the continuation, including its lifted params (see
            [Lifted_free_var.place_lifted]). *)
    lifted_params : Bound_parameters.t;
        (** If the continuation has been lifted, its lifted params (which are
            also in [params]). *)
    lifted_free_vars : Lifted_free_var.t list;
        (** The free variables that the lifted params replace, in the same
            order. *)
    handler : Flambda.Expr.t;
    is_exn_handler : bool;
    is_cold : bool
  }

val create :
  cont:Continuation.t ->
  params:Bound_parameters.t ->
  lifted_params:Bound_parameters.t ->
  lifted_free_vars:Lifted_free_var.t list ->
  handler:Flambda.Expr.t ->
  is_exn_handler:bool ->
  is_cold:bool ->
  t

val print : Format.formatter -> t -> unit
