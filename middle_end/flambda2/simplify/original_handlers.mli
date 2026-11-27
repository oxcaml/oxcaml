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
  | Recursive of
      { invariant_params : Bound_parameters.t;
            (** The invariant params, including the lifted params (see
                [Lifted_free_var.place_lifted]). *)
        lifted_params : Bound_parameters.t;
            (** If the continuations have been lifted, their lifted params
                (which are also in [invariant_params]). *)
        lifted_free_vars : Lifted_free_var.t list;
            (** The free variables that the lifted params replace, in the same
                order. *)
        continuation_handlers : One_recursive_handler.t Continuation.Lmap.t;
        can_be_lifted : bool
      }
  | Non_recursive of Non_recursive_handler.t

val create_recursive :
  invariant_params:Bound_parameters.t ->
  lifted_params:Bound_parameters.t ->
  lifted_free_vars:Lifted_free_var.t list ->
  continuation_handlers:One_recursive_handler.t Continuation.Lmap.t ->
  can_be_lifted:bool ->
  t

val create_non_recursive : Non_recursive_handler.t -> t

val print : Format.formatter -> t -> unit

val bound_continuations : t -> Continuation.t list

val can_be_lifted : t -> bool

val add_params_to_lift :
  t -> Lifted_free_var.t list -> phantom_vars:Bound_var.t list -> t
