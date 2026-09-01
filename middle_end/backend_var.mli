(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Variables used in the backend, optionally equipped with "provenance"
    information, used for the emission of debugging information. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include module type of struct include Ident end

type backend_var = t

val name_for_debugger : t -> string
val unique_name_for_debugger : t -> string

module Provenance : sig
  type t

  val create
     : module_path:Path.t
    -> location:Debuginfo.t
    -> original_ident:Ident.t
    -> debug_uid:Flambda2_identifiers.Flambda_debug_uid.t
    -> is_parameter:Is_parameter.t
    -> t

  val module_path : t -> Path.t
  val location : t -> Debuginfo.t
  val original_ident : t -> Ident.t
  val debug_uid : t -> Flambda2_identifiers.Flambda_debug_uid.t
  val is_parameter : t -> Is_parameter.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

module With_provenance : sig
  (** Values of type [t] should be used for variables in binding position. *)
  type t

  val print : Format.formatter -> t -> unit

  val create : ?provenance:Provenance.t -> backend_var -> t

  val var : t -> backend_var
  val provenance : t -> Provenance.t option
  val is_parameter : t -> Is_parameter.t

  val name : t -> string

  val rename : t -> t
end
