(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Augmented version of [Shape.Uid.t] that can track variables forming parts
    of unboxed products. *)

type t = private
  | Uid of Shape.Uid.t
  | Proj of Shape.Uid.t * int

val internal_not_actually_unique : t

val uid : Shape.Uid.t -> t

val proj : Shape.Uid.t -> field:int -> t

val add_proj_debugging_uids_to_fields :
  duid:Lambda.debug_uid ->
  (Ident.t * Flambda_kind.With_subkind.t) list ->
  (Ident.t * t * Flambda_kind.With_subkind.t) list



include Identifiable.S with type t := t
