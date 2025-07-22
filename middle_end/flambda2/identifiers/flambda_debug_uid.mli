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

(** Augmented version of [Lambda.debug_uid] that can track variables forming parts
    of unboxed products. *)

type t = private
  | Uid of Lambda.debug_uid
  | Proj of
      { uid : Lambda.debug_uid;
        unboxed_field : int
      }

val none : t

val of_lambda_debug_uid : Lambda.debug_uid -> t

val of_lambda_debug_uid_proj : Lambda.debug_uid -> field:int -> t

val add_proj_debugging_uids_to_fields :
  duid:Lambda.debug_uid ->
  (Ident.t * Flambda_kind.With_subkind.t) list ->
  (Ident.t * t * Flambda_kind.With_subkind.t) list

include Identifiable.S with type t := t
