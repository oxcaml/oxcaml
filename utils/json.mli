(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Null
  | False
  | True
  | String of string
  | Number of string
  | Object of (string * t) list
  | Array of t list

val int : int -> t

val float : float -> t

val option : t option -> t

module Print_mode : sig
  type t =
    | Pretty  (** Output the json in a human-readable format. *)
    | Compact  (** Output the json without any whitespace. *)
end

val print : mode:Print_mode.t -> Format.formatter -> t -> unit

val to_string_pretty : t -> string
