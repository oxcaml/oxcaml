(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Name modes} *)

module Mode : sig
  type t =
    | Normal
    | Phantom

  val print : Format.formatter -> t -> unit
end

(** {1 Free variables with name modes} *)

type t

val print : Format.formatter -> t -> unit

val empty : t

val singleton : mode:Mode.t -> Backend_var.t -> t

val add : mode:Mode.t -> Backend_var.t -> t -> t

val union : t -> t -> t

val remove : Backend_var.t -> t -> t

val is_empty : t -> bool

val mode : Backend_var.t -> t -> Mode.t option

val mem : mode:Mode.t -> Backend_var.t -> t -> bool
