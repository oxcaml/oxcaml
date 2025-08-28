(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Static exception labels used in Lambda intermediate representation.

    Static exceptions provide a mechanism for local control flow transfer
    within functions. Unlike regular exceptions, the target handler is
    statically known and control flow cannot escape function boundaries.
*)

type t

include Identifiable.S with type t := t

val to_string : t -> string

val format : Format.formatter -> t -> unit

(** Special static label used for anticipated static raises (guards).
    This corresponds to the legacy hardcoded value 0. *)
val fail : t

(** A sequence for generating fresh static labels. *)
type sequence

(** Create a new sequence for generating static labels. *)
val make_sequence : unit -> sequence

(** Reset a sequence to start from label 0. *)
val reset : sequence -> unit

(** Get the next label that would be generated without consuming it. *)
val get : sequence -> t

(** Generate and consume the next label from the sequence. *)
val get_and_incr : sequence -> t

(** Convert a static label to its underlying integer representation.
    This function should only be used when interfacing with legacy code
    that expects raw integers. *)
val to_int_unsafe : t -> int

(** Convert an integer to a static label.
    This function should only be used when interfacing with legacy code
    that provides raw integers. *)
val of_int_unsafe : int -> t