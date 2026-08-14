(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Arbitrary-precision signed integers.

    Values are mathematical integers: arithmetic does not wrap at the
    bounds of the machine [int] type.  This is the executable counterpart
    of the mathematical-integer sort that specification checking reasons
    about.

    Interface rule: every operation exported here, other than the
    runtime-only conversions at the end, must be interpretable as an SMT
    [Int] operation.  An operation without an interpretation becomes an
    uninterpreted function, and specifications that use it silently stop
    being provable.  That is why there is no division, modulo or
    exponentiation.  Do not add a convenient function here without also
    giving it a solver interpretation.

    @since 5.4 *)

type t : immutable_data
(** The representation is canonical, so polymorphic equality agrees with
    [equal] and [Hashtbl.hash] is consistent with it.  Polymorphic
    ordering does not agree with mathematical order, because it compares
    the representation; use [compare], [lt], [le], [gt] or [ge]
    instead. *)

val zero : t
val one : t
val of_int : int -> t

val is_zero : t -> bool
val equal : t -> t -> bool

(** [compare] implements the mathematical order and returns [-1], [0]
    or [1]. *)
val compare : t -> t -> int

val lt : t -> t -> bool
val le : t -> t -> bool
val gt : t -> t -> bool
val ge : t -> t -> bool

val neg : t -> t
val abs : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

(** {1 Runtime-only conversions}

    These have no solver interpretation.  Do not use them in
    specifications. *)

(** [to_int_opt value] is [Some integer] exactly when [value] is
    representable as a machine [int]. *)
val to_int_opt : t -> int option

(** Canonical decimal notation: an optional leading minus followed by
    decimal digits with no redundant leading zeroes.  [to_string]
    produces it and [of_string] accepts exactly it, raising [Failure]
    on anything else. *)
val to_string : t -> string
val of_string : string -> t
