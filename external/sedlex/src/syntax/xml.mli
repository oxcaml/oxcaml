(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)

(** Unicode classes from XML *)

open Sedlex_cset

val letter : t
val digit : t
val extender : t
val base_char : t
val ideographic : t
val combining_char : t
val blank : t
