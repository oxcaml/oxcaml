[@@@ocaml.warning "+a-40-41-42"]

(** Z3 support for CFG validators. Internal Datalog is the normal validation
    path, this module produces and runs Z3 code as a fallback if internal
    validation fails. *)

val run_z3 : string -> string

(** Runs a Z3 validation reproducer and reports whether it independently
    reproduces the internal validator failure. *)
val run_validation_fallback : string -> string

val fmt_fact : Format.formatter -> string -> string list -> unit
