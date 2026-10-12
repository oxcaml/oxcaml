[@@@ocaml.warning "+a-40-41-42"]

(** Z3 support for CFG validators. Internal Datalog is the normal validation
    path, this module produces and runs Z3 code as a fallback if internal
    validation fails. *)

val run_z3 : string -> string

(** Runs a Z3 validation reproducer and reports whether it independently
    reproduces the internal validator failure. *)
val run_validation_fallback : string -> string

val fmt_fact : Format.formatter -> string -> string list -> unit

module Instruction_id_gen : sig
  type t

  val create : InstructionId.t list -> t

  val get_id : t -> key:InstructionId.t -> string

  val get_id_int : t -> key:InstructionId.t -> int

  val width : t -> int
end

module Reg_id_gen : sig
  type t

  val create : Reg.t list -> t

  val get_id : t -> key:Reg.t -> string

  val get_id_int : t -> key:Reg.t -> int

  val width : t -> int
end

val create_instruction_id_gen : Cfg.t -> Instruction_id_gen.t

val create_reg_id_gen : Cfg.t -> Reg_id_gen.t
