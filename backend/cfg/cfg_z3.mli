[@@@ocaml.warning "+a-40-41-42"]

val run_z3 : string -> string

val fmt_fact : Format.formatter -> string -> string list -> unit

module type Id_key = sig
  type t

  val compare : t -> t -> int

  val format : Format.formatter -> t -> unit

  module Tbl : Hashtbl.S with type key = t
end

module type Id_gen_S = sig
  type key

  type t

  val create : key list -> t

  val get_id : t -> key:key -> string

  val width : t -> int

  val length : t -> int

  val key_of_id_exn : t -> int -> key
end

module Make_id_gen (Key : Id_key) : Id_gen_S with type key = Key.t

module Label_id_gen : Id_gen_S with type key = Label.t

module Instruction_id_gen : Id_gen_S with type key = InstructionId.t

module Reg_id_gen : Id_gen_S with type key = Reg.t

val create_label_id_gen : Cfg.t -> Label_id_gen.t

val create_instruction_id_gen : Cfg.t -> Instruction_id_gen.t

val create_reg_id_gen : Cfg.t -> Reg_id_gen.t

val z3_graph_of_cfg :
  Format.formatter -> cfg:Cfg.t -> id_gen:Label_id_gen.t -> unit
