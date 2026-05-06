module type S = sig
  module Module : sig end

  module Module_gen : sig end [@@merlin.ppx_template_generated]

  val value : unit
  val value_gen : unit [@@merlin.ppx_template_generated]

  val func : unit -> unit
  val func_gen : unit -> unit [@@merlin.ppx_template_generated]

  type t_abstract
  type t_abstract_gen [@@merlin.ppx_template_generated]

  type t_concrete = int
  type t_concrete_gen = int [@@merlin.ppx_template_generated]
end
