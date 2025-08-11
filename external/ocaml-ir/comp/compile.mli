open Ocaml_ir_common

val invoke_compiler
  :  filename:string
  -> compiler_flags:string list
  -> output_prefix:string
  -> languages:Language.Set.t
  -> setup_hooks:(unit -> unit)
  -> unit Result.Fiber.t

module V1 : sig
  module Compile : sig
    type t = unit

    val run
      :  filename:string
      -> compiler_flags:string list
      -> output_prefix:string
      -> t Result.Fiber.t
  end
end

module V2 : sig
  module Compile : sig
    type t = Ir_with_mappings.t Language.Map.t [@@deriving sexp]

    include sig
      [@@@ocaml.warning "-32"]

      include Sexplib0.Sexpable.S with type t := t
    end
    [@@ocaml.doc "@inline"] [@@merlin.hide]

    val run
      :  filename:string
      -> compiler_flags:string list
      -> languages:Language.Set.t
      -> output_prefix:string
      -> t Result.Fiber.t
  end
end
