open Ocaml_ir_fiber

module type S = sig
  type t

  val includes : t -> string list Result.t Fiber.t
  val comp_working_dir : t -> string
  val infer : source_dir:string -> source_file_name:string -> t Result.t Fiber.t
  val find_compiler : source_dir:string -> string Result.t Fiber.t
  val get_cmi_path : modname:string -> t -> string
  val get_ppx_exe : exe:string -> filename:string -> t -> string
  val merlin_directives : t -> Ocaml_ir_merlin.Dot_protocol.directive list
  val compiler_path : t -> string
  val get_styler : filename:string -> t -> string

  module Compiler_spec : sig
    type t =
      | One_of of t list
      | All_of of t list
      | Lang of Language.t

    val always_true : t
  end

  val find_ocaml_ir_comp_path : compiler_spec:Compiler_spec.t -> string -> string Result.t
end
