open Ocaml_ir_common
module V1 :
sig
  val run :
    filename:string ->
      output_prefix:string ->
        compiler_flags:string list ->
          as_org_mode:bool -> Inlining_report.t Result.Fiber.t
end
