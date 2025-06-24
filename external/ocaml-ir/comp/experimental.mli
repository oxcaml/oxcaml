open Ocaml_ir_common
val shape :
  filename:string ->
    output_prefix:string ->
      compiler_flags:string list -> string Result.Fiber.t
