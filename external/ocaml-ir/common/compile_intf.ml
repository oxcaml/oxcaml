open Ocaml_ir_fiber
type 'a t =
  (module Ocaml_compiler_intf.S) ->
    filename:string ->
      compiler_flags:string list -> output_prefix:string -> 'a
module type S  = sig val compile : unit Result.t Fiber.t t end
