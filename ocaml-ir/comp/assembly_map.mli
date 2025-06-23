[@@@ocaml.text
  " Module that constructs mapping between a source file and the Assembly IR. "]
open Ocaml_ir_common
open Ocaml_ir_fiber
val get_mappings :
  output_prefix:string ->
    (Ir_map.Label.t * Location.Simple.t) list ->
      (string * Ir_map.raw) Fiber.t[@@ocaml.doc
                                     " Return mappings based on reading the .s assembly file. "]
val asm_file : string -> string
module For_testing :
sig
  val format_file :
    file:string ->
      content:string ->
        (string * (Ir_map.Line_col.t * Location.Simple.t) list)
end
