module type S = sig
  module Clflags : sig
    val dump_parsetree : bool ref
    val dump_cmm : bool ref
    val dump_selection : bool ref
    val dump_combine : bool ref
    val dump_cse : bool ref
    val dump_spill : bool ref
    val dump_split : bool ref
    val dump_live : bool ref
    val dump_reload : bool ref
    val dump_linear : bool ref
    val keep_asm_file : bool ref
  end

  type t

  val setup : compiler_flags:string list -> t
  val implementation : source_file:string -> output_prefix:string -> t -> unit
  val interface : source_file:string -> output_prefix:string -> t -> unit
  val exn_to_result : exn -> t -> 'a Result.t
end
