[@@@ocaml.warning "+a-40-41-42"]

type reg_class =
  | GPR
      (** 64-bit integer registers. The same name is used regardless of the
          width of the value stored in the register. *)
  | SIMD
      (** 128/256/512-bit SIMD registers. Different names are used when storing
          different sized values. *)

type[@ocamlformat "disable"] _ phys_reg_classed =
  | RAX : [> `GPR] phys_reg_classed | RBX : [> `GPR] phys_reg_classed
  | RDI : [> `GPR] phys_reg_classed | RSI : [> `GPR] phys_reg_classed
  | RDX : [> `GPR] phys_reg_classed | RCX : [> `GPR] phys_reg_classed
  | R8  : [> `GPR] phys_reg_classed | R9  : [> `GPR] phys_reg_classed
  | R12 : [> `GPR] phys_reg_classed | R13 : [> `GPR] phys_reg_classed
  | R10 : [> `GPR] phys_reg_classed | R11 : [> `GPR] phys_reg_classed
  | RBP : [> `GPR] phys_reg_classed

  | MM0  : [> `SIMD] phys_reg_classed | MM1  : [> `SIMD] phys_reg_classed
  | MM2  : [> `SIMD] phys_reg_classed | MM3  : [> `SIMD] phys_reg_classed
  | MM4  : [> `SIMD] phys_reg_classed | MM5  : [> `SIMD] phys_reg_classed
  | MM6  : [> `SIMD] phys_reg_classed | MM7  : [> `SIMD] phys_reg_classed
  | MM8  : [> `SIMD] phys_reg_classed | MM9  : [> `SIMD] phys_reg_classed
  | MM10 : [> `SIMD] phys_reg_classed | MM11 : [> `SIMD] phys_reg_classed
  | MM12 : [> `SIMD] phys_reg_classed | MM13 : [> `SIMD] phys_reg_classed
  | MM14 : [> `SIMD] phys_reg_classed | MM15 : [> `SIMD] phys_reg_classed

type phys_reg = P : _ phys_reg_classed -> phys_reg [@@unboxed]

val phys_gpr_regs_classed : [`GPR] phys_reg_classed array

val phys_gpr_regs : phys_reg array

val phys_simd_regs_classed : [`SIMD] phys_reg_classed array

val phys_simd_regs : phys_reg array

include
  Regs_utils.T with type Reg_class.t = reg_class with type Phys_reg.t = phys_reg

module Reg_class_tbl : Regs_utils.Reg_class_tbl with type reg_class = reg_class

module Save_simd_regs : sig
  type t =
    | Save_none
    | Save_xmm
    | Save_ymm
    | Save_zmm

  val all : t list

  val extension_name : t -> string option

  val symbol_suffix : t -> string
end

val gc_regs_offset :
  simd:Save_simd_regs.t -> Cmm.machtype_component -> Phys_reg.t -> int
