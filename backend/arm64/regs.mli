[@@@ocaml.warning "+a-40-41-42"]

type reg_class =
  | Int64 (* general purpose registers *)
  | Float128 (* neon registers *)

type[@ocamlformat "disable"] phys_reg =
  | X0  | X1  | X2  | X3  | X4  | X5  | X6  | X7
  | X8  | X9  | X10 | X11 | X12 | X13 | X14 | X15
                    | X19 | X20 | X21 | X22 | X23
  | X24 | X25 | X26 | X27 | X28
  | X16 | X17

  | D0  | D1  | D2  | D3  | D4  | D5  | D6  | D7
  | D8  | D9  | D10 | D11 | D12 | D13 | D14 | D15
  | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23
  | D24 | D25 | D26 | D27 | D28 | D29 | D30 | D31

val phys_gpr_regs : phys_reg array

val phys_simd_regs : phys_reg array

include
  Regs_utils.T with type Reg_class.t = reg_class with type Phys_reg.t = phys_reg

module Reg_class_tbl : Regs_utils.Reg_class_tbl with type reg_class = reg_class
