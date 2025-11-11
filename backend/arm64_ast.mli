(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024--2025 Jane Street Group LLC                              *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Typed DSL for AArch64 assembly instructions. *)

[@@@ocaml.warning "+a-40-41-42"]

(** The same physical register has different names in the assembly encoding of
    instructions. The name determines the type of data the instruction operates
    on. *)

module Neon_reg_name : sig
  module Lane_index : sig
    (** Neon vector register lane indices. *)
    type t = private int

    val create : int -> t
  end
end

(* CR sspies: rename Reg.t, since it conflicts with the registers of the linear
   IR. *)
module Reg : sig
  type 'a t

  val reg_x : int -> [> `GP of [> `X]] t

  val reg_w : int -> [> `GP of [> `W]] t

  val reg_d : int -> [> `Neon of [> `Scalar of [> `D]]] t

  val reg_s : int -> [> `Neon of [> `Scalar of [> `S]]] t

  val reg_q : int -> [> `Neon of [> `Scalar of [> `Q]]] t

  val reg_v2d : int -> [> `Neon of [> `Vector of [> `V2D]]] t

  val reg_v16b : int -> [> `Neon of [> `Vector of [> `V16B]]] t

  val reg_v8b : int -> [> `Neon of [> `Vector of [> `V8B]]] t

  val reg_b : int -> [> `Neon of [> `Scalar of [> `B]]] t

  val sp : unit -> [> `GP of [> `SP]] t
end

module Symbol : sig
  type _ reloc_directive =
    | LOWER_TWELVE : [> `Twelve] reloc_directive
    | GOT_PAGE : [> `Twenty_one] reloc_directive
    | GOT_PAGE_OFF : [> `Twelve] reloc_directive
    | GOT : [> `Sixty_four] reloc_directive
    | GOT_LOWER_TWELVE : [> `Twelve] reloc_directive
    | PAGE : [> `Twenty_one] reloc_directive
    | PAGE_OFF : [> `Twelve] reloc_directive

  type 'width t

  (** Any OS-specific escaping/etc must have been applied first. *)
  val create : ?reloc:'w reloc_directive -> ?offset:int -> string -> 'w t

  val print : Format.formatter -> _ t -> unit
end

module Operand : sig
  type _ t

  module Shift : sig
    module Kind : sig
      type 'op t =
        | LSL : [`Lsl] t
        | ASR : [`Asr] t
        | LSR : [`Lsr] t
    end
  end
end

module Float_cond : sig
  type t =
    | EQ
    | GT
    | LE
    | GE
    | LT
    | NE
    | CC
    | CS
    | LS
    | HI
end

module Cond : sig
  type t =
    | EQ
    | NE
    | CS  (** alias HS *)
    | CC  (** alias LO *)
    | MI
    | PL
    | VS
    | VC
    | HI
    | LS
    | GE
    | LT
    | GT
    | LE
        (** AL and NV are not supported, because NV means AL, but has a
            different encoding.  Use unconditional branching instead. *)

  val of_float_cond : Float_cond.t -> t
end

module Rounding_mode : sig
  type t =
    | M
    | P
    | Z
    | X
    | N
end

module Memory_barrier : sig
  type t =
    | SY
        (** full system barrier operation; the default; use this for [dmb]/[dsb]
         without arguments *)
    | LD  (** waits only for loads to complete *)
    | ST  (** waits only for stores to complete *)
    | ISH  (** waits only for the inner sharable domain *)
    | ISHLD  (** waits only for loads and only for the inner sharable domain *)
    | ISHST  (** waits only for stores and only for the inner sharable domain *)
    | NSH  (** only out to the point of unification *)
    | NSHLD
        (** waits only for loads and only out to the point of unification *)
    | NSHST
        (** only for stores to complete and only out to the point of
            unification *)
    | OSH  (** only to the outer shareable domain *)
    | OSHLD  (** waits only for loads and only to the outer shareable domain *)
    | OSHST  (** waits only for stores and only to the outer shareable domain *)
end

module Instruction_name : sig
  type any_vector =
    [ `V8B
    | `V16B
    | `V4H
    | `V8H
    | `V2S
    | `V4S
    | `V1D
    | `V2D ]

  (* CR mshinwell: a few of these e.g. ABS_vector don't follow the below
     convention, we should fix the names. *)

  (** The intention is that none of these are aliases.  Expansions of
      instructions that are aliases are done by the "ins_*" functions below.

      Names such as "ADD_immediate" are intended to correspond to the titles
      such as "ADD (immediate)" in the ARM Architecture Reference Manual. *)
  type _ t =
    | ABS_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | ADDP_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | ADDS
        : ([< `Reg of [< `GP of [< `X | `XZR]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `SP]]] Operand.t
          * [< `Imm of [< `Twelve]] Operand.t
          * [< `Fixed_shift of [< `Lsl_by_twelve]] Operand.t option)
          t
    | ADDV
        : ([< `Reg of [< `Neon of [< `Scalar of [< `B]]]] Operand.t
          * [< `Reg of [< `Neon of [< `Vector of _]]] Operand.t)
          t
    | ADD_immediate
        : ([< `Reg of [< `GP of [< `X | `SP | `FP]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `SP | `FP]]] Operand.t
          * [< `Imm of [< `Twelve]] Operand.t
          * [< `Fixed_shift of [< `Lsl_by_twelve]] Operand.t option)
          t
    | ADD_shifted_register
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option)
          t
    | ADD_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | ADR
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Imm of [< `Twenty_one]] Operand.t)
          t
    (* CR mshinwell: try to improve constraint on ADRP second operand? *)
    | ADRP : ([< `Reg of [< `GP of [< `X]]] Operand.t * _ Operand.t) t
    | AND_immediate
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Bitmask] Operand.t)
          t
    | AND_shifted_register
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option)
          t
    | AND_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | ASRV
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | B : [< `Imm of _] Operand.t t
    | BL : [< `Imm of _] Operand.t t
    | BLR : [< `Reg of [< `GP of [< `X]]] Operand.t t
    | BR : [< `Reg of [< `GP of [< `X]]] Operand.t t
    | B_cond : Cond.t -> [< `Imm of _] Operand.t t
    | B_cond_float : Float_cond.t -> [< `Imm of _] Operand.t t
    | CBNZ
        : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Imm of _] Operand.t) t
    | CBZ
        : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Imm of _] Operand.t) t
    | CLZ
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | CM_register :
        Cond.t
        -> ([< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
           * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
           * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t)
           t
    | CM_zero :
        Cond.t
        -> ([< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
           * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t)
           t
    | CNT
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | CNT_vector
        : (([< `Reg of [< `Neon of [< `Vector of _]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | CSEL
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Cond] Operand.t)
          t
    | CSINC
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t
          * [< `Cond] Operand.t)
          t
    | CTZ
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | CVT_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | DMB : Memory_barrier.t -> unit t
    | DSB : Memory_barrier.t -> unit t
    | DUP
        : (Neon_reg_name.Lane_index.t
          * ([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | EOR_immediate
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Bitmask] Operand.t)
          t
    | EOR_shifted_register
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option)
          t
    | EOR_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | EXT
        : ([< `Reg of [< `Neon of [< `Vector of [`V8B | `V16B]]]] Operand.t
          * [< `Reg of [< `Neon of [< `Vector of [`V8B | `V16B]]]] Operand.t
          * [< `Reg of [< `Neon of [< `Vector of [`V8B | `V16B]]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t)
          t
    | FABS
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FADD
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FADDP_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FADD_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FCMP
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FCM_register :
        Float_cond.t
        -> ([< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
           * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
           * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t)
           t
    | FCM_zero :
        Float_cond.t
        -> ([< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
           * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t)
           t
    | FCSEL
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t
          * [< `Cond] Operand.t)
          t
    | FCVT
        : ([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t
          * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t)
          t
    | FCVTL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FCVTNS
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t)
          t
    | FCVTNS_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FCVTN_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FCVTZS
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t)
          t
    | FCVTZS_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FDIV
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FDIV_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMADD
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMAX
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMAX_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMIN
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMIN_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMOV_general_or_register
      (* CR mshinwell: ideally this would enforce that only one of the two
         operands is a GP (resp. Neon) reg. *)
        : ([< `Reg of
              [< `Neon of [< `Scalar of [< `S | `D]] | `GP of [< `X | `W]] ]
           Operand.t
          * [< `Reg of
               [< `Neon of [< `Scalar of [< `S | `D]]
               | `GP of [< `X | `XZR | `W | `WZR] ] ]
            Operand.t)
          t
    | FMOV_scalar_immediate
        : ([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t
          * [< `Imm of [< `Sixty_four]] Operand.t)
          t
    | FMOV_vector_immediate
        : ([< `Reg of [< `Neon of [< `Vector of _]]] Operand.t
          * [< `Imm of [< `Sixty_four]] Operand.t)
          t
    | FMSUB
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMUL
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FMUL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FNEG
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FNEG_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FNMADD
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FNMSUB
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FNMUL
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FRECPE_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FRINT :
        Rounding_mode.t
        -> (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r)
            Operand.t
           * 'r Operand.t)
           t
    | FRINT_vector :
        Rounding_mode.t
        -> (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
           * 'r Operand.t)
           t
    | FRSQRTE_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FSQRT
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FSQRT_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | FSUB
        : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | FSUB_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | INS
        : (Neon_reg_name.Lane_index.t
          * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
          * ([< `Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]]
             as
             'rs)
            Operand.t)
          t
    | INS_V
        : (Neon_reg_name.Lane_index.t
          * Neon_reg_name.Lane_index.t
          * ([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | LDAR
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t * [< `Mem] Operand.t) t
    | LDP
        : ([< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t
          * [< `Mem] Operand.t)
          t
    | LDR
        : ([< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t
          * [< `Mem] Operand.t)
          t
    | LDRB : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t
    | LDRH : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t
    | LDRSB : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Mem] Operand.t) t
    | LDRSH : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Mem] Operand.t) t
    | LDRSW : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Mem] Operand.t) t
    | LDR_simd_and_fp
        : ([< `Reg of [< `Neon of [< `Scalar of [< `D | `S | `Q]]]] Operand.t
          * [< `Mem] Operand.t)
          t
    | LSLV
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | LSRV
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | MADD
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t)
          t
    | MOV
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `W | `XZR | `WZR]] | `Imm of _]
            Operand.t)
          t
    | MOVI
        : ([< `Reg of [< `Neon of _]] Operand.t
          * [< `Imm of [< `Twelve]] Operand.t)
          t
    | MOVK
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Imm of [< `Sixty_four]] Operand.t
          * [< `Shift of [< `Lsl] * [`Six]] Operand.t)
          t
    | MOVN
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Imm of [< `Twelve | `Sixty_four]] Operand.t
          * [< `Shift of [< `Lsl] * [`Six]] Operand.t option)
          t
    | MOVZ
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Imm of [< `Sixty_four]] Operand.t
          * [< `Shift of [< `Lsl] * [`Six]] Operand.t option)
          t
    | MOV_vector
        : (([< `Reg of [< `Neon of [< `Vector of _]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | MSUB
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
        (** LDR and STR do not quite follow the ARMARM nomenclature: the
            details of the particular addressing mode are hidden within a
            [ `Mem] Operand.t. *)
    | MULL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | MUL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | MVN_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | NEG_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | NOP : unit t
    | ORR_immediate
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t
          * [< `Bitmask] Operand.t)
          t
    | ORR_shifted_register
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option)
          t
    | ORR_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | RBIT
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | RET : unit t
    | REV
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `W]]] Operand.t)
          t
    | REV16
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `W]]] Operand.t)
          t
      (* CR mshinwell: we should try to constrain the width/relocs on the
         immediate for B and BL *)
    | SBFM
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t
          * [< `Imm of [< `Six]] Operand.t)
          t
    | SCVTF
        : ([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | SCVTF_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | SDIV
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | SHL
        : ([< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
          * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t)
          t
    | SMAX_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SMIN_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SMOV
        : (Neon_reg_name.Lane_index.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of
               [< `Neon of
                  [< `Vector of [`V8B | `V16B | `V4H | `V8H | `V2S | `V4S]] ] ]
            Operand.t)
          t
    | SMULH
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | SMULL2_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SMULL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SQADD_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SQSUB_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SQXTN
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | SQXTN2
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | SSHL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SSHR
        : ([< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
          * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t)
          t
    | STP
        : ([< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t
          * [< `Mem] Operand.t)
          t
    | STR
        : ([< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t
          * [< `Mem] Operand.t)
          t
    | STRB : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t
    | STRH : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t
    | STR_simd_and_fp
        : ([< `Reg of [< `Neon of [< `Scalar of [< `D | `S | `Q]]]] Operand.t
          * [< `Mem] Operand.t)
          t
    | SUBS_immediate
        : ([< `Reg of [< `GP of [< `W | `WZR | `X | `XZR]]] Operand.t
          * [< `Reg of [< `GP of [< `W | `X | `SP]]] Operand.t
          * [< `Imm of [< `Twelve]] Operand.t
          * [< `Fixed_shift of [< `Lsl_by_twelve]] Operand.t option)
          t
    | SUBS_shifted_register
        : ([< `Reg of [< `GP of [< `X | `XZR]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `SP]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option)
          t
    | SUB_immediate
        : ([< `Reg of [< `GP of [< `X | `SP]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `SP]]] Operand.t
          * [< `Imm of [< `Twelve]] Operand.t
          * [< `Fixed_shift of [< `Lsl_by_twelve]] Operand.t option)
          t
    | SUB_shifted_register
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option)
          t
    | SUB_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | SXTL
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | TBNZ
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t
          * [< `Imm of _] Operand.t)
          t
    | TBZ
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t
          * [< `Imm of _] Operand.t)
          t
    | TST : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Bitmask] Operand.t) t
    | UADDLP_vector
        : ([< `Reg of
              [< `Neon of
                 [< `Vector of [`V4H | `V8H | `V2S | `V4S | `V1D | `V2D]] ] ]
           Operand.t
          * [< `Reg of
               [< `Neon of
                  [< `Vector of [`V8B | `V16B | `V4H | `V8H | `V2S | `V4S]] ] ]
            Operand.t)
          t
    | UBFM
        : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Reg of [< `GP of [< `X | `W]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t
          * [< `Imm of [< `Six]] Operand.t)
          t
    | UMAX_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | UMIN_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | UMOV
        : (Neon_reg_name.Lane_index.t
          * ([< `Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]]
             as
             'rd)
            Operand.t
          * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t)
          t
    | UMULH
        : ([< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t
          * [< `Reg of [< `GP of [< `X]]] Operand.t)
          t
    | UMULL2_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | UMULL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | UQADD_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | UQSUB_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | UQXTN
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | UQXTN2
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | USHL_vector
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | USHR
        : ([< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
          * [< `Reg of [< `Neon of [< `Vector of any_vector]]] Operand.t
          * [< `Imm of [< `Six]] Operand.t)
          t
    | UXTL
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | XTN
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | XTN2
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t)
          t
    | YIELD : unit t
    | ZIP1
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
    | ZIP2
        : (([< `Reg of [< `Neon of [< `Vector of any_vector]]] as 'r) Operand.t
          * 'r Operand.t
          * 'r Operand.t)
          t
end

module DSL : sig
  val reg_op : 'a Reg.t -> [> `Reg of 'a] Operand.t

  val imm : int -> [> `Imm of [> `Twelve]] Operand.t

  val imm_six : int -> [> `Imm of [> `Six]] Operand.t

  val imm_float : float -> [> `Imm of [> `Sixty_four]] Operand.t

  val imm_nativeint : nativeint -> [> `Imm of [> `Sixty_four]] Operand.t

  val bitmask : nativeint -> [> `Bitmask] Operand.t

  val symbol : 'w Symbol.t -> [> `Imm of 'w] Operand.t

  val shift :
    kind:'op Operand.Shift.Kind.t ->
    amount:int ->
    [> `Shift of 'op * [`Six]] Operand.t

  val mem : base:[< `GP of [< `X | `SP]] Reg.t -> [> `Mem] Operand.t

  val mem_offset :
    base:[< `GP of [< `X | `SP]] Reg.t -> offset:int -> [> `Mem] Operand.t

  val mem_symbol :
    base:[< `GP of [< `X | `SP]] Reg.t ->
    symbol:'w Symbol.t ->
    [> `Mem] Operand.t

  val mem_pre :
    base:[< `GP of [< `X | `SP]] Reg.t -> offset:int -> [> `Mem] Operand.t

  val mem_post :
    base:[< `GP of [< `X | `SP]] Reg.t -> offset:int -> [> `Mem] Operand.t

  val cond : Cond.t -> [> `Cond] Operand.t

  val float_cond : Float_cond.t -> [> `Float_cond] Operand.t

  (** The functions below are shorthands for composing [reg_op] and the
      respective function from [Reg] *)
  val reg_v2d :
    int -> [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Operand.t

  val reg_v2s :
    int -> [> `Reg of [> `Neon of [> `Vector of [> `V2S]]]] Operand.t

  val reg_v4s :
    int -> [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Operand.t

  val reg_v8b :
    int -> [> `Reg of [> `Neon of [> `Vector of [> `V8B]]]] Operand.t

  val reg_v16b :
    int -> [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Operand.t

  val reg_v8h :
    int -> [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Operand.t

  val reg_v4h :
    int -> [> `Reg of [> `Neon of [> `Vector of [> `V4H]]]] Operand.t

  val reg_b : int -> [> `Reg of [> `Neon of [> `Scalar of [> `B]]]] Operand.t

  val reg_s : int -> [> `Reg of [> `Neon of [> `Scalar of [> `S]]]] Operand.t

  val reg_d : int -> [> `Reg of [> `Neon of [> `Scalar of [> `D]]]] Operand.t

  val reg_q : int -> [> `Reg of [> `Neon of [> `Scalar of [> `Q]]]] Operand.t

  val reg_x : int -> [> `Reg of [> `GP of [> `X]]] Operand.t

  val reg_w : int -> [> `Reg of [> `GP of [> `W]]] Operand.t

  val sp : unit -> [> `Reg of [> `GP of [> `SP]]] Operand.t

  val lr : unit -> [> `Reg of [> `GP of [> `LR]]] Operand.t

  val fp : unit -> [> `Reg of [> `GP of [> `FP]]] Operand.t

  val xzr : unit -> [> `Reg of [> `GP of [> `XZR]]] Operand.t

  val wzr : unit -> [> `Reg of [> `GP of [> `WZR]]] Operand.t

  val reglane_v4s :
    int ->
    lane:int ->
    [> `Reg of [> `Neon of [> `Lane of [> `Vector of [> `V4S]]]]] Operand.t

  val reglane_v2d :
    int ->
    lane:int ->
    [> `Reg of [> `Neon of [> `Lane of [> `Vector of [> `V2D]]]]] Operand.t

  val reglane_b :
    int ->
    lane:int ->
    [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `B]]]]] Operand.t

  val reglane_h :
    int ->
    lane:int ->
    [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `H]]]]] Operand.t

  val reglane_s :
    int ->
    lane:int ->
    [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `S]]]]] Operand.t

  val reglane_d :
    int ->
    lane:int ->
    [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `D]]]]] Operand.t

  val print_ins : 'operands Instruction_name.t -> 'operands -> string

  module Acc : sig
    val set_emit_string : emit_string:(string -> unit) -> unit

    (** Passes the instruction to the function provided to [set_emit_string].
        (Can't directly reference [Emitaux] due to a circular dependency.) *)
    val ins : 'operands Instruction_name.t -> 'operands -> unit

    (** Expansion of instructions that are aliases *)

    val ins_mul :
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      unit

    val ins_lsl_immediate :
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      shift_in_bits:int ->
      unit

    val ins_lsr_immediate :
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      shift_in_bits:int ->
      unit

    val ins_asr_immediate :
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      shift_in_bits:int ->
      unit

    val ins_uxtb :
      [< `Reg of [< `GP of [< `W]]] Operand.t ->
      [< `Reg of [< `GP of [< `W]]] Operand.t ->
      unit

    val ins_uxth :
      [< `Reg of [< `GP of [< `W]]] Operand.t ->
      [< `Reg of [< `GP of [< `W]]] Operand.t ->
      unit

    val ins_cmp :
      [< `Reg of [< `GP of [< `W | `X | `SP]]] Operand.t ->
      [< `Imm of [< `Twelve]] Operand.t ->
      [< `Fixed_shift of [< `Lsl_by_twelve]] Operand.t option ->
      unit

    val ins_cmp_reg :
      [< `Reg of [< `GP of [< `X | `SP]]] Operand.t ->
      [< `Reg of [< `GP of [< `X]]] Operand.t ->
      [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option ->
      unit

    val ins_cmn :
      [< `Reg of [< `GP of [< `X | `SP]]] Operand.t ->
      [< `Imm of [< `Twelve]] Operand.t ->
      [< `Fixed_shift of [< `Lsl_by_twelve]] Operand.t option ->
      unit

    val ins_cset : [< `Reg of [< `GP of [< `X]]] Operand.t -> Cond.t -> unit

    val ins_mov_from_sp :
      dst:[< `Reg of [< `GP of [< `X | `FP]]] Operand.t -> unit

    val ins_mov_to_sp :
      src:[< `Reg of [< `GP of [< `X | `FP]]] Operand.t -> unit
  end
end
