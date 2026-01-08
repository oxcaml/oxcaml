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

type any_vector =
  [ `V8B
  | `V16B
  | `V4H
  | `V8H
  | `V2S
  | `V4S
  | `V1D
  | `V2D ]

type any_width =
  [ `B
  | `H
  | `S
  | `D ]

module Neon_reg_name : sig
  module Vector : sig
    type (_, _) t = private
      | V8B : ([`V8B], [`B]) t
      | V16B : ([`V16B], [`B]) t
      | V4H : ([`V4H], [`H]) t
      | V8H : ([`V8H], [`H]) t
      | V2S : ([`V2S], [`S]) t
      | V4S : ([`V4S], [`S]) t
      | V1D : ([`V1D], [`D]) t
      | V2D : ([`V2D], [`D]) t
  end

  module Scalar : sig
    type _ t = private
      | B : [`B] t
      | H : [`H] t
      | S : [`S] t
      | D : [`D] t
      | Q : [`Q] t
  end

  module Lane_index : sig
    (** Neon vector register lane indices. *)
    type t

    type lane_index = t

    val create : int -> t

    val to_int : t -> int

    module Src_and_dest : sig
      type t

      val create : src:lane_index -> dest:lane_index -> t

      val dest_index : t -> lane_index

      val src_index : t -> lane_index
    end
  end

  module Lane : sig
    type 'a r = private
      | V : ('v, 's) Vector.t -> [`Vector of 'v * 's] r
      | S : 's Scalar.t -> [`Scalar of 's] r

    type 'a t = private
      { r : 'a r;
        lane : Lane_index.t
      }
  end

  type _ t = private
    | Vector : ('v, 's) Vector.t -> [`Vector of 'v * 's] t
    | Scalar : 's Scalar.t -> [`Scalar of 's] t
    | Lane : 'l Lane.t -> [`Lane of 'l] t
end

module GP_reg_name : sig
  type _ t = private
    | W : [`W] t
    | X : [`X] t
    | WZR : [`WZR] t
    | XZR : [`XZR] t
    | WSP : [`WSP] t
    | SP : [`SP] t
    | LR : [`LR] t
    | FP : [`FP] t
end

module Reg_name : sig
  type _ t = private
    | GP : 'a GP_reg_name.t -> [`GP of 'a] t
    | Neon : 'a Neon_reg_name.t -> [`Neon of 'a] t
end

(* CR sspies: rename Reg.t, since it conflicts with the registers of the linear
   IR. *)
module Reg : sig
  type 'a t = private
    { reg_name : 'a Reg_name.t;
      index : int
    }

  val reg_x : int -> [`GP of [`X]] t

  val reg_w : int -> [`GP of [`W]] t

  val reg_d : int -> [`Neon of [`Scalar of [`D]]] t

  val reg_s : int -> [`Neon of [`Scalar of [`S]]] t

  val reg_q : int -> [`Neon of [`Scalar of [`Q]]] t

  val reg_v2d : int -> [`Neon of [`Vector of [`V2D] * [`D]]] t

  val reg_v16b : int -> [`Neon of [`Vector of [`V16B] * [`B]]] t

  val reg_v8b : int -> [`Neon of [`Vector of [`V8B] * [`B]]] t

  val reg_b : int -> [`Neon of [`Scalar of [`B]]] t

  val sp : [`GP of [`SP]] t

  val xzr : [`GP of [`XZR]] t

  val wzr : [`GP of [`WZR]] t

  val name : _ t -> string

  val gp_encoding : [`GP of _] t -> int

  val gp_sf : [`GP of _] t -> int
end

(** Symbol references and relocations.

    [Same_section_and_unit] is for references that the assembler can resolve
    directly, such as PC-relative branches within a function. These use 19-bit
    signed offsets giving +/-1MB range.

    [Needs_reloc] is for references requiring linker involvement:
    - ADRP instructions need PAGE relocations because they compute page-aligned
      addresses that the assembler can't resolve until link time.
    - ADR instructions for local labels within the same section and
      compilation unit use Same_section_and_unit (no relocation needed).
    - Calls to runtime functions (e.g. caml_call_gc) in other compilation
      units need CALL26/JUMP26 relocations.
    - GOT-relative references for position-independent code. *)
module Symbol : sig
  type _ reloc_directive =
    | LOWER_TWELVE : [`Twelve] reloc_directive
    | GOT_PAGE : [`Twenty_one] reloc_directive
    | GOT_PAGE_OFF : [`Twelve] reloc_directive
    | GOT_LOWER_TWELVE : [`Twelve] reloc_directive
    | PAGE : [`Twenty_one] reloc_directive
    | PAGE_OFF : [`Twelve] reloc_directive
    | CALL26 : [`Twenty_six] reloc_directive
    | JUMP26 : [`Twenty_six] reloc_directive

  type 'w same_unit_or_reloc =
    | Same_section_and_unit : [`Nineteen] same_unit_or_reloc
    | Needs_reloc : 'w reloc_directive -> 'w same_unit_or_reloc

  type target =
    | Label of Asm_targets.Asm_label.t
    | Symbol of Asm_targets.Asm_symbol.t

  type 'w t = private
    { target : target;
      offset : int;
      reloc : 'w same_unit_or_reloc
    }

  val create_label :
    'w same_unit_or_reloc -> ?offset:int -> Asm_targets.Asm_label.t -> 'w t

  val create_symbol :
    'w same_unit_or_reloc -> ?offset:int -> Asm_targets.Asm_symbol.t -> 'w t

  (** Returns true if the target is a label. *)
  val is_label : _ t -> bool

  val print_target : Format.formatter -> target -> unit

  val print : Format.formatter -> _ t -> unit
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

(** Condition codes for AdvSIMD integer compare instructions.
    These map to CMEQ, CMGE, CMGT, CMLE, CMLT, CMHI, CMHS instructions.
    Unlike [Cond.t], this excludes conditions that don't have corresponding
    SIMD compare instructions (NE, CS, CC, MI, PL, VS, VC, LS). *)
module Simd_int_cmp : sig
  type t =
    | EQ  (** equal *)
    | GE  (** greater or equal, signed *)
    | GT  (** greater than, signed *)
    | LE  (** less or equal, signed *)
    | LT  (** less than, signed *)
    | HI  (** higher, unsigned *)
    | HS  (** higher or same, unsigned *)

  val to_string : t -> string
end

(** Immediates used in instruction operands. *)
module Immediate : sig
  type 'width t = private
    | Six : int -> [`Six] t
    | Twelve : int -> [`Twelve] t
    | Sixteen_unsigned : int -> [`Sixteen_unsigned] t
    | Sym : 'w Symbol.t -> [`Sym of 'w] t
    | Float : float -> [`Sixty_four] t
    | Nativeint : nativeint -> [`Sixty_four] t
end

(** Offsets used in addressing modes. *)
module Addressing_offset : sig
  type 'width t = private
    | Seven_signed_scaled : int -> [`Seven_signed] t
    | Nine_signed_unscaled : int -> [`Nine_signed_unscaled] t
    | Twelve_unsigned_scaled : int -> [`Twelve_unsigned_scaled] t
end

module Addressing_mode : sig
  module Offset : sig
    type _ t = private
      | Imm : 'w Addressing_offset.t -> 'w t
      | Symbol_with_reloc : [`Twelve] Symbol.t -> [`Twelve_unsigned_scaled] t
  end

  type single =
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]

  type _ t = private
    | Reg : [`GP of [< `X | `SP]] Reg.t -> [> `Base_reg] t
    | Offset_imm :
        [`GP of [< `X | `SP]] Reg.t
        * [`Twelve_unsigned_scaled] Addressing_offset.t
        -> [> `Offset_imm] t
    | Offset_unscaled :
        [`GP of [< `X | `SP]] Reg.t
        * [`Nine_signed_unscaled] Addressing_offset.t
        -> [> `Offset_unscaled] t
    | Offset_sym :
        [`GP of [< `X | `SP]] Reg.t * [`Twelve] Symbol.t
        -> [> `Offset_sym] t
    | Literal :
        [`GP of [< `X | `SP]] Reg.t * [`Nineteen] Symbol.t
        -> [> `Literal] t
    | Pre :
        [`GP of [< `X | `SP]] Reg.t * [`Nine_signed_unscaled] Offset.t
        -> [> `Pre] t
    | Post :
        [`GP of [< `X | `SP]] Reg.t * [`Nine_signed_unscaled] Offset.t
        -> [> `Post] t
    | Offset_pair :
        [`GP of [< `X | `SP]] Reg.t * [`Seven_signed] Offset.t
        -> [> `Offset_pair] t
    | Pre_pair :
        [`GP of [< `X | `SP]] Reg.t * [`Seven_signed] Offset.t
        -> [> `Pre_pair] t
    | Post_pair :
        [`GP of [< `X | `SP]] Reg.t * [`Seven_signed] Offset.t
        -> [> `Post_pair] t
end

module Operand : sig
  module Bitmask : sig
    type t = private nativeint

    val decode_n_immr_imms : t -> int * int * int
  end

  module Shift : sig
    module Kind : sig
      type 'op t =
        | LSL : [`Lsl] t
        | ASR : [`Asr] t
        | LSR : [`Lsr] t
    end

    type ('op, 'amount) t = private
      { kind : 'op Kind.t;
        amount : 'amount Immediate.t
      }
  end

  type _ t = private
    | Imm : 'w Immediate.t -> [`Imm of 'w] t
    | Reg : 'a Reg.t -> [`Reg of 'a] t
    | Lsl_by_twelve : [`Fixed_shift of [`Lsl_by_twelve]] t
    | Shift : ('op, 'amount) Shift.t -> [`Shift of 'op * 'amount] t
    | Cond : Cond.t -> [`Cond] t
    | Float_cond : Float_cond.t -> [`Float_cond] t
    | Mem : 'm Addressing_mode.t -> [`Mem of 'm] t
    | Bitmask : Bitmask.t -> [`Bitmask] t
    | Optional : 'a t option -> [`Optional of 'a option] t
    | Unit : unit t
end

module Rounding_mode : sig
  type t =
    | A  (** ties Away *)
    | I  (** current mode - use FPCR rounding mode *)
    | M  (** towards Minus infinity *)
    | N  (** to Nearest, ties to even *)
    | P  (** towards Plus infinity *)
    | X  (** to nearest, eXact - may signal inexact *)
    | Z  (** towards Zero *)
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

type singleton = [`Singleton]

type pair = [`Pair]

type triple = [`Triple]

type quad = [`Quad]

type (_, _) many =
  | Singleton : 'a Operand.t -> (singleton, 'a) many
  | Pair : 'a Operand.t * 'b Operand.t -> (pair, 'a * 'b) many
  | Triple :
      'a Operand.t * 'b Operand.t * 'c Operand.t
      -> (triple, 'a * 'b * 'c) many
  | Quad :
      'a Operand.t * 'b Operand.t * 'c Operand.t * 'd Operand.t
      -> (quad, 'a * 'b * 'c * 'd) many

(** Witness types for widening vector operations.
    Widening: element count stays the same, element size doubles.
    8B->8H, 4H->4S, 2S->2D *)
module Widen : sig
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8H_V8B : ([`V8B], [`B], [`V8H], [`H]) t
    | V4S_V4H : ([`V4H], [`H], [`V4S], [`S]) t
    | V2D_V2S : ([`V2S], [`S], [`V2D], [`D]) t
end

(** Witness types for narrowing vector operations.
    Narrowing: element count stays the same, element size halves.
    8H->8B, 4S->4H, 2D->2S *)
module Narrow : sig
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8B_V8H : ([`V8H], [`H], [`V8B], [`B]) t
    | V4H_V4S : ([`V4S], [`S], [`V4H], [`H]) t
    | V2S_V2D : ([`V2D], [`D], [`V2S], [`S]) t
end

(** Witness types for narrowing to upper half (XTN2, etc).
    16B->8H, 8H->4S, 4S->2D *)
module Narrow2 : sig
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V16B_V8H : ([`V8H], [`H], [`V16B], [`B]) t
    | V8H_V4S : ([`V4S], [`S], [`V8H], [`H]) t
    | V4S_V2D : ([`V2D], [`D], [`V4S], [`S]) t
end

(** Witness types for widening multiply (SMULL, UMULL).
    Sources are narrow, dest is wide.
    8B,8B->8H, 4H,4H->4S, 2S,2S->2D *)
module Widen_mul : sig
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8H_V8B : ([`V8B], [`B], [`V8H], [`H]) t
    | V4S_V4H : ([`V4H], [`H], [`V4S], [`S]) t
    | V2D_V2S : ([`V2S], [`S], [`V2D], [`D]) t
end

(** Witness types for widening multiply upper half (SMULL2, UMULL2).
    Sources are 128-bit, dest is wide.
    16B,16B->8H, 8H,8H->4S, 4S,4S->2D *)
module Widen_mul2 : sig
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8H_V16B : ([`V16B], [`B], [`V8H], [`H]) t
    | V4S_V8H : ([`V8H], [`H], [`V4S], [`S]) t
    | V2D_V4S : ([`V4S], [`S], [`V2D], [`D]) t
end

(** Witness types for LDP/STP register width.
    Both registers in a pair must be the same width class. *)
module LDP_STP_width : sig
  type (_, _) t =
    | X : ([< `X | `LR], [< `X | `LR]) t
    | W : ([< `W], [< `W]) t
end

module Instruction_name : sig
  (* CR mshinwell: a few of these e.g. ABS_vector don't follow the below
     convention, we should fix the names. *)

  (** The intention is that none of these are aliases.  Expansions of
      instructions that are aliases are done by the "ins_*" functions below.

      Names such as "ADD_immediate" are intended to correspond to the titles
      such as "ADD (immediate)" in the ARM Architecture Reference Manual. *)

  type (_, _) t =
    | ABS_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | ADDP_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | ADDS
        : ( quad,
            [`Reg of [`GP of [< `X | `XZR]]]
            * [`Reg of [`GP of [< `X | `SP]]]
            * [`Imm of [< `Twelve]]
            * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
          t
    | ADDV
        : ( pair,
            [`Reg of [`Neon of [< `Scalar of ([< `B | `H | `S] as 'w)]]]
            * [ `Reg of
                [ `Neon of
                  [`Vector of [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * 'w]
                ] ] )
          t
    | ADD_immediate
        : ( quad,
            [`Reg of [`GP of [< `X | `SP | `FP]]]
            * [`Reg of [`GP of [< `X | `SP | `FP]]]
            * [`Imm of [< `Twelve | `Sym of [`Twelve]]]
            * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
          t
    | ADD_shifted_register
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]]
            * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
          )
          t
    | ADD_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | ADR : (pair, [`Reg of [`GP of [`X]]] * [`Imm of [`Sym of [`Nineteen]]]) t
    | ADRP
        : (pair, [`Reg of [`GP of [`X]]] * [`Imm of [`Sym of [`Twenty_one]]]) t
    | AND_immediate
        : ( triple,
            [`Reg of [`GP of [< `X]]] * [`Reg of [`GP of [< `X]]] * [< `Bitmask]
          )
          t
    | AND_shifted_register
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]]
            * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
          )
          t
    | AND_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | ASRV
        : ( triple,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]] )
          t
    | B : (singleton, [`Imm of [`Sym of _]]) t
    | BL : (singleton, [`Imm of [`Sym of _]]) t
    | BLR : (singleton, [`Reg of [`GP of [`X]]]) t
    | BR : (singleton, [`Reg of [`GP of [`X]]]) t
    | B_cond : Cond.t -> (singleton, [`Imm of [`Sym of _]]) t
    | B_cond_float : Float_cond.t -> (singleton, [`Imm of [`Sym of _]]) t
    | CBNZ : (pair, [`Reg of [`GP of [< `X | `W]]] * [`Imm of [`Sym of _]]) t
    | CBZ : (pair, [`Reg of [`GP of [< `X | `W]]] * [`Imm of [`Sym of _]]) t
    | CLZ
        : ( pair,
            [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]] )
          t
    | CM_register :
        Simd_int_cmp.t
        -> ( triple,
             [ `Reg of
               [ `Neon of
                 [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ]
             ]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
           t
    | CM_zero :
        Simd_int_cmp.t
        -> ( pair,
             [ `Reg of
               [ `Neon of
                 [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ]
             ]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
           t
    | CNT
        : ( pair,
            [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]] )
          t
    | CNT_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | CSEL
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]]
            * [`Cond] )
          t
    | CSINC
        : ( quad,
            [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
            * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
            * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
            * [`Cond] )
          t
    | CTZ
        : ( pair,
            [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]] )
          t
    | DMB : Memory_barrier.t -> (singleton, unit) t
    | DSB : Memory_barrier.t -> (singleton, unit) t
    | DUP :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [ `Reg of
               [ `Neon of
                 [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ]
             ]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
           t
    | EOR_immediate
        : ( triple,
            [`Reg of [`GP of [< `X]]] * [`Reg of [`GP of [< `X]]] * [< `Bitmask]
          )
          t
    | EOR_shifted_register
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]]
            * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
          )
          t
    | EOR_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | EXT
        : ( quad,
            [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]]
            * [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]]
            * [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]]
            * [`Imm of [`Six]] )
          t
    | FABS
        : ( pair,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FADD
        : ( triple,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FADDP_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FADD_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FCMP
        : ( pair,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FCM_register :
        Float_cond.t
        -> ( triple,
             [ `Reg of
               [ `Neon of
                 [ `Vector of
                   ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
           t
    | FCM_zero :
        Float_cond.t
        -> ( pair,
             [ `Reg of
               [ `Neon of
                 [ `Vector of
                   ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
           t
    | FCSEL
        : ( quad,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Cond] )
          t
    | FCVT
        : ( pair,
            [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]]
            * [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FCVTL_vector
        : ( pair,
            [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
            * [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
          )
          t
    | FCVTNS
        : ( pair,
            [`Reg of [`GP of [< `X]]]
            * [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FCVTNS_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FCVTN_vector
        : ( pair,
            [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
            * [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
          )
          t
    | FCVTZS
        : ( pair,
            [`Reg of [`GP of [< `X]]]
            * [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FCVTZS_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FDIV
        : ( triple,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FDIV_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FMADD
        : ( quad,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FMAX
        : ( triple,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FMAX_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FMIN
        : ( triple,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FMIN_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    (* FMOV FP-to-FP: same precision copy *)
    | FMOV_fp
        : ( pair,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    (* FMOV GP-to-FP: move from GP register to FP register *)
    | FMOV_gp_to_fp_32
        : ( pair,
            [`Reg of [`Neon of [`Scalar of [`S]]]]
            * [`Reg of [`GP of [< `W | `WZR]]] )
          t
    | FMOV_gp_to_fp_64
        : ( pair,
            [`Reg of [`Neon of [`Scalar of [`D]]]]
            * [`Reg of [`GP of [< `X | `XZR]]] )
          t
    | FMOV_fp_to_gp_32
        : ( pair,
            [`Reg of [`GP of [`W]]] * [`Reg of [`Neon of [< `Scalar of [< `S]]]]
          )
          t
    | FMOV_fp_to_gp_64
        : ( pair,
            [`Reg of [`GP of [`X]]] * [`Reg of [`Neon of [< `Scalar of [< `D]]]]
          )
          t
    | FMOV_scalar_immediate
        : ( pair,
            [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]]
            * [`Imm of [`Sixty_four]] )
          t
    | FMSUB
        : ( quad,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FMUL
        : ( triple,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FMUL_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FNEG
        : ( pair,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FNEG_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FNMADD
        : ( quad,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FNMSUB
        : ( quad,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FNMUL
        : ( triple,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FRECPE_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FRINT :
        Rounding_mode.t
        -> ( pair,
             [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
             * [`Reg of [`Neon of [`Scalar of 'p]]] )
           t
    | FRINT_vector :
        Rounding_mode.t
        -> ( pair,
             [ `Reg of
               [ `Neon of
                 [ `Vector of
                   ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
             * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
           t
    | FRSQRTE_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FSQRT
        : ( pair,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FSQRT_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | FSUB
        : ( triple,
            [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]]
            * [`Reg of [`Neon of [`Scalar of 'p]]] )
          t
    | FSUB_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | INS :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
             * [`Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]]
           )
           t
    | INS_V :
        Neon_reg_name.Lane_index.Src_and_dest.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
             * [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
           )
           t
    | LDAR : (pair, [`Reg of [`GP of [< `X | `W]]] * [`Mem of [`Base_reg]]) t
    | LDP :
        ('w1, 'w2) LDP_STP_width.t
        -> ( triple,
             [`Reg of [`GP of 'w1]]
             * [`Reg of [`GP of 'w2]]
             * [`Mem of [`Offset_pair | `Pre_pair | `Post_pair]] )
           t
    | LDR
        : ( pair,
            [`Reg of [`GP of [< `X | `W | `LR]]]
            * [`Mem of Addressing_mode.single] )
          t
    | LDRB
        : (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | LDRH
        : (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | LDRSB
        : (pair, [`Reg of [`GP of [< `X]]] * [`Mem of Addressing_mode.single]) t
    | LDRSH
        : (pair, [`Reg of [`GP of [< `X]]] * [`Mem of Addressing_mode.single]) t
    | LDRSW
        : (pair, [`Reg of [`GP of [< `X]]] * [`Mem of Addressing_mode.single]) t
    | LDR_simd_and_fp
        : ( pair,
            [`Reg of [`Neon of [< `Scalar of [< `D | `S | `Q]]]]
            * [`Mem of Addressing_mode.single] )
          t
    | LSLV
        : ( triple,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]] )
          t
    | LSRV
        : ( triple,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]] )
          t
    | MADD
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]] )
          t
    | MOVI
        : ( pair,
            [ `Reg of
              [ `Neon of
                [< `Scalar of _ | `Vector of [< any_vector] * [< any_width]] ]
            ]
            * [`Imm of [< `Twelve]] )
          t
    | MOVK
        : ( triple,
            [`Reg of [`GP of [< `X | `W]]]
            * [`Imm of [`Sixteen_unsigned]]
            * [`Shift of [`Lsl] * [`Six]] )
          t
    | MOVN
        : ( triple,
            [`Reg of [`GP of [< `X | `W]]]
            * [`Imm of [`Sixteen_unsigned]]
            * [`Optional of [`Shift of [`Lsl] * [`Six]] option] )
          t
    | MOVZ
        : ( triple,
            [`Reg of [`GP of [< `X | `W]]]
            * [`Imm of [`Sixteen_unsigned]]
            * [`Optional of [`Shift of [`Lsl] * [`Six]] option] )
          t
    | MSUB
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]] )
          t
    | MUL_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                  * ([< `B | `H | `S] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | MVN_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | NEG_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | NOP : (singleton, unit) t
    | ORR_immediate
        : ( triple,
            [`Reg of [`GP of [< `X]]]
            * [`Reg of [`GP of [< `X | `XZR]]]
            * [< `Bitmask] )
          t
    | ORR_shifted_register
        : ( quad,
            [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
            * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
            * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
            * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
          )
          t
    | ORR_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | RBIT
        : ( pair,
            [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]] )
          t
    | RET : (singleton, unit) t
    | REV
        : ( pair,
            [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]] )
          t
    | REV16
        : ( pair,
            [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]] )
          t
    | SBFM
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Imm of [`Six]]
            * [`Imm of [`Six]] )
          t
    | SCVTF
        : ( pair,
            [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]]
            * [`Reg of [`GP of [< `X]]] )
          t
    | SCVTF_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | SDIV
        : ( triple,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]] )
          t
    | SHL
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Imm of [`Six]] )
          t
    | SMAX_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                  * ([< any_width] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | SMIN_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                  * ([< any_width] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | SMOV :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [`Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]]
             * [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
           )
           t
    | SMULH
        : ( triple,
            [`Reg of [`GP of [`X]]]
            * [`Reg of [`GP of [`X]]]
            * [`Reg of [`GP of [`X]]] )
          t
    | SMULL2_vector :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Widen_mul2.t
        -> ( triple,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | SMULL_vector :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Widen_mul.t
        -> ( triple,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | SQADD_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | SQSUB_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | SQXTN :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Narrow.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | SQXTN2 :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Narrow2.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | SSHL_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | SSHR
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Imm of [`Six]] )
          t
    | STP :
        ('w1, 'w2) LDP_STP_width.t
        -> ( triple,
             [`Reg of [`GP of 'w1]]
             * [`Reg of [`GP of 'w2]]
             * [`Mem of [`Offset_pair | `Pre_pair | `Post_pair]] )
           t
    | STR
        : ( pair,
            [`Reg of [`GP of [< `X | `W | `LR]]]
            * [`Mem of Addressing_mode.single] )
          t
    | STRB
        : (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | STRH
        : (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | STR_simd_and_fp
        : ( pair,
            [`Reg of [`Neon of [< `Scalar of [< `D | `S | `Q]]]]
            * [`Mem of Addressing_mode.single] )
          t
    | SUBS_immediate
        : ( quad,
            [`Reg of [`GP of [< `W | `WZR | `X | `XZR]]]
            * [`Reg of [`GP of [< `W | `X | `SP]]]
            * [`Imm of [< `Twelve]]
            * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
          t
    | SUBS_shifted_register
        : ( quad,
            [`Reg of [`GP of [< `X | `XZR]]]
            * [`Reg of [`GP of [< `X | `SP]]]
            * [`Reg of [`GP of [< `X]]]
            * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
          )
          t
    | SUB_immediate
        : ( quad,
            [`Reg of [`GP of [< `X | `SP]]]
            * [`Reg of [`GP of [< `X | `SP]]]
            * [`Imm of [< `Twelve]]
            * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
          t
    | SUB_shifted_register
        : ( quad,
            [`Reg of [`GP of ([< `X | `W] as 'w)]]
            * [`Reg of [`GP of 'w]]
            * [`Reg of [`GP of 'w]]
            * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
          )
          t
    | SUB_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | SXTL :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Widen.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | TBNZ
        : ( triple,
            [`Reg of [`GP of [`X]]] * [`Imm of [`Six]] * [`Imm of [`Sym of _]]
          )
          t
    | TBZ
        : ( triple,
            [`Reg of [`GP of [`X]]] * [`Imm of [`Six]] * [`Imm of [`Sym of _]]
          )
          t
    | TST : (pair, [`Reg of [`GP of [< `X]]] * [< `Bitmask]) t
    | UADDLP_vector
        : ( pair,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  [< `V4H | `V8H | `V2S | `V4S | `V1D | `V2D] * [< any_width] ]
              ] ]
            * [ `Reg of
                [ `Neon of
                  [ `Vector of
                    [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                  ] ] ] )
          t
    | UBFM
        : ( quad,
            [`Reg of [`GP of [< `X | `W]]]
            * [`Reg of [`GP of [< `X | `W]]]
            * [`Imm of [`Six]]
            * [`Imm of [`Six]] )
          t
    | UMAX_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                  * ([< any_width] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | UMIN_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [ `Vector of
                  ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                  * ([< any_width] as 'w) ] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | UMOV :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [`Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]]
             * [`Reg of [`Neon of [`Vector of [< any_vector] * [< any_width]]]]
           )
           t
    | UMULH
        : ( triple,
            [`Reg of [`GP of [`X]]]
            * [`Reg of [`GP of [`X]]]
            * [`Reg of [`GP of [`X]]] )
          t
    | UMULL2_vector :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Widen_mul2.t
        -> ( triple,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | UMULL_vector :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Widen_mul.t
        -> ( triple,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | UQADD_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | UQSUB_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | UQXTN :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Narrow.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | UQXTN2 :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Narrow2.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | USHL_vector
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | USHR
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Imm of [`Six]] )
          t
    | UXTL :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Widen.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | XTN :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Narrow.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | XTN2 :
        ('src_arr, 'src_w, 'dst_arr, 'dst_w) Narrow2.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of 'dst_arr * 'dst_w]]]
             * [`Reg of [`Neon of [`Vector of 'src_arr * 'src_w]]] )
           t
    | YIELD : (singleton, unit) t
    | ZIP1
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
    | ZIP2
        : ( triple,
            [ `Reg of
              [ `Neon of
                [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
            * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
          t
end

module Instruction : sig
  type t =
    | I :
        { name : ('num, 'operands) Instruction_name.t;
          operands : ('num, 'operands) many
        }
        -> t

  val print : Format.formatter -> t -> unit
end

module DSL : sig
  val reg_op : 'a Reg.t -> [`Reg of 'a] Operand.t

  val imm : int -> [`Imm of [`Twelve]] Operand.t

  val imm_six : int -> [`Imm of [`Six]] Operand.t

  val imm_sixteen : int -> [`Imm of [`Sixteen_unsigned]] Operand.t

  (** Like [imm_sixteen] but takes a [nativeint] and validates it's in range
      [0, 65535]. Raises [Misc.fatal_error] if out of range. *)
  val imm_sixteen_of_nativeint :
    nativeint -> [`Imm of [`Sixteen_unsigned]] Operand.t

  val imm_float : float -> [`Imm of [`Sixty_four]] Operand.t

  val imm_nativeint : nativeint -> [`Imm of [`Sixty_four]] Operand.t

  val bitmask : nativeint -> [`Bitmask] Operand.t

  val symbol : 'w Symbol.t -> [`Imm of [`Sym of 'w]] Operand.t

  val shift :
    kind:'op Operand.Shift.Kind.t ->
    amount:int ->
    [`Shift of 'op * [`Six]] Operand.t

  val optional_shift :
    kind:'op Operand.Shift.Kind.t ->
    amount:int ->
    [`Optional of [`Shift of 'op * [`Six]] option] Operand.t

  val optional_none : [`Optional of 'a option] Operand.t

  val unit_operand : unit Operand.t

  val mem :
    base:[`GP of [< `X | `SP]] Reg.t -> [`Mem of [> `Base_reg]] Operand.t

  val mem_offset :
    base:[`GP of [< `X | `SP]] Reg.t ->
    offset:int ->
    [`Mem of [> `Offset_imm | `Offset_unscaled]] Operand.t

  val mem_symbol :
    base:[`GP of [< `X | `SP]] Reg.t ->
    symbol:[`Twelve] Symbol.t ->
    [`Mem of [> `Offset_sym]] Operand.t

  val mem_pre :
    base:[`GP of [< `X | `SP]] Reg.t ->
    offset:int ->
    [`Mem of [> `Pre]] Operand.t

  val mem_post :
    base:[`GP of [< `X | `SP]] Reg.t ->
    offset:int ->
    [`Mem of [> `Post]] Operand.t

  val mem_offset_pair :
    base:[`GP of [< `X | `SP]] Reg.t ->
    offset:int ->
    [`Mem of [> `Offset_pair]] Operand.t

  val mem_pre_pair :
    base:[`GP of [< `X | `SP]] Reg.t ->
    offset:int ->
    [`Mem of [> `Pre_pair]] Operand.t

  val mem_post_pair :
    base:[`GP of [< `X | `SP]] Reg.t ->
    offset:int ->
    [`Mem of [> `Post_pair]] Operand.t

  val cond : Cond.t -> [`Cond] Operand.t

  val float_cond : Float_cond.t -> [`Float_cond] Operand.t

  (** The functions below are shorthands for composing [reg_op] and the
      respective function from [Reg] *)
  val reg_v2d : int -> [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Operand.t

  val reg_v2s : int -> [`Reg of [`Neon of [`Vector of [`V2S] * [`S]]]] Operand.t

  val reg_v4s : int -> [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Operand.t

  val reg_v8b : int -> [`Reg of [`Neon of [`Vector of [`V8B] * [`B]]]] Operand.t

  val reg_v16b :
    int -> [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Operand.t

  val reg_v8h : int -> [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Operand.t

  val reg_v4h : int -> [`Reg of [`Neon of [`Vector of [`V4H] * [`H]]]] Operand.t

  val reg_b : int -> [`Reg of [`Neon of [`Scalar of [`B]]]] Operand.t

  val reg_s : int -> [`Reg of [`Neon of [`Scalar of [`S]]]] Operand.t

  val reg_d : int -> [`Reg of [`Neon of [`Scalar of [`D]]]] Operand.t

  val reg_q : int -> [`Reg of [`Neon of [`Scalar of [`Q]]]] Operand.t

  val reg_x : int -> [`Reg of [`GP of [`X]]] Operand.t

  val reg_w : int -> [`Reg of [`GP of [`W]]] Operand.t

  val sp : [`Reg of [`GP of [`SP]]] Operand.t

  val lr : [`Reg of [`GP of [`LR]]] Operand.t

  val fp : [`Reg of [`GP of [`FP]]] Operand.t

  val xzr : [`Reg of [`GP of [`XZR]]] Operand.t

  val wzr : [`Reg of [`GP of [`WZR]]] Operand.t

  val reglane_v4s :
    int ->
    lane:Neon_reg_name.Lane_index.t ->
    [`Reg of [`Neon of [`Lane of [`Vector of [`V4S] * [`S]]]]] Operand.t

  val reglane_v2d :
    int ->
    lane:Neon_reg_name.Lane_index.t ->
    [`Reg of [`Neon of [`Lane of [`Vector of [`V2D] * [`D]]]]] Operand.t

  val reglane_b :
    int ->
    lane:Neon_reg_name.Lane_index.t ->
    [`Reg of [`Neon of [`Lane of [`Scalar of [`B]]]]] Operand.t

  val reglane_h :
    int ->
    lane:Neon_reg_name.Lane_index.t ->
    [`Reg of [`Neon of [`Lane of [`Scalar of [`H]]]]] Operand.t

  val reglane_s :
    int ->
    lane:Neon_reg_name.Lane_index.t ->
    [`Reg of [`Neon of [`Lane of [`Scalar of [`S]]]]] Operand.t

  val reglane_d :
    int ->
    lane:Neon_reg_name.Lane_index.t ->
    [`Reg of [`Neon of [`Lane of [`Scalar of [`D]]]]] Operand.t

  val print_ins :
    ('num, 'operands) Instruction_name.t -> ('num, 'operands) many -> string

  module Acc : sig
    val set_emit_string : emit_string:(string -> unit) -> unit

    (** Set a callback to receive instructions for binary emission (JIT). *)
    val set_emit_instruction : emit_instruction:(Instruction.t -> unit) -> unit

    (** Clear the binary emission callback. *)
    val clear_emit_instruction : unit -> unit

    (** Passes the instruction to the function provided to [set_emit_string].
        Also passes to [set_emit_instruction] callback if set.
        (Can't directly reference [Emitaux] due to a circular dependency.) *)
    val ins :
      ('num, 'operands) Instruction_name.t -> ('num, 'operands) many -> unit

    (** Execute [f] with emission disabled, counting how many instructions
        would be emitted. Returns the instruction count. *)
    val with_measuring : f:(unit -> unit) -> int

    val ins1 : (singleton, 'a) Instruction_name.t -> 'a Operand.t -> unit

    val ins2 :
      (pair, 'a * 'b) Instruction_name.t -> 'a Operand.t -> 'b Operand.t -> unit

    val ins3 :
      (triple, 'a * 'b * 'c) Instruction_name.t ->
      'a Operand.t ->
      'b Operand.t ->
      'c Operand.t ->
      unit

    val ins4 :
      (quad, 'a * 'b * 'c * 'd) Instruction_name.t ->
      'a Operand.t ->
      'b Operand.t ->
      'c Operand.t ->
      'd Operand.t ->
      unit

    val ins0 : (singleton, unit) Instruction_name.t -> unit

    (** Tupled versions of ins2/ins3/ins4 for convenience with helper functions
        that return tuples. *)
    module Tupled : sig
      val ins2 :
        (pair, 'a * 'b) Instruction_name.t ->
        'a Operand.t * 'b Operand.t ->
        unit

      val ins3 :
        (triple, 'a * 'b * 'c) Instruction_name.t ->
        'a Operand.t * 'b Operand.t * 'c Operand.t ->
        unit

      val ins4 :
        (quad, 'a * 'b * 'c * 'd) Instruction_name.t ->
        'a Operand.t * 'b Operand.t * 'c Operand.t * 'd Operand.t ->
        unit
    end

    (** Expansion of instructions that are aliases *)

    val ins_mul :
      [`Reg of [`GP of ([< `W | `X] as 'w)]] Operand.t ->
      [`Reg of [`GP of 'w]] Operand.t ->
      [`Reg of [`GP of 'w]] Operand.t ->
      unit

    val ins_lsl_immediate :
      [`Reg of [`GP of ([< `W | `X] as 'w)]] Operand.t ->
      [`Reg of [`GP of 'w]] Operand.t ->
      shift_in_bits:int ->
      unit

    val ins_lsr_immediate :
      [`Reg of [`GP of ([< `W | `X] as 'w)]] Operand.t ->
      [`Reg of [`GP of 'w]] Operand.t ->
      shift_in_bits:int ->
      unit

    val ins_asr_immediate :
      [`Reg of [`GP of ([< `W | `X] as 'w)]] Operand.t ->
      [`Reg of [`GP of 'w]] Operand.t ->
      shift_in_bits:int ->
      unit

    val ins_uxtb :
      [`Reg of [`GP of ([< `W | `X] as 'w)]] Operand.t ->
      [`Reg of [`GP of 'w]] Operand.t ->
      unit

    val ins_uxth :
      [`Reg of [`GP of ([< `W | `X] as 'w)]] Operand.t ->
      [`Reg of [`GP of 'w]] Operand.t ->
      unit

    val ins_cmp :
      [`Reg of [`GP of [< `SP | `W | `X]]] Operand.t ->
      [`Imm of [< `Twelve]] Operand.t ->
      [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] Operand.t ->
      unit

    val ins_cmp_reg :
      [`Reg of [`GP of [< `SP | `X]]] Operand.t ->
      [`Reg of [`GP of [< `X]]] Operand.t ->
      [`Optional of [`Shift of [< `Asr | `Lsl | `Lsr] * [`Six]] option]
      Operand.t ->
      unit

    val ins_cmn :
      [`Reg of [`GP of [< `SP | `X]]] Operand.t ->
      [`Imm of [< `Twelve]] Operand.t ->
      [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] Operand.t ->
      unit

    val ins_cset : [`Reg of [`GP of [< `X | `XZR]]] Operand.t -> Cond.t -> unit

    val ins_mov_from_sp :
      dst:[`Reg of [`GP of [< `FP | `SP | `X]]] Operand.t -> unit

    val ins_mov_to_sp :
      src:[`Reg of [`GP of [< `FP | `SP | `X]]] Operand.t -> unit

    val ins_mov_vector :
      [ `Reg of
        [`Neon of [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)]]
      ]
      Operand.t ->
      [`Reg of [`Neon of [`Vector of 'v * 'w]]] Operand.t ->
      unit

    val ins_mov_reg :
      [`Reg of [`GP of [< `X]]] Operand.t ->
      [`Reg of [`GP of [< `X | `XZR]]] Operand.t ->
      unit

    val ins_mov_reg_w :
      [`Reg of [`GP of [< `W]]] Operand.t ->
      [`Reg of [`GP of [< `W]]] Operand.t ->
      unit

    val ins_mov_imm :
      [`Reg of [`GP of [< `X | `W]]] Operand.t ->
      [`Imm of [`Sixteen_unsigned]] Operand.t ->
      unit
  end
end
