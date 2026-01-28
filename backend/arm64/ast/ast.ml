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

[@@@ocaml.warning "+a-40-41-42"]

(* We disable warning 37 ("constructor X is never used to build values") in a
   few places to allow various types below to be a full description of what is
   permitted by the architecture, even though we don't yet use all of them. *)

open! Int_replace_polymorphic_compare
module Asm_label = Asm_targets.Asm_label
module Asm_symbol = Asm_targets.Asm_symbol

let check_index first last index =
  if index < first || index > last
  then Misc.fatal_errorf "Illegal register index %d" index ()

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

(** Constraint tying vector element width to GP register width for INS and UMOV.
    - B/H/S elements use W (32-bit GP)
    - D elements use X (64-bit GP) *)
module Element_to_GP = struct
  type (_, _) t =
    | B : ([`B], [`W]) t
    | H : ([`H], [`W]) t
    | S : ([`S], [`W]) t
    | D : ([`D], [`X]) t
end

(** Constraint tying vector element width to GP register width for SMOV.
    - B/H elements can use W or X (sign-extending to 32 or 64 bits)
    - S elements must use X (sign-extending 32 to 64 bits)
    - D elements are not valid for SMOV (no sign extension needed) *)
module Smov_element_to_GP = struct
  type (_, _) t =
    | B_to_W : ([`B], [`W]) t
    | B_to_X : ([`B], [`X]) t
    | H_to_W : ([`H], [`W]) t
    | H_to_X : ([`H], [`X]) t
    | S_to_X : ([`S], [`X]) t
end

(* Float/SIMD register description *)
module Neon_reg_name = struct
  module Vector = struct
    type (_, _) t =
      | V8B : ([`V8B], [`B]) t
      | V16B : ([`V16B], [`B]) t
      | V4H : ([`V4H], [`H]) t
      | V8H : ([`V8H], [`H]) t
      | V2S : ([`V2S], [`S]) t
      | V4S : ([`V4S], [`S]) t
      | V1D : ([`V1D], [`D]) t
      | V2D : ([`V2D], [`D]) t
    [@@ocaml.warning "-37"]

    let to_string (type v s) (t : (v, s) t) =
      match t with
      | V8B -> "8B"
      | V16B -> "16B"
      | V4H -> "4H"
      | V8H -> "8H"
      | V2S -> "2S"
      | V4S -> "4S"
      | V1D -> "1D"
      | V2D -> "2D"

    let num_lanes (type v s) (t : (v, s) t) =
      match t with
      | V8B -> 8
      | V16B -> 16
      | V4H -> 4
      | V8H -> 8
      | V2S -> 2
      | V4S -> 4
      | V1D -> 1
      | V2D -> 2

    let name t index = Printf.sprintf "V%d.%s" index (to_string t)

    type _ testi = I : [< `V8B | `V16B] testi [@@warning "-37"]

    type _ testo =
      | O_V8B : [`V8B] testo
      | O_V16B : [`V16B] testo
      | O_V4H : [`V4H] testo
    [@@warning "-37"]

    let _foo : type v. v testi -> v testo -> int =
     fun i o -> match i, o with I, O_V8B -> 42 | I, O_V16B -> 42
  end

  (* module Scalar_vector_equality = struct type ('scalar, 'vector) t = |
     Equal_B : ([`B], [`V8B | `V16B]) t | Equal_H : ([`H], [`V4H | `V8H]) t |
     Equal_S : ([`S], [`V2S | `V4S]) t | Equal_D : ([`D], [`V1D | `V2D]) t

     (* let create (type a b) (s : a Scalar.t) (v : b Vector.t) : (a Scalar.t *
     b Vector.t) t = match[@warning "-4"] s, v with | B, (V8B | V16B) -> Equal_B
     | (V4H | V8H), H -> Equal_H | S, (V2S | V4S) -> Equal_S | D, (V1D | V2D) ->
     Equal_D | _, _ -> assert false *) end *)

  module Scalar = struct
    type _ t =
      | B : [`B] t
      | H : [`H] t
      | S : [`S] t
      | D : [`D] t
      | Q : [`Q] t

    let num_lanes : type a. a t -> int = function
      | B -> 16
      | H -> 8
      | S -> 4
      | D -> 2
      | Q -> 1

    let to_string : type a. a t -> string = function
      | B -> "b"
      | H -> "h"
      | S -> "s"
      | D -> "d"
      | Q -> "q"

    let of_vector : type v s. (v, s) Vector.t -> s t =
     fun vec ->
      match vec with
      | V8B -> B
      | V16B -> B
      | V4H -> H
      | V8H -> H
      | V2S -> S
      | V4S -> S
      | V1D -> D
      | V2D -> D

    let name t index = Printf.sprintf "%s%d" (to_string t) index
  end

  module Lane_index : sig
    type t = private int

    type lane_index = t

    val create : int -> t

    val to_int : t -> int

    module Src_and_dest : sig
      type t

      val create : src:lane_index -> dest:lane_index -> t

      val dest_index : t -> lane_index

      val src_index : t -> lane_index
    end
  end = struct
    type t = int

    type lane_index = t

    let create i = i

    let to_int i = i

    module Src_and_dest = struct
      type nonrec t =
        { src : t;
          dest : t
        }

      let create ~src ~dest = { src; dest }

      let dest_index t = t.dest

      let src_index t = t.src
    end
  end

  module Lane = struct
    (** Support representation with and without the optional number of lanes,
        for example Vn.4S[1] and Vn.S[1]. *)
    type 'a r =
      | V : ('v, 's) Vector.t -> [`Vector of 'v * 's] r
      | S : 's Scalar.t -> [`Scalar of 's] r

    type 'a t =
      { r : 'a r;
        lane : Lane_index.t
      }

    let num_lanes (type a) (r : a r) =
      match r with V v -> Vector.num_lanes v | S s -> Scalar.num_lanes s

    let check_index t =
      let last = num_lanes t.r - 1 in
      check_index 0 last (Lane_index.to_int t.lane)

    let name (type a) (t : a t) index =
      let suffix =
        match t.r with V v -> Vector.to_string v | S s -> Scalar.to_string s
      in
      Printf.sprintf "V%d.%s[%d]" index suffix (Lane_index.to_int t.lane)
  end

  type _ t =
    | Vector : ('v, 's) Vector.t -> [`Vector of 'v * 's] t
    | Scalar : 's Scalar.t -> [`Scalar of 's] t
    | Lane : 'l Lane.t -> [`Lane of 'l] t

  let last = 31

  let check_index (type a) (t : a t) index =
    check_index 0 last index;
    match t with Vector _ | Scalar _ -> () | Lane l -> Lane.check_index l

  let name (type a) (t : a t) index =
    match t with
    | Vector v -> Vector.name v index
    | Scalar s -> Scalar.name s index
    | Lane l -> Lane.name l index

  let _lane_of_vector vector ~lane =
    let scalar_type = Scalar.of_vector vector in
    Lane { r = S scalar_type; lane }
end

(* General-purpose register description *)
module GP_reg_name = struct
  type _ t =
    | W : [`W] t
    | X : [`X] t
    | WZR : [`WZR] t
    | XZR : [`XZR] t
    | WSP : [`WSP] t
    | SP : [`SP] t
    | LR : [`LR] t
    | FP : [`FP] t
  [@@ocaml.warning "-37"]

  let last_numbered = 30

  let last = 31

  let check_index (type a) (t : a t) index =
    match t with
    | W | X -> check_index 0 last_numbered index
    | WZR | XZR | WSP | SP | LR | FP -> check_index last last index

  let name (type a) (t : a t) index =
    match t with
    | W -> Printf.sprintf "w%d" index
    | X -> Printf.sprintf "x%d" index
    | WZR -> "wzr"
    | XZR -> "xzr"
    | WSP -> "wsp"
    | SP -> "sp"
    | LR -> "lr"
    | FP -> "fp"

  let encoding (type a) (t : a t) index =
    match t with
    | W | X -> index
    | WZR | XZR | WSP | SP -> 31
    | LR -> 30
    | FP -> 29
end

(* Register representation *)
module Reg_name = struct
  type _ t =
    | GP : 'a GP_reg_name.t -> [`GP of 'a] t
    | Neon : 'a Neon_reg_name.t -> [`Neon of 'a] t

  let check_index (type a) (t : a t) index =
    match t with
    | GP rn -> GP_reg_name.check_index rn index
    | Neon rn -> Neon_reg_name.check_index rn index

  let name (type a) (t : a t) index =
    match t with
    | GP rn -> GP_reg_name.name rn index
    | Neon rn -> Neon_reg_name.name rn index

  let encoding (type a) (t : a t) index =
    match t with GP rn -> GP_reg_name.encoding rn index | Neon _ -> index
end

module Reg = struct
  type 'a t =
    { reg_name : 'a Reg_name.t;
      index : int
    }

  let create (type a) (reg_name : a Reg_name.t) index : a t =
    Reg_name.check_index reg_name index;
    { reg_name; index }

  let name (type a) (t : a t) = Reg_name.name t.reg_name t.index

  let _encoding (type a) (t : a t) = Reg_name.encoding t.reg_name t.index

  let gp_encoding : type a. [`GP of a] t -> int =
   fun t -> match t.reg_name with GP rn -> GP_reg_name.encoding rn t.index

  let gp_sf : type a. [`GP of a] t -> int =
   fun t ->
    match t.reg_name with
    | GP W | GP WZR | GP WSP -> 0
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 1

  (* for special GP registers we use the last index *)
  (* CR mshinwell: why is this? *)
  let sp = create (GP SP) GP_reg_name.last

  let lr = create (GP LR) GP_reg_name.last

  let fp = create (GP FP) GP_reg_name.last

  let xzr = create (GP XZR) GP_reg_name.last

  let wzr = create (GP WZR) GP_reg_name.last

  let reg_x i = create (GP GP_reg_name.X) i

  let reg_w i = create (GP GP_reg_name.W) i

  let reg_s i = create (Neon (Scalar S)) i

  let reg_d i = create (Neon (Scalar D)) i

  let reg_q i = create (Neon (Scalar Q)) i

  let reg_v2d i = create (Neon (Vector V2D)) i

  let reg_v2s i = create (Neon (Vector V2S)) i

  let reg_v4h i = create (Neon (Vector V4H)) i

  let reg_v4s i = create (Neon (Vector V4S)) i

  let reg_v8b i = create (Neon (Vector V8B)) i

  let reg_v8h i = create (Neon (Vector V8H)) i

  let reg_v16b i = create (Neon (Vector V16B)) i

  let reg_b i = create (Neon (Scalar B)) i
end

module Float_cond = struct
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

  let to_string t =
    match t with
    | EQ -> "eq"
    | GT -> "gt"
    | LE -> "le"
    | GE -> "ge"
    | LT -> "lt"
    | NE -> "ne"
    | CC -> "cc"
    | CS -> "cs"
    | LS -> "ls"
    | HI -> "hi"
end

module Cond = struct
  type t =
    | EQ
    | NE
    | CS (* alias HS *)
    | CC (* alias LO *)
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
  (* | AL *)
  (* | NV *)

  let to_string t =
    match t with
    | EQ -> "eq"
    | NE -> "ne"
    | CS -> "cs"
    | CC -> "cc"
    | MI -> "mi"
    | PL -> "pl"
    | VS -> "vs"
    | VC -> "vc"
    | HI -> "hi"
    | LS -> "ls"
    | GE -> "ge"
    | LT -> "lt"
    | GT -> "gt"
    | LE -> "le"

  let invert t =
    match t with
    | EQ -> NE
    | NE -> EQ
    | CS -> CC
    | CC -> CS
    | MI -> PL
    | PL -> MI
    | VS -> VC
    | VC -> VS
    | HI -> LS
    | LS -> HI
    | GE -> LT
    | LT -> GE
    | GT -> LE
    | LE -> GT

  let of_float_cond (cond : Float_cond.t) : t =
    match cond with
    | EQ -> EQ
    | GT -> GT
    | GE -> GE
    | LT -> LT
    | LE -> LE
    | NE -> NE
    | CC -> CC
    | CS -> CS
    | HI -> HI
    | LS -> LS
end

(** Condition codes for B.cond instruction. Unifies integer and floating-point
    conditions - architecturally it's the same instruction, but we preserve the
    distinction for semantic clarity. *)
module Branch_cond = struct
  type t =
    | Int of Cond.t
    | Float of Float_cond.t

  let to_string = function
    | Int c -> Cond.to_string c
    | Float c -> Float_cond.to_string c
end

module Simd_int_cmp = struct
  type t =
    | EQ
    | GE
    | GT
    | LE
    | LT
    | HI
    | HS

  let to_string t =
    match t with
    | EQ -> "eq"
    | GE -> "ge"
    | GT -> "gt"
    | LE -> "le"
    | LT -> "lt"
    | HI -> "hi"
    | HS -> "hs"
end

module Rounding_mode = struct
  type t =
    | A  (** ties Away *)
    | I  (** current mode - use FPCR rounding mode *)
    | M  (** towards Minus infinity *)
    | N  (** to Nearest, ties to even *)
    | P  (** towards Plus infinity *)
    | X  (** to nearest, eXact - may signal inexact *)
    | Z  (** towards Zero *)

  let to_string t =
    match t with
    | A -> "a"
    | I -> "i"
    | M -> "m"
    | N -> "n"
    | P -> "p"
    | X -> "x"
    | Z -> "z"
end

module Memory_barrier = struct
  type t =
    | SY
    | LD
    | ST
    | ISH
    | ISHLD
    | ISHST
    | NSH
    | NSHLD
    | NSHST
    | OSH
    | OSHLD
    | OSHST

  let to_string b =
    match b with
    | SY -> "sy"
    | LD -> "ld"
    | ST -> "st"
    | ISH -> "ish"
    | ISHLD -> "ishld"
    | ISHST -> "ishst"
    | NSH -> "nsh"
    | NSHLD -> "nshld"
    | NSHST -> "nshst"
    | OSH -> "osh"
    | OSHLD -> "oshld"
    | OSHST -> "oshst"
end

module Symbol = struct
  module L = Asm_targets.Asm_label
  module S = Asm_targets.Asm_symbol

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
    | Label of L.t
    | Symbol of S.t

  type 'w t =
    { target : target;
      offset : int;
      reloc : 'w same_unit_or_reloc
    }

  let create_label (type w) (reloc : w same_unit_or_reloc) ?(offset = 0) lbl :
      w t =
    { target = Label lbl; offset; reloc }

  let create_symbol (type w) (reloc : w same_unit_or_reloc) ?(offset = 0) sym :
      w t =
    { target = Symbol sym; offset; reloc }

  let is_label t = match t.target with Label _ -> true | Symbol _ -> false

  let print_target ppf (target : target) =
    match target with
    | Label lbl -> Asm_label.print ppf lbl
    | Symbol sym -> Asm_symbol.print ppf sym

  let print_with_reloc_directive : type w.
      Format.formatter -> string * w reloc_directive -> unit =
   fun ppf (s, reloc) ->
    let macosx = Target_system.is_macos () in
    match reloc with
    | LOWER_TWELVE -> Format.fprintf ppf ":lo12:%s" s
    | GOT_LOWER_TWELVE -> Format.fprintf ppf ":got_lo12:%s" s
    | GOT_PAGE ->
      if macosx
      then Format.fprintf ppf "%s@GOTPAGE" s
      else Format.fprintf ppf ":got:%s" s
    | GOT_PAGE_OFF ->
      if macosx
      then Format.fprintf ppf "%s@GOTPAGEOFF" s
      else Format.fprintf ppf ":got_lo12:%s" s
    | PAGE ->
      if macosx
      then Format.fprintf ppf "%s@PAGE" s
      else Format.fprintf ppf "%s" s
    | PAGE_OFF ->
      if macosx
      then Format.fprintf ppf "%s@PAGEOFF" s
      else Format.fprintf ppf ":lo12:%s" s
    | CALL26 | JUMP26 ->
      (* For B/BL to external symbols, just print the symbol name; the assembler
         handles the relocation *)
      Format.fprintf ppf "%s" s

  let print_int_offset ppf ofs =
    if ofs > 0
    then Format.fprintf ppf "+%d" ofs
    else if ofs < 0
    then Format.fprintf ppf "-%d" (-ofs)
    else ()

  let print : type w. Format.formatter -> w t -> unit =
   fun ppf t ->
    let target_name =
      match t.target with
      | Label lbl -> Asm_label.encode lbl
      | Symbol sym -> Asm_symbol.encode sym
    in
    match t.reloc with
    | Same_section_and_unit ->
      Format.fprintf ppf "%s%a" target_name print_int_offset t.offset
    | Needs_reloc reloc ->
      Format.fprintf ppf "%a%a" print_with_reloc_directive (target_name, reloc)
        print_int_offset t.offset
end

(** Immediates used in instruction operands. Signedness: Six is unsigned (0-63),
    Twelve is unsigned (0-4095). *)
module Immediate = struct
  type 'width t =
    | Six : int -> [`Six] t
    | Twelve : int -> [`Twelve] t
    | Sixteen_unsigned : int -> [`Sixteen_unsigned] t
    | Sym : 'w Symbol.t -> [`Sym of 'w] t
    | Float : float -> [`Sixty_four] t
    | Nativeint : nativeint -> [`Sixty_four] t
  [@@warning "-37"]

  let print : type w. Format.formatter -> w t -> unit =
   fun ppf t ->
    match t with
    | Six n -> Format.fprintf ppf "#%d" n
    | Twelve n -> Format.fprintf ppf "#%d" n
    | Sixteen_unsigned n -> Format.fprintf ppf "#%d" n
    | Sym s -> Symbol.print ppf s
    | Float f -> Format.fprintf ppf "#%.7f" f
    | Nativeint n -> Format.fprintf ppf "#%s" (Nativeint.to_string n)
end

(** Offsets used in addressing modes. *)
module Addressing_offset = struct
  type 'width t =
    | Seven_signed_scaled : int -> [`Seven_signed] t
    | Nine_signed_unscaled : int -> [`Nine_signed_unscaled] t
    | Twelve_unsigned_scaled : int -> [`Twelve_unsigned_scaled] t
  [@@warning "-37"]

  let print : type w. Format.formatter -> w t -> unit =
   fun ppf t ->
    match t with
    | Seven_signed_scaled n -> Format.fprintf ppf "#%d" n
    | Nine_signed_unscaled n -> Format.fprintf ppf "#%d" n
    | Twelve_unsigned_scaled n -> Format.fprintf ppf "#%d" n
end

module Addressing_mode = struct
  module Offset = struct
    type _ t =
      | Imm : 'w Addressing_offset.t -> 'w t
      | Symbol_with_reloc : [`Twelve] Symbol.t -> [`Twelve_unsigned_scaled] t
    [@@warning "-37"]

    let print : type a. Format.formatter -> a t -> unit =
     fun ppf t ->
      match t with
      | Imm i -> Addressing_offset.print ppf i
      | Symbol_with_reloc s -> Symbol.print ppf s
  end

  type single =
    [ `Base_reg
    | `Offset_twelve_unsigned_scaled
    | `Offset_nine_signed_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]

  (* ARMARM Section C1.3.3, Table C1-8 *)
  type _ t =
    | Reg : [`GP of [< `X | `SP]] Reg.t -> [> `Base_reg] t
    | Offset_twelve_unsigned_scaled :
        [`GP of [< `X | `SP]] Reg.t
        * [`Twelve_unsigned_scaled] Addressing_offset.t
        -> [> `Offset_twelve_unsigned_scaled] t
    | Offset_nine_signed_unscaled :
        [`GP of [< `X | `SP]] Reg.t
        * [`Nine_signed_unscaled] Addressing_offset.t
        -> [> `Offset_nine_signed_unscaled] t
    | Offset_sym :
        [`GP of [< `X | `SP]] Reg.t * [`Twelve] Symbol.t
        -> [> `Offset_sym] t
    | Literal : [`Nineteen] Symbol.t -> [> `Literal] t
    | Pre :
        [`GP of [< `X | `SP]] Reg.t * [`Nine_signed_unscaled] Offset.t
        -> [> `Pre] t
    | Post :
        [`GP of [< `X | `SP]] Reg.t * [`Nine_signed_unscaled] Offset.t
        -> [> `Post] t
    (* Addressing modes for load/store pair (LDP/STP) *)
    | Offset_pair :
        [`GP of [< `X | `SP]] Reg.t * [`Seven_signed] Offset.t
        -> [> `Offset_pair] t
    | Pre_pair :
        [`GP of [< `X | `SP]] Reg.t * [`Seven_signed] Offset.t
        -> [> `Pre_pair] t
    | Post_pair :
        [`GP of [< `X | `SP]] Reg.t * [`Seven_signed] Offset.t
        -> [> `Post_pair] t
  [@@warning "-37"]

  let print : type a. Format.formatter -> a t -> unit =
   fun ppf t ->
    let open Format in
    match t with
    | Reg r -> fprintf ppf "[%s]" (Reg.name r)
    | Offset_twelve_unsigned_scaled (r, off) ->
      fprintf ppf "[%s, %a]" (Reg.name r) Addressing_offset.print off
    | Offset_nine_signed_unscaled (r, off) ->
      fprintf ppf "[%s, %a]" (Reg.name r) Addressing_offset.print off
    | Offset_sym (r, sym) ->
      fprintf ppf "[%s, %a]" (Reg.name r) Symbol.print sym
    | Literal sym -> fprintf ppf "%a" Symbol.print sym
    | Pre (r, off) -> fprintf ppf "[%s, %a]!" (Reg.name r) Offset.print off
    | Post (r, off) -> fprintf ppf "[%s], %a" (Reg.name r) Offset.print off
    | Offset_pair (r, off) ->
      fprintf ppf "[%s, %a]" (Reg.name r) Offset.print off
    | Pre_pair (r, off) -> fprintf ppf "[%s, %a]!" (Reg.name r) Offset.print off
    | Post_pair (r, off) -> fprintf ppf "[%s], %a" (Reg.name r) Offset.print off
end

module Operand = struct
  module Bitmask = struct
    type t = nativeint

    let print ppf n = Format.fprintf ppf "#%nd" n

    let decode_n_immr_imms (bitmask : t) : int * int * int =
      let { Logical_immediates.n; immr; imms } =
        Logical_immediates.encode_logical_immediate_fields bitmask
      in
      n, immr, imms
  end

  module Shift = struct
    module Kind = struct
      type 'op t =
        | LSL : [`Lsl] t
        | ASR : [`Asr] t
        | LSR : [`Lsr] t

      let to_string (type op) (t : op t) =
        match t with LSL -> "lsl" | ASR -> "asr" | LSR -> "lsr"
    end

    type ('op, 'amount) t =
      { kind : 'op Kind.t;
        amount : 'amount Immediate.t
      }

    let print ppf t =
      let { kind; amount } = t in
      Format.fprintf ppf "%s %a" (Kind.to_string kind) Immediate.print amount
  end

  (** LSL shift positions for MOVK/MOVN/MOVZ instructions. These position a
      16-bit immediate within the register. Architecturally constrained to
      \{0,16,32,48\} for X-form and \{0,16\} for W-form. The GADT encodes this:
      S32 and S48 are only valid for X. *)
  module Lsl_by_multiple_of_16_bits = struct
    type _ t =
      | S0 : [< `X | `W] t
      | S16 : [< `X | `W] t
      | S32 : [`X] t
      | S48 : [`X] t

    let to_int : type w. w t -> int = function
      | S0 -> 0
      | S16 -> 16
      | S32 -> 32
      | S48 -> 48

    let print ppf t = Format.fprintf ppf "lsl #%d" (to_int t)
  end

  (** Shift amount for vector shift instructions (SHL, SSHR, USHR). The valid
      range depends on element width:
      - B (8-bit): 0-7
      - H (16-bit): 0-15
      - S (32-bit): 0-31
      - D (64-bit): 0-63 The GADT ties the shift amount to the element width. *)
  module Shift_by_element_width = struct
    type _ t =
      | For_B : int -> [`B] t
      | For_H : int -> [`H] t
      | For_S : int -> [`S] t
      | For_D : int -> [`D] t

    let to_int : type w. w t -> int = function
      | For_B n -> n
      | For_H n -> n
      | For_S n -> n
      | For_D n -> n

    let print ppf t = Format.fprintf ppf "#%d" (to_int t)
  end

  let _print_separator ppf () = Format.fprintf ppf ", "

  type _ t =
    | Imm : 'w Immediate.t -> [`Imm of 'w] t
    | Reg : 'a Reg.t -> [`Reg of 'a] t
    | Lsl_by_twelve : [`Fixed_shift of [`Lsl_by_twelve]] t
    | Shift : ('op, 'amount) Shift.t -> [`Shift of 'op * 'amount] t
    | Lsl_by_multiple_of_16_bits :
        'w Lsl_by_multiple_of_16_bits.t
        -> [`Lsl_by_multiple_of_16_bits of 'w] t
    | Shift_by_element_width :
        'w Shift_by_element_width.t
        -> [`Shift_by_element_width of 'w] t
    | Cond : Cond.t -> [`Cond] t
    | Float_cond : Float_cond.t -> [`Float_cond] t
    | Mem : 'm Addressing_mode.t -> [`Mem of 'm] t
    | Bitmask : Bitmask.t -> [`Bitmask] t
    | Optional : 'a t option -> [`Optional of 'a option] t
    | Unit : unit t
  [@@ocaml.warning "-37"]

  type 'a operand = 'a t

  (** GP register operand *)
  type 'w gp = [`Reg of [`GP of 'w]] t

  (** Neon scalar register operand *)
  type 's neon_scalar = [`Reg of [`Neon of [`Scalar of 's]]] t

  (** Neon vector register operand with arrangement and element width *)
  type ('arr, 'w) neon_vector = [`Reg of [`Neon of [`Vector of 'arr * 'w]]] t

  let rec print : type a. Format.formatter -> a t -> unit =
   fun ppf t ->
    match t with
    | Imm imm -> Immediate.print ppf imm
    | Reg r -> Format.fprintf ppf "%s" (Reg.name r)
    | Lsl_by_twelve -> Format.fprintf ppf "lsr #12"
    | Shift s -> Shift.print ppf s
    | Lsl_by_multiple_of_16_bits s -> Lsl_by_multiple_of_16_bits.print ppf s
    | Shift_by_element_width s -> Shift_by_element_width.print ppf s
    | Cond c -> Format.fprintf ppf "%s" (Cond.to_string c)
    | Float_cond c -> Format.fprintf ppf "%s" (Float_cond.to_string c)
    | Mem m -> Format.fprintf ppf "%a" Addressing_mode.print m
    | Bitmask b -> Bitmask.print ppf b
    | Optional opt -> ( match opt with Some op -> print ppf op | None -> ())
    | Unit -> ()

  module Wrapped = struct
    type t = O : _ operand -> t

    let create (type a) (o : a operand) : t = O o

    let print ppf (O t) = print ppf t
  end
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

(** Witness types for widening vector operations. Widening: element count stays
    the same, element size doubles. 8B->8H, 4H->4S, 2S->2D *)
module Widen = struct
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8H_V8B : ([`V8B], [`B], [`V8H], [`H]) t
    | V4S_V4H : ([`V4H], [`H], [`V4S], [`S]) t
    | V2D_V2S : ([`V2S], [`S], [`V2D], [`D]) t
end

(** Witness types for narrowing vector operations. Narrowing: element count
    stays the same, element size halves. 8H->8B, 4S->4H, 2D->2S *)
module Narrow = struct
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8B_V8H : ([`V8H], [`H], [`V8B], [`B]) t
    | V4H_V4S : ([`V4S], [`S], [`V4H], [`H]) t
    | V2S_V2D : ([`V2D], [`D], [`V2S], [`S]) t
end

(** Witness types for narrowing to upper half (XTN2, etc). 16B->8H, 8H->4S,
    4S->2D *)
module Narrow2 = struct
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V16B_V8H : ([`V8H], [`H], [`V16B], [`B]) t
    | V8H_V4S : ([`V4S], [`S], [`V8H], [`H]) t
    | V4S_V2D : ([`V2D], [`D], [`V4S], [`S]) t
end

(** Witness types for widening multiply (SMULL, UMULL). Sources are narrow, dest
    is wide. 8B,8B->8H, 4H,4H->4S, 2S,2S->2D *)
module Widen_mul = struct
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8H_V8B : ([`V8B], [`B], [`V8H], [`H]) t
    | V4S_V4H : ([`V4H], [`H], [`V4S], [`S]) t
    | V2D_V2S : ([`V2S], [`S], [`V2D], [`D]) t
end

(** Witness types for widening multiply upper half (SMULL2, UMULL2). Sources are
    128-bit, dest is wide. 16B,16B->8H, 8H,8H->4S, 4S,4S->2D *)
module Widen_mul2 = struct
  type ('src_arr, 'src_w, 'dst_arr, 'dst_w) t =
    | V8H_V16B : ([`V16B], [`B], [`V8H], [`H]) t
    | V4S_V8H : ([`V8H], [`H], [`V4S], [`S]) t
    | V2D_V4S : ([`V4S], [`S], [`V2D], [`D]) t
end

(** Witness types for LDP/STP register width. Both registers in a pair must be
    the same width class. *)
module LDP_STP_width = struct
  type (_, _) t =
    | X : ([< `X | `LR], [< `X | `LR]) t
    | W : ([< `W], [< `W]) t
end

module Instruction_name = struct
  type (_, _) t =
    | ABS_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | ADDP_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | ADDS :
        ( quad,
          [`Reg of [`GP of [< `X | `XZR]]]
          * [`Reg of [`GP of [< `X | `SP]]]
          * [`Imm of [< `Twelve]]
          * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
        t
    | ADDV :
        ( pair,
          [`Reg of [`Neon of [< `Scalar of ([< `B | `H | `S] as 'w)]]]
          * [ `Reg of
              [ `Neon of
                [`Vector of [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * 'w]
              ] ] )
        t
    | ADD_immediate :
        ( quad,
          [`Reg of [`GP of [< `X | `SP | `FP]]]
          * [`Reg of [`GP of [< `X | `SP | `FP]]]
          * [`Imm of [< `Twelve | `Sym of [`Twelve]]]
          * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
        t
    | ADD_shifted_register :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]]
          * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
        )
        t
    | ADD_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | ADR : (pair, [`Reg of [`GP of [`X]]] * [`Imm of [`Sym of [`Nineteen]]]) t
    | ADRP :
        (pair, [`Reg of [`GP of [`X]]] * [`Imm of [`Sym of [`Twenty_one]]]) t
    | AND_immediate :
        ( triple,
          [`Reg of [`GP of [< `X]]] * [`Reg of [`GP of [< `X]]] * [< `Bitmask]
        )
        t
    | AND_shifted_register :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]]
          * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
        )
        t
    | AND_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | ASRV :
        ( triple,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]] )
        t
    | B : (singleton, [`Imm of [`Sym of _]]) t
    | BL : (singleton, [`Imm of [`Sym of _]]) t
    | BLR : (singleton, [`Reg of [`GP of [`X]]]) t
    | BR : (singleton, [`Reg of [`GP of [`X]]]) t
    | B_cond : Branch_cond.t -> (singleton, [`Imm of [`Sym of _]]) t
    | CBNZ : (pair, [`Reg of [`GP of [< `X | `W]]] * [`Imm of [`Sym of _]]) t
    | CBZ : (pair, [`Reg of [`GP of [< `X | `W]]] * [`Imm of [`Sym of _]]) t
    | CLZ :
        (pair, [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]]) t
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
    | CNT :
        (pair, [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]]) t
    | CNT_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | CSEL :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]]
          * [`Cond] )
        t
    | CSINC :
        ( quad,
          [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
          * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
          * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
          * [`Cond] )
        t
    | CTZ :
        (pair, [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]]) t
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
    | EOR_immediate :
        ( triple,
          [`Reg of [`GP of [< `X]]] * [`Reg of [`GP of [< `X]]] * [< `Bitmask]
        )
        t
    | EOR_shifted_register :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]]
          * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
        )
        t
    | EOR_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | EXT :
        ( quad,
          [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]]
          * [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]]
          * [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]]
          * [`Imm of [`Six]] )
        t
    | FABS :
        ( pair,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FADD :
        ( triple,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FADDP_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FADD_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FCMP :
        ( pair,
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
    | FCSEL :
        ( quad,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Cond] )
        t
    | FCVT :
        ( pair,
          [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]]
          * [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]] )
        t
    | FCVTL_vector :
        ( pair,
          [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]]
          * [`Reg of [`Neon of [`Vector of [< `V2S | `V4S] * [`S]]]] )
        t
    | FCVTNS :
        ( pair,
          [`Reg of [`GP of [< `X]]]
          * [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]] )
        t
    | FCVTNS_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FCVTN_vector :
        ( pair,
          [`Reg of [`Neon of [`Vector of [< `V2S | `V4S] * [`S]]]]
          * [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] )
        t
    | FCVTZS :
        ( pair,
          [`Reg of [`GP of [< `X]]]
          * [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]] )
        t
    | FCVTZS_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FDIV :
        ( triple,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FDIV_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FMADD :
        ( quad,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FMAX :
        ( triple,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FMAX_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FMIN :
        ( triple,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FMIN_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FMOV_fp :
        ( pair,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FMOV_gp_to_fp_32 :
        ( pair,
          [`Reg of [`Neon of [`Scalar of [`S]]]]
          * [`Reg of [`GP of [< `W | `WZR]]] )
        t
    | FMOV_gp_to_fp_64 :
        ( pair,
          [`Reg of [`Neon of [`Scalar of [`D]]]]
          * [`Reg of [`GP of [< `X | `XZR]]] )
        t
    | FMOV_fp_to_gp_32 :
        ( pair,
          [`Reg of [`GP of [`W]]] * [`Reg of [`Neon of [< `Scalar of [< `S]]]]
        )
        t
    | FMOV_fp_to_gp_64 :
        ( pair,
          [`Reg of [`GP of [`X]]] * [`Reg of [`Neon of [< `Scalar of [< `D]]]]
        )
        t
    | FMOV_scalar_immediate :
        ( pair,
          [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]]
          * [`Imm of [`Sixty_four]] )
        t
    | FMSUB :
        ( quad,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FMUL :
        ( triple,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FMUL_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FNEG :
        ( pair,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FNEG_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FNMADD :
        ( quad,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FNMSUB :
        ( quad,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FNMUL :
        ( triple,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FRECPE_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
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
    | FRSQRTE_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FSQRT :
        ( pair,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FSQRT_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | FSUB :
        ( triple,
          [`Reg of [`Neon of [`Scalar of ([< `S | `D] as 'p)]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]]
          * [`Reg of [`Neon of [`Scalar of 'p]]] )
        t
    | FSUB_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< `V2S | `V4S | `V2D] as 'v) * ([< `S | `D] as 'w)]
            ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | INS :
        ('elem, 'gp) Element_to_GP.t * Neon_reg_name.Lane_index.t
        -> ( pair,
             [`Reg of [`Neon of [`Vector of [< any_vector] * 'elem]]]
             * [`Reg of [`GP of 'gp]] )
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
    | LDR :
        ( pair,
          [`Reg of [`GP of [< `X | `W | `LR]]] * [`Mem of Addressing_mode.single]
        )
        t
    | LDRB :
        (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | LDRH :
        (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | LDRSB :
        (pair, [`Reg of [`GP of [< `X]]] * [`Mem of Addressing_mode.single]) t
    | LDRSH :
        (pair, [`Reg of [`GP of [< `X]]] * [`Mem of Addressing_mode.single]) t
    | LDRSW :
        (pair, [`Reg of [`GP of [< `X]]] * [`Mem of Addressing_mode.single]) t
    | LDR_simd_and_fp :
        ( pair,
          [`Reg of [`Neon of [< `Scalar of [< `D | `S | `Q]]]]
          * [`Mem of Addressing_mode.single] )
        t
    | LSLV :
        ( triple,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]] )
        t
    | LSRV :
        ( triple,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]] )
        t
    | MADD :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]] )
        t
    | MOVI :
        ( pair,
          [ `Reg of
            [ `Neon of
              [< `Scalar of _ | `Vector of [< any_vector] * [< any_width]] ] ]
          * [`Imm of [< `Twelve]] )
        t
    | MOVK :
        ( triple,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Imm of [`Sixteen_unsigned]]
          * [`Lsl_by_multiple_of_16_bits of 'w] )
        t
    | MOVN :
        ( triple,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Imm of [`Sixteen_unsigned]]
          * [`Optional of [`Lsl_by_multiple_of_16_bits of 'w] option] )
        t
    | MOVZ :
        ( triple,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Imm of [`Sixteen_unsigned]]
          * [`Optional of [`Lsl_by_multiple_of_16_bits of 'w] option] )
        t
    | MSUB :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]] )
        t
    | MUL_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [ `Vector of
                ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                * ([< `B | `H | `S] as 'w) ] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | MVN_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | NEG_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | NOP : (singleton, unit) t
    | ORR_immediate :
        ( triple,
          [`Reg of [`GP of [< `X]]]
          * [`Reg of [`GP of [< `X | `XZR]]]
          * [< `Bitmask] )
        t
    | ORR_shifted_register :
        ( quad,
          [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
          * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
          * [`Reg of [`GP of [< `X | `W | `XZR | `WZR]]]
          * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
        )
        t
    | ORR_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | RBIT :
        (pair, [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]]) t
    | RET : (singleton, unit) t
    | REV :
        (pair, [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]]) t
    | REV16 :
        (pair, [`Reg of [`GP of ([< `X | `W] as 'w)]] * [`Reg of [`GP of 'w]]) t
    | SBFM :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Imm of [`Six]]
          * [`Imm of [`Six]] )
        t
    | SCVTF :
        ( pair,
          [`Reg of [`Neon of [< `Scalar of [< `S | `D]]]]
          * [`Reg of [`GP of [< `X]]] )
        t
    | SCVTF_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | SDIV :
        ( triple,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]] )
        t
    | SHL :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Shift_by_element_width of 'w] )
        t
    | SMAX_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [ `Vector of
                ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                * ([< any_width] as 'w) ] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | SMIN_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [ `Vector of
                ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                * ([< any_width] as 'w) ] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | SMOV :
        ('elem, 'gp) Smov_element_to_GP.t * Neon_reg_name.Lane_index.t
        -> ( pair,
             [`Reg of [`GP of 'gp]]
             * [`Reg of [`Neon of [`Vector of [< any_vector] * 'elem]]] )
           t
    | SMULH :
        ( triple,
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
    | SQADD_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | SQSUB_vector :
        ( triple,
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
    | SSHL_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | SSHR :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Shift_by_element_width of 'w] )
        t
    | STP :
        ('w1, 'w2) LDP_STP_width.t
        -> ( triple,
             [`Reg of [`GP of 'w1]]
             * [`Reg of [`GP of 'w2]]
             * [`Mem of [`Offset_pair | `Pre_pair | `Post_pair]] )
           t
    | STR :
        ( pair,
          [`Reg of [`GP of [< `X | `W | `LR]]] * [`Mem of Addressing_mode.single]
        )
        t
    | STRB :
        (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | STRH :
        (pair, [`Reg of [`GP of [< `W]]] * [`Mem of Addressing_mode.single]) t
    | STR_simd_and_fp :
        ( pair,
          [`Reg of [`Neon of [< `Scalar of [< `D | `S | `Q]]]]
          * [`Mem of Addressing_mode.single] )
        t
    | SUBS_immediate :
        ( quad,
          [`Reg of [`GP of [< `W | `WZR | `X | `XZR]]]
          * [`Reg of [`GP of [< `W | `X | `SP]]]
          * [`Imm of [< `Twelve]]
          * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
        t
    | SUBS_shifted_register :
        ( quad,
          [`Reg of [`GP of [< `X | `XZR]]]
          * [`Reg of [`GP of [< `X | `SP]]]
          * [`Reg of [`GP of [< `X]]]
          * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
        )
        t
    | SUB_immediate :
        ( quad,
          [`Reg of [`GP of [< `X | `SP]]]
          * [`Reg of [`GP of [< `X | `SP]]]
          * [`Imm of [< `Twelve]]
          * [`Optional of [`Fixed_shift of [`Lsl_by_twelve]] option] )
        t
    | SUB_shifted_register :
        ( quad,
          [`Reg of [`GP of ([< `X | `W] as 'w)]]
          * [`Reg of [`GP of 'w]]
          * [`Reg of [`GP of 'w]]
          * [`Optional of [`Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] option]
        )
        t
    | SUB_vector :
        ( triple,
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
    | TBNZ :
        ( triple,
          [`Reg of [`GP of [`X]]] * [`Imm of [`Six]] * [`Imm of [`Sym of _]] )
        t
    | TBZ :
        ( triple,
          [`Reg of [`GP of [`X]]] * [`Imm of [`Six]] * [`Imm of [`Sym of _]] )
        t
    | TST : (pair, [`Reg of [`GP of [< `X]]] * [< `Bitmask]) t
    | UADDLP_vector :
        ( pair,
          [ `Reg of
            [ `Neon of
              [ `Vector of
                [< `V4H | `V8H | `V2S | `V4S | `V1D | `V2D] * [< any_width] ] ]
          ]
          * [ `Reg of
              [ `Neon of
                [ `Vector of
                  [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                ] ] ] )
        t
    | UBFM :
        ( quad,
          [`Reg of [`GP of [< `X | `W]]]
          * [`Reg of [`GP of [< `X | `W]]]
          * [`Imm of [`Six]]
          * [`Imm of [`Six]] )
        t
    | UMAX_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [ `Vector of
                ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                * ([< any_width] as 'w) ] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | UMIN_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [ `Vector of
                ([< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] as 'v)
                * ([< any_width] as 'w) ] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | UMOV :
        ('elem, 'gp) Element_to_GP.t * Neon_reg_name.Lane_index.t
        -> ( pair,
             [`Reg of [`GP of 'gp]]
             * [`Reg of [`Neon of [`Vector of [< any_vector] * 'elem]]] )
           t
    | UMULH :
        ( triple,
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
    | UQADD_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | UQSUB_vector :
        ( triple,
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
    | USHL_vector :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | USHR :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Shift_by_element_width of 'w] )
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
    | ZIP1 :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t
    | ZIP2 :
        ( triple,
          [ `Reg of
            [ `Neon of
              [`Vector of ([< any_vector] as 'v) * ([< any_width] as 'w)] ] ]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]]
          * [`Reg of [`Neon of [`Vector of 'v * 'w]]] )
        t

  type ('num_operands, 'operands) instr = ('num_operands, 'operands) t

  module Wrapped = struct
    type t = I : (_, _) instr -> t

    let to_string t =
      match t with
      | I instr -> (
        match instr with
        | ABS_vector -> "abs"
        | ADD_immediate | ADD_shifted_register | ADD_vector -> "add"
        | ADDP_vector -> "addp"
        | ADDS -> "adds"
        | ADDV -> "addv"
        | ADR -> "adr"
        | ADRP -> "adrp"
        | AND_immediate | AND_shifted_register | AND_vector -> "and"
        | ASRV -> "asrv"
        | B -> "b"
        | B_cond c -> "b." ^ Branch_cond.to_string c
        | BL -> "bl"
        | BLR -> "blr"
        | BR -> "br"
        | CBNZ -> "cbnz"
        | CBZ -> "cbz"
        | CLZ -> "clz"
        | CM_register cond -> "cm" ^ Simd_int_cmp.to_string cond
        | CM_zero cond -> "cm" ^ Simd_int_cmp.to_string cond
        | CNT -> "cnt"
        | CNT_vector -> "cnt"
        | CSEL -> "csel"
        | CSINC -> "csinc"
        | CTZ -> "ctz"
        | DMB b -> "dmb\t" ^ Memory_barrier.to_string b
        | DSB b -> "dsb\t" ^ Memory_barrier.to_string b
        | DUP _ -> "dup"
        | EOR_immediate | EOR_shifted_register | EOR_vector -> "eor"
        | EXT -> "ext"
        | FABS -> "fabs"
        | FADD -> "fadd"
        | FADD_vector -> "fadd"
        | FADDP_vector -> "faddp"
        | FCM_register cond -> "fcm" ^ Float_cond.to_string cond
        | FCM_zero cond -> "fcm" ^ Float_cond.to_string cond
        | FCMP -> "fcmp"
        | FCSEL -> "fcsel"
        | FCVT -> "fcvt"
        | FCVTL_vector -> "fcvtl"
        | FCVTN_vector -> "fcvtn"
        | FCVTNS -> "fcvtns"
        | FCVTNS_vector -> "fcvtns"
        | FCVTZS -> "fcvtzs"
        | FCVTZS_vector -> "fcvtzs"
        | FDIV -> "fdiv"
        | FDIV_vector -> "fdiv"
        | FMADD -> "fmadd"
        | FMAX -> "fmax"
        | FMAX_vector -> "fmax"
        | FMIN -> "fmin"
        | FMIN_vector -> "fmin"
        | FMOV_fp | FMOV_gp_to_fp_32 | FMOV_gp_to_fp_64 | FMOV_fp_to_gp_32
        | FMOV_fp_to_gp_64 | FMOV_scalar_immediate ->
          "fmov"
        | FMSUB -> "fmsub"
        | FMUL -> "fmul"
        | FMUL_vector -> "fmul"
        | FNEG -> "fneg"
        | FNEG_vector -> "fneg"
        | FNMADD -> "fnmadd"
        | FNMSUB -> "fnmsub"
        | FNMUL -> "fnmul"
        | FRECPE_vector -> "frecpe"
        | FRINT rm -> "frint" ^ Rounding_mode.to_string rm
        | FRINT_vector rm -> "frint" ^ Rounding_mode.to_string rm
        | FRSQRTE_vector -> "frsqrte"
        | FSQRT -> "fsqrt"
        | FSQRT_vector -> "fsqrt"
        | FSUB -> "fsub"
        | FSUB_vector -> "fsub"
        | INS _ -> "ins"
        | INS_V _ -> "ins"
        | LDAR -> "ldar"
        | LDP _ -> "ldp"
        | LDR -> "ldr"
        | LDR_simd_and_fp -> "ldr"
        | LDRB -> "ldrb"
        | LDRH -> "ldrh"
        | LDRSB -> "ldrsb"
        | LDRSH -> "ldrsh"
        | LDRSW -> "ldrsw"
        | LSLV -> "lslv"
        | LSRV -> "lsrv"
        | MADD -> "madd"
        | MOVI -> "movi"
        | MOVK -> "movk"
        | MOVN -> "movn"
        | MOVZ -> "movz"
        | MSUB -> "msub"
        | MUL_vector -> "mul"
        | MVN_vector -> "mvn"
        | NEG_vector -> "neg"
        | NOP -> "nop"
        | ORR_immediate | ORR_shifted_register | ORR_vector -> "orr"
        | RBIT -> "rbit"
        | RET -> "ret"
        | REV -> "rev"
        | REV16 -> "rev16"
        | SBFM -> "sbfm"
        | SCVTF -> "scvtf"
        | SCVTF_vector -> "scvtf"
        | SDIV -> "sdiv"
        | SHL -> "shl"
        | SMAX_vector -> "smax"
        | SMIN_vector -> "smin"
        | SMOV _ -> "smov"
        | SMULH -> "smulh"
        | SMULL2_vector _ -> "smull2"
        | SMULL_vector _ -> "smull"
        | SQADD_vector -> "sqadd"
        | SQSUB_vector -> "sqsub"
        | SQXTN _ -> "sqxtn"
        | SQXTN2 _ -> "sqxtn2"
        | SSHL_vector -> "sshl"
        | SSHR -> "sshr"
        | STP _ -> "stp"
        | STR -> "str"
        | STR_simd_and_fp -> "str"
        | STRB -> "strb"
        | STRH -> "strh"
        | SUB_immediate | SUB_shifted_register | SUB_vector -> "sub"
        | SUBS_immediate | SUBS_shifted_register -> "subs"
        | SXTL _ -> "sxtl"
        | TBNZ -> "tbnz"
        | TBZ -> "tbz"
        | TST -> "tst"
        | UADDLP_vector -> "uaddlp"
        | UBFM -> "ubfm"
        | UMAX_vector -> "umax"
        | UMIN_vector -> "umin"
        | UMOV _ -> "umov"
        | UMULH -> "umulh"
        | UMULL2_vector _ -> "umull2"
        | UMULL_vector _ -> "umull"
        | UQADD_vector -> "uqadd"
        | UQSUB_vector -> "uqsub"
        | UQXTN _ -> "uqxtn"
        | UQXTN2 _ -> "uqxtn2"
        | USHL_vector -> "ushl"
        | USHR -> "ushr"
        | UXTL _ -> "uxtl"
        | XTN _ -> "xtn"
        | XTN2 _ -> "xtn2"
        | YIELD -> "yield"
        | ZIP1 -> "zip1"
        | ZIP2 -> "zip2")
  end

  module Untyped = struct
    let operands_as_array (type num operands) (instr : (num, operands) t)
        (ops : (num, operands) many) =
      let o = Operand.Wrapped.create in
      (* Local helper - duplicates DSL.imm but DSL is defined later *)
      let imm n : _ Operand.t = Imm (Twelve n) in
      (* Helper to convert a vector register operand to a lane-indexed scalar *)
      let vector_to_lane_operand (type a) (reg_op : a Operand.t)
          (lane : Neon_reg_name.Lane_index.t) =
        match reg_op with
        | Reg reg -> (
          let index = reg.index in
          match reg.reg_name with
          | Neon (Vector vector) ->
            let scalar_type = Neon_reg_name.Scalar.of_vector vector in
            let lane_reg_name =
              Reg_name.Neon (Lane { r = S scalar_type; lane })
            in
            Operand.Wrapped.create
              (Operand.Reg (Reg.create lane_reg_name index))
          | GP _ | Neon (Scalar _) | Neon (Lane _) ->
            failwith "vector_to_lane_operand: not a vector register")
        | Imm _ | Lsl_by_twelve | Shift _ | Lsl_by_multiple_of_16_bits _
        | Shift_by_element_width _ | Cond _ | Float_cond _ | Mem _ | Bitmask _
        | Optional _ | Unit ->
          failwith "vector_to_lane_operand: not a register operand"
      in
      match instr with
      | ABS_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | ADDP_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ADDS -> (
        let (Quad (rd, rn, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rn; o imm |]
        | Optional (Some shift) -> [| o rd; o rn; o imm; o shift |])
      | ADDV ->
        let (Pair (rd, src)) = ops in
        [| o rd; o src |]
      | ADD_immediate -> (
        let (Quad (rd, rs, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o imm |]
        | Optional (Some shift) -> [| o rd; o rs; o imm; o shift |])
      | ADD_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | ADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ADR ->
        let (Pair (rd, label)) = ops in
        [| o rd; o label |]
      | ADRP ->
        let (Pair (rd, symbol)) = ops in
        [| o rd; o symbol |]
      | AND_immediate ->
        let (Triple (rd, rs, bitmask)) = ops in
        [| o rd; o rs; o bitmask |]
      | AND_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | AND_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ASRV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | B ->
        let (Singleton target) = ops in
        [| o target |]
      | BL ->
        let (Singleton target) = ops in
        [| o target |]
      | BLR ->
        let (Singleton rn) = ops in
        [| o rn |]
      | BR ->
        let (Singleton rn) = ops in
        [| o rn |]
      | B_cond _ ->
        let (Singleton target) = ops in
        [| o target |]
      | CBNZ ->
        let (Pair (reg, target)) = ops in
        [| o reg; o target |]
      | CBZ ->
        let (Pair (reg, target)) = ops in
        [| o reg; o target |]
      | CLZ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | CM_register _cond ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | CM_zero _cond ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn; o (imm 0) |]
      | CNT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | CNT_vector ->
        let (Pair (rd, src)) = ops in
        [| o rd; o src |]
      | CSEL ->
        let (Quad (rd, rn, rm, cond)) = ops in
        [| o rd; o rn; o rm; o cond |]
      | CSINC ->
        let (Quad (rd, rn, rm, cond)) = ops in
        [| o rd; o rn; o rm; o cond |]
      | CTZ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | DMB _ -> [||]
      | DSB _ -> [||]
      | DUP lane ->
        let (Pair (rd, rs)) = ops in
        [| o rd; vector_to_lane_operand rs lane |]
      | EOR_immediate ->
        let (Triple (rd, rs, bitmask)) = ops in
        [| o rd; o rs; o bitmask |]
      | EOR_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | EOR_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | EXT ->
        let (Quad (rd, rs1, rs2, idx)) = ops in
        [| o rd; o rs1; o rs2; o idx |]
      | FABS ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FADD ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FADDP_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FCMP ->
        let (Pair (rn, rm)) = ops in
        [| o rn; o rm |]
      | FCM_register _cond ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FCM_zero _cond ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn; o (Imm (Float 0.)) |]
      | FCSEL ->
        let (Quad (rd, rn, rm, cond)) = ops in
        [| o rd; o rn; o rm; o cond |]
      | FCVT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FCVTL_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FCVTNS ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FCVTNS_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FCVTN_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FCVTZS ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FCVTZS_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FDIV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FDIV_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FMADD ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FMAX ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FMAX_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FMIN ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FMIN_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FMOV_fp ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FMOV_gp_to_fp_32 ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FMOV_gp_to_fp_64 ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FMOV_fp_to_gp_32 ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FMOV_fp_to_gp_64 ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FMOV_scalar_immediate ->
        let (Pair (rd, imm)) = ops in
        [| o rd; o imm |]
      | FMSUB ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FMUL ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FMUL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FNEG ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FNEG_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FNMADD ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FNMSUB ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FNMUL ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FRECPE_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FRINT _ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FRINT_vector _ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FRSQRTE_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FSQRT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FSQRT_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FSUB ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FSUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | INS (_, lane) ->
        let (Pair (rd, rs)) = ops in
        [| vector_to_lane_operand rd lane; o rs |]
      | INS_V lanes ->
        let (Pair (rd, rs)) = ops in
        [| vector_to_lane_operand rd
             (Neon_reg_name.Lane_index.Src_and_dest.dest_index lanes);
           vector_to_lane_operand rs
             (Neon_reg_name.Lane_index.Src_and_dest.src_index lanes)
        |]
      | LDAR ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDP _ ->
        let (Triple (rt1, rt2, addr)) = ops in
        [| o rt1; o rt2; o addr |]
      | LDR ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRB ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRH ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRSB ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRSH ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRSW ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDR_simd_and_fp ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LSLV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | LSRV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | MADD ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | MOVI ->
        let (Pair (rd, imm)) = ops in
        [| o rd; o imm |]
      | MOVK ->
        let (Triple (rd, imm, shift)) = ops in
        [| o rd; o imm; o shift |]
      | MOVN -> (
        let (Triple (rd, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o imm |]
        | Optional (Some shift) -> [| o rd; o imm; o shift |])
      | MOVZ -> (
        let (Triple (rd, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o imm |]
        | Optional (Some shift) -> [| o rd; o imm; o shift |])
      | MSUB ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | MUL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | MVN_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | NEG_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | NOP -> [||]
      | ORR_immediate ->
        let (Triple (rd, rs, bitmask)) = ops in
        [| o rd; o rs; o bitmask |]
      | ORR_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | ORR_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | RBIT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | RET -> [||]
      | REV ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | REV16 ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | SBFM ->
        let (Quad (rd, rn, immr, imms)) = ops in
        [| o rd; o rn; o immr; o imms |]
      | SCVTF ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | SCVTF_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | SDIV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | SHL ->
        let (Triple (rd, rs, imm)) = ops in
        [| o rd; o rs; o imm |]
      | SMAX_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SMIN_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SMOV (_, lane) ->
        let (Pair (rd, rs)) = ops in
        [| o rd; vector_to_lane_operand rs lane |]
      | SMULH ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | SMULL2_vector _ ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SMULL_vector _ ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SQADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SQSUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SQXTN _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | SQXTN2 _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | SSHL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SSHR ->
        let (Triple (rd, rs, imm)) = ops in
        [| o rd; o rs; o imm |]
      | STP _ ->
        let (Triple (rt1, rt2, addr)) = ops in
        [| o rt1; o rt2; o addr |]
      | STR ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | STRB ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | STRH ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | STR_simd_and_fp ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | SUBS_immediate -> (
        let (Quad (rd, rn, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rn; o imm |]
        | Optional (Some shift) -> [| o rd; o rn; o imm; o shift |])
      | SUBS_shifted_register -> (
        let (Quad (rd, rn, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rn; o reg |]
        | Optional (Some shift) -> [| o rd; o rn; o reg; o shift |])
      | SUB_immediate -> (
        let (Quad (rd, rs, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o imm |]
        | Optional (Some shift) -> [| o rd; o rs; o imm; o shift |])
      | SUB_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | SUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SXTL _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | TBNZ ->
        let (Triple (reg, bit, target)) = ops in
        [| o reg; o bit; o target |]
      | TBZ ->
        let (Triple (reg, bit, target)) = ops in
        [| o reg; o bit; o target |]
      | TST ->
        let (Pair (rn, op2)) = ops in
        [| o rn; o op2 |]
      | UADDLP_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | UBFM ->
        let (Quad (rd, rn, immr, imms)) = ops in
        [| o rd; o rn; o immr; o imms |]
      | UMAX_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UMIN_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UMOV (_, lane) ->
        let (Pair (rd, rs)) = ops in
        [| o rd; vector_to_lane_operand rs lane |]
      | UMULH ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | UMULL2_vector _ ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UMULL_vector _ ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UQADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UQSUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UQXTN _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | UQXTN2 _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | USHL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | USHR ->
        let (Triple (rd, rs, imm)) = ops in
        [| o rd; o rs; o imm |]
      | UXTL _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | XTN _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | XTN2 _ ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | YIELD -> [||]
      | ZIP1 ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ZIP2 ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
  end
end

module Instruction = struct
  type t =
    | I :
        { name : ('num, 'operands) Instruction_name.t;
          operands : ('num, 'operands) many
        }
        -> t

  let create name ~operands = I { name; operands }

  let print ppf (I { name; operands }) =
    let name_str = Instruction_name.Wrapped.to_string (I name) in
    let operands_arr =
      Instruction_name.Untyped.operands_as_array name operands
    in
    if Array.length operands_arr = 0
    then Format.fprintf ppf "%s" name_str
    else (
      Format.fprintf ppf "%s\t" name_str;
      Array.iteri
        (fun i op ->
          if i > 0 then Format.fprintf ppf ", ";
          Operand.Wrapped.print ppf op)
        operands_arr)
end

module DSL = struct
  open Addressing_offset

  let symbol (type w) (s : w Symbol.t) : _ Operand.t = Imm (Sym s)

  let mem ~(base : [`GP of [< `X | `SP]] Reg.t) = Operand.Mem (Reg base)

  let check_nine_signed_unscaled offset =
    if offset < -256 || offset > 255
    then
      Misc.fatal_errorf
        "Offset %d out of range for 9-bit signed unscaled (-256 to 255)" offset

  let check_seven_signed_scaled offset =
    if offset mod 8 <> 0
    then
      Misc.fatal_errorf "Offset %d must be a multiple of 8 for pair addressing"
        offset;
    if offset < -512 || offset > 504
    then
      Misc.fatal_errorf
        "Offset %d out of range for 7-bit signed scaled (-512 to 504)" offset

  module Validated_mem_offset = struct
    type operand_kind =
      | Scaled
      | Unscaled

    type t =
      { scale : int;
        offset : int;
        kind : operand_kind
      }

    (* Validate an offset for use with load/store instructions.

       - Use unsigned scaled encoding for non-negative, aligned offsets.

       - Use signed unscaled encoding for negative or unaligned offsets.

       The scaled encoding requires alignment to access size and the offset
       divided by scale must fit in 12 bits. *)
    let create ~scale ~offset =
      let max_scaled_offset = (4096 - 1) * scale in
      if offset >= 0 && offset mod scale = 0 && offset <= max_scaled_offset
      then Some { scale; offset; kind = Scaled }
      else if offset >= -256 && offset <= 255
      then Some { scale; offset; kind = Unscaled }
      else None

    let to_operand ~(base : [`GP of [< `X | `SP]] Reg.t) t =
      match t.kind with
      | Scaled ->
        Operand.Mem
          (Offset_twelve_unsigned_scaled (base, Twelve_unsigned_scaled t.offset))
      | Unscaled ->
        Operand.Mem
          (Offset_nine_signed_unscaled (base, Nine_signed_unscaled t.offset))

    let offset t = t.offset

    let scale t = t.scale

    let is_valid ~scale ~offset = Option.is_some (create ~scale ~offset)

    (* Pre-validated offset for probe semaphore access (2-byte at offset 2) *)
    let probe_semaphore_offset = { scale = 2; offset = 2; kind = Scaled }
  end

  type 'a mem_offset_result =
    | Ok of 'a
    | Offset_out_of_range

  let mem_offset ~(base : [`GP of [< `X | `SP]] Reg.t) ~scale ~offset =
    match Validated_mem_offset.create ~scale ~offset with
    | Some validated -> Ok (Validated_mem_offset.to_operand ~base validated)
    | None -> Offset_out_of_range

  let mem_symbol ~(base : [`GP of [< `X | `SP]] Reg.t) ~symbol =
    Operand.Mem (Offset_sym (base, symbol))

  let mem_pre ~(base : [`GP of [< `X | `SP]] Reg.t) ~offset =
    check_nine_signed_unscaled offset;
    Operand.Mem (Pre (base, Imm (Nine_signed_unscaled offset)))

  let mem_post ~(base : [`GP of [< `X | `SP]] Reg.t) ~offset =
    check_nine_signed_unscaled offset;
    Operand.Mem (Post (base, Imm (Nine_signed_unscaled offset)))

  let mem_offset_pair ~(base : [`GP of [< `X | `SP]] Reg.t) ~offset =
    check_seven_signed_scaled offset;
    Operand.Mem (Offset_pair (base, Imm (Seven_signed_scaled offset)))

  let mem_pre_pair ~(base : [`GP of [< `X | `SP]] Reg.t) ~offset =
    check_seven_signed_scaled offset;
    Operand.Mem (Pre_pair (base, Imm (Seven_signed_scaled offset)))

  let mem_post_pair ~(base : [`GP of [< `X | `SP]] Reg.t) ~offset =
    check_seven_signed_scaled offset;
    Operand.Mem (Post_pair (base, Imm (Seven_signed_scaled offset)))

  let shift ~kind ~amount = Operand.Shift { kind; amount = Six amount }

  let optional_shift ~kind ~amount =
    Operand.Optional (Some (shift ~kind ~amount))

  (** Create an LSL shift for MOVK/MOVN/MOVZ. The amount must be one of 0, 16,
      32, or 48. Returns X-typed shift since 32 and 48 are X-only. *)
  let lsl_by_multiple_of_16_bits (amount : int) :
      [`Lsl_by_multiple_of_16_bits of [`X]] Operand.t =
    match amount with
    | 0 -> Lsl_by_multiple_of_16_bits S0
    | 16 -> Lsl_by_multiple_of_16_bits S16
    | 32 -> Lsl_by_multiple_of_16_bits S32
    | 48 -> Lsl_by_multiple_of_16_bits S48
    | _ ->
      Misc.fatal_errorf
        "lsl_by_multiple_of_16_bits: invalid shift amount %d (must be \
         0,16,32,48)"
        amount

  (** Create an LSL shift for W-form MOVK/MOVN/MOVZ. Only accepts 0 or 16. *)
  let lsl_by_multiple_of_16_bits_w (amount : int) :
      [`Lsl_by_multiple_of_16_bits of [`W]] Operand.t =
    match amount with
    | 0 -> Lsl_by_multiple_of_16_bits S0
    | 16 -> Lsl_by_multiple_of_16_bits S16
    | _ ->
      Misc.fatal_errorf
        "lsl_by_multiple_of_16_bits_w: invalid shift amount %d (must be 0 or \
         16)"
        amount

  let optional_lsl_by_multiple_of_16_bits (amount : int) :
      [`Optional of [`Lsl_by_multiple_of_16_bits of [`X]] option] Operand.t =
    Operand.Optional (Some (lsl_by_multiple_of_16_bits amount))

  let optional_lsl_by_multiple_of_16_bits_w (amount : int) :
      [`Optional of [`Lsl_by_multiple_of_16_bits of [`W]] option] Operand.t =
    Operand.Optional (Some (lsl_by_multiple_of_16_bits_w amount))

  (** Create a shift amount for SHL/SSHR/USHR on B (8-bit) elements. Valid
      range: 0-7. *)
  let shift_by_element_width_b (amount : int) :
      [`Shift_by_element_width of [`B]] Operand.t =
    if amount < 0 || amount > 7
    then
      Misc.fatal_errorf
        "shift_by_element_width_b: invalid shift amount %d (must be 0-7)" amount
    else Shift_by_element_width (For_B amount)

  (** Create a shift amount for SHL/SSHR/USHR on H (16-bit) elements. Valid
      range: 0-15. *)
  let shift_by_element_width_h (amount : int) :
      [`Shift_by_element_width of [`H]] Operand.t =
    if amount < 0 || amount > 15
    then
      Misc.fatal_errorf
        "shift_by_element_width_h: invalid shift amount %d (must be 0-15)"
        amount
    else Shift_by_element_width (For_H amount)

  (** Create a shift amount for SHL/SSHR/USHR on S (32-bit) elements. Valid
      range: 0-31. *)
  let shift_by_element_width_s (amount : int) :
      [`Shift_by_element_width of [`S]] Operand.t =
    if amount < 0 || amount > 31
    then
      Misc.fatal_errorf
        "shift_by_element_width_s: invalid shift amount %d (must be 0-31)"
        amount
    else Shift_by_element_width (For_S amount)

  (** Create a shift amount for SHL/SSHR/USHR on D (64-bit) elements. Valid
      range: 0-63. *)
  let shift_by_element_width_d (amount : int) :
      [`Shift_by_element_width of [`D]] Operand.t =
    if amount < 0 || amount > 63
    then
      Misc.fatal_errorf
        "shift_by_element_width_d: invalid shift amount %d (must be 0-63)"
        amount
    else Shift_by_element_width (For_D amount)

  let optional_none = Operand.Optional None

  let unit_operand = Operand.Unit

  let reglane index ~lane r =
    let reg_name = Reg_name.(Neon Neon_reg_name.(Lane { r; lane })) in
    Operand.Reg (Reg.create reg_name index)

  let reglane_v4s index ~lane =
    let r = Neon_reg_name.(Lane.V Vector.V4S) in
    reglane index ~lane r

  let reglane_v2d index ~lane =
    let r = Neon_reg_name.(Lane.V Vector.V2D) in
    reglane index ~lane r

  (* [reglane_*]: Clang 17 assembler does not accept optional number of lanes
     notation of the form Vn.4S[lane], even though it is required to do so in
     ARMARM. Emit Vn.S[lane]. *)
  let reglane_b index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.B) in
    reglane index ~lane r

  let reglane_h index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.H) in
    reglane index ~lane r

  let reglane_s index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.S) in
    reglane index ~lane r

  let reglane_d index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.D) in
    reglane index ~lane r

  let reg_v2s index : _ Operand.t = Reg (Reg.reg_v2s index)

  let reg_v4s index : _ Operand.t = Reg (Reg.reg_v4s index)

  let reg_v2d index : _ Operand.t = Reg (Reg.reg_v2d index)

  let reg_v8b index : _ Operand.t = Reg (Reg.reg_v8b index)

  let reg_v16b index : _ Operand.t = Reg (Reg.reg_v16b index)

  let reg_v8h index : _ Operand.t = Reg (Reg.reg_v8h index)

  let reg_v4h index : _ Operand.t = Reg (Reg.reg_v4h index)

  let reg_b index : _ Operand.t = Reg (Reg.reg_b index)

  let reg_s index : _ Operand.t = Reg (Reg.reg_s index)

  let reg_d index : _ Operand.t = Reg (Reg.reg_d index)

  let reg_q index : _ Operand.t = Reg (Reg.reg_q index)

  let reg_w index : _ Operand.t = Reg (Reg.reg_w index)

  let reg_x index : _ Operand.t = Reg (Reg.reg_x index)

  let sp : _ Operand.t = Reg Reg.sp

  let lr : _ Operand.t = Reg Reg.lr

  let fp : _ Operand.t = Reg Reg.fp

  let xzr : _ Operand.t = Reg Reg.xzr

  let wzr : _ Operand.t = Reg Reg.wzr

  let reg_op reg = Operand.Reg reg

  let imm n : _ Operand.t = Imm (Twelve n)

  let imm_six n : _ Operand.t = Imm (Six n)

  let imm_sixteen n : _ Operand.t = Imm (Sixteen_unsigned n)

  let imm_sixteen_of_nativeint n : _ Operand.t =
    if Nativeint.compare n 0n < 0 || Nativeint.compare n 0xFFFFn > 0
    then
      Misc.fatal_errorf
        "imm_sixteen_of_nativeint: value %nd out of range [0, 65535]" n;
    Imm (Sixteen_unsigned (Nativeint.to_int n))

  let imm_float f : _ Operand.t = Imm (Float f)

  let imm_nativeint n : _ Operand.t = Imm (Nativeint n)

  let bitmask n =
    if not (Logical_immediates.is_logical_immediate n)
    then
      Misc.fatal_errorf
        "Cannot encode logical immediate %nd as a bitmask immediate" n;
    Operand.Bitmask n

  let cond c = Operand.Cond c

  (* CR sspies: probably these should be part of the instruction name instead *)
  let float_cond c = Operand.Float_cond c

  let print_ins name operands =
    Format.asprintf "%a" Instruction.print (Instruction.create name ~operands)

  (* CR mshinwell: Acc should not be in this file *)
  module Acc = struct
    let emit_string = ref None

    let emit_instruction : (Instruction.t -> unit) option ref = ref None

    let set_emit_string ~emit_string:emit = emit_string := Some emit

    let set_emit_instruction ~emit_instruction:emit =
      emit_instruction := Some emit

    let clear_emit_instruction () = emit_instruction := None

    let ins (type num a) (name : (num, a) Instruction_name.t)
        (operands : (num, a) many) =
      let instr = Instruction.create name ~operands in
      (* Emit to binary emitter if configured *)
      (match !emit_instruction with Some emit -> emit instr | None -> ());
      (* Emit to text if configured *)
      let str = Format.asprintf "\t%a\n" Instruction.print instr in
      match !emit_string with None -> () | Some emit_string -> emit_string str

    let with_measuring ~f =
      let saved_emit_string = !emit_string in
      let saved_emit_instruction = !emit_instruction in
      let count = ref 0 in
      emit_string := None;
      emit_instruction := Some (fun _ -> incr count);
      f ();
      emit_string := saved_emit_string;
      emit_instruction := saved_emit_instruction;
      !count

    let ins1 name a = ins name (Singleton a)

    let ins2 name a b = ins name (Pair (a, b))

    let ins3 name a b c = ins name (Triple (a, b, c))

    let ins4 name a b c d = ins name (Quad (a, b, c, d))

    let ins0 name = ins name (Singleton Operand.Unit)

    module Tupled = struct
      let ins2 name (a, b) = ins2 name a b

      let ins3 name (a, b, c) = ins3 name a b c

      let ins4 name (a, b, c, d) = ins4 name a b c d
    end

    (* Instructions that are expanded into others *)
    let ins_mul rd rn rm = ins MADD (Quad (rd, rn, rm, reg_op Reg.xzr))

    (* LSL <Xd>, <Xn>, #<shift> -> UBFM <Xd>, <Xn>, #(-<shift> MOD 64),
       #(63-<shift>) *)
    let ins_lsl_immediate rd rn ~shift_in_bits =
      (* CR mshinwell: range checks on shift? *)
      let n = -shift_in_bits in
      let n' = if n < 0 then n + 64 else n in
      let immr : _ Operand.t = Imm (Six n') in
      let imms : _ Operand.t = Imm (Six (63 - shift_in_bits)) in
      ins UBFM (Quad (rd, rn, immr, imms))

    (* LSR <Xd>, <Xn>, #<shift> -> UBFM <Xd>, <Xn>, #<shift>, #63 *)
    let ins_lsr_immediate rd rn ~shift_in_bits =
      (* CR mshinwell: range checks on shift? *)
      let immr : _ Operand.t = Imm (Six shift_in_bits) in
      let imms : _ Operand.t = Imm (Six 63) in
      ins UBFM (Quad (rd, rn, immr, imms))

    (* ASR <Xd>, <Xn>, #<shift> -> SBFM <Xd>, <Xn>, #<shift>, #63 *)
    let ins_asr_immediate rd rn ~shift_in_bits =
      (* CR mshinwell: range checks on shift? *)
      let immr : _ Operand.t = Imm (Six shift_in_bits) in
      let imms : _ Operand.t = Imm (Six 63) in
      ins SBFM (Quad (rd, rn, immr, imms))

    (* UXTB <Wd>, <Wn> -> UBFM <Wd>, <Wn>, #0, #7 Note: The type signature
       enforces matching register widths. Using W registers produces the
       canonical UXTB encoding. X registers would work via UBFM but emit.ml only
       uses W registers. *)
    let ins_uxtb wd wn =
      let immr : _ Operand.t = Imm (Six 0) in
      let imms : _ Operand.t = Imm (Six 7) in
      ins UBFM (Quad (wd, wn, immr, imms))

    (* UXTH <Wd>, <Wn> -> UBFM <Wd>, <Wn>, #0, #15 *)
    let ins_uxth wd wn =
      let immr : _ Operand.t = Imm (Six 0) in
      let imms : _ Operand.t = Imm (Six 15) in
      ins UBFM (Quad (wd, wn, immr, imms))

    (* CMP <Rn|SP>, #<imm>{, <shift>} -> SUBS ZR, <Rn|SP>, #<imm>{, <shift>} *)
    let ins_cmp rn imm shift_opt =
      let is_32bit = match rn with Operand.Reg r -> Reg.gp_sf r = 0 in
      if is_32bit
      then ins SUBS_immediate (Quad (wzr, rn, imm, shift_opt))
      else ins SUBS_immediate (Quad (xzr, rn, imm, shift_opt))

    (* CMP <Xn>, <Xm>{, <shift>} -> SUBS XZR, <Xn>, <Xm>{, <shift>} Note:
       shifted register form is 64-bit only *)
    let ins_cmp_reg rn rm shift_opt =
      ins SUBS_shifted_register (Quad (xzr, rn, rm, shift_opt))

    (* CMN <Xn|SP>, #<imm>{, <shift>} -> ADDS XZR, <Xn|SP>, #<imm>{, <shift>}
       Note: type signature restricts to X registers *)
    let ins_cmn rn imm shift_opt = ins ADDS (Quad (xzr, rn, imm, shift_opt))

    (* CSET <Xd>, <invcond> -> CSINC <Xd>, XZR, XZR, <cond> *)
    let ins_cset rd invcond =
      ins CSINC
        (Quad
           ( rd,
             reg_op Reg.xzr,
             reg_op Reg.xzr,
             Operand.Cond (Cond.invert invcond) ))

    (* MOV from SP -> ADD <Xd>, SP, #0 *)
    let ins_mov_from_sp ~dst:rd =
      ins ADD_immediate (Quad (rd, reg_op Reg.sp, imm 0, Optional None))

    (* MOV to SP -> ADD SP, <Xn>, #0 *)
    let ins_mov_to_sp ~src:rn =
      ins ADD_immediate (Quad (reg_op Reg.sp, rn, imm 0, Optional None))

    (* MOV <Vd>, <Vn> -> ORR <Vd>, <Vn>, <Vn> *)
    let ins_mov_vector rd rn = ins ORR_vector (Triple (rd, rn, rn))

    (* MOV <Xd>, <Xm> -> ORR <Xd>, XZR, <Xm> *)
    let ins_mov_reg rd (rm : [`Reg of [`GP of [< `X | `XZR]]] Operand.t) =
      ins ORR_shifted_register (Quad (rd, xzr, rm, Optional None))

    (* MOV <Wd>, <Wm> -> ORR <Wd>, WZR, <Wm> *)
    let ins_mov_reg_w rd rm =
      ins ORR_shifted_register (Quad (rd, wzr, rm, Optional None))

    (* MOV <Xd>, #<imm16> -> MOVZ <Xd>, #<imm16>, LSL #0 MOV <Wd>, #<imm16> ->
       MOVZ <Wd>, #<imm16>, LSL #0 *)
    let ins_mov_imm rd imm16 = ins MOVZ (Triple (rd, imm16, Optional None))
  end
end
