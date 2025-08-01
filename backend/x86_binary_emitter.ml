(***********************************************************************)
(*                                                                     *)
(*                              OCaml                                  *)
(*                                                                     *)
(*  Copyright 2014, OCamlPro. All rights reserved.                     *)
(*  All rights reserved. This file is distributed under the terms of   *)
(*  the GNU Lesser General Public License version 2.1                  *)
(*                                                                     *)
(***********************************************************************)
(*
  Contributors:
  * Fabrice LE FESSANT (INRIA/OCamlPro)
*)

[@@@ocaml.warning "+A-4-9-42-69"]
(* CR sspies: Warning 40 is enabled for this file, leading to unnecessarily
   verbose code (see [constant] below). Disable it in the future. *)

open! Int_replace_polymorphic_compare
open X86_ast
open X86_proc
module String = Misc.Stdlib.String


module D = Asm_targets.Asm_directives.Directive
module C = D.Constant


type section = {
  sec_name : string;
  mutable sec_instrs : asm_line array;
}

type data_size = B8 | B16 | B32 | B64

module IntSet = Set.Make (Int)

module StringMap = Map.Make (String)

let print_old_arg ppf = function
  | Imm _ -> Format.fprintf ppf "Imm"
  | Reg8L _ -> Format.fprintf ppf "Reg8L"
  | Reg8H _ -> Format.fprintf ppf "Reg8H"
  | Reg16 _ -> Format.fprintf ppf "Reg16"
  | Reg32 _ -> Format.fprintf ppf "Reg32"
  | Reg64 _ -> Format.fprintf ppf "Reg64"
  | Regf _ -> Format.fprintf ppf "Regf"
  | Mem _ -> Format.fprintf ppf "Mem"
  | Mem64_RIP _ -> Format.fprintf ppf "Mem64_RIP"
  | Sym _ -> Format.fprintf ppf "Sym"

(*
TODO:

If a or-pattern contains both "Reg64 ... | Reg32 ... ", it means that
we didn't discriminate between 32 bit and 64 bit modes for that
instruction. It also means that using this instruction on a 32-bit
register in 64 bit mode will not generate the 32-bit version of the
instruction, but the 64-bit version...

*)

module Relocation = struct
  module Kind = struct
    type t =
      (* 32 bits offset usually in data section *)
      | REL32 of string * int64
      | DIR32 of string * int64
      | DIR64 of string * int64
  end

  type t = { offset_from_section_beginning : int; kind : Kind.t }
end

type symbol_binding = Sy_local | Sy_global | Sy_weak

type symbol = {
  sy_name : string;
  mutable sy_type : Asm_targets.Asm_directives.symbol_type option;
  mutable sy_size : int option;
  mutable sy_binding : symbol_binding;
  mutable sy_protected : bool;
  mutable sy_sec : section;
  mutable sy_pos : int option;
  mutable sy_num : int option; (* position in .symtab *)
}

type buffer = {
  sec : section;
  buf : Buffer.t;
  labels : symbol String.Tbl.t;
  mutable patches : (int * data_size * int64) list;
  mutable relocations : Relocation.t list;
}

type local_reloc =
  | RelocCall of string
  | RelocShortJump of string * int (* loc *)
  | RelocLongJump of string
  | RelocConstant of C.t * data_size

type result =
  | Rint of int64
  | Rabs of string * int64 (* absolute label + offset *)
  | Rrel of string * int64

(* relative label + offset *)

(*
let string_of_result = function
  Rint n -> Printf.sprintf "Rint %Ld" n
  | Rabs (s, n) -> Printf.sprintf "Rabs (%S, %Ld)" s n
  | Rrel (s, n) -> Printf.sprintf "Rrel (%S, %Ld)" s n
*)

let get_symbol b s =
  try String.Tbl.find b.labels s
  with Not_found ->
    let sy =
      {
        sy_name = s;
        sy_type = None;
        sy_size = None;
        sy_pos = None;
        sy_binding = Sy_local;
        sy_protected = false;
        sy_num = None;
        sy_sec = b.sec;
      }
    in
    String.Tbl.add b.labels s sy ;
    sy

let buf_int8 b i = Buffer.add_char b.buf (char_of_int (i land 0xff))

let buf_int8L b iL = buf_int8 b (Int64.to_int iL)

let buf_int16L b iL =
  buf_int8L b iL;
  buf_int8L b (Int64.shift_right iL 8)

let buf_int32L b iL =
  buf_int16L b iL;
  buf_int16L b (Int64.shift_right iL 16)

let buf_int64L b iL =
  buf_int32L b iL;
  buf_int32L b (Int64.shift_right iL 32)

let str_int8L s pos v = Bytes.set s pos (char_of_int (Int64.to_int v land 0xff))

let str_int16L s pos v =
  str_int8L s pos v;
  str_int8L s (pos + 1) (Int64.shift_right_logical v 8)

let str_int32L s pos v =
  str_int16L s pos v;
  str_int16L s (pos + 2) (Int64.shift_right_logical v 16)

let str_int64L s pos v =
  str_int32L s pos v;
  str_int32L s (pos + 4) (Int64.shift_right_logical v 32)

(* When a jump has to be generated, we compare the offset between the
   source instruction and the target instruction, in number of
   instructions.

   If the offset is less than [short_jump_threshold] instructions,
   we generate a short jump during the first pass. 16 is a "safe"
   value, as most instructions are shorter than 8 bytes: [REX] +
   [OPCODE] + [MODRM] + [SIB] + [IMM32] *)

let local_relocs = ref []

let local_labels = String.Tbl.create 100

let forced_long_jumps = ref IntSet.empty

let instr_size = ref 4

let new_buffer sec =
  {
    sec;
    buf = Buffer.create 10000;
    labels = String.Tbl.create 100;
    relocations = [];
    patches = [];
  }

let label_pos b lbl =
  match (String.Tbl.find b.labels lbl).sy_pos with
  | None -> raise Not_found
  | Some pos -> pos

(* Try to compute some statically computable arithmetic expressions
   in labels, or to simplify them to a form that is encodable by
   relocations. *)
let eval_const b current_pos cst =
  let rec eval = function
    | C.Signed_int n -> Rint n
    | C.Unsigned_int n -> Rint (Numbers.Uint64.to_int64 n)
    | C.This -> Rabs ("", 0L)
    | C.Named_thing lbl -> Rabs (lbl, 0L)
    | C.Sub (c1, c2) -> (
        let c1 = eval c1 and c2 = eval c2 in
        match (c1, c2) with
        | Rint n1, Rint n2 -> Rint (Int64.sub n1 n2)
        | Rabs (s, n1), Rint n2 -> Rabs (s, Int64.sub n1 n2)
        | Rrel (s, n1), Rint n2 -> Rrel (s, Int64.sub n1 n2)
        | Rabs ("", n1), Rabs ("", n2) -> Rint (Int64.sub n1 n2)
        | Rabs ("", n1), Rabs (s2, n2) -> (
            try
              let sy2 = String.Tbl.find b.labels s2 in
              match sy2.sy_pos with
              | Some pos2 ->
                  let pos2 = Int64.of_int pos2 in
                  Rint
                    (Int64.sub
                       (Int64.add n1 (Int64.of_int current_pos))
                       (Int64.add pos2 n2))
              | _ -> assert false
            with Not_found -> assert false)
        | Rabs (s, n1), Rabs ("", n2) -> (
            try
              let sy = String.Tbl.find b.labels s in
              match sy.sy_pos with
              | Some pos ->
                  let pos = Int64.of_int pos in
                  Rint
                    (Int64.sub (Int64.add pos n1)
                       (Int64.add n2 (Int64.of_int current_pos)))
              | _ -> assert false
            with Not_found -> Rrel (s, Int64.sub n1 n2))
        | Rabs (s1, n1), Rabs (s2, n2) -> (
            try
              let sy2 = String.Tbl.find b.labels s2 in
              try
                let sy1 = String.Tbl.find b.labels s1 in
                assert (sy1.sy_sec == sy2.sy_sec);
                match (sy1.sy_pos, sy2.sy_pos) with
                | Some pos1, Some pos2 ->
                    let pos1 = Int64.of_int pos1 in
                    let pos2 = Int64.of_int pos2 in
                    Rint (Int64.sub (Int64.add pos1 n1) (Int64.add pos2 n2))
                | _ -> assert false
              with Not_found -> (
                match sy2.sy_pos with
                | Some pos2 ->
                    let pos2 = Int64.of_int pos2 in
                    Rrel
                      ( s1,
                        Int64.sub
                          (Int64.add n1 (Int64.of_int current_pos))
                          (Int64.add pos2 n2) )
                | _ -> assert false)
            with Not_found -> assert false)
        | _ -> assert false)
    | C.Add (c1, c2) -> (
        let c1 = eval c1 and c2 = eval c2 in
        match (c1, c2) with
        | Rint n1, Rint n2 -> Rint (Int64.add n1 n2)
        | Rabs (s, n1), Rint n2 | Rint n2, Rabs (s, n1) ->
            Rabs (s, Int64.add n1 n2)
        | Rrel (s, n1), Rint n2 | Rint n2, Rrel (s, n1) ->
            Rrel (s, Int64.add n1 n2)
        (* TODO: we could add another case, easy to solve: adding a
           Rrel to a Rabs where the symbol is local, in which case it
           can be computed. *)
        | Rrel (s, n1), Rabs ("", n2) -> Rabs (s, Int64.add n1 n2)
        | _ -> assert false)
  in
  try
    let r = eval cst in
    (*
    if debug then
      Printf.eprintf "eval_const (%s) = %s at @%d\n%!"
        (X86_gas.string_of_constant cst)
        (string_of_result r) current_pos;
*)
    r
  with e ->
    Printf.eprintf "Error in eval_const: exception %S\n%!"
      (*(X86_gas.string_of_constant cst)*) (Printexc.to_string e);
    raise e

let is_imm32L n = Int64.compare n 0x8000_0000L < 0 && Int64.compare n (-0x8000_0000L) >= 0

let is_imm8L x = Int64.compare x 128L < 0 && Int64.compare x (-128L) >= 0

let is_imm16L n = Int64.compare n 32768L < 0 && Int64.compare n (-32768L) >= 0

let is_x86 = function | X86 -> true | X64 -> false

let rd_of_regf regf =
  match regf with
  | XMM n | YMM n | ZMM n -> n

let rd_of_reg64 = function
  | RAX -> 0
  | RCX -> 1
  | RDX -> 2
  | RBX -> 3
  | RSP -> 4
  | RBP -> 5
  | RSI -> 6
  | RDI -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15

let rd_of_reg8 = function
  | Reg8L r -> rd_of_reg64 r
  | Reg8H AH -> 4
  | Reg8H CH -> 5
  | Reg8H DH -> 6
  | Reg8H BH -> 7
  | _ -> assert false

let cd_of_condition condition =
  match condition with
  | O -> 0
  | NO -> 1
  | B -> 2
  | AE -> 3
  | E -> 4
  | NE -> 5
  | BE -> 6
  | A -> 7
  | S -> 8
  | NS -> 9
  | P -> 10
  | NP -> 11
  | L -> 12
  | GE -> 13
  | LE -> 14
  | G -> 15

(* We should precompute a position for each label depending on
   the number of instructions: heuristics = offset_in_instrs x 7
*)

let no_rex = 0

let rex = 0b01000000

let rexr = 0b00000100 (* extension of r *)

let rexr_reg reg = if reg > 7 then rexr else 0

let rexw = rex lor 0b00001000

let rexx = 0b00000010

let rexx_index reg = if reg > 7 then rexx else 0

let rexb = 0b00000001

let rexb_opcode reg = if reg > 7 then rexb else 0

let rexb_rm reg = if reg > 7 then rexb else 0

let rexb_base reg = if reg > 7 then rexb else 0

let reg7 reg = reg land 0x07

let rex_of_reg8 = function Reg8L (RSP | RBP | RSI | RDI) -> rex | _ -> 0

(* TODO: we should check conformance with page 3-2, vol 2A of Intel Spec ? *)

let rex_of_reg16 = function
  | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI -> 0
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -> rex

let mod_rm_reg m rm reg = (m lsl 6) + reg7 rm + (reg7 reg lsl 3)

let sib scale index base =
  let scale =
    match scale with 1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3 | _ -> assert false
  in
  (scale lsl 6) lor (reg7 index lsl 3) lor reg7 base

let record_reloc b offset_from_section_beginning kind =
  b.relocations <-
    { Relocation.offset_from_section_beginning; kind } :: b.relocations

let declare_label b s =
  let sy = get_symbol b s in
  assert (Option.is_none sy.sy_pos);
  let pos = Buffer.length b.buf in
  sy.sy_pos <- Some pos

let buf_opcodes b opcodes =
  ListLabels.iter ~f:(fun opcode -> buf_int8 b opcode) opcodes

let arch64 = String.equal Config.architecture "amd64"

let emit_rex b rexcode =
  if arch64 && rexcode <> 0 then buf_int8 b (rexcode lor rex)

let buf_int16_imm b = function
  | Imm n ->
      assert (is_imm16L n);
      buf_int16L b n
  | _ -> assert false

let buf_int32_imm b = function
  | Imm n ->
      assert (is_imm32L n);
      buf_int32L b n
  | Sym symbol ->
      record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR32 (symbol, 0L));
      buf_int32L b 0L
  | _ -> assert false

type offset_exp = OImm8 of int64 | OImm32 of string option * int64

let sym32 b sym =
  record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR32 (sym, 0L));
  buf_int32L b 0L

let sym64 b sym =
  record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR64 (sym, 0L));
  buf_int64L b 0L

let buf_sym b sym offset =
  match sym with
  | None -> buf_int32L b offset
  | Some lbl ->
      (* TODO: assert we are in 32 bits ? *)
      record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR32 (lbl, offset));
      buf_int32L b 0L

let emit_prefix_modrm b opcodes rm reg ~prefix =
  (* When required for a particular instruction, the REX / REXW flag is added in
     [emit_mod_rm_reg]. This function otherwise assumes [~rex:0] for Reg32,
     Reg64, Regf, and addressing modes. *)
  match rm with
  | Reg32 rm ->
      let rm = rd_of_reg64 rm in
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | Reg64 rm ->
      let rm = rd_of_reg64 rm in
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | (Reg8L _ | Reg8H _) as reg8 ->
      let rm = rd_of_reg8 reg8 in
      prefix b ~rex:(rex_of_reg8 reg8) ~rexr:(rexr_reg reg)
               ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | Reg16 reg16 ->
      let rm = rd_of_reg64 reg16 in
      prefix b ~rex:(rex_of_reg16 reg16) ~rexr:(rexr_reg reg)
               ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | Regf regf ->
      let rm = rd_of_regf regf in
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  (* 64 bits memory access *)
  | Mem64_RIP (_, symbol, offset) ->
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:0 ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b00 0b101 reg);
      record_reloc b (Buffer.length b.buf)
        (Relocation.Kind.REL32 (symbol, Int64.of_int offset));
      buf_int32L b 0L
  | Mem { arch; typ = _; idx; scale; base; sym; displ } -> (
      let offset =
        let displ = Int64.of_int displ in
        match sym with
        | None ->
            if is_imm8L displ then OImm8 displ
            else if is_imm32L displ then OImm32 (None, displ)
            else assert false
        | Some s -> OImm32 (Some s, displ)
      in
      let idx_reg = idx in
      let idx = rd_of_reg64 idx in
      if scale = 0 then (
        assert (Option.is_none base && (is_x86 arch));
        match offset with
        | OImm8 _ -> assert false
        | OImm32 (sym, offset) ->
            (* No prefix; 32-bit mode. *)
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 0b101 reg);
            buf_sym b sym offset)
      else
        match base with
        | None -> (
            match (idx_reg, scale, offset) with
            | (RSP | R12), 1, OImm8 0L ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b00 idx reg);
                buf_int8 b (sib 1 0b100 idx)
            | (RSP | R12), 1, OImm8 offset8 ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b01 0b100 reg);
                buf_int8 b (sib 1 0b100 idx);
                buf_int8L b offset8
            | (RSP | R12), 1, OImm32 (sym, offset) ->
                (* to 0x??(%rsp) *)
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b10 0b100 reg);
                buf_int8 b (sib 1 0b100 idx);
                buf_sym b sym offset
            | (RBP | R13), 1, OImm8 _ -> (
                (* to 0x??(%rbp) *)
                (* TODO check if offset8 = 0 is enough *)
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b01 idx reg);
                match offset with
                | OImm8 offset8 -> buf_int8L b offset8
                | _ -> assert false)
            | _, 1, OImm8 0L ->
                (* to 0x00(%r??) except %rsp and %rbp *)
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_rm idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b00 idx reg)
            | _, 1, OImm8 offset8 ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_rm idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b01 idx reg);
                buf_int8L b offset8
            | _, 1, OImm32 (sym, offset) ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_rm idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b10 idx reg);
                buf_sym b sym offset
            | _, _, _ -> (
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:0 ~rexx:(rexx_index idx);
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b00 0b100 reg);
                buf_int8 b (sib scale idx 0b101);
                match offset with
                | OImm8 offset8 -> buf_int32L b offset8
                | OImm32 (sym, offset) -> buf_sym b sym offset))
        | Some base_reg -> (
            assert (scale = 1 || scale = 2 || scale = 4 || scale = 8);
            let base = rd_of_reg64 base_reg in
            prefix b ~rex:0 ~rexr:(rexr_reg reg)
                     ~rexb:(rexb_base base) ~rexx:(rexx_index idx);
            buf_opcodes b opcodes;
            match (base_reg, offset) with
            | (RBP | R13), OImm8 0L ->
                (* to 0x00(%rbp+reg) *)
                buf_int8 b (mod_rm_reg 0b01 0b100 reg);
                buf_int8 b (sib scale idx base);
                buf_int8 b 0
            | _, OImm8 0L ->
                buf_int8 b (mod_rm_reg 0b00 0b100 reg);
                buf_int8 b (sib scale idx base)
            | _, OImm8 offset ->
                buf_int8 b (mod_rm_reg 0b01 0b100 reg);
                buf_int8 b (sib scale idx base);
                buf_int8L b offset
            | _, OImm32 (sym, offset) ->
                buf_int8 b (mod_rm_reg 0b10 0b100 reg);
                buf_int8 b (sib scale idx base);
                buf_sym b sym offset))
  | Imm _ | Sym _ -> assert false

let emit_mod_rm_reg b rex_always opcodes rm reg =
  emit_prefix_modrm b opcodes rm reg ~prefix:(fun b ~rex ~rexr ~rexb ~rexx ->
    emit_rex b (rex_always lor rex lor rexr lor rexb lor rexx))

let emit_bsf b ~dst ~src =
  match (dst, src) with
  | Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm)
  | Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSF r16, r/m16 and BSF r32, r/m32 *)
    emit_mod_rm_reg b 0 [ 0x0F; 0xBC ] rm (rd_of_reg64 reg)
  | Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSF r64, r/m64 *)
    emit_mod_rm_reg b rexw [ 0x0F; 0xBC ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_bsr b ~dst ~src =
  match (dst, src) with
  | Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm)
  | Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSR r16, r/m16 and BSR r32, r/m32 *)
    emit_mod_rm_reg b 0 [ 0x0F; 0xBD ] rm (rd_of_reg64 reg)
  | Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSR r64, r/m64 *)
    emit_mod_rm_reg b rexw [ 0x0F; 0xBD ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_MOV b dst src =
  match (dst, src) with
  (* movb *)
  | ((Reg8L (RAX | RCX | RDX | RBX) | Reg8H _) as r8), Imm n ->
      assert (is_imm8L n);
      buf_opcodes b [ 0xB0 + reg7 (rd_of_reg8 r8) ];
      buf_int8L b n
  | ((Mem _ | Mem64_RIP _) as rm), ((Reg8L _ | Reg8H _) as reg) ->
      emit_mod_rm_reg b (rex_of_reg8 reg) [ 0x88 ] rm (rd_of_reg8 reg)
  (* no REX.W *)
  (* movw *)
  | ((Mem _ | Mem64_RIP _) as rm), Reg16 reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b rex [ 0x89 ] rm (rd_of_reg64 reg) (* no REX.W *)
  | Reg16 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b rex [ 0x8B ] rm (rd_of_reg64 reg) (* no REX.W *)
  (* movl *)
  | Reg32 reg32, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg32 in
      emit_mod_rm_reg b 0 [ 0x8B ] rm reg
  | ((Mem _ | Mem64_RIP _) as rm), Reg32 reg32 ->
      let reg = rd_of_reg64 reg32 in
      emit_mod_rm_reg b 0 [ 0x89 ] rm reg
  | (Mem { typ = DWORD } as rm), ((Imm _ | Sym _) as n) ->
      emit_mod_rm_reg b 0 [ 0xC7 ] rm 0;
      buf_int32_imm b n
  | (Mem { typ = NONE; arch = X86 } as rm), ((Imm _ | Sym _) as n) ->
      let reg = 0 in
      emit_mod_rm_reg b 0 [ 0xC7 ] rm reg;
      buf_int32_imm b n
  | Reg32 r32, ((Imm _ | Sym _) as n) ->
      let n =
        match n with
        | Imm n ->
            (* "Shift" [n] from [0, 0xFFFF_FFFF] to [-0x8000_0000, 0x7FFF_FFFF] *)
            Imm (Int64.of_int32 (Int64.to_int32 n))
        | _ as n -> n
      in
      let reg = rd_of_reg64 r32 in
      emit_rex b (rexb_opcode reg);
      buf_int8 b (0xB8 lor reg7 reg);
      buf_int32_imm b n
  (* movq *)
  | Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
      emit_mod_rm_reg b rexw [ 0x8B ] rm (rd_of_reg64 reg)
  | ((Mem _ | Mem64_RIP _) as rm), Reg64 reg ->
      emit_mod_rm_reg b rexw [ 0x89 ] rm (rd_of_reg64 reg)
  | Reg64 r64, Imm n when not (is_imm32L n) ->
      (* MOVNoneQ *)
      let reg = rd_of_reg64 r64 in
      emit_rex b (rexw lor rexb_opcode reg);
      buf_int8 b (0xB8 lor reg7 reg);
      buf_int64L b n
  | Reg64 r64, Sym symbol when windows ->
      let reg = rd_of_reg64 r64 in
      emit_rex b (rexw lor rexb_opcode reg);
      buf_int8 b (0xB8 lor reg7 reg);
      sym64 b symbol
  | ((Mem { arch = X64 } | Reg64 _) as rm), ((Imm _ | Sym _) as n) ->
      emit_mod_rm_reg b rexw [ 0xC7 ] rm 0;
      buf_int32_imm b n
  | _ ->
      Format.printf "dst = %a@." print_old_arg dst;
      Format.printf "src = %a@." print_old_arg src;
      assert false

let emit_vex3 buf ~rexr ~rexx ~rexb ~vex_m ~vex_w ~vex_v ~vex_l ~vex_p =
  buf_int8 buf 0xC4; (* We only emit 3-byte VEX instructions. *)
  buf_int8 buf (((rexr lxor 1) lsl 7) lor
                ((rexx lxor 1) lsl 6) lor
                ((rexb lxor 1) lsl 5) lor
                vex_m);
  buf_int8 buf ((vex_w lsl 7) lor
                ((vex_v lxor 15) lsl 3) lor
                (vex_l lsl 2) lor
                vex_p)

let vex_prefix_adaptor f =
  fun b ~rex:_ ~rexr ~rexb ~rexx ->
    let rexr = if rexr <> 0 then 1 else 0 in
    let rexb = if rexb <> 0 then 1 else 0 in
    let rexx = if rexx <> 0 then 1 else 0 in
    f b ~rexr ~rexx ~rexb

let emit_vex_rm_reg b ops rm reg ~vex_m ~vex_w ~vex_v ~vex_l ~vex_p =
  let vex_w, vex_l = Bool.to_int vex_w, Bool.to_int vex_l in
  emit_prefix_modrm b ops rm reg ~prefix:(vex_prefix_adaptor (fun b ~rexr ~rexx ~rexb ->
    emit_vex3 b ~rexr ~rexx ~rexb ~vex_m ~vex_w ~vex_v ~vex_l ~vex_p))

let rd_of_reg = function
  | Regf reg -> rd_of_regf reg
  | Reg32 reg | Reg64 reg -> rd_of_reg64 reg
  | _ -> assert false

let emit_simd b (instr : Amd64_simd_instrs.instr) args =
  let open Amd64_simd_defs in
  let imm, args =
    let n = Array.length args in
    match instr.imm with
    | Imm_spec | Imm_reg ->
      Some args.(0), Array.sub args 1 (n - 1)
    | Imm_none -> None, args
  in
  let enc i =
    match instr.res with
    | First_arg | Res { enc = Implicit; _ } -> instr.args.(i).enc
    | Res { enc; _ } when i = 0 -> enc
    | Res _ -> instr.args.(i - 1).enc
  in
  let rm_only () =
    match args with
    | [| dst |] ->
      (match enc 0 with
      | RM_rm -> dst
      | _ -> failwith instr.mnemonic)
    | _ -> failwith instr.mnemonic
  in
  let rm_reg () =
    match args with
    | [| src; dst |] ->
      (match enc 1, enc 0 with
      | RM_rm, RM_r -> src, rd_of_reg dst
      | RM_r, RM_rm -> dst, rd_of_reg src
      | _ -> failwith instr.mnemonic)
    | _ -> failwith instr.mnemonic
  in
  let rm_vexv () =
    match args with
    | [| src; dst |] ->
      (match enc 1, enc 0 with
      | RM_rm, Vex_v -> src, rd_of_reg dst
      | Vex_v, RM_rm -> dst, rd_of_reg src
      | _ -> failwith instr.mnemonic)
    | _ -> failwith instr.mnemonic
  in
  let rm_vexv_reg () =
    match args with
    | [| src; dst |] ->
      (match enc 1, enc 0 with
      | RM_rm, RM_r -> src, 0, rd_of_reg dst
      | RM_r, RM_rm -> dst, 0, rd_of_reg src
      | _ -> failwith instr.mnemonic)
    | [| src2; src1; dst |] ->
      (match enc 2, enc 1, enc 0 with
      | RM_rm, Vex_v, RM_r -> src2, rd_of_reg src1, rd_of_reg dst
      | RM_r, Vex_v, RM_rm -> dst, rd_of_reg src1, rd_of_reg src2
      | _ -> failwith instr.mnemonic)
    | _ -> failwith instr.mnemonic
  in
  let emit_legacy_prefix = function
    | Prx_none -> ()
    | Prx_66 -> buf_int8 b 0x66
    | Prx_F3 -> buf_int8 b 0xF3
    | Prx_F2 -> buf_int8 b 0xF2
  in
  let legacy_escape = function
    | Esc_none -> [instr.enc.opcode]
    | Esc_0F -> [0x0F; instr.enc.opcode]
    | Esc_0F38 -> [0x0F; 0x38; instr.enc.opcode]
    | Esc_0F3A -> [0x0F; 0x3A; instr.enc.opcode]
  in
  let mk_rex = function
    | Rex_none -> no_rex
    | Rex -> rex
    | Rex_w -> rexw
  in
  let vex_map = function
    | Vexm_0F -> 1
    | Vexm_0F38 -> 2
    | Vexm_0F3A -> 3
  in
  let vex_prefix = function
    | Prx_none -> 0
    | Prx_66 -> 1
    | Prx_F3 -> 2
    | Prx_F2 -> 3
  in
  (match instr.enc.rm_reg, instr.enc.prefix with
  | Spec rmod, Legacy { prefix; rex; escape } ->
    let rm = rm_only () in
    emit_legacy_prefix prefix;
    emit_mod_rm_reg b (mk_rex rex) (legacy_escape escape) rm rmod
  | Reg, Legacy { prefix; rex; escape } ->
    let rm, reg = rm_reg () in
    emit_legacy_prefix prefix;
    emit_mod_rm_reg b (mk_rex rex) (legacy_escape escape) rm reg
  | Reg, Vex { vex_m; vex_w; vex_l; vex_p } ->
    let rm, vex_v, reg = rm_vexv_reg () in
    emit_vex_rm_reg b [instr.enc.opcode] rm reg
      ~vex_m:(vex_map vex_m) ~vex_w ~vex_v ~vex_l ~vex_p:(vex_prefix vex_p)
  | Spec rmod, Vex { vex_m; vex_w; vex_l; vex_p } ->
    let rm, vex_v = rm_vexv () in
    emit_vex_rm_reg b [instr.enc.opcode] rm rmod
      ~vex_m:(vex_map vex_m) ~vex_w ~vex_v ~vex_l ~vex_p:(vex_prefix vex_p));
  match imm with
  | Some (Imm imm) -> buf_int8 b (Int64.to_int imm)
  | Some (Regf (XMM n | YMM n | ZMM n)) -> buf_int8 b (n lsl 4)
  | Some _ -> failwith instr.mnemonic
  | None -> ()

type simple_encoding = {
  rm8_r8 : int list;
  rm64_r64 : int list;
  r8_rm8 : int list;
  r64_rm64 : int list;
  al_imm8 : int list;
  rax_imm32 : int list;
  rm8_imm8 : int list;
  rm16_imm16 : int list;
  rm64_imm32 : int list;
  rm64_imm8 : int list;
  reg : int;
}

let emit_simple_encoding enc b dst src =
  match (enc, dst, src) with
  (* 64 bits encodings *)
  | { rm64_r64 = opcodes }, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Reg64 reg
    ->
      emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)
  | { rm64_r64 = opcodes }, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Reg32 reg
    ->
      emit_mod_rm_reg b 0 opcodes rm (rd_of_reg64 reg)
  | { r64_rm64 = opcodes }, Reg64 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)
  | { r64_rm64 = opcodes }, Reg32 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      emit_mod_rm_reg b 0 opcodes rm (rd_of_reg64 reg)
  | ( { rm64_imm8 = opcodes; reg },
      ((Reg64 _ | Mem { typ = NONE | QWORD | REAL8; arch = X64 }) as rm),
      Imm n )
    when is_imm8L n ->
      emit_mod_rm_reg b rexw opcodes rm reg;
      buf_int8L b n
  | ( { rm8_imm8 = opcodes; reg },
      ((Reg8L _ | Reg8H _ | Mem { typ = BYTE; arch = X64 }) as rm),
      Imm n ) ->
      assert (is_imm8L n);
      emit_mod_rm_reg b rexw opcodes rm reg;
      buf_int8L b n
  | ( { rm64_imm8 = opcodes; reg },
      ((Reg32 _ | Mem { typ = DWORD | REAL4 } | Mem { typ = NONE; arch = X86 })
      as rm),
      Imm n )
    when is_imm8L n ->
      emit_mod_rm_reg b 0 opcodes rm reg;
      buf_int8L b n
  | { rax_imm32 = opcodes }, Reg64 RAX, ((Imm _ | Sym _) as n) ->
      emit_rex b rexw;
      buf_opcodes b opcodes;
      buf_int32_imm b n
  | { rax_imm32 = opcodes }, Reg32 RAX, ((Imm _ | Sym _) as n) ->
      buf_opcodes b opcodes;
      buf_int32_imm b n
  | ( { rm16_imm16 = opcodes; reg },
      ((Reg16 _ | Mem { typ = WORD })
      as rm),
      (Imm _ as n) ) ->
      emit_mod_rm_reg b 0 opcodes rm reg;
      buf_int16_imm b n
  | ( { rm64_imm32 = opcodes; reg },
      ((Reg32 _ | Mem { typ = NONE; arch = X86 } | Mem { typ = DWORD | REAL4 })
      as rm),
      ((Imm _ | Sym _) as n) ) ->
      emit_mod_rm_reg b 0 opcodes rm reg;
      buf_int32_imm b n
  | ( { rm64_imm32 = opcodes; reg },
      ((Reg64 _ | Mem _ | Mem64_RIP _) as rm),
      ((Imm _ | Sym _) as n) ) ->
      emit_mod_rm_reg b rexw opcodes rm reg;
      buf_int32_imm b n
  | _ ->
      Format.eprintf "src=%a dst=%a@." print_old_arg src print_old_arg dst;
      assert false

let emit_simple_encoding base reg =
  emit_simple_encoding
    {
      rm8_r8 = [ base ];
      rm64_r64 = [ base + 1 ];
      r8_rm8 = [ base + 2 ];
      r64_rm64 = [ base + 3 ];
      al_imm8 = [ base + 4 ];
      rax_imm32 = [ base + 5 ];
      rm8_imm8 = [ 0x80 ];
      rm16_imm16 = [ 0x81 ];
      rm64_imm32 = [ 0x81 ];
      rm64_imm8 = [ 0x83 ];
      reg;
    }

let emit_ADD = emit_simple_encoding 0x00 0

let emit_OR = emit_simple_encoding 0x08 1

let emit_AND = emit_simple_encoding 0x20 4

let emit_SUB = emit_simple_encoding 0x28 5

let emit_XOR = emit_simple_encoding 0x30 6

let emit_CMP = emit_simple_encoding 0x38 7

let emit_test b dst src =
  match (dst, src) with
  | ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Reg32 reg ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x85 ] rm reg
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Reg64 reg ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x85 ] rm reg
  | Reg64 RAX, ((Imm _ | Sym _) as n) ->
      emit_rex b rexw;
      buf_opcodes b [ 0xA9 ];
      buf_int32_imm b n
  | Reg32 RAX, ((Imm _ | Sym _) as n) ->
      buf_opcodes b [ 0xA9 ];
      buf_int32_imm b n
  | ((Reg32 _ | Reg64 _ | Mem _ | Mem64_RIP _) as rm), ((Imm _ | Sym _) as n) ->
      emit_mod_rm_reg b rexw [ 0xF7 ] rm 0;
      buf_int32_imm b n
  | Reg8L RAX, Imm n ->
      assert (is_imm8L n);
      buf_opcodes b [ 0xA8 ];
      buf_int8L b n
  | ((Reg8L _ | Reg8H _) as rm), Imm n ->
      assert (is_imm8L n);
      emit_mod_rm_reg b 0 [ 0xF6 ] rm 0;
      buf_int8L b n
  | _ -> assert false

(* 3-390 -> 452 *)
let emit_imul b dst src =
  match (dst, src) with
  | Some (Reg32 reg), ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x0F; 0xAF ] rm reg
  | Some (Reg64 reg), ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xAF ] rm reg
  | Some ((Reg64 reg | Reg32 reg) as rm), Imm n when is_imm8L n ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x6B ] rm reg;
      buf_int8L b n
  | Some ((Reg64 reg | Reg32 reg) as rm), ((Imm _ | Sym _) as n) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x69 ] rm reg;
      buf_int32_imm b n
  | None, ((Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = 5 in
      emit_mod_rm_reg b rexw [ 0xF7 ] rm reg
  | _ -> assert false

let emit_mul b ~src =
  let opcode_extension = 4 in
  match src with
  | ((Reg8H _ | Reg8L _ | Mem {typ = BYTE; _} | Mem64_RIP (BYTE, _, _)) as rm) ->
    emit_mod_rm_reg b rex [ 0xF6 ] rm opcode_extension
  | ((Reg16 _ | Mem {typ = WORD; _} | Mem64_RIP (WORD, _, _)) as rm)
  | ((Reg32 _ | Mem {typ = DWORD; _} | Mem64_RIP (DWORD, _, _)) as rm) ->
    emit_mod_rm_reg b no_rex [ 0xF7 ] rm opcode_extension
  | ((Reg64 _ | Mem {typ = QWORD; _} | Mem64_RIP (QWORD, _, _)) as rm) ->
    emit_mod_rm_reg b rexw [ 0xF7 ] rm opcode_extension
  | _ -> assert false

let emit_idiv b dst =
  let reg = 7 in
  match dst with
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b rexw [ 0xF7 ] rm reg
  | _ -> assert false

let emit_shift reg b dst src =
  match (dst, src) with
  | ((Reg64 _ | Reg32 _ | Mem _) as rm), Imm 1L ->
      emit_mod_rm_reg b rexw [ 0xD1 ] rm reg
  | ((Reg64 _ | Reg32 _ | Mem _) as rm), Imm n ->
      assert (is_imm8L n);
      emit_mod_rm_reg b rexw [ 0xC1 ] rm reg;
      buf_int8L b n
  | ((Reg64 _ | Reg32 _) as rm), Reg8L RCX ->
      emit_mod_rm_reg b rexw [ 0xD3 ] rm reg
  | _ ->
      Format.eprintf "emit_shift: src=%a dst=%a@." print_old_arg src
        print_old_arg dst;
      assert false

let emit_SAL b dst src = emit_shift 4 b dst src

let emit_SHR b dst src = emit_shift 5 b dst src

let emit_SAR b dst src = emit_shift 7 b dst src

let record_local_reloc b ?(offset=0) local_reloc =
  local_relocs := (Buffer.length b.buf + offset, local_reloc) :: !local_relocs

let emit_reloc_jump near_opcodes far_opcodes b loc symbol =
  if String.Tbl.mem local_labels symbol then
    (* local_reloc *)
    let target_loc = String.Tbl.find local_labels symbol in
    if target_loc < loc then (
      (* backward *)
      (* The target position is known, and so is the actual offset.  We can
         thus decide locally if a short jump can be used. *)
      let target_pos =
        try label_pos b symbol with Not_found -> assert false
      in
      let source_pos = Buffer.length b.buf in
      assert (target_pos < source_pos);
      let togo = Int64.of_int (target_pos - source_pos) in
      let togo_short =
        Int64.sub togo (Int64.of_int (1 + List.length near_opcodes))
      in

      (*      Printf.printf "%s/%i: backward  togo_short=%Ld\n%!" symbol loc togo_short; *)
      if Int64.compare togo_short (-128L)  >= 0 && Int64.compare togo_short 128L < 0 then (
        buf_opcodes b near_opcodes;
        buf_int8L b togo_short)
      else (
        buf_opcodes b far_opcodes;
        buf_int32L b
          (Int64.sub togo (Int64.of_int (4 + List.length far_opcodes)))))
    else
      (* forward *)
      (* Is the target too far forward (in term of instruction count)
         or have we detected previously that this jump instruction needs
         to be a long one?

         The str_size constant (see below) is chosen to avoid a second
         pass most oftenm while not being overly pessimistic. *)

      (*
      if Int64.of_int ((target_loc - loc) * !instr_size) >= 120L then
        Printf.printf "%s/%i: probably too far (%i)\n%!" symbol loc target_loc
      else if IntSet.mem loc !forced_long_jumps then
        Printf.printf "%s/%i: forced long jump\n%!" symbol loc
      else
        Printf.printf "%s/%i: short\n%!" symbol loc;
*)
      let force_far =
        Int64.compare (Int64.of_int ((target_loc - loc) * !instr_size)) 120L >= 0
        || IntSet.mem loc !forced_long_jumps
      in
      if force_far then (
        buf_opcodes b far_opcodes;
        record_local_reloc b (RelocLongJump symbol);
        buf_int32L b 0L)
      else (
        buf_opcodes b near_opcodes;
        record_local_reloc b (RelocShortJump (symbol, loc));
        buf_int8L b 0L)
  else (
    (* external symbol, must reloc *)

    (*    Printf.printf "%s/%i: non local\n%!" symbol loc; *)
    buf_opcodes b far_opcodes;
    record_reloc b (Buffer.length b.buf) (Relocation.Kind.REL32 (symbol, 0L));
    buf_int32L b 0L)

let emit_jmp b loc dst =
  match dst with
  | Sym symbol -> emit_reloc_jump [ 0xEB ] [ 0xE9 ] b loc symbol
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      let reg = 4 in
      emit_mod_rm_reg b 0 [ 0xFF ] rm reg
      (* no REX *)
  | _ -> assert false

let emit_call b dst =
  match dst with
  | Sym symbol ->
      buf_int8 b 0xE8;
      if String.Tbl.mem local_labels symbol then
        record_local_reloc b (RelocCall symbol)
      else
        (* external symbol, must reloc *)
        record_reloc b (Buffer.length b.buf)
          (Relocation.Kind.REL32 (symbol, 0L));
      buf_int32L b 0L
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b no_rex [ 0xFF ] rm 2
  | _ -> assert false

let emit_j b loc condition dst =
  match dst with
  | Sym symbol ->
      let opcode_offset = cd_of_condition condition in
      emit_reloc_jump [ 0x70 + opcode_offset ]
        [ 0x0F; 0x80 + opcode_offset ]
        b loc symbol
  | _ -> assert false

let emit_cmov b condition dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm)
    ->
      emit_mod_rm_reg b rexw
        [ 0x0F; 0x40 + cd_of_condition condition ]
        rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_set b condition dst =
  match dst with
  | (Reg8L _ | Reg8H _) as rm ->
      emit_mod_rm_reg b 0 [ 0x0F; 0x90 + cd_of_condition condition ] rm 0
  | _ -> assert false

let emit_movsx b dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Mem { typ = BYTE } | Reg8L _ | Reg8H _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rex [ 0x0F; 0xBE ] rm reg
      (* no REX.W *)
  | (Reg64 reg | Reg32 reg), ((Mem { typ = WORD } | Reg16 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xBF ] rm reg
  | _ -> assert false

let emit_movsxd b dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Mem _ | Mem64_RIP _ | Reg32 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x63 ] rm reg
  | _ -> assert false

let emit_MOVZX b dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Mem { typ = BYTE } | Reg8L _ | Reg8H _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xB6 ] rm reg
  | Reg64 reg, ((Mem { typ = WORD } | Reg16 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xB7 ] rm reg
  | Reg32 reg, ((Mem { typ = WORD } | Reg16 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x0F; 0xB7 ] rm reg
  | _ -> assert false

let emit_neg b dst =
  match dst with
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b rexw [ 0xF7 ] rm 3
  | _ -> assert false

let emit_LEA b dst src =
  match (dst, src) with
  | Reg64 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x8D ] rm reg
  | Reg32 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x8D ] rm reg
  (*
        | Reg16 reg, (Mem _ | Mem64_RIP _ as rm) ->
        let reg = rd_of_reg64 reg in
        emit_mod_rm_reg b 0 [ 0x8D ] rm reg
    *)
  | _ ->
      Format.eprintf "lea src=%a dst=%a@." print_old_arg src print_old_arg dst;
      assert false

let emit_lock_op ~ops b dst src =
  let rex, rm, reg = match (dst, src) with
  | ((Mem _ | Mem64_RIP _) as rm), Reg64 reg ->
    rexw, rm, rd_of_reg64 reg
  | ((Mem _ | Mem64_RIP _) as rm), Reg32 reg ->
    no_rex, rm, rd_of_reg64 reg
  | _ ->
    Misc.fatal_errorf "lock op src=%a dst=%a@." print_old_arg src print_old_arg dst
  in
  buf_int8 b 0xF0;
  emit_mod_rm_reg b rex ops rm reg

let emit_lock_cmpxchg = emit_lock_op ~ops:[ 0x0F; 0xB1 ]
let emit_lock_xadd = emit_lock_op ~ops:[ 0x0F; 0xC1 ]
let emit_lock_add = emit_lock_op ~ops:[ 0x01 ]
let emit_lock_sub = emit_lock_op ~ops:[ 0x29 ]
let emit_lock_and = emit_lock_op ~ops:[ 0x21 ]
let emit_lock_or = emit_lock_op ~ops:[ 0x09 ]
let emit_lock_xor = emit_lock_op ~ops:[ 0x31 ]

let emit_stack_reg b opcode dst =
  match dst with
  | Reg64 reg ->
      let reg = rd_of_reg64 reg in
      if reg > 7 then emit_rex b (rex lor rexb_opcode reg);
      buf_int8 b (opcode + reg7 reg)
  | Reg32 reg ->
      let reg = rd_of_reg64 reg in
      buf_int8 b (opcode + reg7 reg)
  | _ -> assert false

let emit_push b dst =
  match dst with
  | Reg32 _ | Reg64 _ -> emit_stack_reg b 0x50 dst
  | (Mem _ | Mem64_RIP _) as rm -> emit_mod_rm_reg b no_rex [ 0xFF ] rm 6
  | Imm n ->
      if is_imm8L n then (
        buf_int8 b 0x6A;
        buf_int8L b n)
      else (
        assert (is_imm32L n);
        buf_int8 b 0x68;
        buf_int32L b n)
  | Sym sym ->
      buf_int8 b 0x68;
      sym32 b sym
  | _ -> assert false

let emit_pop b dst =
  match dst with
  | Reg32 _ | Reg64 _ -> emit_stack_reg b 0x58 dst
  | (Mem _ | Mem64_RIP _) as rm -> emit_mod_rm_reg b no_rex [ 0x8F ] rm 0
  | _ -> assert false

let emit_popcnt b ~dst ~src =
  match (dst, src) with
  | (Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm))
  | (Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* POPCNT r16, r/m16 and POPCNT r32, r/m32 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b no_rex [ 0x0F; 0xB8 ] rm (rd_of_reg64 reg);
  | (Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* POPCNT r64, r/m64 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b rexw [ 0x0F; 0xB8 ] rm (rd_of_reg64 reg);
  | _ -> assert false

let emit_tzcnt b ~dst ~src =
  match (dst, src) with
  | (Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm))
  | (Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* TZCNT r16, r/m16 and TZCNT r32, r/m32 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b no_rex [ 0x0F; 0xBC ] rm (rd_of_reg64 reg);
  | (Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* TZCNT r64, r/m64 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b rexw [ 0x0F; 0xBC ] rm (rd_of_reg64 reg);
  | _ -> assert false

let emit_lzcnt b ~dst ~src =
  match (dst, src) with
  | (Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm))
  | (Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* LZCNT r16, r/m16 and LZCNT r32, r/m32 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b no_rex [ 0x0F; 0xBD ] rm (rd_of_reg64 reg);
  | (Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* LZCNT r64, r/m64 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b rexw [ 0x0F; 0xBD ] rm (rd_of_reg64 reg);
  | _ -> assert false

let rd_of_prefetch_hint = function
  | Nta -> 0
  | T0 -> 1
  | T1 -> 2
  | T2 -> 3

let emit_cldemote b rm = emit_mod_rm_reg b no_rex [ 0x0F; 0x1C ] rm 0

let emit_prefetch b ~is_write ~hint rm =
  match (is_write, hint, rm) with
  | (false, _, (Mem _ | Mem64_RIP _)) ->
    (* PREFETCHT0 m8, PREFETCHT1 m8, PREFETCHT1 m8 and PREFETCHNTA m8 *)
    emit_mod_rm_reg b no_rex [ 0x0F; 0x18 ] rm (rd_of_prefetch_hint hint)
  | (true, T0, (Mem _ | Mem64_RIP _)) ->
    (* PREFETCHW m8 *)
    emit_mod_rm_reg b no_rex [ 0x0F; 0x0D ] rm (rd_of_prefetch_hint hint)
  | (true, (T1 | T2 | Nta), (Mem _ | Mem64_RIP _)) ->
    (* PREFETCHWT1 m8 *)
    (* This sticks to X86_gas' behaviour which emit prefetchwt1 if hint is
       [T2 | Nta] *)
    emit_mod_rm_reg b no_rex [ 0x0F; 0x0D ] rm (rd_of_prefetch_hint T1)
  | _ -> assert false

let emit_pause b = buf_opcodes b [ 0xF3; 0x90 ]

let emit_rdtsc b = buf_opcodes b [ 0x0F; 0x31 ]

let emit_rdpmc b = buf_opcodes b [ 0x0F; 0x33 ]

let emit_lfence b = buf_opcodes b [ 0x0F; 0xAE; 0xE8 ]

let emit_sfence b = buf_opcodes b [ 0x0F; 0xAE; 0xF8 ]

let emit_mfence b = buf_opcodes b [ 0x0F; 0xAE; 0xF0 ]

let emit_leave b = buf_int8 b 0xC9

let emit_inc b = function
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b rexw [ 0xFF ] rm 0
  | _ -> assert false

let emit_DEC b = function
  (* FE /1 DEC r/m8 M Valid Valid *)
  | [ ((Reg8L _ | Reg8H _ | Mem { typ = BYTE }) as rm) ] ->
      emit_mod_rm_reg b no_rex [ 0xFE ] rm 1
  (* FF /1 DEC r/m16 M Valid Valid *)
  | [ ((Reg16 _ | Mem { typ = WORD }) as rm) ] ->
      emit_mod_rm_reg b no_rex [ 0x66; 0xFF ] rm 1
  (* FF /1 DEC r/m32 M Valid Valid *)
  | [ ((Reg32 _ | Mem { typ = DWORD }) as rm) ] ->
      emit_mod_rm_reg b no_rex [ 0xFF ] rm 1
  (* REX.W + FF /1 DEC r/m64 M Valid N.E. *)
  | [ ((Reg64 _ | Mem { typ = QWORD }) as rm) ] ->
      emit_mod_rm_reg b rexw [ 0xFF ] rm 1
  | _ -> assert false

let emit_ret b = buf_int8 b 0xC3

let emit_cqto b =
  emit_rex b rexw;
  buf_int8 b 0x99

let emit_BSWAP b = function
  | Reg32 reg -> buf_opcodes b [ 0x0F; 0xC8 + reg7 (rd_of_reg64 reg) ]
  | Reg64 reg ->
      let reg = rd_of_reg64 reg in
      emit_rex b (rexw lor rexb_opcode reg);
      buf_opcodes b [ 0x0F; 0xC8 + reg7 reg ]
  | _ -> assert false

let emit_XCHG b src dst =
  (* TODO: test ! *)
  match (dst, src) with
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Reg64 reg
  | Reg64 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      (* r64, r/m64 *)
      emit_mod_rm_reg b rexw [ 0x87 ] rm (rd_of_reg64 reg)
  | ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Reg32 reg
  | Reg32 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      (* r32, r/m32 *)
      emit_mod_rm_reg b no_rex [ 0x87 ] rm (rd_of_reg64 reg)
  | ((Reg16 _ | Mem _ | Mem64_RIP _) as rm), Reg16 reg
  | Reg16 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      (* r16, r/m16 *)
      emit_mod_rm_reg b rex [ 0x66; 0x87 ] rm (rd_of_reg64 reg)
  | ( ((Reg8L _ | Reg8H _ | Mem _ | Mem64_RIP _) as rm),
      ((Reg8L _ | Reg8H _) as reg) )
  | ((Reg8L _ | Reg8H _) as reg), ((Mem _ | Mem64_RIP _) as rm) ->
      (* r8, r/m8 *)
      emit_mod_rm_reg b no_rex [ 0x86 ] rm (rd_of_reg8 reg)
  | _ -> assert false

let assemble_instr b loc = function
  | ADD (src, dst) -> emit_ADD b dst src
  | AND (src, dst) -> emit_AND b dst src
  | BSF (src, dst) -> emit_bsf b ~dst ~src
  | BSR (src, dst) -> emit_bsr b ~dst ~src
  | BSWAP arg -> emit_BSWAP b arg
  | CALL dst -> emit_call b dst
  | CLDEMOTE rm -> emit_cldemote b rm
  | CQO -> emit_cqto b
  | CMP (src, dst) -> emit_CMP b dst src
  | CMOV (condition, src, dst) -> emit_cmov b condition dst src
  | CDQ -> buf_int8 b 0x99
  | DEC dst -> emit_DEC b [ dst ]
  | HLT -> buf_int8 b 0xF4
  | INC dst -> emit_inc b dst
  | IMUL (src, dst) -> emit_imul b dst src
  | MUL src -> emit_mul b ~src
  | IDIV dst -> emit_idiv b dst
  | J (condition, dst) -> emit_j b !loc condition dst
  | JMP dst -> emit_jmp b !loc dst
  | LEAVE -> emit_leave b
  | LEA (src, dst) -> emit_LEA b dst src
  | LOCK_CMPXCHG (src, dst) -> emit_lock_cmpxchg b dst src
  | LOCK_XADD (src, dst) -> emit_lock_xadd b dst src
  | LOCK_ADD (src, dst) -> emit_lock_add b dst src
  | LOCK_SUB (src, dst) -> emit_lock_sub b dst src
  | LOCK_AND (src, dst) -> emit_lock_and b dst src
  | LOCK_OR (src, dst) -> emit_lock_or b dst src
  | LOCK_XOR (src, dst) -> emit_lock_xor b dst src
  | MOV (src, dst) -> emit_MOV b dst src
  | MOVSX (src, dst) -> emit_movsx b dst src
  | MOVZX (src, dst) -> emit_MOVZX b dst src
  | MOVSXD (src, dst) -> emit_movsxd b dst src
  | NEG dst -> emit_neg b dst
  | NOP -> buf_int8 b 0x90
  | OR (src, dst) -> emit_OR b dst src
  | PAUSE -> emit_pause b
  | PUSH dst -> emit_push b dst
  | POP dst -> emit_pop b dst
  | POPCNT (src, dst) -> emit_popcnt b ~dst ~src
  | PREFETCH (is_write, hint, rm) -> emit_prefetch b ~is_write ~hint rm
  | RDTSC -> emit_rdtsc b
  | RDPMC -> emit_rdpmc b
  | LFENCE -> emit_lfence b
  | SFENCE -> emit_sfence b
  | MFENCE -> emit_mfence b
  | RET -> emit_ret b
  | SAL (src, dst) -> emit_SAL b dst src
  | SAR (src, dst) -> emit_SAR b dst src
  | SHR (src, dst) -> emit_SHR b dst src
  | SUB (src, dst) -> emit_SUB b dst src
  | SET (condition, dst) -> emit_set b condition dst
  | TEST (src, dst) -> emit_test b dst src
  | XCHG (src, dst) -> emit_XCHG b dst src
  | XOR (src, dst) -> emit_XOR b dst src
  | TZCNT (src, dst) -> emit_tzcnt b ~dst ~src
  | LZCNT (src, dst) -> emit_lzcnt b ~dst ~src
  | SIMD (instr, args) -> emit_simd b instr args


let[@warning "+4"] constant b cst
      (width: D.Constant_with_width.width_in_bytes) =
  let open D.Constant_with_width in
  match cst, width with
  | C.Signed_int n, Eight -> buf_int8L b n
  | C.Signed_int n, Sixteen -> buf_int16L b n
  | C.Signed_int n, Thirty_two -> buf_int32L b n
  | C.Signed_int n, Sixty_four -> buf_int64L b n
  | C.Unsigned_int n, Eight -> buf_int8L b (Numbers.Uint64.to_int64 n)
  | C.Unsigned_int n, Sixteen -> buf_int16L b (Numbers.Uint64.to_int64 n)
  | C.Unsigned_int n, Thirty_two -> buf_int32L b (Numbers.Uint64.to_int64 n)
  | C.Unsigned_int n, Sixty_four -> buf_int64L b (Numbers.Uint64.to_int64 n)
  | (C.This | C.Named_thing _ | C.Add _ | C.Sub _), Eight ->
    record_local_reloc b (RelocConstant (cst, B8));
    buf_int8L b 0L
  | (C.This | C.Named_thing _ | C.Add _ | C.Sub _), Sixteen ->
    record_local_reloc b (RelocConstant (cst, B16));
    buf_int16L b 0L
  | (C.This | C.Named_thing _ | C.Add _ | C.Sub _), Thirty_two ->
    record_local_reloc b (RelocConstant (cst, B32));
    buf_int32L b 0L
  | (C.This | C.Named_thing _ | C.Add _ | C.Sub _), Sixty_four ->
    record_local_reloc b (RelocConstant (cst, B64));
    buf_int64L b 0L

let assemble_line b loc ins =
  try
    match ins with
    | Ins instr ->
        assemble_instr b loc instr;
        incr loc
    | Directive (D.Comment _ )-> ()
    | Directive (D.Global sym) -> (get_symbol b sym).sy_binding <- Sy_global
    | Directive (D.Weak sym) -> (get_symbol b sym).sy_binding <- Sy_weak
    | Directive (D.Protected sym) -> (get_symbol b sym).sy_protected <- true
    | Directive (D.Const {constant = c; comment = _ }) ->
      constant b
              (D.Constant_with_width.constant c)
              (D.Constant_with_width.width_in_bytes c)
    | Directive (D.New_label (s, _)) -> declare_label b s
    | Directive (D.Bytes { str; comment = _ }) -> Buffer.add_string b.buf str
    | Directive (D.External _) -> ()
    | Directive (D.Direct_assignment _) -> assert false
    | Directive (D.Section _) -> assert false
    | Directive D.Cfi_startproc -> ()
    | Directive D.Cfi_endproc -> ()
    | Directive (D.Cfi_adjust_cfa_offset _) -> ()
    | Directive (D.Cfi_remember_state) -> ()
    | Directive (D.Cfi_restore_state) -> ()
    | Directive (D.Cfi_def_cfa_register _)  -> ()
    | Directive (D.Cfi_def_cfa_offset _) -> ()
    | Directive (D.Cfi_offset _) -> ()
    | Directive (D.File _) -> ()
    | Directive (D.Loc _) -> ()
    | Directive (D.Private_extern _) -> assert false
    | Directive (D.Indirect_symbol _) -> assert false
    | Directive (D.Type (lbl, kind)) -> (get_symbol b lbl).sy_type <- Some kind
    | Directive (D.Size (lbl, cst)) -> (
        match eval_const b (Buffer.length b.buf) cst with
        | Rint n -> (get_symbol b lbl).sy_size <- Some (Int64.to_int n)
        | _ -> assert false)
    | Directive (D.Align { fill_x86_bin_emitter=data; bytes = n}) -> (
        (* TODO: Buffer.length = 0 => set section align *)
        let pos = Buffer.length b.buf in
        let current = pos mod n in
        if current > 0 then
          let n = n - current in
          match data with
          | Asm_targets.Asm_directives.Zero ->
            for _ = 1 to n do
              buf_int8 b 0x00
            done
          | Asm_targets.Asm_directives.Nop ->
            match n with
            | 0 -> ()
            | 1 -> buf_int8 b 0x90
            | 2 -> buf_opcodes b [ 0x66; 0x90 ]
            | 3 -> buf_opcodes b [ 0x0f; 0x1f; 0x00 ]
            | 4 -> buf_opcodes b [ 0x0f; 0x1f; 0x40; 0x00 ]
            | 5 -> buf_opcodes b [ 0x0f; 0x1f; 0x44; 0x00; 0x00 ]
            | 6 ->
                buf_opcodes b [ 0x66; 0x0f; 0x1f; 0x44 ];
                buf_int16L b 0L
            | 7 ->
                buf_opcodes b [ 0x0f; 0x1f; 0x80 ];
                buf_int32L b 0L
            | _ ->
                for _ = 9 to n do
                  buf_int8 b 0x66
                done;
                buf_opcodes b [ 0x0f; 0x1f; 0x84; 0x00 ];
                buf_int32L b 0L)
    | Directive (D.Space { bytes = n }) ->
        (* TODO: in text section, should be NOP *)
        for _ = 1 to n do
          buf_int8 b 0
        done
    | Directive (D.Hidden _) | Directive D.New_line -> ()
    | Directive (D.Reloc { name = D.R_X86_64_PLT32;
              expr = C.Sub (C.Named_thing wrap_label, C.Signed_int 4L);
              offset = C.Sub (C.This, C.Signed_int 4L);
            })  when String.Tbl.mem local_labels wrap_label ->
      record_local_reloc b ~offset:(-4) (RelocCall wrap_label)
    | Directive (D.Reloc _)
    | Directive (D.Sleb128 _)
    | Directive (D.Uleb128 _) ->
      X86_gas.generate_asm Out_channel.stderr [ins];
      Misc.fatal_errorf "x86_binary_emitter: unsupported instruction"
  with e ->
    Printf.eprintf "Exception %s:\n%!" (Printexc.to_string e);
    (*
    Printf.eprintf "   masm: %s%!"
      (string_of_buffer X86_masm.bprint_instr !arch64 ins);
    Printf.eprintf "   gas : %s%!"
      (string_of_buffer X86_gas.bprint_instr !arch64 ins);
*)
    raise e

let add_patch b pos size v = b.patches <- (pos, size, v) :: b.patches

let assemble_section arch section =
  (match arch with X86 -> instr_size := 5 | X64 -> instr_size := 6);
  forced_long_jumps := IntSet.empty;
  String.Tbl.clear local_labels;

  let icount = ref 0 in
  ArrayLabels.iter section.sec_instrs ~f:(function
    | Directive (D.New_label (lbl, _)) ->
        String.Tbl.add local_labels lbl !icount
    | Ins _ -> incr icount
    | _ -> ());

  let passes = ref 0 in

  let rec iter_assemble () =
    incr passes;

    (*     if !passes >= 2 then Printf.eprintf "[binary backend] pass %i\n%!" !passes; *)
    let b = new_buffer section in
    local_relocs := [];

    let loc = ref 0 in
    ArrayLabels.iter ~f:(assemble_line b loc) section.sec_instrs;

    let retry = ref false in

    let do_local_reloc pos = function
      | RelocShortJump (label, loc) ->
          let source_pos = pos + 1 in
          let target_pos = label_pos b label in
          let n = target_pos - source_pos in
          if n >= -128 && n < 128 then add_patch b pos B8 (Int64.of_int n)
          else (
            (* We thought this could be a short jump, but actually, this is
               not the case.  Force another pass and remember to use
               a long jump for this instruction. *)
            forced_long_jumps := IntSet.add loc !forced_long_jumps;
            retry := true)
      | RelocCall label | RelocLongJump label ->
          let source_pos = pos + 4 in
          let target_pos = label_pos b label in
          let n = target_pos - source_pos in
          add_patch b pos B32 (Int64.of_int n)
      (* TODO: here, we resolve all computations in each section, i.e. we can only
         allow one external symbol per expression. We could tolerate more complex
         expressions if we delay resolution later, i.e. after all sections have
         been generated and all symbol positions are known. *)
      | RelocConstant (cst, data_size) -> (
          (* Printf.eprintf "RelocConstant (%s, %s)\n%!"
             (X86_gas.string_of_constant cst)
             (string_of_data_size data_size); *)
          let v = eval_const b pos cst in
          match (v, data_size) with
          | Rint n, _ -> add_patch b pos data_size n
          | Rabs (lbl, offset), B32 ->
              record_reloc b pos (Relocation.Kind.DIR32 (lbl, offset))
          | Rabs (lbl, offset), B64 ->
              record_reloc b pos (Relocation.Kind.DIR64 (lbl, offset))
          (* Relative relocation in data segment. We add an offset of 4 because
              REL32 relocations are computed with a PC at the end, while here, it
              is at the beginning. *)
          | Rrel (lbl, offset), B32 ->
              record_reloc b pos
                (Relocation.Kind.REL32 (lbl, Int64.add offset 4L))
          | Rrel _, _ -> assert false
          | Rabs _, _ -> assert false)
    in

    ListLabels.iter !local_relocs ~f:(fun (pos, local_reloc) ->
        do_local_reloc pos local_reloc);

    if !retry then iter_assemble () else b
  in
  iter_assemble ()

(* Relocations: we should compute all non-local relocations completely at the
   end. We should keep the last string/bytes couple to avoid duplication.
   All external labels should be absolute (ConstLabelAbs), while internal
   labels should be replaced by a relative computation. The goal is to make
   all computations either absolute, or relative to the current offset.
*)

let size b = Buffer.length b.buf

let add_patch ~offset ~size ~data t = add_patch t offset size data

let contents_mut b =
  let buf = Buffer.to_bytes b.buf in
  ListLabels.iter b.patches ~f:(fun (pos, nbits, v) ->
      (*    Printf.eprintf "Apply patch %s @%d\n%!" (string_of_data_size nbits) pos; *)
      match nbits with
      | B64 -> str_int64L buf pos v
      | B32 -> str_int32L buf pos v
      | B16 -> str_int16L buf pos v
      | B8 -> str_int8L buf pos v);
  buf

let contents b =
  Bytes.to_string (contents_mut b)

let relocations b = b.relocations

let labels b = b.labels
