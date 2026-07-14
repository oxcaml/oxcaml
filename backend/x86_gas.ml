(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open X86_ast
open X86_proc
open Amd64_simd_instrs
module DLL = Doubly_linked_list

let bprintf = Printf.bprintf

let print_reg b f r =
  Buffer.add_char b '%';
  Buffer.add_string b (f r)

let opt_displ b displ =
  if displ = 0
  then ()
  else if displ > 0
  then bprintf b "+%d" displ
  else bprintf b "%d" displ

let arg_mem b { arch; typ = _; idx; scale; base; sym; displ } =
  let string_of_gpr = string_of_gpr arch in
  let string_of_reg_idx = string_of_reg_idx arch in
  (match sym with
  | None ->
    if displ <> 0 || scale = 0 then Buffer.add_string b (Int.to_string displ)
  | Some s ->
    Buffer.add_string b s;
    opt_displ b displ);
  if scale <> 0
  then (
    Buffer.add_char b '(';
    (match base with None -> () | Some base -> print_reg b string_of_gpr base);
    if base != None || scale <> 1 then Buffer.add_char b ',';
    print_reg b string_of_reg_idx idx;
    if scale <> 1 then bprintf b ",%s" (Int.to_string scale);
    Buffer.add_char b ')')

let arg b = function
  | Sym x ->
    Buffer.add_char b '$';
    Buffer.add_string b x
  | Imm x -> bprintf b "$%Ld" x
  | Reg8L x -> print_reg b string_of_reg8l x
  | Reg8H x -> print_reg b string_of_reg8h x
  | Reg16 x -> print_reg b string_of_reg16 x
  | Reg32 x -> print_reg b string_of_reg32 x
  | Reg64 x -> print_reg b string_of_reg64 x
  | Regf x -> print_reg b string_of_regf x
  | Regmask k -> bprintf b "%%k%d" k
  | Mem addr -> arg_mem b addr
  | Mem64_RIP (_, s, displ) -> bprintf b "%s%a(%%rip)" s opt_displ displ

let typeof = function
  | Mem { typ; _ } | Mem64_RIP (typ, _, _) -> typ
  | Reg8L _ | Reg8H _ -> BYTE
  | Reg16 _ -> WORD
  | Reg32 _ -> DWORD
  | Reg64 _ -> QWORD
  | Imm _ | Sym _ -> NONE
  | Regf _ | Regmask _ -> assert false

let suf arg =
  match typeof arg with
  | BYTE -> "b"
  | WORD -> "w"
  | DWORD | REAL8 -> "l"
  | QWORD -> "q"
  | REAL4 -> "s"
  | VEC128 | VEC256 | VEC512 | NONE -> ""
  | NEAR -> Misc.fatal_error "X86_gas.suf: unexpected datatype NEAR"
  | PROC -> Misc.fatal_error "X86_gas.suf: unexpected datatype PROC"

let i0 b s = bprintf b "\t%s" s

let i1 b s x = bprintf b "\t%s\t%a" s arg x

let i1_s b s x = bprintf b "\t%s%s\t%a" s (suf x) arg x

let i2 b s x y = bprintf b "\t%s\t%a, %a" s arg x arg y

let i2_s b s x y = bprintf b "\t%s%s\t%a, %a" s (suf y) arg x arg y

let i2_sx b s x y = bprintf b "\t%s%s\t%a, %a" s (suf x) arg x arg y

let i2_ss b s x y = bprintf b "\t%s%s%s\t%a, %a" s (suf x) (suf y) arg x arg y

let i3 b s x y z = bprintf b "\t%s\t%a, %a, %a" s arg x arg y arg z

let i4 b s x y z w = bprintf b "\t%s\t%a, %a, %a, %a" s arg x arg y arg z arg w

let evex_rounding : Amd64_simd_defs.evex_rounding -> string = function
  | Rnd_near -> "{rn-sae}, "
  | Rnd_down -> "{rd-sae}, "
  | Rnd_up -> "{ru-sae}, "
  | Rnd_zero -> "{rz-sae}, "

let evex_broadcast evex_w (len : Amd64_simd_defs.evex_length) =
  let bits = match len with L128 -> 128 | L256 -> 256 | L512 -> 512 in
  Printf.sprintf "{1to%d}" (bits / if evex_w then 64 else 32)

let ievex b (instr : Amd64_simd_instrs.instr) args =
  let has_mem = Array.exists X86_ast_utils.is_mem args in
  let zeroing, rounding, broadcast =
    match instr.enc.prefix with
    | Evex { evex_z; evex_b; evex_ll; evex_w; _ } ->
      let rounding, broadcast =
        match evex_ll with
        | Ll_round rnd -> evex_rounding rnd, ""
        | Ll_len len ->
          if not evex_b
          then "", ""
          else if has_mem
          then "", evex_broadcast evex_w len
          else "{sae}, ", ""
      in
      (if evex_z then "{z}" else ""), rounding, broadcast
    | Legacy _ | Vex _ -> Misc.fatal_error "expected EVEX encoding"
  in
  let mask b = function None -> () | Some m -> bprintf b "{%a}" arg m in
  (* [emit_simd_instr] passes the immediate first, then the writemask, then the
     remaining operands in AT&T order. *)
  let imm, args =
    match instr.imm with
    | Imm_spec | Imm_reg ->
      Some args.(0), Array.sub args 1 (Array.length args - 1)
    | Imm_none -> None, args
  in
  let writemask, args =
    if Amd64_simd_defs.instr_expects_mask instr
    then Some args.(0), Array.sub args 1 (Array.length args - 1)
    else None, args
  in
  bprintf b "\t%s\t" instr.mnemonic;
  (* The assembler requires the rounding mode to follow the immediate. *)
  (match imm with
  | Some imm -> bprintf b "%a, " arg imm
  | None -> ());
  Buffer.add_string b rounding;
  let last = Array.length args - 1 in
  Array.iteri
    (fun i a ->
      if i > 0 then Buffer.add_string b ", ";
      arg b a;
      if X86_ast_utils.is_mem a then Buffer.add_string b broadcast;
      if i = last then bprintf b "%a%s" mask writemask zeroing)
    args

let i1_call_jmp b s = function
  (* this is the encoding of jump labels: don't use * *)
  | Mem { arch = X86; idx = _; scale = 0; base = None; sym = Some _; _ } as x ->
    i1 b s x
  | (Reg32 _ | Reg64 _ | Mem { arch = X64 | X86; _ } | Mem64_RIP _) as x ->
    bprintf b "\t%s\t*%a" s arg x
  | Sym x -> bprintf b "\t%s\t%s" s x
  | (Imm _ | Reg8L _ | Reg8H _ | Reg16 _ | Regf _ | Regmask _) as x ->
    let buf = Buffer.create 16 in
    arg buf x;
    Misc.fatal_errorf "X86_gas.i1_call_jmp: invalid operand %s"
      (Buffer.contents buf)

let print_instr b = function
  | ADD (arg1, arg2) -> i2_s b "add" arg1 arg2
  | ADC (arg1, arg2) -> i2_s b "adc" arg1 arg2
  | AND (arg1, arg2) -> i2_s b "and" arg1 arg2
  | BSF (arg1, arg2) -> i2_s b "bsf" arg1 arg2
  | BSR (arg1, arg2) -> i2_s b "bsr" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg
  | CALL arg -> i1_call_jmp b "call" arg
  | CDQ -> i0 b "cltd"
  | CLDEMOTE arg -> i1 b "cldemote" arg
  | CMOV (c, arg1, arg2) -> i2 b ("cmov" ^ string_of_condition c) arg1 arg2
  | CMP (arg1, arg2) -> i2_s b "cmp" arg1 arg2
  | CQO -> i0 b "cqto"
  | DEC arg -> i1_s b "dec" arg
  | HLT -> i0 b "hlt"
  | IDIV arg -> i1_s b "idiv" arg
  | IMUL (arg, None) -> i1_s b "imul" arg
  | IMUL (arg1, Some arg2) -> i2_s b "imul" arg1 arg2
  | MUL arg -> i1_s b "mul" arg
  | INC arg -> i1_s b "inc" arg
  | J (c, arg) -> i1_call_jmp b ("j" ^ string_of_condition c) arg
  | JMP arg -> i1_call_jmp b "jmp" arg
  | LEA (arg1, arg2) -> i2_s b "lea" arg1 arg2
  | LOCK_CMPXCHG (arg1, arg2) -> i2_sx b "lock cmpxchg" arg1 arg2
  | LOCK_XADD (arg1, arg2) -> i2_sx b "lock xadd" arg1 arg2
  | LOCK_ADD (arg1, arg2) -> i2_sx b "lock add" arg1 arg2
  | LOCK_SUB (arg1, arg2) -> i2_sx b "lock sub" arg1 arg2
  | LOCK_AND (arg1, arg2) -> i2_sx b "lock and" arg1 arg2
  | LOCK_OR (arg1, arg2) -> i2_sx b "lock or" arg1 arg2
  | LOCK_XOR (arg1, arg2) -> i2_sx b "lock xor" arg1 arg2
  | LEAVE -> i0 b "leave"
  | MOV ((Imm n as arg1), (Reg64 _ as arg2))
    when not
           (Int64.compare n 0x7FFF_FFFFL <= 0
           && Int64.compare n (-0x8000_0000L) >= 0) ->
    i2 b "movabsq" arg1 arg2
  | MOV ((Sym _ as arg1), (Reg64 _ as arg2)) when windows ->
    i2 b "movabsq" arg1 arg2
  | MOV
      ( (( Reg8L _ | Imm _ | Sym _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _
         | Regf _ | Regmask _ | Mem _
         | Mem64_RIP (_, _, _) ) as arg1),
        (( Reg8L _ | Imm _ | Sym _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _
         | Regf _ | Regmask _ | Mem _
         | Mem64_RIP (_, _, _) ) as arg2) ) ->
    i2_s b "mov" arg1 arg2
  | MOVSX (arg1, arg2) -> i2_ss b "movs" arg1 arg2
  | MOVSXD (arg1, arg2) -> i2 b "movslq" arg1 arg2
  | MOVZX (arg1, arg2) -> i2_ss b "movz" arg1 arg2
  | NEG arg -> i1 b "neg" arg
  | NOP -> i0 b "nop"
  | OR (arg1, arg2) -> i2_s b "or" arg1 arg2
  | PAUSE -> i0 b "pause"
  | POP arg -> i1_s b "pop" arg
  | PREFETCH (is_write, hint, arg1) -> (
    match is_write, hint with
    | true, T0 -> i1 b "prefetchw" arg1
    | true, (T1 | T2 | Nta) -> i1 b "prefetchwt1" arg1
    | false, (T0 | T1 | T2 | Nta) ->
      i1 b ("prefetch" ^ string_of_prefetch_temporal_locality_hint hint) arg1)
  | PUSH arg -> i1_s b "push" arg
  | RDTSC -> i0 b "rdtsc"
  | RDPMC -> i0 b "rdpmc"
  | LFENCE -> i0 b "lfence"
  | SFENCE -> i0 b "sfence"
  | MFENCE -> i0 b "mfence"
  | RET -> i0 b "ret"
  | SAL (arg1, arg2) -> i2_s b "sal" arg1 arg2
  | SAR (arg1, arg2) -> i2_s b "sar" arg1 arg2
  | SET (c, arg) -> i1 b ("set" ^ string_of_condition c) arg
  | SHR (arg1, arg2) -> i2_s b "shr" arg1 arg2
  | SUB (arg1, arg2) -> i2_s b "sub" arg1 arg2
  | SBB (arg1, arg2) -> i2_s b "sbb" arg1 arg2
  | TEST (arg1, arg2) -> i2_s b "test" arg1 arg2
  | UD2 -> i0 b "ud2"
  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | XOR (arg1, arg2) -> i2_s b "xor" arg1 arg2
  | SIMD (instr, args) when Amd64_simd_defs.instr_is_evex instr ->
    ievex b instr args
  | SIMD (instr, args) -> (
    match[@warning "-4"] instr.id, args with
    (* The assembler won't accept these mnemonics directly. *)
    | Cmpps, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "ps") arg1 arg2
    | Cmppd, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "pd") arg1 arg2
    | Cmpss, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "ss") arg1 arg2
    | Cmpsd, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "sd") arg1 arg2
    (* The assembler needs a suffix to disambiguate the memory argument. *)
    | Crc32_r64_r64m64, [| arg1; arg2 |] -> i2 b "crc32q" arg1 arg2
    | Cvtsi2sd_X_r64m64, [| arg1; arg2 |] -> i2 b "cvtsi2sdq" arg1 arg2
    | Cvtsi2ss_X_r64m64, [| arg1; arg2 |] -> i2 b "cvtsi2ssq" arg1 arg2
    | Vcvtsi2sd_X_X_r64m64, [| arg1; arg2; arg3 |] ->
      i3 b "vcvtsi2sdq" arg1 arg2 arg3
    | Vcvtsi2ss_X_X_r64m64, [| arg1; arg2; arg3 |] ->
      i3 b "vcvtsi2ssq" arg1 arg2 arg3
    (* All other simd instructions. *)
    | _, [| arg1; arg2 |] -> i2 b instr.mnemonic arg1 arg2
    | _, [| arg1; arg2; arg3 |] -> i3 b instr.mnemonic arg1 arg2 arg3
    | _, [| arg1; arg2; arg3; arg4 |] -> i4 b instr.mnemonic arg1 arg2 arg3 arg4
    | _, _ ->
      Misc.fatal_errorf "unexpected instruction layout for %s (%d args)"
        instr.mnemonic (Array.length args))

let print_line b i =
  match i with
  | Ins i -> print_instr b i
  | Directive d -> Asm_targets.Asm_directives.Directive.print b d

let map_arg (f : arg -> arg) (instr : instruction) : instruction =
  match instr with
  | ADD (a, b) -> ADD (f a, f b)
  | ADC (a, b) -> ADC (f a, f b)
  | AND (a, b) -> AND (f a, f b)
  | BSF (a, b) -> BSF (f a, f b)
  | BSR (a, b) -> BSR (f a, f b)
  | BSWAP a -> BSWAP (f a)
  | CALL a -> CALL (f a)
  | CDQ -> CDQ
  | CLDEMOTE a -> CLDEMOTE (f a)
  | CMOV (c, a, b) -> CMOV (c, f a, f b)
  | CMP (a, b) -> CMP (f a, f b)
  | CQO -> CQO
  | DEC a -> DEC (f a)
  | HLT -> HLT
  | IDIV a -> IDIV (f a)
  | IMUL (a, b) -> IMUL (f a, Option.map f b)
  | MUL a -> MUL (f a)
  | INC a -> INC (f a)
  | J (c, a) -> J (c, f a)
  | JMP a -> JMP (f a)
  | LEA (a, b) -> LEA (f a, f b)
  | LOCK_CMPXCHG (a, b) -> LOCK_CMPXCHG (f a, f b)
  | LOCK_XADD (a, b) -> LOCK_XADD (f a, f b)
  | LOCK_ADD (a, b) -> LOCK_ADD (f a, f b)
  | LOCK_SUB (a, b) -> LOCK_SUB (f a, f b)
  | LOCK_AND (a, b) -> LOCK_AND (f a, f b)
  | LOCK_OR (a, b) -> LOCK_OR (f a, f b)
  | LOCK_XOR (a, b) -> LOCK_XOR (f a, f b)
  | LEAVE -> LEAVE
  | MOV (a, b) -> MOV (f a, f b)
  | MOVSX (a, b) -> MOVSX (f a, f b)
  | MOVSXD (a, b) -> MOVSXD (f a, f b)
  | MOVZX (a, b) -> MOVZX (f a, f b)
  | NEG a -> NEG (f a)
  | NOP -> NOP
  | OR (a, b) -> OR (f a, f b)
  | PAUSE -> PAUSE
  | POP a -> POP (f a)
  | PREFETCH (w, h, a) -> PREFETCH (w, h, f a)
  | PUSH a -> PUSH (f a)
  | RDTSC -> RDTSC
  | RDPMC -> RDPMC
  | LFENCE -> LFENCE
  | SFENCE -> SFENCE
  | MFENCE -> MFENCE
  | RET -> RET
  | SAL (a, b) -> SAL (f a, f b)
  | SAR (a, b) -> SAR (f a, f b)
  | SET (c, a) -> SET (c, f a)
  | SHR (a, b) -> SHR (f a, f b)
  | SUB (a, b) -> SUB (f a, f b)
  | SBB (a, b) -> SBB (f a, f b)
  | TEST (a, b) -> TEST (f a, f b)
  | UD2 -> UD2
  | XCHG (a, b) -> XCHG (f a, f b)
  | XOR (a, b) -> XOR (f a, f b)
  | SIMD (simd_instr, args) -> SIMD (simd_instr, Array.map f args)

let generate_asm oc lines =
  let b = Buffer.create 10000 in
  output_string oc "\t.file \"\"\n";
  (* PR#7037 *)
  DLL.iter lines ~f:(fun i ->
      Buffer.clear b;
      print_line b i;
      Buffer.add_char b '\n';
      Buffer.output_buffer oc b)

let format_asm_for_expect_asm ~name ~body ~hidden_gc_jump_pads =
  let module D = Asm_targets.Asm_directives.Directive in
  let module L = Asm_targets.Asm_label in
  let tab_stops = [| 2; 8 |] in
  let tabs_to_spaces s =
    let result = Buffer.create (String.length s) in
    let col = ref 0 in
    let tab_index = ref 0 in
    String.iter
      (fun c ->
        if Char.equal c '\t' && !tab_index < Array.length tab_stops
        then (
          let target_col = tab_stops.(!tab_index) in
          let spaces = max 1 (target_col - !col) in
          for _ = 1 to spaces do
            Buffer.add_char result ' '
          done;
          col := !col + spaces;
          incr tab_index)
        else (
          Buffer.add_char result c;
          incr col))
      s;
    Buffer.contents result
  in
  let label_map : (string, L.t) Hashtbl.t = Hashtbl.create 16 in
  let next_id = ref 0 in
  List.iter
    (fun line ->
      match[@warning "-4"] line with
      | Directive (D.New_label (D.Label l, _)) ->
        let old_str = L.encode l in
        let new_label = L.create_int (L.section l) !next_id in
        Hashtbl.add label_map old_str new_label;
        incr next_id
      | Ins _ | Directive _ -> ())
    body;
  let hidden_labels : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun line ->
      match[@warning "-4"] line with
      | Directive (D.New_label (D.Label l, _)) ->
        Hashtbl.add hidden_labels (L.encode l) ()
      | Ins _ | Directive _ -> ())
    hidden_gc_jump_pads;
  let rewrite_str s =
    match Hashtbl.find_opt label_map s with
    | None -> if Hashtbl.mem hidden_labels s then "<hidden GC jump pad>" else s
    | Some new_label -> L.encode new_label
  in
  let rewrite_label l =
    match Hashtbl.find_opt label_map (L.encode l) with
    | None -> l
    | Some new_label -> new_label
  in
  let rewrite_arg (a : arg) : arg =
    match a with
    | Sym s -> Sym (rewrite_str s)
    | Mem ({ sym; _ } as addr) ->
      Mem { addr with sym = Option.map rewrite_str sym }
    | Mem64_RIP (typ, s, displ) ->
      let s =
        if
          String.starts_with ~prefix:"camlTOP" s
          || String.starts_with ~prefix:".L" s
        then "<hidden PC-relative offset>"
        else rewrite_str s
      in
      Mem64_RIP (typ, s, displ)
    | ( Imm _ | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _
      | Regmask _ ) as a ->
      a
  in
  let body =
    List.map
      (fun line ->
        match line with
        | Ins instr -> Ins (map_arg rewrite_arg instr)
        | Directive d -> Directive (D.map_new_label rewrite_label d))
      body
  in
  let buf = Buffer.create 1024 in
  bprintf buf "%s:\n" name;
  List.iter
    (fun line ->
      let should_output =
        match[@warning "-4"] line with
        | Ins _ | Directive (New_label _) -> true
        | Directive _ -> false
      in
      if should_output
      then (
        let line_buf = Buffer.create 128 in
        print_line line_buf line;
        Buffer.add_string buf (tabs_to_spaces (Buffer.contents line_buf));
        Buffer.add_char buf '\n'))
    body;
  Buffer.contents buf
