(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
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
module DLL = Oxcaml_utils.Doubly_linked_list

module Section_name = struct
  module S = struct
    type t =
      { name : string list;
        name_str : string;
        flags : string option;
        args : string list
      }

    let equal t1 t2 = List.equal String.equal t1.name t2.name

    let hash t = Hashtbl.hash t.name

    let compare t1 t2 = List.compare String.compare t1.name t2.name

    let make name flags args =
      { name; name_str = String.concat "," name; flags; args }

    let of_string name =
      { name = [name]; name_str = name; flags = None; args = [] }

    let to_string t = t.name_str

    let flags t = t.flags

    let alignment t =
      let rec align = function
        | [] -> 0L
        | [hd] -> Option.value ~default:0L (Int64.of_string_opt hd)
        | _hd :: tl -> align tl
      in
      align t.args

    let is_text_like t = String.starts_with ~prefix:".text" t.name_str

    let is_data_like t = String.starts_with ~prefix:".data" t.name_str

    let is_note_like t = String.starts_with ~prefix:".note" t.name_str
  end

  include S
  module Map = Map.Make (S)
  module Tbl = Hashtbl.Make (S)
end

type system =
  (* 32 bits and 64 bits *)
  | S_macosx
  | S_gnu
  | S_cygwin
  (* 32 bits only *)
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  (* 64 bits only *)
  | S_win64
  | S_linux
  | S_mingw64
  | S_freebsd
  | S_netbsd
  | S_openbsd
  | S_unknown

let system =
  match Config.system with
  | "macosx" -> S_macosx
  | "solaris" -> S_solaris
  | "win32" -> S_win32
  | "linux_elf" -> S_linux_elf
  | "bsd_elf" -> S_bsd_elf
  | "beos" -> S_beos
  | "gnu" -> S_gnu
  | "cygwin" -> S_cygwin
  | "mingw" -> S_mingw
  | "mingw64" -> S_mingw64
  | "win64" -> S_win64
  | "linux" -> S_linux
  | "freebsd" -> S_freebsd
  | "netbsd" -> S_netbsd
  | "openbsd" -> S_openbsd
  | _ -> S_unknown

let windows =
  match[@warning "-4"] system with
  | S_mingw64 | S_cygwin | S_win64 -> true
  | _ -> false

let is_linux = function[@warning "-4"] S_linux -> true | _ -> false

let is_macosx = function[@warning "-4"] S_macosx -> true | _ -> false

let is_win32 = function[@warning "-4"] S_win32 -> true | _ -> false

let is_win64 = function[@warning "-4"] S_win64 -> true | _ -> false

let is_solaris = function[@warning "-4"] S_solaris -> true | _ -> false

let string_of_substring_literal k n s =
  let between x low high =
    Char.compare x low >= 0 && Char.compare x high <= 0
  in
  let b = Buffer.create (n + 2) in
  let last_was_escape = ref false in
  for i = k to k + n - 1 do
    let c = s.[i] in
    if between c '0' '9'
    then
      if !last_was_escape
      then Printf.bprintf b "\\%o" (Char.code c)
      else Buffer.add_char b c
    else if between c ' ' '~'
            && (not (Char.equal c '"'))
            (* '"' *) && not (Char.equal c '\\')
    then (
      Buffer.add_char b c;
      last_was_escape := false)
    else (
      Printf.bprintf b "\\%o" (Char.code c);
      last_was_escape := true)
  done;
  Buffer.contents b

let string_of_string_literal s =
  string_of_substring_literal 0 (String.length s) s

let string_of_symbol prefix s =
  let spec = ref false in
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> ()
    | _ -> spec := true
  done;
  if not !spec
  then if String.equal prefix "" then s else prefix ^ s
  else
    let b = Buffer.create (String.length s + 10) in
    Buffer.add_string b prefix;
    String.iter
      (function
        | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.') as c ->
          Buffer.add_char b c
        | c -> Printf.bprintf b "$%02x" (Char.code c))
      s;
    Buffer.contents b

let string_of_prefetch_temporal_locality_hint = function
  | Nta -> "nta"
  | T2 -> "t2"
  | T1 -> "t1"
  | T0 -> "t0"

let buf_bytes_directive b directive s =
  let pos = ref 0 in
  for i = 0 to String.length s - 1 do
    if !pos = 0
    then (
      if i > 0 then Buffer.add_char b '\n';
      Buffer.add_char b '\t';
      Buffer.add_string b directive;
      Buffer.add_char b '\t')
    else Buffer.add_char b ',';
    Printf.bprintf b "%d" (Char.code s.[i]);
    incr pos;
    if !pos >= 16 then pos := 0
  done

let string_of_reg64 = function
  | RAX -> "rax"
  | RBX -> "rbx"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | RBP -> "rbp"
  | RSP -> "rsp"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let string_of_reg8l = function
  | RAX -> "al"
  | RBX -> "bl"
  | RCX -> "cl"
  | RDX -> "dl"
  | RSP -> "spl"
  | RBP -> "bpl"
  | RSI -> "sil"
  | RDI -> "dil"
  | R8 -> "r8b"
  | R9 -> "r9b"
  | R10 -> "r10b"
  | R11 -> "r11b"
  | R12 -> "r12b"
  | R13 -> "r13b"
  | R14 -> "r14b"
  | R15 -> "r15b"

let string_of_reg8h = function
  | AH -> "ah"
  | BH -> "bh"
  | CH -> "ch"
  | DH -> "dh"

let string_of_reg16 = function
  | RAX -> "ax"
  | RBX -> "bx"
  | RCX -> "cx"
  | RDX -> "dx"
  | RSP -> "sp"
  | RBP -> "bp"
  | RSI -> "si"
  | RDI -> "di"
  | R8 -> "r8w"
  | R9 -> "r9w"
  | R10 -> "r10w"
  | R11 -> "r11w"
  | R12 -> "r12w"
  | R13 -> "r13w"
  | R14 -> "r14w"
  | R15 -> "r15w"

let string_of_reg32 = function
  | RAX -> "eax"
  | RBX -> "ebx"
  | RCX -> "ecx"
  | RDX -> "edx"
  | RSP -> "esp"
  | RBP -> "ebp"
  | RSI -> "esi"
  | RDI -> "edi"
  | R8 -> "r8d"
  | R9 -> "r9d"
  | R10 -> "r10d"
  | R11 -> "r11d"
  | R12 -> "r12d"
  | R13 -> "r13d"
  | R14 -> "r14d"
  | R15 -> "r15d"

let string_of_regf = function
  | XMM n -> Printf.sprintf "xmm%d" n
  | YMM n -> Printf.sprintf "ymm%d" n
  | ZMM n -> Printf.sprintf "zmm%d" n

let string_of_condition = function
  | E -> "e"
  | AE -> "ae"
  | A -> "a"
  | GE -> "ge"
  | G -> "g"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | L -> "l"
  | LE -> "le"
  | NP -> "np"
  | P -> "p"
  | NS -> "ns"
  | S -> "s"
  | NO -> "no"
  | O -> "o"

let imm_of_float_condition = function
  | EQf -> Imm 0L
  | LTf -> Imm 1L
  | LEf -> Imm 2L
  | UNORDf -> Imm 3L
  | NEQf -> Imm 4L
  | NLTf -> Imm 5L
  | NLEf -> Imm 6L
  | ORDf -> Imm 7L

let string_of_float_condition = function
  | EQf -> "eq"
  | LTf -> "lt"
  | LEf -> "le"
  | UNORDf -> "unord"
  | NEQf -> "neq"
  | NLTf -> "nlt"
  | NLEf -> "nle"
  | ORDf -> "ord"

let float_condition_of_imm = function
  | Imm 0L -> EQf
  | Imm 1L -> LTf
  | Imm 2L -> LEf
  | Imm 3L -> UNORDf
  | Imm 4L -> NEQf
  | Imm 5L -> NLTf
  | Imm 6L -> NLEf
  | Imm 7L -> ORDf
  | Sym _ | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ | Mem _
  | Mem64_RIP _ | Imm _ ->
    Misc.fatal_errorf "Invalid float condition immediate arg"

let string_of_float_condition_imm imm =
  float_condition_of_imm imm |> string_of_float_condition

let string_of_rounding = function
  | RoundDown -> "roundsd.down"
  | RoundUp -> "roundsd.up"
  | RoundTruncate -> "roundsd.trunc"
  | RoundNearest -> "roundsd.near"
  | RoundCurrent -> "roundsd"

(*= Control fields for [roundsd] operation is specified as a 4-bit immediate:
   bit 3: whether to signal Precision Floating-Point Exception.
   bit 2: if set, select rounding mode from MXCSR.RC, else use bits 0 and 1.
   bits 0 and 1: rounding mode, according to  Table 4-17 of
   Intel® 64 and IA-32 Architectures Software Developer’s Manual Volume 2. *)
let imm_of_rounding = function
  | RoundNearest -> Imm 8L
  | RoundDown -> Imm 9L
  | RoundUp -> Imm 10L
  | RoundTruncate -> Imm 11L
  | RoundCurrent -> Imm 12L

let internal_assembler = ref None

let register_internal_assembler f = internal_assembler := Some f

(* Which asm conventions to use *)
let masm =
  match[@warning "-4"] system with S_win32 | S_win64 -> true | _ -> false

let use_plt =
  match system with
  | S_macosx | S_mingw64 | S_cygwin | S_win64 -> false
  | S_linux | S_gnu | S_solaris | S_win32 | S_linux_elf | S_bsd_elf | S_beos
  | S_mingw | S_freebsd | S_netbsd | S_openbsd | S_unknown ->
    !Clflags.dlcode

(* Shall we use an external assembler command ? If [binary_content] contains
   some data, we can directly save it. Otherwise, we have to ask an external
   command. *)
let binary_content = ref None

let compile infile outfile =
  if masm
  then
    Ccomp.command
      (Config.asm ^ Filename.quote outfile ^ " " ^ Filename.quote infile
      ^ if !Clflags.verbose then "" else ">NUL")
  else
    let dwarf_flag =
      if !Clflags.native_code && !Clflags.debug
      then Dwarf_flags.get_dwarf_as_toolchain_flag ()
      else ""
    in
    Ccomp.command
      (Config.asm ^ " "
      ^ String.concat " " (Misc.debug_prefix_map_flags ())
      ^ dwarf_flag ^ " -o " ^ Filename.quote outfile ^ " "
      ^ Filename.quote infile)

let assemble_file infile outfile =
  match !binary_content with
  | None -> compile infile outfile
  | Some content ->
    content outfile;
    binary_content := None;
    0

let asm_code = DLL.make_empty ()

let asm_code_current_section = ref (DLL.make_empty ())

let asm_code_by_section = Section_name.Tbl.create 100

let delayed_sections = Section_name.Tbl.create 100

(* Cannot use Emitaux directly here or there would be a circular dep *)
let create_asm_file = ref true

let directive dir =
  if !create_asm_file then DLL.add_end asm_code dir;
  match[@warning "-4"] dir with
  | Directive
      (Asm_targets.Asm_directives.Directive.Section
        { names = name; flags; args; is_delayed }) -> (
    let name = Section_name.make name flags args in
    let where = if is_delayed then delayed_sections else asm_code_by_section in
    match Section_name.Tbl.find_opt where name with
    | Some x -> asm_code_current_section := x
    | None ->
      let new_section = DLL.make_empty () in
      asm_code_current_section := new_section;
      Section_name.Tbl.add where name new_section)
  | dir -> DLL.add_end !asm_code_current_section dir

let emit ins = directive (Ins ins)

let reset_asm_code () =
  DLL.clear asm_code;
  asm_code_current_section := DLL.make_empty ();
  Section_name.Tbl.clear asm_code_by_section

(* Peephole optimization for x86 instruction lists.

   TODO: This code should eventually be moved to a separate module in
   backend/x86_peephole/ to better organize the peephole optimization
   infrastructure and rules. *)

module X86_peephole = struct
  (* Equality functions for x86_ast types, avoiding polymorphic equality *)

  let equal_reg64 left right =
    match left, right with
    | RAX, RAX
    | RBX, RBX
    | RCX, RCX
    | RDX, RDX
    | RSP, RSP
    | RBP, RBP
    | RSI, RSI
    | RDI, RDI
    | R8, R8
    | R9, R9
    | R10, R10
    | R11, R11
    | R12, R12
    | R13, R13
    | R14, R14
    | R15, R15 ->
      true
    | ( ( RAX | RBX | RCX | RDX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11
        | R12 | R13 | R14 | R15 ),
        _ ) ->
      false

  let equal_reg8h left right =
    match left, right with
    | AH, AH | BH, BH | CH, CH | DH, DH -> true
    | (AH | BH | CH | DH), _ -> false

  let equal_regf left right =
    match left, right with
    | XMM n1, XMM n2 | YMM n1, YMM n2 | ZMM n1, ZMM n2 -> n1 = n2
    | (XMM _ | YMM _ | ZMM _), _ -> false

  let equal_arch left right =
    match left, right with
    | X64, X64 | X86, X86 -> true
    | (X64 | X86), _ -> false

  let equal_data_type left right =
    match left, right with
    | NONE, NONE
    | REAL4, REAL4
    | REAL8, REAL8
    | BYTE, BYTE
    | WORD, WORD
    | DWORD, DWORD
    | QWORD, QWORD
    | VEC128, VEC128
    | VEC256, VEC256
    | VEC512, VEC512
    | NEAR, NEAR
    | PROC, PROC ->
      true
    | ( ( NONE | REAL4 | REAL8 | BYTE | WORD | DWORD | QWORD | VEC128 | VEC256
        | VEC512 | NEAR | PROC ),
        _ ) ->
      false

  let equal_addr left right =
    equal_arch left.arch right.arch
    && equal_data_type left.typ right.typ
    && equal_reg64 left.idx right.idx
    && left.scale = right.scale
    && Option.equal equal_reg64 left.base right.base
    && Option.equal String.equal left.sym right.sym
    && left.displ = right.displ

  (* Hard barriers are instruction boundaries that stop peephole optimization.
     These include: - Labels (control flow targets) - Section changes -
     Alignment directives *)
  let is_hard_barrier = function
    | Directive d -> (
      match d with
      | Asm_targets.Asm_directives.Directive.New_label _ -> true
      | Asm_targets.Asm_directives.Directive.Section _ -> true
      | Asm_targets.Asm_directives.Directive.Align _ -> true
      | Asm_targets.Asm_directives.Directive.Bytes _
      | Asm_targets.Asm_directives.Directive.Cfi_adjust_cfa_offset _
      | Asm_targets.Asm_directives.Directive.Cfi_def_cfa_offset _
      | Asm_targets.Asm_directives.Directive.Cfi_endproc
      | Asm_targets.Asm_directives.Directive.Cfi_offset _
      | Asm_targets.Asm_directives.Directive.Cfi_startproc
      | Asm_targets.Asm_directives.Directive.Cfi_remember_state
      | Asm_targets.Asm_directives.Directive.Cfi_restore_state
      | Asm_targets.Asm_directives.Directive.Cfi_def_cfa_register _
      | Asm_targets.Asm_directives.Directive.Comment _
      | Asm_targets.Asm_directives.Directive.Const _
      | Asm_targets.Asm_directives.Directive.Direct_assignment _
      | Asm_targets.Asm_directives.Directive.File _
      | Asm_targets.Asm_directives.Directive.Global _
      | Asm_targets.Asm_directives.Directive.Indirect_symbol _
      | Asm_targets.Asm_directives.Directive.Loc _
      | Asm_targets.Asm_directives.Directive.New_line
      | Asm_targets.Asm_directives.Directive.Private_extern _
      | Asm_targets.Asm_directives.Directive.Size _
      | Asm_targets.Asm_directives.Directive.Sleb128 _
      | Asm_targets.Asm_directives.Directive.Space _
      | Asm_targets.Asm_directives.Directive.Type _
      | Asm_targets.Asm_directives.Directive.Uleb128 _
      | Asm_targets.Asm_directives.Directive.Protected _
      | Asm_targets.Asm_directives.Directive.Hidden _
      | Asm_targets.Asm_directives.Directive.Weak _
      | Asm_targets.Asm_directives.Directive.External _
      | Asm_targets.Asm_directives.Directive.Reloc _ ->
        false)
    | Ins _ -> false

  (* Utility: get at most n cells starting from the given cell. Returns a list
     of cells (may be shorter than n if we reach the end). *)
  let get_cells cell n =
    let rec loop acc remaining current_opt =
      if remaining <= 0
      then List.rev acc
      else
        match current_opt with
        | None -> List.rev acc
        | Some current ->
          loop (current :: acc) (remaining - 1) (DLL.next current)
    in
    loop [] n (Some cell)

  (* Compare two args for equality *)
  let equal_args arg1 arg2 =
    match[@warning "-4"] arg1, arg2 with
    | Imm i1, Imm i2 -> Int64.equal i1 i2
    | Sym s1, Sym s2 -> String.equal s1 s2
    | Reg8L r1, Reg8L r2 -> equal_reg64 r1 r2
    | Reg8H r1, Reg8H r2 -> equal_reg8h r1 r2
    | Reg16 r1, Reg16 r2 -> equal_reg64 r1 r2
    | Reg32 r1, Reg32 r2 -> equal_reg64 r1 r2
    | Reg64 r1, Reg64 r2 -> equal_reg64 r1 r2
    | Regf rf1, Regf rf2 -> equal_regf rf1 rf2
    | Mem addr1, Mem addr2 -> equal_addr addr1 addr2
    | Mem64_RIP (t1, s1, i1), Mem64_RIP (t2, s2, i2) ->
      equal_data_type t1 t2 && String.equal s1 s2 && i1 = i2
    | _, _ -> false

  (* Check if an argument is a register *)
  let is_register = function
    | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ -> true
    | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> false

  (* Check if an arg is a safe-to-optimize self-move *)
  let is_safe_self_move_arg = function[@warning "-4"]
    | Reg8L _ | Reg8H _ | Reg16 _ | Reg64 _ -> true
    | _ -> false

  (* Check if an instruction is a control flow instruction (jump, call, return).
     These act as basic block boundaries for peephole optimization. *)
  let is_control_flow = function[@warning "-4"]
    | J _ | JMP _ | CALL _ | RET -> true
    | _ -> false

  (* Check if an instruction writes to a given argument. Conservative: only
     handles common cases explicitly. *)
  let writes_to_arg target = function[@warning "-4"]
    | MOV (_, dst)
    | MOVSX (_, dst)
    | MOVSXD (_, dst)
    | MOVZX (_, dst)
    | LEA (_, dst)
    | ADD (_, dst)
    | SUB (_, dst)
    | AND (_, dst)
    | OR (_, dst)
    | XOR (_, dst)
    | SAL (_, dst)
    | SAR (_, dst)
    | SHR (_, dst)
    | BSF (_, dst)
    | BSR (_, dst)
    | POPCNT (_, dst)
    | TZCNT (_, dst)
    | LZCNT (_, dst)
    | CMOV (_, _, dst) ->
      equal_args target dst
    | INC dst | DEC dst | NEG dst | BSWAP dst | SET (_, dst) ->
      equal_args target dst
    | POP dst -> equal_args target dst
    | IMUL (dst, None) -> equal_args target dst
    | IMUL (_, Some dst) -> equal_args target dst
    | LOCK_XADD (_, dst) -> equal_args target dst
    | XCHG (op1, op2) -> equal_args target op1 || equal_args target op2
    | _ -> false

  (* Check if an instruction reads from a given argument. Conservative: returns
     true if unsure. *)
  let reads_from_arg target = function[@warning "-4"]
    | MOV (src, _) | MOVSX (src, _) | MOVSXD (src, _) | MOVZX (src, _) ->
      equal_args target src
    | PUSH src -> equal_args target src
    | ADD (src, dst)
    | SUB (src, dst)
    | AND (src, dst)
    | OR (src, dst)
    | XOR (src, dst)
    | CMP (src, dst)
    | TEST (src, dst) ->
      equal_args target src || equal_args target dst
    | LEA (src, _)
    | BSF (src, _)
    | BSR (src, _)
    | POPCNT (src, _)
    | TZCNT (src, _)
    | LZCNT (src, _)
    | SAL (src, _)
    | SAR (src, _)
    | SHR (src, _) ->
      equal_args target src
    | CMOV (_, src, dst) -> equal_args target src || equal_args target dst
    | INC dst | DEC dst | NEG dst | BSWAP dst -> equal_args target dst
    | IMUL (op, None) -> equal_args target op
    | IMUL (op1, Some op2) -> equal_args target op1 || equal_args target op2
    | MUL op | IDIV op -> equal_args target op
    | CALL arg | JMP arg | J (_, arg) -> equal_args target arg
    | XCHG (op1, op2) -> equal_args target op1 || equal_args target op2
    | LOCK_CMPXCHG (op1, op2)
    | LOCK_XADD (op1, op2)
    | LOCK_ADD (op1, op2)
    | LOCK_SUB (op1, op2)
    | LOCK_AND (op1, op2)
    | LOCK_OR (op1, op2)
    | LOCK_XOR (op1, op2) ->
      equal_args target op1 || equal_args target op2
    | SET (_, dst) -> equal_args target dst
    | POP _ -> false
    | CLDEMOTE arg -> equal_args target arg
    | PREFETCH (_, _, arg) -> equal_args target arg
    | _ ->
      (* Conservative: for unknown instructions, assume they might read. *)
      false

  (* Find the next occurrence of a register within the same basic block.
     Returns: - `WriteFound if the next occurrence is a write (without a read) -
     `ReadFound if a read occurs before any write - `NotFound if we reach the
     end of block or the list *)
  type next_occurrence =
    | WriteFound
    | ReadFound
    | NotFound

  let find_next_occurrence_of_register target start_cell =
    let rec loop cell_opt =
      match cell_opt with
      | None -> NotFound
      | Some cell -> (
        match DLL.value cell with
        | Ins instr ->
          if is_control_flow instr
          then NotFound
          else if reads_from_arg target instr
          then ReadFound
          else if writes_to_arg target instr
          then WriteFound
          else loop (DLL.next cell)
        | Directive _ ->
          if is_hard_barrier (DLL.value cell)
          then NotFound
          else loop (DLL.next cell))
    in
    loop (DLL.next start_cell)

  (* Rewrite rule: remove MOV x, x (moving a value to itself) Note: We can only
     safely remove self-moves for registers that don't have zero-extension side
     effects. On x86-64: - 32-bit moves (Reg32) zero the upper 32 bits - SIMD
     moves (Regf) may zero upper bits depending on instruction encoding So we
     only optimize 8/16/64-bit integer register self-moves. *)
  let remove_mov_x_x cell =
    match[@warning "-4"] DLL.value cell with
    | Ins (MOV (src, dst)) when equal_args src dst && is_safe_self_move_arg src
      ->
      (* Get next cell before deleting *)
      let next = DLL.next cell in
      (* Delete the redundant instruction *)
      DLL.delete_curr cell;
      (* Continue from the next cell *) Some next
    | _ -> None

  (* Rewrite rule: remove useless MOV x, y; MOV y, x pattern *)
  let remove_useless_mov cell =
    match get_cells cell 2 with
    | [cell1; cell2] -> (
      match[@warning "-4"] DLL.value cell1, DLL.value cell2 with
      | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2))
        when equal_args src1 dst2 && equal_args dst1 src2 ->
        (* Get the cell after cell2 before deleting *)
        let after_cell2 = DLL.next cell2 in
        (* Delete the second MOV (the first one is still useful) *)
        DLL.delete_curr cell2;
        (* Continue from the cell after the deleted one *)
        Some after_cell2
      | _, _ -> None)
    | _ -> None

  (* Rewrite rule: combine adjacent ADD to RSP with CFI directives. Pattern:
     addq $n1, %rsp; .cfi_adjust_cfa_offset d1; addq $n2, %rsp;
     .cfi_adjust_cfa_offset d2 Rewrite: addq $(n1+n2), %rsp;
     .cfi_adjust_cfa_offset (d1+d2)

     This only applies when d1 = -n1 and d2 = -n2 (i.e., the CFI offsets
     correctly track the stack adjustment). *)
  let combine_add_rsp cell =
    match get_cells cell 4 with
    | [cell1; cell2; cell3; cell4] -> (
      match[@warning "-4"]
        DLL.value cell1, DLL.value cell2, DLL.value cell3, DLL.value cell4
      with
      | ( Ins (ADD (Imm n1, Reg64 RSP)),
          Directive
            (Asm_targets.Asm_directives.Directive.Cfi_adjust_cfa_offset d1),
          Ins (ADD (Imm n2, Reg64 RSP)),
          Directive
            (Asm_targets.Asm_directives.Directive.Cfi_adjust_cfa_offset d2) )
        when Int64.equal (Int64.of_int d1) (Int64.neg n1)
             && Int64.equal (Int64.of_int d2) (Int64.neg n2) ->
        (* Combine the instructions *)
        let combined_imm = Int64.add n1 n2 in
        let combined_offset = d1 + d2 in
        (* Update cells with combined values *)
        DLL.set_value cell1 (Ins (ADD (Imm combined_imm, Reg64 RSP)));
        DLL.set_value cell2
          (Directive
             (Asm_targets.Asm_directives.make_cfi_adjust_cfa_offset_directive
                combined_offset));
        (* Delete the redundant cells *)
        DLL.delete_curr cell3;
        DLL.delete_curr cell4;
        (* Return cell1 to allow iterative combination of multiple ADDs *)
        Some (Some cell1)
      | _, _, _, _ -> None)
    | _ -> None

  (* Rewrite rule: optimize MOV chain that writes to intermediate register.
     Pattern: mov A, x; mov x, y; mov B, x Rewrite: mov A, y; mov B, x

     This is safe when B ≠ x (otherwise we'd incorrectly eliminate a write). The
     transformation preserves the final values: x = B, y = A.

     Additionally, both x and y must be registers to ensure the rewritten
     instruction mov A, y is valid (x86 cannot have both operands as memory). *)
  let remove_mov_chain cell =
    match get_cells cell 3 with
    | [cell1; cell2; cell3] -> (
      match[@warning "-4"]
        DLL.value cell1, DLL.value cell2, DLL.value cell3
      with
      | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2)), Ins (MOV (src3, dst3))
        when equal_args dst1 src2 && equal_args dst1 dst3
             && (not (equal_args src3 dst3))
             && is_register dst1 && is_register dst2 ->
        (* Pattern: mov A, x; mov x, y; mov B, x where B ≠ x and x, y are
           registers *)
        (* Rewrite to: mov A, y; mov B, x *)
        DLL.set_value cell1 (Ins (MOV (src1, dst2)));
        DLL.set_value cell2 (Ins (MOV (src3, dst3)));
        DLL.delete_curr cell3;
        (* Return cell1 to allow iterative combination of MOV chains *)
        Some (Some cell1)
      | _, _, _ -> None)
    | _ -> None

  (* Check if a register is safe for dead register optimization. We restrict to
     Reg64 to avoid aliasing issues: our liveness analysis doesn't track that
     writes to %eax (Reg32) also affect %rax (Reg64). *)
  let is_safe_for_dead_register_opt = function[@warning "-4"]
    | Reg64 _ -> true
    | _ -> false

  (* Rewrite rule: optimize MOV to register that is overwritten before use.
     Pattern: mov A, x; mov x, y where the next occurrence of x is a write.
     Rewrite: mov A, y

     This is safe when both x and y are registers and x is not read before the
     next write to x within the same basic block. The transformation preserves
     semantics: y gets the value of A, and x is overwritten before being read.

     We restrict x to Reg64 to avoid register aliasing issues. *)
  let remove_mov_to_dead_register cell =
    match get_cells cell 2 with
    | [cell1; cell2] -> (
      match[@warning "-4"] DLL.value cell1, DLL.value cell2 with
      | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2))
        when equal_args dst1 src2 && is_register dst1 && is_register dst2
             && is_safe_for_dead_register_opt dst1 -> (
        (* Pattern: mov A, x; mov x, y where x and y are registers *)
        (* Check if the next occurrence of x is a write *)
        match find_next_occurrence_of_register dst1 cell2 with
        | WriteFound ->
          (* x is written before being read, so we can optimize *)
          (* Rewrite to: mov A, y *)
          DLL.set_value cell1 (Ins (MOV (src1, dst2)));
          DLL.delete_curr cell2;
          (* Return cell1 to allow iterative combination *)
          Some (Some cell1)
        | ReadFound | NotFound ->
          (* x is read before write, or we can't determine - don't optimize *)
          None)
      | _, _ -> None)
    | _ -> None

  (* Apply all rewrite rules in sequence. Returns Some continuation_cell if a
     rule matched, None otherwise. *)
  let apply_rules cell =
    match remove_mov_x_x cell with
    | Some cont -> Some cont
    | None -> (
      match remove_useless_mov cell with
      | Some cont -> Some cont
      | None -> (
        match remove_mov_chain cell with
        | Some cont -> Some cont
        | None -> (
          match remove_mov_to_dead_register cell with
          | Some cont -> Some cont
          | None -> (
            match combine_add_rsp cell with
            | Some cont -> Some cont
            | None -> None))))

  (* Main optimization loop for a single asm_program. Iterates through the
     instruction list, applying rewrite rules and respecting hard barriers. *)
  let peephole_optimize_asm_program asm_program =
    let rec optimize_from cell_opt =
      match cell_opt with
      | None -> ()
      | Some cell -> (
        if is_hard_barrier (DLL.value cell)
        then
          (* Skip hard barriers and continue after them *)
          optimize_from (DLL.next cell)
        else
          match apply_rules cell with
          | Some continuation_cell ->
            (* A rule was applied, continue from the continuation point *)
            optimize_from continuation_cell
          | None ->
            (* No rule matched, move to the next instruction *)
            optimize_from (DLL.next cell))
    in
    optimize_from (DLL.hd_cell asm_program)
end

let generate_code asm =
  (* Apply peephole optimizations to asm_code. TODO: Extend this to optimize all
     sections in asm_code_by_section, not just asm_code. *)
  if !Oxcaml_flags.x86_peephole_optimize
  then X86_peephole.peephole_optimize_asm_program asm_code;
  (match asm with
  | Some f -> Profile.record ~accumulate:true "write_asm" f asm_code
  | None -> ());
  match !internal_assembler with
  | Some f ->
    let get sections =
      Section_name.Tbl.fold
        (fun name instrs acc -> (name, instrs) :: acc)
        sections []
    in
    let instrs = get asm_code_by_section in
    let delayed () = get delayed_sections in
    binary_content := Some (f ~delayed instrs)
  | None -> binary_content := None
