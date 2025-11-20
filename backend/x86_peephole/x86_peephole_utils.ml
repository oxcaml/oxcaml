[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

type next_occurrence =
  | WriteFound
  | ReadFound
  | NotFound

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
  match left, right with X64, X64 | X86, X86 -> true | (X64 | X86), _ -> false

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

let get_cells cell n =
  let rec loop acc remaining current_opt =
    if remaining <= 0
    then List.rev acc
    else
      match current_opt with
      | None -> List.rev acc
      | Some current -> loop (current :: acc) (remaining - 1) (DLL.next current)
  in
  loop [] n (Some cell)

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

let is_register = function
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ -> true
  | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> false

let is_safe_self_move_arg = function[@warning "-4"]
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg64 _ -> true
  | _ -> false

let is_safe_for_dead_register_opt = function[@warning "-4"]
  | Reg64 _ -> true
  | _ -> false

let underlying_reg64 = function[@warning "-4"]
  | Reg64 r | Reg32 r | Reg16 r | Reg8L r -> Some r
  | Reg8H h -> (
    match[@warning "-4"] h with
    | AH -> Some RAX
    | BH -> Some RBX
    | CH -> Some RCX
    | DH -> Some RDX)
  | Regf _ | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> None

let registers_alias arg1 arg2 =
  match underlying_reg64 arg1, underlying_reg64 arg2 with
  | Some r1, Some r2 -> equal_reg64 r1 r2
  | _ -> false

let reg_appears_in_arg target arg =
  if registers_alias target arg
  then true
  else
    match[@warning "-4"] arg with
    | Mem addr -> (
      match[@warning "-4"] target with
      | Reg64 r | Reg32 r | Reg16 r | Reg8L r ->
        (match addr.base with
        | Some base when equal_reg64 r base -> true
        | _ -> false)
        || (addr.scale <> 0 && equal_reg64 r addr.idx)
      | _ -> false)
    | _ -> false

let reg_is_written_by_arg target arg =
  match[@warning "-4"] arg with
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ ->
    registers_alias target arg
  | Regf _ -> equal_args target arg
  | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> false

let reg_in_memory_address target arg =
  match[@warning "-4"] arg with
  | Mem addr -> (
    match underlying_reg64 target with
    | Some target_r64 ->
      (match addr.base with
      | Some base when equal_reg64 target_r64 base -> true
      | _ -> false)
      || (addr.scale <> 0 && equal_reg64 target_r64 addr.idx)
    | None -> false)
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ | Imm _ | Sym _
  | Mem64_RIP _ ->
    false

let is_control_flow = function[@warning "-4"]
  | J _ | JMP _ | CALL _ | RET -> true
  | _ -> false

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
    reg_is_written_by_arg target dst
  | INC dst | DEC dst | NEG dst | BSWAP dst | SET (_, dst) ->
    reg_is_written_by_arg target dst
  | POP dst -> reg_is_written_by_arg target dst
  | IMUL (_, Some dst) -> reg_is_written_by_arg target dst
  | LOCK_XADD (_, dst) -> reg_is_written_by_arg target dst
  | XCHG (op1, op2) ->
    reg_is_written_by_arg target op1 || reg_is_written_by_arg target op2
  | MUL _ | IMUL (_, None) ->
    equal_args target (Reg64 RAX) || equal_args target (Reg64 RDX)
  | IDIV _ -> equal_args target (Reg64 RAX) || equal_args target (Reg64 RDX)
  | CDQ -> equal_args target (Reg32 RAX)
  | CQO -> equal_args target (Reg64 RDX)
  | LOCK_CMPXCHG (_, dst) ->
    reg_is_written_by_arg target dst || equal_args target (Reg64 RAX)
  | _ -> false

let reads_from_arg target = function[@warning "-4"]
  | MOV (src, dst) | MOVSX (src, dst) | MOVSXD (src, dst) | MOVZX (src, dst) ->
    reg_appears_in_arg target src || reg_in_memory_address target dst
  | PUSH src -> reg_appears_in_arg target src
  | ADD (src, dst)
  | SUB (src, dst)
  | AND (src, dst)
  | OR (src, dst)
  | XOR (src, dst)
  | CMP (src, dst)
  | TEST (src, dst) ->
    reg_appears_in_arg target src || reg_appears_in_arg target dst
  | LEA (src, _)
  | BSF (src, _)
  | BSR (src, _)
  | POPCNT (src, _)
  | TZCNT (src, _)
  | LZCNT (src, _) ->
    reg_appears_in_arg target src
  | SAL (src, dst) | SAR (src, dst) | SHR (src, dst) ->
    reg_appears_in_arg target src || reg_appears_in_arg target dst
  | CMOV (_, src, dst) ->
    reg_appears_in_arg target src || reg_appears_in_arg target dst
  | INC dst | DEC dst | NEG dst | BSWAP dst -> reg_appears_in_arg target dst
  | IMUL (op1, Some op2) ->
    reg_appears_in_arg target op1 || reg_appears_in_arg target op2
  | MUL op -> reg_appears_in_arg target op || equal_args target (Reg64 RAX)
  | IMUL (op, None) ->
    reg_appears_in_arg target op || equal_args target (Reg64 RAX)
  | IDIV op ->
    reg_appears_in_arg target op
    || equal_args target (Reg64 RAX)
    || equal_args target (Reg64 RDX)
  | CDQ -> equal_args target (Reg32 RAX)
  | CQO -> equal_args target (Reg64 RAX)
  | CALL arg | JMP arg | J (_, arg) -> reg_appears_in_arg target arg
  | XCHG (op1, op2) ->
    reg_appears_in_arg target op1 || reg_appears_in_arg target op2
  | LOCK_CMPXCHG (op1, op2) ->
    reg_appears_in_arg target op1
    || reg_appears_in_arg target op2
    || equal_args target (Reg64 RAX)
  | LOCK_XADD (op1, op2)
  | LOCK_ADD (op1, op2)
  | LOCK_SUB (op1, op2)
  | LOCK_AND (op1, op2)
  | LOCK_OR (op1, op2)
  | LOCK_XOR (op1, op2) ->
    reg_appears_in_arg target op1 || reg_appears_in_arg target op2
  | SET (_, dst) -> reg_in_memory_address target dst
  | POP dst -> reg_in_memory_address target dst
  | CLDEMOTE arg -> reg_appears_in_arg target arg
  | PREFETCH (_, _, arg) -> reg_appears_in_arg target arg
  | _ -> true

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

let reads_flags = function[@warning "-4"]
  | J _ | CMOV _ | SET _ -> true
  | _ -> false

let writes_flags = function[@warning "-4"]
  | ADD _ | SUB _ | AND _ | OR _ | XOR _ | CMP _ | TEST _ | INC _ | DEC _
  | NEG _ | MUL _ | IMUL _ | IDIV _ | BSF _ | BSR _ | SAL _ | SAR _ | SHR _
  | POPCNT _ | TZCNT _ | LZCNT _ | LOCK_ADD _ | LOCK_SUB _ | LOCK_AND _
  | LOCK_OR _ | LOCK_XOR _ | LOCK_XADD _ | LOCK_CMPXCHG _ ->
    true
  | _ -> false

let find_next_flag_use start_cell =
  let rec loop cell_opt =
    match cell_opt with
    | None -> NotFound
    | Some cell -> (
      match DLL.value cell with
      | Ins instr ->
        if is_control_flow instr
        then if reads_flags instr then ReadFound else NotFound
        else if reads_flags instr
        then ReadFound
        else if writes_flags instr
        then WriteFound
        else loop (DLL.next cell)
      | Directive _ ->
        if is_hard_barrier (DLL.value cell)
        then NotFound
        else loop (DLL.next cell))
  in
  loop (DLL.next start_cell)
