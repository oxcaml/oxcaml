[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list
open X86_ast_utils

type rule_result =
  | No_match
  | Matched of asm_line DLL.cell option

let is_control_flow = function
  | J _ | JMP _ | CALL _ | RET | HLT | LEAVE -> true
  | MOV _ | MOVSX _ | MOVSXD _ | MOVZX _ | PUSH _ | POP _ | LEA _ | ADD _
  | SUB _ | IMUL _ | MUL _ | IDIV _ | AND _ | OR _ | XOR _ | SAL _ | SAR _
  | SHR _ | CMP _ | TEST _ | INC _ | DEC _ | NEG _ | CDQ | CQO | SET _ | CMOV _
  | BSF _ | BSR _ | BSWAP _ | XCHG _ | LOCK_CMPXCHG _ | LOCK_XADD _ | LOCK_ADD _
  | LOCK_SUB _ | LOCK_AND _ | LOCK_OR _ | LOCK_XOR _ | CLDEMOTE _ | PREFETCH _
  | NOP | PAUSE | RDTSC | RDPMC | LFENCE | SFENCE | MFENCE | SIMD _ | ADC _
  | SBB _ ->
    false

let is_hard_barrier = function
  | Directive d -> (
    match d with
    | New_label _ | Bytes _ | Cfi_startproc | Cfi_endproc | Section _ -> true
    | Align _ | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _ | Cfi_offset _
    | Cfi_remember_state | Cfi_restore_state | Cfi_def_cfa_register _
    | Comment _ | Const _ | Direct_assignment _ | File _ | Global _
    | Indirect_symbol _ | Loc _ | New_line | Private_extern _ | Size _
    | Sleb128 _ | Space _ | Type _ | Uleb128 _ | Protected _ | Hidden _ | Weak _
    | External _ | Reloc _ ->
      false)
  | Ins instr -> is_control_flow instr

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

let is_register = function
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ -> true
  | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> false

let underlying_reg64 = function
  | Reg64 r | Reg32 r | Reg16 r | Reg8L r -> Some r
  | Reg8H h -> (
    match h with
    | AH -> Some RAX
    | BH -> Some RBX
    | CH -> Some RCX
    | DH -> Some RDX)
  | Regf _ | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> None

let is_reg64_subregister reg arg =
  match underlying_reg64 arg with Some r -> equal_reg64 reg r | None -> false

let arg_contains_reg64 target arg =
  match arg with
  | Mem addr -> (
    (match addr.base with Some r -> equal_reg64 target r | None -> false)
    || addr.scale <> 0
       &&
       match addr.idx with
       | Scalar r -> equal_reg64 target r
       | Vector _ -> false)
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ ->
    equal_reg64 target (underlying_reg64 arg |> Option.get)
  | Imm _ | Sym _ | Regf _ | Mem64_RIP _ -> false

let reg64_read_when_writing target arg =
  match arg with
  | Mem _ -> arg_contains_reg64 target arg
  (* Writing to small registers implicitly reads the old register value to
     update. *)
  | Reg8L _ | Reg8H _ | Reg16 _ ->
    equal_reg64 target (underlying_reg64 arg |> Option.get)
  | Reg32 _ | Reg64 _ | Regf _ | Imm _ | Sym _ | Mem64_RIP _ -> false

let writes_to_reg64 target = function
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
  | CMOV (_, _, dst)
  | ADC (_, dst)
  | SBB (_, dst)
  | INC dst
  | DEC dst
  | NEG dst
  | BSWAP dst
  | SET (_, dst)
  | IMUL (_, Some dst)
  | LOCK_ADD (_, dst)
  | LOCK_SUB (_, dst)
  | LOCK_AND (_, dst)
  | LOCK_OR (_, dst)
  | LOCK_XOR (_, dst) ->
    is_reg64_subregister target dst
  | POP dst -> is_reg64_subregister target dst || equal_reg64 target RSP
  | LOCK_XADD (src, dst) ->
    is_reg64_subregister target src || is_reg64_subregister target dst
  | XCHG (op1, op2) ->
    is_reg64_subregister target op1 || is_reg64_subregister target op2
  | MUL _ | IMUL (_, None) | IDIV _ ->
    equal_reg64 target RAX || equal_reg64 target RDX
  | CDQ | CQO -> equal_reg64 target RDX
  | LOCK_CMPXCHG (_, dst) ->
    is_reg64_subregister target dst || equal_reg64 target RAX
  | PUSH _ -> equal_reg64 target RSP
  | LEAVE -> equal_reg64 target RBP || equal_reg64 target RSP
  | RDTSC | RDPMC ->
    (* Rare instructions, let's be conservative. *)
    true
  | J _ | JMP _ | CALL _ | RET | HLT ->
    (* These are all control flow operations, there is no point in assuming
       anything. *)
    true
  | CMP _ | TEST _ | CLDEMOTE _ | PREFETCH _ | NOP | PAUSE | LFENCE | SFENCE
  | MFENCE ->
    false
  | SIMD _ ->
    (* Conservative: assume any SIMD instruction may write to target. *)
    true

let reads_from_reg64 target = function
  | MOV (src, dst)
  | MOVSX (src, dst)
  | MOVSXD (src, dst)
  | MOVZX (src, dst)
  | LEA (src, dst)
  | BSF (src, dst)
  | BSR (src, dst) ->
    arg_contains_reg64 target src || reg64_read_when_writing target dst
  | PUSH src -> arg_contains_reg64 target src || equal_reg64 target RSP
  | ADD (src, dst)
  | SUB (src, dst)
  | AND (src, dst)
  | OR (src, dst)
  | XOR (src, dst)
  | CMP (src, dst)
  | TEST (src, dst)
  | ADC (src, dst)
  | SBB (src, dst)
  | SAL (src, dst)
  | SAR (src, dst)
  | SHR (src, dst)
  | CMOV (_, src, dst) ->
    arg_contains_reg64 target src || arg_contains_reg64 target dst
  | INC dst | DEC dst | NEG dst | BSWAP dst -> arg_contains_reg64 target dst
  | IMUL (op1, Some op2)
  | XCHG (op1, op2)
  | LOCK_XADD (op1, op2)
  | LOCK_ADD (op1, op2)
  | LOCK_SUB (op1, op2)
  | LOCK_AND (op1, op2)
  | LOCK_OR (op1, op2)
  | LOCK_XOR (op1, op2) ->
    arg_contains_reg64 target op1 || arg_contains_reg64 target op2
  | MUL op | IMUL (op, None) ->
    arg_contains_reg64 target op || equal_reg64 target RAX
  | IDIV op ->
    arg_contains_reg64 target op
    || equal_reg64 target RAX || equal_reg64 target RDX
  | CDQ -> equal_reg64 target RAX
  | CQO -> equal_reg64 target RAX
  | LOCK_CMPXCHG (op1, op2) ->
    arg_contains_reg64 target op1
    || arg_contains_reg64 target op2
    || equal_reg64 target RAX
  | SET (_, dst) -> reg64_read_when_writing target dst
  | POP dst -> reg64_read_when_writing target dst || equal_reg64 target RSP
  | LEAVE -> equal_reg64 target RBP
  | CLDEMOTE arg -> arg_contains_reg64 target arg
  | PREFETCH (_, _, arg) -> arg_contains_reg64 target arg
  | J _ | JMP _ | CALL _ | RET | HLT ->
    (* These are all control flow operations, there is no point in assuming
       anything. *)
    true
  | RDTSC | RDPMC ->
    (* Rare instructions, let's be conservative. *)
    true
  | NOP | PAUSE | LFENCE | SFENCE | MFENCE -> false
  (* Conservative: assume SIMD instructions may read from target. *)
  | SIMD _ -> true

let reg64_is_never_read target start_cell =
  let rec loop cell_opt =
    match cell_opt with
    | None -> false
    | Some cell -> (
      let value = DLL.value cell in
      if is_hard_barrier value
      then false
      else
        match value with
        | Ins instr ->
          (not (reads_from_reg64 target instr))
          && (writes_to_reg64 target instr || loop (DLL.next cell))
        | Directive _ -> loop (DLL.next cell))
  in
  loop (DLL.next start_cell)

let writes_flags = function
  | ADD _ | SUB _ | AND _ | OR _ | XOR _ | CMP _ | TEST _ | INC _ | DEC _
  | NEG _ | MUL _ | IMUL _ | IDIV _ | BSF _ | BSR _ | SAL _ | SAR _ | SHR _
  | LOCK_ADD _ | LOCK_SUB _ | LOCK_AND _ | LOCK_OR _ | LOCK_XOR _ | LOCK_XADD _
  | LOCK_CMPXCHG _ | ADC _ | SBB _ ->
    true
  | MOV _ | MOVSX _ | MOVSXD _ | MOVZX _ | PUSH _ | POP _ | LEA _ | CDQ | CQO
  | SET _ | CMOV _ | BSWAP _ | XCHG _ | CLDEMOTE _ | PREFETCH _ | NOP | PAUSE
  | RDTSC | RDPMC | LFENCE | SFENCE | MFENCE ->
    false
  | J _ | JMP _ | CALL _ | RET | HLT | LEAVE ->
    (* These are all control flow operations, there is no point in assuming
       anything. *)
    true
  | SIMD _ ->
    (* Conservative: some SIMD instructions (e.g. comisd, ucomiss) write
       flags. *)
    true
