[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open X86_ast
module DLL = Doubly_linked_list
open X86_ast_utils

type rule_result =
  | No_match
  | Matched of asm_line DLL.cell option

let is_control_flow = function
  | J _ | JMP _ | CALL _ | RET | HLT | UD2 -> true
  | LEAVE | MOV _ | MOVSX _ | MOVSXD _ | MOVZX _ | PUSH _ | POP _ | LEA _
  | ADD _ | SUB _ | IMUL _ | MUL _ | IDIV _ | DIV _ | AND _ | OR _ | XOR _
  | SAL _ | SAR _ | SHR _ | CMP _ | TEST _ | INC _ | DEC _ | NEG _ | NOT _ | CDQ
  | CQO | SET _ | CMOV _ | BSF _ | BSR _ | BSWAP _ | XCHG _ | LOCK_CMPXCHG _
  | LOCK_XADD _ | LOCK_ADD _ | LOCK_SUB _ | LOCK_AND _ | LOCK_OR _ | LOCK_XOR _
  | CLDEMOTE _ | PREFETCH _ | NOP | PAUSE | RDTSC | RDPMC | LFENCE | SFENCE
  | MFENCE | SIMD _ | ADC _ | SBB _ ->
    false

let is_hard_barrier = function
  | Directive d -> (
    match d with
    | New_label (Label label, _) ->
      not (Asm_targets.Asm_label.is_dwarf_only_label label)
    | New_label (Symbol _, _)
    | Bytes _ | Cfi_startproc | Cfi_endproc | Section _ ->
      true
    | Align _ | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _ | Cfi_offset _
    | Cfi_rel_offset _ | Cfi_restore _ | Cfi_remember_state | Cfi_restore_state
    | Cfi_def_cfa_register _ | Comment _ | Const _ | Direct_assignment _
    | File _ | Global _ | Indirect_symbol _ | Loc _ | New_line
    | Private_extern _ | Size _ | Sleb128 _ | Space _ | Type _ | Uleb128 _
    | Protected _ | Hidden _ | Weak _ | External _ | Reloc _ | Delta_uleb128 _
      ->
      false)
  | Ins instr -> is_control_flow instr

(* Directives that only carry debugging information: source locations,
   comments, and labels that only delimit DWARF ranges. Rules see the
   instructions on either side of them as adjacent, so that the generated code
   does not depend on whether debugging information is emitted. *)
(* CR-someday mshinwell: this could be extended to CFI directives, whose
   presence also depends on the configuration, but there are tricky details:

   - [Cfi_startproc] and [Cfi_endproc] delimit functions and must remain
   barriers.

   - a CFI directive describes the state at its position, so a rule that merges
   or deletes instructions across one must make sure that it still describes
   the right position.

   - [combine_add_rsp] matches on [Cfi_adjust_cfa_offset] and rewrites it.

   - after [Cfi_def_cfa_register] unwinding relies on the run-time value of a
   register, and after [Cfi_offset] on the contents of a stack slot. The rules
   would have to track which register holds the CFA and which slots hold saved
   registers, and treat these as read: [reg64_is_never_read] knows nothing of
   them, so [remove_mov_to_dead_register] could forward or remove the move that
   sets up the CFA register, leaving correct code with wrong unwinding
   information.

   - [Cfi_remember_state] and [Cfi_restore_state] save and restore all of the
   above. *)
(* CR mshinwell: looking through DWARF-only labels keeps the generated code
   independent of the debugging information, but those labels delimit the
   ranges over which variables are described as being in a register, which
   were computed before this pass. A rule that removes or forwards the
   definition of a register across such a label (for example
   [remove_mov_to_dead_register]) can leave a variable described as being in a
   register that no longer holds it, until that register is next written.

   This is one case of a more general problem: the ranges are computed on
   Linear, before the code is final. They should probably be computed on the
   x86 and arm64 DSLs instead. Operations such as allocation could then be
   expanded in those representations before the ranges are computed, rather
   than being opaque to the DWARF analyses, which in some cases makes them
   invalidate register information prematurely. *)
let is_debug_directive = function
  | Directive d -> (
    match d with
    | New_label (Label label, _) ->
      Asm_targets.Asm_label.is_dwarf_only_label label
    | Loc _ | File _ | Comment _ | New_line -> true
    | New_label (Symbol _, _)
    | Bytes _ | Cfi_startproc | Cfi_endproc | Section _ | Align _
    | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _ | Cfi_offset _
    | Cfi_rel_offset _ | Cfi_restore _ | Cfi_remember_state | Cfi_restore_state
    | Cfi_def_cfa_register _ | Const _ | Direct_assignment _ | Global _
    | Indirect_symbol _ | Private_extern _ | Size _ | Sleb128 _ | Space _
    | Type _ | Uleb128 _ | Protected _ | Hidden _ | Weak _ | External _
    | Reloc _ | Delta_uleb128 _ ->
      false)
  | Ins _ -> false

let get_cells cell n =
  let rec loop acc remaining current_opt =
    if remaining <= 0
    then List.rev acc
    else
      match current_opt with
      | None -> List.rev acc
      | Some current ->
        if is_debug_directive (DLL.value current)
        then loop acc remaining (DLL.next current)
        else loop (current :: acc) (remaining - 1) (DLL.next current)
  in
  if n <= 0 then [] else loop [cell] (n - 1) (DLL.next cell)

let is_register = function
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ -> true
  | Regmask _ | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> false

let underlying_reg64 = function
  | Reg64 r | Reg32 r | Reg16 r | Reg8L r -> Some r
  | Reg8H h -> (
    match h with
    | AH -> Some RAX
    | BH -> Some RBX
    | CH -> Some RCX
    | DH -> Some RDX)
  | Regf _ | Regmask _ | Imm _ | Sym _ | Mem _ | Mem64_RIP _ -> None

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
  | Imm _ | Sym _ | Regf _ | Regmask _ | Mem64_RIP _ -> false

let reg64_read_when_writing target arg =
  match arg with
  | Mem _ -> arg_contains_reg64 target arg
  (* Writing to small registers implicitly reads the old register value to
     update. *)
  | Reg8L _ | Reg8H _ | Reg16 _ ->
    equal_reg64 target (underlying_reg64 arg |> Option.get)
  | Reg32 _ | Reg64 _ | Regf _ | Regmask _ | Imm _ | Sym _ | Mem64_RIP _ ->
    false

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
  | NOT dst
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
  | MUL _ | IMUL (_, None) | IDIV _ | DIV _ ->
    equal_reg64 target RAX || equal_reg64 target RDX
  | CDQ | CQO -> equal_reg64 target RDX
  | LOCK_CMPXCHG (_, dst) ->
    is_reg64_subregister target dst || equal_reg64 target RAX
  | PUSH _ -> equal_reg64 target RSP
  | LEAVE -> equal_reg64 target RBP || equal_reg64 target RSP
  | RDTSC | RDPMC ->
    (* Rare instructions, let's be conservative. *)
    true
  | J _ | JMP _ | CALL _ | RET | HLT | UD2 ->
    (* These are all control flow operations, there is no point in assuming
       anything. *)
    true
  | CMP _ | TEST _ | CLDEMOTE _ | PREFETCH _ | NOP | PAUSE | LFENCE | SFENCE
  | MFENCE ->
    false
  | SIMD _ ->
    (* Conservative: assume any SIMD instruction may write to target. *)
    true

let arg_unchanged_by arg instr =
  match arg with
  | Imm _ | Sym _ -> true
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ ->
    not (writes_to_reg64 (underlying_reg64 arg |> Option.get) instr)
  | Regmask _ | Regf _ | Mem _ | Mem64_RIP _ -> false

let reads_from_reg64 target = function
  | MOV (src, dst)
  | MOVSX (src, dst)
  | MOVSXD (src, dst)
  | MOVZX (src, dst)
  | LEA (src, dst) ->
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
  (* BSF/BSR leave the destination unchanged when the source is zero (or
     truncated to 32bit when using 32bit operand size on some older Intel
     processors), so, like CMOV, they conditionally preserve - i.e. read - the
     destination. *)
  | BSF (src, dst)
  | BSR (src, dst)
  | CMOV (_, src, dst) ->
    arg_contains_reg64 target src || arg_contains_reg64 target dst
  | INC dst | DEC dst | NEG dst | NOT dst | BSWAP dst ->
    arg_contains_reg64 target dst
  | IMUL (op1, Some op2)
  | XCHG (op1, op2)
  | LOCK_XADD (op1, op2)
  | LOCK_ADD (op1, op2)
  | LOCK_SUB (op1, op2)
  | LOCK_AND (op1, op2)
  | LOCK_OR (op1, op2)
  | LOCK_XOR (op1, op2) ->
    arg_contains_reg64 target op1 || arg_contains_reg64 target op2
  | MUL op | IMUL (op, None) | IDIV op | DIV op ->
    (* MUL / IMUL don't usually read RDX, but the 16bit version does and in any
       case, we assume that these instructions write RDX, which is not true for
       the 8bit version. Assuming that we read RDX makes this conservative,
       because first reading and then writing can simulate no change. *)
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
  | J _ | JMP _ | CALL _ | RET | HLT | UD2 ->
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

let flags_never_observed start_cell =
  let rec loop cell_opt =
    match cell_opt with
    | None -> false
    | Some cell -> (
      let value = DLL.value cell in
      match value with
      | Directive _ ->
        if is_hard_barrier value then false else loop (DLL.next cell)
      | Ins instr -> (
        match instr with
        (* Instructions that read flags. *)
        | J _ | SET _ | CMOV _ | ADC _ | SBB _ -> false
        (* We don't assume anything beyond control flow instrutions. *)
        | JMP _ | CALL _ | RET | HLT | UD2 -> false
        (* Conservative: assume SIMD instructions may read flags. *)
        | SIMD _ -> false
        (* Instructions that overwrite all observable flags. *)
        | ADD _ | SUB _ | AND _ | OR _ | XOR _ | CMP _ | TEST _ | NEG _
        | LOCK_ADD _ | LOCK_SUB _ | LOCK_AND _ | LOCK_OR _ | LOCK_XOR _
        | LOCK_XADD _ | LOCK_CMPXCHG _ ->
          true
        (* Instructions that write only some flags or leave some undefined: keep
           scanning. *)
        | INC _ | DEC _ | MUL _ | IMUL _ | IDIV _ | DIV _ | BSF _ | BSR _
        | SAL _ | SAR _ | SHR _ ->
          loop (DLL.next cell)
        (* Instructions that don't touch the flags. *)
        | MOV _ | MOVSX _ | MOVSXD _ | MOVZX _ | PUSH _ | POP _ | LEA _ | CDQ
        | CQO | BSWAP _ | XCHG _ | CLDEMOTE _ | PREFETCH _ | NOP | PAUSE | RDTSC
        | RDPMC | LFENCE | SFENCE | MFENCE | LEAVE | NOT _ ->
          loop (DLL.next cell)))
  in
  loop (DLL.next start_cell)

let maybe_writes_flags = function
  | ADD _ | SUB _ | AND _ | OR _ | XOR _ | CMP _ | TEST _ | INC _ | DEC _
  | NEG _ | MUL _ | IMUL _ | IDIV _ | DIV _ | BSF _ | BSR _ | SAL _ | SAR _
  | SHR _ | LOCK_ADD _ | LOCK_SUB _ | LOCK_AND _ | LOCK_OR _ | LOCK_XOR _
  | LOCK_XADD _ | LOCK_CMPXCHG _ | ADC _ | SBB _ ->
    true
  | MOV _ | MOVSX _ | MOVSXD _ | MOVZX _ | PUSH _ | POP _ | LEA _ | CDQ | CQO
  | SET _ | CMOV _ | BSWAP _ | XCHG _ | CLDEMOTE _ | PREFETCH _ | NOP | PAUSE
  | RDTSC | RDPMC | LFENCE | SFENCE | MFENCE | LEAVE | NOT _ ->
    false
  | J _ | JMP _ | CALL _ | RET | HLT | UD2 ->
    (* These are all control flow operations, there is no point in assuming
       anything. *)
    true
  | SIMD _ ->
    (* Conservative: some SIMD instructions (e.g. comisd, ucomiss) write
       flags. *)
    true
