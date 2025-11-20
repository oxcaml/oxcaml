[@@@ocaml.warning "+a-29-40-41-42-4"]

open! Int_replace_polymorphic_compare
open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list
module U = X86_peephole_utils

type peephole_stats =
  { mutable remove_mov_x_x : int;
    mutable remove_useless_mov : int;
    mutable remove_mov_chain : int;
    mutable remove_mov_to_dead_register : int;
    mutable rewrite_mov_sequence_to_xchg : int;
    mutable rewrite_mov_add_to_lea : int;
    mutable rewrite_mov_add_reg_to_lea : int;
    mutable remove_redundant_cmp : int;
    mutable combine_add_rsp : int
  }

let create_peephole_stats () =
  { remove_mov_x_x = 0;
    remove_useless_mov = 0;
    remove_mov_chain = 0;
    remove_mov_to_dead_register = 0;
    rewrite_mov_sequence_to_xchg = 0;
    rewrite_mov_add_to_lea = 0;
    rewrite_mov_add_reg_to_lea = 0;
    remove_redundant_cmp = 0;
    combine_add_rsp = 0
  }

let peephole_stats_to_counters stats =
  Profile.Counters.create ()
  |> Profile.Counters.set "x86_peephole.remove_mov_x_x" stats.remove_mov_x_x
  |> Profile.Counters.set "x86_peephole.remove_useless_mov"
       stats.remove_useless_mov
  |> Profile.Counters.set "x86_peephole.remove_mov_chain" stats.remove_mov_chain
  |> Profile.Counters.set "x86_peephole.remove_mov_to_dead_register"
       stats.remove_mov_to_dead_register
  |> Profile.Counters.set "x86_peephole.rewrite_mov_sequence_to_xchg"
       stats.rewrite_mov_sequence_to_xchg
  |> Profile.Counters.set "x86_peephole.rewrite_mov_add_to_lea"
       stats.rewrite_mov_add_to_lea
  |> Profile.Counters.set "x86_peephole.rewrite_mov_add_reg_to_lea"
       stats.rewrite_mov_add_reg_to_lea
  |> Profile.Counters.set "x86_peephole.remove_redundant_cmp"
       stats.remove_redundant_cmp
  |> Profile.Counters.set "x86_peephole.combine_add_rsp" stats.combine_add_rsp

(* Rewrite rule: remove MOV x, x (moving a value to itself) Note: We can only
   safely remove self-moves for registers that don't have zero-extension side
   effects. On x86-64: - 32-bit moves (Reg32) zero the upper 32 bits - SIMD
   moves (Regf) may zero upper bits depending on instruction encoding So we only
   optimize 8/16/64-bit integer register self-moves. *)
let remove_mov_x_x stats cell =
  match[@warning "-4"] DLL.value cell with
  | Ins (MOV (src, dst))
    when U.equal_args src dst && U.is_safe_self_move_arg src ->
    (* Get next cell before deleting *)
    let next = DLL.next cell in
    (* Delete the redundant instruction *)
    DLL.delete_curr cell;
    stats.remove_mov_x_x <- stats.remove_mov_x_x + 1;
    (* Continue from the next cell *) Some next
  | _ -> None

(* Rewrite rule: remove useless MOV x, y; MOV y, x pattern *)
let remove_useless_mov stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    match[@warning "-4"] DLL.value cell1, DLL.value cell2 with
    | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2))
      when U.equal_args src1 dst2 && U.equal_args dst1 src2 ->
      (* Get the cell after cell2 before deleting *)
      let after_cell2 = DLL.next cell2 in
      (* Delete the second MOV (the first one is still useful) *)
      DLL.delete_curr cell2;
      stats.remove_useless_mov <- stats.remove_useless_mov + 1;
      (* Continue from the cell after the deleted one *)
      Some after_cell2
    | _, _ -> None)
  | _ -> None

(* Rewrite rule: combine adjacent ADD to RSP with CFI directives. Pattern: addq
   $n1, %rsp; .cfi_adjust_cfa_offset d1; addq $n2, %rsp; .cfi_adjust_cfa_offset
   d2 Rewrite: addq $(n1+n2), %rsp; .cfi_adjust_cfa_offset (d1+d2)

   This only applies when d1 = -n1 and d2 = -n2 (i.e., the CFI offsets correctly
   track the stack adjustment). *)
let combine_add_rsp stats cell =
  match U.get_cells cell 4 with
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
      stats.combine_add_rsp <- stats.combine_add_rsp + 1;
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
let remove_mov_chain stats cell =
  match U.get_cells cell 3 with
  | [cell1; cell2; cell3] -> (
    match[@warning "-4"] DLL.value cell1, DLL.value cell2, DLL.value cell3 with
    | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2)), Ins (MOV (src3, dst3))
      when U.equal_args dst1 src2 && U.equal_args dst1 dst3
           && (not (U.equal_args src3 dst3))
           && U.is_register dst1 && U.is_register dst2 ->
      (* Pattern: mov A, x; mov x, y; mov B, x where B ≠ x and x, y are
         registers *)
      (* Rewrite to: mov A, y; mov B, x *)
      DLL.set_value cell1 (Ins (MOV (src1, dst2)));
      DLL.set_value cell2 (Ins (MOV (src3, dst3)));
      DLL.delete_curr cell3;
      stats.remove_mov_chain <- stats.remove_mov_chain + 1;
      (* Return cell1 to allow iterative combination of MOV chains *)
      Some (Some cell1)
    | _, _, _ -> None)
  | _ -> None

(* Rewrite rule: optimize MOV to register that is overwritten before use.
   Pattern: mov A, x; mov x, y where the next occurrence of x is a write.
   Rewrite: mov A, y

   This is safe when both x and y are registers and x is not read before the
   next write to x within the same basic block. The transformation preserves
   semantics: y gets the value of A, and x is overwritten before being read.

   We restrict x to Reg64 to avoid register aliasing issues. *)
let remove_mov_to_dead_register stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    match[@warning "-4"] DLL.value cell1, DLL.value cell2 with
    | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2))
      when U.equal_args dst1 src2 && U.is_register dst1 && U.is_register dst2
           && U.is_safe_for_dead_register_opt dst1 -> (
      (* Pattern: mov A, x; mov x, y where x and y are registers *)
      (* Check if the next occurrence of x is a write *)
      match U.find_next_occurrence_of_register dst1 cell2 with
      | WriteFound ->
        (* x is written before being read, so we can optimize *)
        (* Rewrite to: mov A, y *)
        DLL.set_value cell1 (Ins (MOV (src1, dst2)));
        DLL.delete_curr cell2;
        stats.remove_mov_to_dead_register
          <- stats.remove_mov_to_dead_register + 1;
        (* Return cell1 to allow iterative combination *)
        Some (Some cell1)
      | ReadFound | NotFound ->
        (* x is read before write, or we can't determine - don't optimize *)
        None)
    | _, _ -> None)
  | _ -> None

(* Rewrite rule: optimize MOV sequence to XCHG. Pattern: mov %a, %b; mov %c, %a;
   mov %b, %c Rewrite: xchg %a, %c

   This is safe when: - All operands are Reg64 registers (to avoid aliasing
   issues) - %a, %b, %c are all distinct registers - %b is dead after the
   sequence (not read before next write)

   The transformation changes %b's final value, so %b must not be read. *)
let rewrite_mov_sequence_to_xchg stats cell =
  match U.get_cells cell 3 with
  | [cell1; cell2; cell3] -> (
    match[@warning "-4"] DLL.value cell1, DLL.value cell2, DLL.value cell3 with
    | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2)), Ins (MOV (src3, dst3))
      -> (
      (* Pattern: mov %a, %b; mov %c, %a; mov %b, %c src1=%a, dst1=%b, src2=%c,
         dst2=%a, src3=%b, dst3=%c *)
      match src1, dst1, src2, dst2, src3, dst3 with
      | Reg64 a, Reg64 b, Reg64 c, Reg64 a', Reg64 b', Reg64 c'
        when U.equal_reg64 a a' && U.equal_reg64 b b' && U.equal_reg64 c c'
             && (not (U.equal_reg64 a b))
             && (not (U.equal_reg64 a c))
             && not (U.equal_reg64 b c) -> (
        (* Check if %b is dead after the sequence *)
        match U.find_next_occurrence_of_register dst1 cell3 with
        | WriteFound ->
          (* %b is dead, safe to optimize *)
          (* Rewrite to: xchg %a, %c *)
          DLL.set_value cell1 (Ins (XCHG (src1, src2)));
          DLL.delete_curr cell2;
          DLL.delete_curr cell3;
          stats.rewrite_mov_sequence_to_xchg
            <- stats.rewrite_mov_sequence_to_xchg + 1;
          (* Return the cell we modified *)
          Some (Some cell1)
        | NotFound | ReadFound -> None)
      | _, _, _, _, _, _ -> None)
    | _, _, _ -> None)
  | _ -> None

(* Helper function: Apply LEA optimization after pattern matching. Checks flag
   liveness, creates LEA instruction, and replaces the instruction sequence. *)
let apply_lea_optimization cell1 cell2 dst idx_reg base_reg_opt displ =
  match U.find_next_flag_use cell2 with
  | WriteFound ->
    (* Flags are dead (written before read), safe to optimize *)
    let mem_operand =
      Mem
        { arch = X64;
          typ = QWORD;
          idx = idx_reg;
          scale = 1;
          base = base_reg_opt;
          sym = None;
          displ
        }
    in
    DLL.set_value cell1 (Ins (LEA (mem_operand, dst)));
    DLL.delete_curr cell2;
    (* Return the cell we modified *)
    Some (Some cell1)
  | ReadFound | NotFound ->
    (* Flags might be live, don't optimize *)
    None

(* Rewrite rule: optimize MOV followed by ADD/SUB/INC/DEC to LEA. Patterns: -
   mov %a, %b; add $CONST, %b → lea CONST(%a), %b - mov %a, %b; sub $CONST, %b →
   lea -CONST(%a), %b - mov %a, %b; inc %b → lea 1(%a), %b - mov %a, %b; dec %b
   → lea -1(%a), %b

   This is safe when: - Both operands are Reg64 registers - For ADD: CONST fits
   in 32-bit signed immediate - For SUB: CONST > Int32.min_int (so -CONST fits
   in 32-bit signed) - For INC/DEC: always safe (±1 always fits) - Flags written
   by ADD/SUB/INC/DEC are dead (LEA doesn't modify flags)

   Key difference: ADD/SUB/INC/DEC set flags (CF, OF, SF, ZF, AF, PF), LEA
   doesn't. *)
let rewrite_mov_add_to_lea stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    match[@warning "-4"] DLL.value cell1, DLL.value cell2 with
    | Ins (MOV (src1, dst1)), Ins (ADD (src2, dst2)) when U.equal_args dst1 dst2
      -> (
      (* Pattern: mov %a, %b; add $CONST, %b *)
      match src1, dst1, src2, dst2 with
      | Reg64 a, Reg64 b, Imm imm, Reg64 b'
        when U.equal_reg64 b b'
             && Int64.compare imm (Int64.of_int32 Int32.min_int) >= 0
             && Int64.compare imm (Int64.of_int32 Int32.max_int) <= 0 ->
        (* Convert int64 to int - safe because we checked range *)
        let displ = Int64.to_int imm in
        let result = apply_lea_optimization cell1 cell2 dst1 a None displ in
        if Option.is_some result
        then stats.rewrite_mov_add_to_lea <- stats.rewrite_mov_add_to_lea + 1;
        result
      | _, _, _, _ -> None)
    | Ins (MOV (src1, dst1)), Ins (SUB (src2, dst2)) when U.equal_args dst1 dst2
      -> (
      (* Pattern: mov %a, %b; sub $CONST, %b *)
      match src1, dst1, src2, dst2 with
      | Reg64 a, Reg64 b, Imm imm, Reg64 b'
        when U.equal_reg64 b b'
             (* For SUB, we negate CONST, so we need CONST != Int32.min_int to
                avoid overflow *)
             && Int64.compare imm (Int64.of_int32 Int32.min_int) > 0
             && Int64.compare imm (Int64.of_int32 Int32.max_int) <= 0 ->
        (* Negate the immediate: sub $CONST, %b becomes lea -CONST(%a), %b *)
        let displ = Int64.to_int (Int64.neg imm) in
        let result = apply_lea_optimization cell1 cell2 dst1 a None displ in
        if Option.is_some result
        then stats.rewrite_mov_add_to_lea <- stats.rewrite_mov_add_to_lea + 1;
        result
      | _, _, _, _ -> None)
    | Ins (MOV (src1, dst1)), Ins (INC dst2) when U.equal_args dst1 dst2 -> (
      (* Pattern: mov %a, %b; inc %b *)
      match src1, dst1, dst2 with
      | Reg64 a, Reg64 b, Reg64 b' when U.equal_reg64 b b' ->
        let result = apply_lea_optimization cell1 cell2 dst1 a None 1 in
        if Option.is_some result
        then stats.rewrite_mov_add_to_lea <- stats.rewrite_mov_add_to_lea + 1;
        result
      | _, _, _ -> None)
    | Ins (MOV (src1, dst1)), Ins (DEC dst2) when U.equal_args dst1 dst2 -> (
      (* Pattern: mov %a, %b; dec %b *)
      match src1, dst1, dst2 with
      | Reg64 a, Reg64 b, Reg64 b' when U.equal_reg64 b b' ->
        let result = apply_lea_optimization cell1 cell2 dst1 a None (-1) in
        if Option.is_some result
        then stats.rewrite_mov_add_to_lea <- stats.rewrite_mov_add_to_lea + 1;
        result
      | _, _, _ -> None)
    | _, _ -> None)
  | _ -> None

(* Rewrite rule: optimize MOV followed by ADD (register) to LEA. Pattern: mov
   %a, %b; add %c, %b Rewrite: lea (%a,%c), %b

   This is safe when: - All operands are Reg64 registers - %b ≠ %c (if %b == %c,
   ADD would use the new value of %b after MOV, but LEA would use the old value)
   - Flags written by ADD are dead (LEA doesn't modify flags)

   Note: SUB cannot be optimized here because LEA computes base + index * scale
   + disp where scale ∈ {1,2,4,8}. To compute %a - %c would require scale = -1,
   which is not supported by x86-64.

   Key difference: ADD sets flags (CF, OF, SF, ZF, AF, PF), LEA doesn't. *)
let rewrite_mov_add_reg_to_lea stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    match[@warning "-4"] DLL.value cell1, DLL.value cell2 with
    | Ins (MOV (src1, dst1)), Ins (ADD (src2, dst2)) when U.equal_args dst1 dst2
      -> (
      (* Pattern: mov %a, %b; add %c, %b *)
      match src1, dst1, src2, dst2 with
      | Reg64 a, Reg64 b, Reg64 c, Reg64 b'
        when U.equal_reg64 b b' && not (U.equal_reg64 b c) ->
        let result = apply_lea_optimization cell1 cell2 dst1 c (Some a) 0 in
        if Option.is_some result
        then
          stats.rewrite_mov_add_reg_to_lea
            <- stats.rewrite_mov_add_reg_to_lea + 1;
        result
      | _, _, _, _ -> None)
    | _, _ -> None)
  | _ -> None

(* Find a redundant CMP instruction with the same operands. Returns Some cell if
   found, None otherwise. *)
let find_redundant_cmp src dst start_cell =
  let rec loop cell_opt =
    match cell_opt with
    | None -> None
    | Some cell -> (
      match DLL.value cell with
      | Ins instr -> (
        if U.is_control_flow instr
        then None
        else
          match[@warning "-4"] instr with
          | CMP (src2, dst2) when U.equal_args src src2 && U.equal_args dst dst2
            ->
            (* Found a redundant CMP! *)
            Some cell
          | _ ->
            (* Check if this instruction invalidates the optimization *)
            if U.writes_flags instr
            then None
            else if U.writes_to_arg src instr || U.writes_to_arg dst instr
            then None
            else loop (DLL.next cell))
      | Directive _ ->
        if U.is_hard_barrier (DLL.value cell)
        then None
        else loop (DLL.next cell))
  in
  loop (DLL.next start_cell)

(* Rewrite rule: remove redundant CMP with identical operands. Pattern: cmp A,
   B; ...; cmp A, B (where ... doesn't write flags or modify A or B) Rewrite:
   cmp A, B; ...

   This is safe when: - Both operands are registers (to avoid memory aliasing
   issues) - Neither operand is modified between the two CMPs - Flags are not
   written between the two CMPs (but can be read) - No control flow or hard
   barriers between the CMPs *)
let remove_redundant_cmp stats cell =
  match[@warning "-4"] DLL.value cell with
  | Ins (CMP (src, dst)) -> (
    (* Only optimize register-register comparisons to avoid aliasing issues *)
    match src, dst with
    | ( (Reg64 _ | Reg32 _ | Reg16 _ | Reg8L _),
        (Reg64 _ | Reg32 _ | Reg16 _ | Reg8L _) ) -> (
      (* Search for a redundant CMP *)
      match find_redundant_cmp src dst cell with
      | Some redundant_cell ->
        (* Delete the redundant CMP *)
        DLL.delete_curr redundant_cell;
        stats.remove_redundant_cmp <- stats.remove_redundant_cmp + 1;
        (* Return the first CMP cell to allow iterative removal *)
        Some (Some cell)
      | None -> None)
    | _, _ -> None)
  | _ -> None

(* Apply all rewrite rules in sequence. Returns Some continuation_cell if a rule
   matched, None otherwise. *)
let apply_rules stats cell =
  match remove_mov_x_x stats cell with
  | Some cont -> Some cont
  | None -> (
    match remove_useless_mov stats cell with
    | Some cont -> Some cont
    | None -> (
      match remove_mov_chain stats cell with
      | Some cont -> Some cont
      | None -> (
        match remove_mov_to_dead_register stats cell with
        | Some cont -> Some cont
        | None -> (
          match rewrite_mov_sequence_to_xchg stats cell with
          | Some cont -> Some cont
          | None -> (
            match rewrite_mov_add_to_lea stats cell with
            | Some cont -> Some cont
            | None -> (
              match rewrite_mov_add_reg_to_lea stats cell with
              | Some cont -> Some cont
              | None -> (
                match remove_redundant_cmp stats cell with
                | Some cont -> Some cont
                | None -> (
                  match combine_add_rsp stats cell with
                  | Some cont -> Some cont
                  | None -> None))))))))

(* Public interface for applying rules *)
let apply stats cell = apply_rules stats cell
