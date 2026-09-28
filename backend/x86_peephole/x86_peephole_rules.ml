[@@@ocaml.warning "+a-29-40-41-42-4"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
open X86_ast
open X86_ast_utils
module DLL = Doubly_linked_list
module U = X86_peephole_utils

type peephole_stats =
  { mutable remove_mov_to_dead_register : int;
    mutable remove_redundant_cmp : int;
    mutable remove_redundant_extension : int;
    mutable combine_add_rsp : int;
    mutable remove_redundant_test : int;
    mutable fuse_and_test : int
  }

let create_peephole_stats () =
  { remove_mov_to_dead_register = 0;
    remove_redundant_cmp = 0;
    remove_redundant_extension = 0;
    combine_add_rsp = 0;
    remove_redundant_test = 0;
    fuse_and_test = 0
  }

let peephole_stats_to_counters stats =
  Profile.Counters.create ()
  |> Profile.Counters.set "x86_peephole.remove_mov_to_dead_register"
       stats.remove_mov_to_dead_register
  |> Profile.Counters.set "x86_peephole.remove_redundant_cmp"
       stats.remove_redundant_cmp
  |> Profile.Counters.set "x86_peephole.remove_redundant_extension"
       stats.remove_redundant_extension
  |> Profile.Counters.set "x86_peephole.combine_add_rsp" stats.combine_add_rsp
  |> Profile.Counters.set "x86_peephole.remove_redundant_test"
       stats.remove_redundant_test
  |> Profile.Counters.set "x86_peephole.fuse_and_test" stats.fuse_and_test

(* Rewrite rule: combine adjacent ADD to RSP with CFI directives. Pattern: addq
   $n1, %rsp; .cfi_adjust_cfa_offset d1; addq $n2, %rsp; .cfi_adjust_cfa_offset
   d2 Rewrite: addq $(n1+n2), %rsp; .cfi_adjust_cfa_offset (d1+d2)

   This only applies when d1 = -n1 and d2 = -n2 (i.e., the CFI offsets correctly
   track the stack adjustment).

   The rewrite does not preserve the flags, so we only apply it when we can
   prove the flags are unobserved. *)
let combine_add_rsp stats cell =
  match U.get_cells cell 4 with
  | [cell1; cell2; cell3; cell4] -> (
    match
      DLL.value cell1, DLL.value cell2, DLL.value cell3, DLL.value cell4
    with
    | ( Ins (ADD (Imm n1, Reg64 RSP)),
        Directive
          (Asm_targets.Asm_directives.Directive.Cfi_adjust_cfa_offset d1),
        Ins (ADD (Imm n2, Reg64 RSP)),
        Directive
          (Asm_targets.Asm_directives.Directive.Cfi_adjust_cfa_offset d2) )
      when U.flags_never_observed cell4 ->
      if
        not
          (Int64.equal (Int64.of_int d1) (Int64.neg n1)
          && Int64.equal (Int64.of_int d2) (Int64.neg n2))
      then
        Misc.fatal_errorf
          "combine_add_rsp: CFI offsets do not track stack adjustment: addq \
           $%Ld, %%rsp with cfi_adjust_cfa_offset %d; addq $%Ld, %%rsp with \
           cfi_adjust_cfa_offset %d"
          n1 d1 n2 d2;
      stats.combine_add_rsp <- stats.combine_add_rsp + 1;
      (* Combine the instructions *)
      let combined_imm = Int64.add n1 n2 in
      let combined_offset = d1 + d2 in
      if combined_offset = 0
      then begin
        let next = DLL.next cell4 in
        DLL.delete_curr cell1;
        DLL.delete_curr cell2;
        DLL.delete_curr cell3;
        DLL.delete_curr cell4;
        U.Matched next
      end
      else begin
        (* Update cells with combined values *)
        DLL.set_value cell1 (Ins (ADD (Imm combined_imm, Reg64 RSP)));
        DLL.set_value cell2
          (Directive
             (Asm_targets.Asm_directives.Directive.Cfi_adjust_cfa_offset
                combined_offset));
        (* Delete the redundant cells *)
        DLL.delete_curr cell3;
        DLL.delete_curr cell4;
        (* Return cell1 to allow iterative combination of multiple ADDs *)
        U.Matched (Some cell1)
      end
    | _, _, _, _ -> U.No_match)
  | _ -> U.No_match

(* Rewrite rule: optimize MOV to register that is overwritten before use.
   Pattern: mov A, x; mov x, B where the next occurrence of x is a write.
   Rewrite: mov A, B

   This is safe when x is a register that is not read before the next write to x
   within the same basic block, and either A or B is a register, as memory to
   memory moves don't exist. B must also not use x in its addressing expression:
   the rewrite would compute the address from a stale value of x (since we are
   deleting the write of A to x). A may use x, since its read happens at the
   same program point in both versions.

   We restrict x to Reg64 to avoid issues with aliasing or zeroed bits. *)
let remove_mov_to_dead_register stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    match DLL.value cell1, DLL.value cell2 with
    | Ins (MOV (src1, Reg64 dst1)), Ins (MOV (Reg64 src2, dst2))
      when equal_reg64 dst1 src2
           && (U.is_register src1 || U.is_register dst2)
           && not (U.arg_contains_reg64 dst1 dst2) ->
      if
        (* Pattern: mov A, x; mov x, B *)
        U.reg64_is_never_read dst1 cell2
      then begin
        (* Rewrite to: mov A, B *)
        DLL.set_value cell1 (Ins (MOV (src1, dst2)));
        DLL.delete_curr cell2;
        stats.remove_mov_to_dead_register
          <- stats.remove_mov_to_dead_register + 1;
        (* Return cell1 to allow iterative combination *)
        U.Matched (Some cell1)
      end
      else U.No_match
    | _, _ -> U.No_match)
  | _ -> U.No_match

let find_redundant_cmp src dst start_cell =
  let rec loop cell_opt =
    match cell_opt with
    | None -> None
    | Some cell -> (
      let value = DLL.value cell in
      if U.is_hard_barrier value
      then None
      else
        match value with
        | Ins instr -> (
          if not (U.arg_unchanged_by src instr && U.arg_unchanged_by dst instr)
          then None
          else
            match instr with
            | CMP (src2, dst2) when equal_args src src2 && equal_args dst dst2
              ->
              Some cell
            | _ ->
              if U.maybe_writes_flags instr then None else loop (DLL.next cell))
        | Directive _ -> loop (DLL.next cell))
  in
  loop (DLL.next start_cell)

(** Rewrite rule: remove redundant CMP with identical operands. Pattern: cmp A,
    B; ...; cmp A, B (where ... doesn't write flags or modify A or B) Rewrite:
    cmp A, B; ...

    This is safe when:
    - Neither operand is modified between the two CMPs (we currently only allow
      immediates and registers)
    - Flags are not written between the two CMPs (but can be read)
    - No hard barriers like control flow between the CMPs *)
let remove_redundant_cmp stats cell =
  match DLL.value cell with
  | Ins (CMP (src, dst)) -> (
    (* Search for a redundant CMP *)
    match find_redundant_cmp src dst cell with
    | Some redundant_cell ->
      (* Delete the redundant CMP *)
      DLL.delete_curr redundant_cell;
      stats.remove_redundant_cmp <- stats.remove_redundant_cmp + 1;
      (* Return the first CMP cell to allow iterative removal *)
      U.Matched (Some cell)
    | None -> U.No_match)
  | _ -> U.No_match

(* Rewrite rule: remove a sign/zero-extension instruction that immediately
   follows an identical one. Pattern: ext src, dst; ext src, dst (where ext is
   one of movsx/movsxd/movzx and src is a register). Rewrite: ext src, dst

   The first instruction only writes [dst], and an extension writes the bits it
   read, unchanged, into the low bits of [dst]. So even when [src] is a
   subregister of [dst], the second instruction recomputes the same value. This
   does not hold for a high-8-bit source such as %ah when [dst] overlaps it (the
   extension moves those bits to the low byte), so such sources are excluded.
   Extensions do not write flags. *)
let is_low_part_register (arg : X86_ast.arg) =
  match arg with Reg8L _ | Reg16 _ | Reg32 _ | Reg64 _ -> true | _ -> false

let remove_redundant_extension stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    match DLL.value cell1, DLL.value cell2 with
    | Ins (MOVSX (src1, dst1)), Ins (MOVSX (src2, dst2))
    | Ins (MOVSXD (src1, dst1)), Ins (MOVSXD (src2, dst2))
    | Ins (MOVZX (src1, dst1)), Ins (MOVZX (src2, dst2))
      when equal_arg src1 src2 && equal_arg dst1 dst2
           && is_low_part_register src1 ->
      DLL.delete_curr cell2;
      stats.remove_redundant_extension <- stats.remove_redundant_extension + 1;
      (* Return cell1 so that a third identical extension is also removed *)
      U.Matched (Some cell1)
    | _, _ -> U.No_match)
  | _ -> U.No_match

(* [and_imm32_clears_sign_bit n] holds when [and $n, %r32] is guaranteed to
   leave bit 31 of its result clear, i.e. when [0 <= n < 2^31]. The emitter uses
   the 32-bit form of AND for non-negative immediates because it zero-extends
   into the full 64-bit register. *)
let and_imm32_clears_sign_bit n =
  Int64.compare n 0L >= 0 && Int64.compare n 0x7FFF_FFFFL <= 0

(* Rewrite rule: fuse an AND with an immediate into the TEST that follows it.
   Pattern: and $n, r; test r, r (where r is a 64-bit register, or a 32-bit
   register with 0 <= n < 2^31, and r is not read afterwards). Rewrite: test $n,
   r

   [test $n, r] computes [r & n] and sets ZF, SF and PF from it while clearing
   CF and OF, exactly as [and $n, r] did (AF is undefined after both); the
   original [test r, r] then recomputed those same flags from that same value.
   With a 32-bit destination, the original sequence observed the zero-extended
   result, whose bit 63 (hence SF) is 0: requiring bit 31 of [n] to be clear
   guarantees that the 32-bit test also yields SF = 0, while ZF and PF do not
   depend on the width. The fused instruction no longer writes [r], so the
   rewrite additionally requires that [r] is never read afterwards. *)
let fuse_and_test stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    let[@local] rewrite n dst_arg =
      DLL.set_value cell2 (Ins (TEST (Imm n, dst_arg)));
      DLL.delete_curr cell1;
      stats.fuse_and_test <- stats.fuse_and_test + 1;
      U.Matched (Some cell2)
    in
    match DLL.value cell1, DLL.value cell2 with
    | ( Ins (AND (Imm n, (Reg64 dst as dst_arg))),
        Ins (TEST (Reg64 src1, Reg64 src2)) )
      when equal_reg64 dst src1 && equal_reg64 dst src2
           && U.reg64_is_never_read dst cell2 ->
      rewrite n dst_arg
    | ( Ins (AND (Imm n, (Reg32 dst as dst_arg))),
        Ins (TEST (Reg64 src1, Reg64 src2)) )
      when equal_reg64 dst src1 && equal_reg64 dst src2
           && and_imm32_clears_sign_bit n
           && U.reg64_is_never_read dst cell2 ->
      rewrite n dst_arg
    | _, _ -> U.No_match)
  | _ -> U.No_match

(* Rewrite rule: remove a TEST made redundant by the preceding instruction.
   Pattern: op src, r; test r, r (where op is one of and/or/xor and r is a
   64-bit register, or op is an and with an immediate 0 <= n < 2^31 and r is a
   32-bit register). Rewrite: op src, r

   AND, OR and XOR set ZF, SF and PF according to their result and clear CF and
   OF - exactly the flag state [test r, r] computes from that same value (AF is
   undefined after both instructions). The deletion therefore leaves the flags
   bit-for-bit identical, whatever condition is read afterwards. Other
   arithmetic instructions (e.g. ADD, SUB) set CF and OF from the computation
   rather than clearing them, so extending the rule to them would require
   checking which flags the following instructions read. Both operands are
   restricted to 64-bit registers so that the flag-setting operation and the
   test have the same width; the only exception is [and $n, %r32] with bit 31 of
   [n] clear, whose zero-extended result has the same ZF, SF (= 0) and PF as its
   32-bit result. This rule acts as the fallback of [fuse_and_test] for the case
   where [r] is still read afterwards. *)
let remove_redundant_test stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    let[@local] remove_test () =
      DLL.delete_curr cell2;
      stats.remove_redundant_test <- stats.remove_redundant_test + 1;
      U.Matched (Some cell1)
    in
    match DLL.value cell1, DLL.value cell2 with
    | ( Ins (AND (_, Reg64 dst) | OR (_, Reg64 dst) | XOR (_, Reg64 dst)),
        Ins (TEST (Reg64 src1, Reg64 src2)) )
      when equal_reg64 dst src1 && equal_reg64 dst src2 ->
      remove_test ()
    | Ins (AND (Imm n, Reg32 dst)), Ins (TEST (Reg64 src1, Reg64 src2))
      when equal_reg64 dst src1 && equal_reg64 dst src2
           && and_imm32_clears_sign_bit n ->
      remove_test ()
    | _, _ -> U.No_match)
  | _ -> U.No_match

(* Apply all rewrite rules in sequence using a pipeline. *)
let apply stats cell =
  let[@inline always] if_no_match ~enabled f result =
    match result with
    | U.Matched _ -> result
    | U.No_match -> if enabled then f stats cell else U.No_match
  in
  U.No_match
  |> if_no_match
       ~enabled:!Oxcaml_flags.x86_peephole_remove_mov_to_dead_register
       remove_mov_to_dead_register
  |> if_no_match
       ~enabled:!Oxcaml_flags.x86_peephole_remove_redundant_cmp
       remove_redundant_cmp
  |> if_no_match
       ~enabled:!Oxcaml_flags.x86_peephole_remove_redundant_extension
       remove_redundant_extension
  |> if_no_match
       ~enabled:!Oxcaml_flags.x86_peephole_combine_add_rsp
       combine_add_rsp
  |> if_no_match ~enabled:!Oxcaml_flags.x86_peephole_fuse_and_test fuse_and_test
  |> if_no_match
       ~enabled:!Oxcaml_flags.x86_peephole_remove_redundant_test
       remove_redundant_test
