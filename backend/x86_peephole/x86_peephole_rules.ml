[@@@ocaml.warning "+a-29-40-41-42-4"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list
module U = X86_peephole_utils

type peephole_stats =
  { mutable remove_mov_to_dead_register : int;
    mutable remove_redundant_cmp : int;
    mutable combine_add_rsp : int
  }

let create_peephole_stats () =
  { remove_mov_to_dead_register = 0;
    remove_redundant_cmp = 0;
    combine_add_rsp = 0
  }

let peephole_stats_to_counters stats =
  Profile.Counters.create ()
  |> Profile.Counters.set "x86_peephole.remove_mov_to_dead_register"
       stats.remove_mov_to_dead_register
  |> Profile.Counters.set "x86_peephole.remove_redundant_cmp"
       stats.remove_redundant_cmp
  |> Profile.Counters.set "x86_peephole.combine_add_rsp" stats.combine_add_rsp

(* CR xclerc: check where the rules should set the "restart" cell to be sure we
   always apply all rules. *)

(* Rewrite rule: combine adjacent ADD to RSP with CFI directives. Pattern: addq
   $n1, %rsp; .cfi_adjust_cfa_offset d1; addq $n2, %rsp; .cfi_adjust_cfa_offset
   d2 Rewrite: addq $(n1+n2), %rsp; .cfi_adjust_cfa_offset (d1+d2)

   This only applies when d1 = -n1 and d2 = -n2 (i.e., the CFI offsets correctly
   track the stack adjustment). *)
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
      U.Matched (Some cell1)
    | _, _, _, _ -> U.No_match)
  | _ -> U.No_match

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
    match DLL.value cell1, DLL.value cell2 with
    | Ins (MOV (src1, dst1)), Ins (MOV (src2, dst2))
      when U.equal_args dst1 src2 && U.is_register dst1 && U.is_register dst2
           && U.is_reg64 dst1 -> (
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
        U.Matched (Some cell1)
      | ReadFound | NotFound ->
        (* x is read before write, or we can't determine - don't optimize *)
        U.No_match)
    | _, _ -> U.No_match)
  | _ -> U.No_match

(* Find a redundant CMP instruction with the same operands. Returns Some cell if
   found, None otherwise. *)
let find_redundant_cmp src dst start_cell =
  let rec loop cell_opt =
    match cell_opt with
    | None -> None
    | Some cell ->
      let value = DLL.value cell in
      if U.is_hard_barrier value
      then None
      else (
        match value with
        | Ins instr -> (
          match instr with
          | CMP (src2, dst2)
            when U.equal_args src src2 && U.equal_args dst dst2 ->
            Some cell
          | _ ->
            if U.writes_flags instr
            then None
            else if U.writes_to_arg src instr || U.writes_to_arg dst instr
            then None
            else loop (DLL.next cell))
        | Directive _ -> loop (DLL.next cell))
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
  match DLL.value cell with
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
        U.Matched (Some cell)
      | None -> U.No_match)
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
       ~enabled:!Oxcaml_flags.x86_peephole_combine_add_rsp
       combine_add_rsp
