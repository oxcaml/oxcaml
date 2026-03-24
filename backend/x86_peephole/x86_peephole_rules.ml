[@@@ocaml.warning "+a-29-40-41-42-4"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
open X86_ast
open X86_ast_utils
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

let peephole_stats_to_string stats =
  Printf.sprintf
    {|"remove_mov_to_dead_register":%d,"remove_redundant_cmp":%d,"combine_add_rsp":%d|}
    stats.remove_mov_to_dead_register stats.remove_redundant_cmp
    stats.combine_add_rsp

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
        stats.combine_add_rsp <- stats.combine_add_rsp + 1;
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
   memory moves don't exist.

   We restrict x to Reg64 to avoid issues with aliasing or zeroed bits. *)
let remove_mov_to_dead_register stats cell =
  match U.get_cells cell 2 with
  | [cell1; cell2] -> (
    match DLL.value cell1, DLL.value cell2 with
    | Ins (MOV (src1, Reg64 dst1)), Ins (MOV (Reg64 src2, dst2))
      when equal_reg64 dst1 src2 && (U.is_register src1 || U.is_register dst2)
      -> (
      (* Pattern: mov A, x; mov x, B *)
      (* Check if the next occurrence of x is a write *)
      match U.find_next_occurrence_of_reg64 dst1 cell2 with
      | WriteFound ->
        (* x is written before being read, so we can optimize *)
        (* Rewrite to: mov A, B *)
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
    | Some cell -> (
      let value = DLL.value cell in
      if U.is_hard_barrier value
      then None
      else
        match value with
        | Ins instr -> (
          match instr with
          | CMP (src2, dst2) when equal_args src src2 && equal_args dst dst2 ->
            Some cell
          | _ ->
            if U.writes_flags instr
            then None
            else if
              U.writes_to_reg64 (U.underlying_reg64 src |> Option.get) instr
              || U.writes_to_reg64 (U.underlying_reg64 dst |> Option.get) instr
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
   written between the two CMPs (but can be read) - No hard barriers like
   control flow between the CMPs *)
let remove_redundant_cmp stats cell =
  match DLL.value cell with
  (* Only optimize register-register comparisons to avoid issues with mutable
     memory *)
  | Ins (CMP (src, dst)) when U.is_register src && U.is_register dst -> (
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
