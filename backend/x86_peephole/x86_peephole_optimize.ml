[@@@ocaml.warning "+a-30-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list
module R = X86_peephole_rules
module U = X86_peephole_utils

(* Main optimization loop for a single asm_program. Iterates through the
   instruction list, applying rewrite rules and respecting hard barriers. *)
let peephole_optimize_asm_program stats asm_program =
  let rec optimize_from cell_opt =
    match cell_opt with
    | None -> ()
    | Some cell -> (
      if U.is_hard_barrier (DLL.value cell)
      then
        (* Skip hard barriers and continue after them *)
        optimize_from (DLL.next cell)
      else
        match R.apply stats cell with
        | Some continuation_cell ->
          (* A rule was applied, continue from the continuation point *)
          optimize_from continuation_cell
        | None ->
          (* No rule matched, move to the next instruction *)
          optimize_from (DLL.next cell))
  in
  optimize_from (DLL.hd_cell asm_program)
