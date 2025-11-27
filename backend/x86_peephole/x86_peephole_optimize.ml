[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
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
        | U.Matched continuation_cell ->
          (* A rule was applied, continue from the continuation point *)
          optimize_from continuation_cell
        | U.No_match ->
          (* No rule matched, move to the next instruction *)
          optimize_from (DLL.next cell))
  in
  optimize_from (DLL.hd_cell asm_program)

let optimize_all_sections main_code asm_code_by_section delayed_sections =
  let stats = R.create_peephole_stats () in
  let counter_f () = R.peephole_stats_to_counters stats in
  let optimize_table section_table =
    X86_section.Section_name.Tbl.iter
      (fun _name section -> peephole_optimize_asm_program stats section)
      section_table
  in
  Profile.record_with_counters ~accumulate:true ~counter_f "x86_peephole"
    (fun () ->
      peephole_optimize_asm_program stats main_code;
      optimize_table asm_code_by_section;
      optimize_table delayed_sections)
    ()
