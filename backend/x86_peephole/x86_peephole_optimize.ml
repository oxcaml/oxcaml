[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Oxcaml_utils.Doubly_linked_list
module R = X86_peephole_rules
module U = X86_peephole_utils

let stats = R.create_peephole_stats ()

(* Main optimization loop. Iterates through the instruction list starting from
   [start], applying rewrite rules and respecting hard barriers. *)
let optimize_from_cell start =
  let counter_f () = R.peephole_stats_to_counters stats in
  Profile.record_with_counters ~accumulate:true ~counter_f "x86_peephole"
    (fun () ->
      let rec optimize cell_opt =
        match cell_opt with
        | None -> ()
        | Some cell -> (
          if U.is_hard_barrier (DLL.value cell)
          then optimize (DLL.next cell)
          else
            match R.apply stats cell with
            | U.Matched continuation_cell -> optimize continuation_cell
            | U.No_match -> optimize (DLL.next cell))
      in
      optimize start)
    ()
