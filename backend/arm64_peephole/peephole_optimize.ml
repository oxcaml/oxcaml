[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Doubly_linked_list
module R = Peephole_rules
module U = Peephole_utils

(* Main optimization loop. Iterates through the buffered function body, applying
   rewrite rules and respecting hard barriers. *)
let optimize buffer =
  (* Fresh stats per call: [Profile] sums the [counter_f] results of successive
     accumulated calls, so each call must report its own delta. *)
  let stats = R.create_peephole_stats () in
  let counter_f () = R.peephole_stats_to_counters stats in
  Profile.record_with_counters ~accumulate:true ~counter_f "arm64_peephole"
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
      optimize (DLL.hd_cell buffer))
    ()
