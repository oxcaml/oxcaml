[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Oxcaml_utils.Doubly_linked_list
module R = X86_peephole_rules
module U = X86_peephole_utils

let stats = R.create_peephole_stats ()

let () =
  at_exit (fun () ->
      Sys.getenv_opt "OXCAML_X86_PEEPHOLE_STATS_FILE"
      |> Option.iter (fun path ->
          let oc =
            open_out_gen [Open_append; Open_creat; Open_text] 0o666 path
          in
          Fun.protect
            ~finally:(fun () -> close_out_noerr oc)
            (fun () ->
              let argv =
                Sys.argv |> Array.to_list
                |> List.map (Printf.sprintf "%S")
                |> String.concat ","
              in
              Printf.fprintf oc "{%s,\"cwd\":%S,\"argv\":[%s]}\n"
                (R.peephole_stats_to_string stats)
                (Sys.getcwd ()) argv)))

(* Main optimization loop. Iterates through the instruction list starting from
   [start], applying rewrite rules and respecting hard barriers. *)
let optimize_from_cell start =
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
  optimize start
