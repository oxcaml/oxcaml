(* TEST
 flags = "-g -alert -do_not_spawn_domains";
 modules = "swapgil_stubs.c";
 include systhreads;
 runtime5;
 hasunix;
 native;
*)

external setup : unit -> unit = "swap_gil_setup"
let () = setup ()

external swap_gil : unit -> unit = "swap_gil"

(* Test that one cannot spawn a domain after switching locking schemes *)

let () = (swap_gil ();
          try
            Domain.join (Domain.Safe.spawn (fun () -> ()))
          with exn ->
            print_endline (Printexc.to_string exn))
