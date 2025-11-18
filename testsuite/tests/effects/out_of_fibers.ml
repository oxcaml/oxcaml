(* TEST
 modules = "out_of_fibers.c";
 exit_status = "2";
 { bytecode; }
 { native; }
*)

external out_of_fibers : unit -> 'a = "test_out_of_fibers"

let () = out_of_fibers ()
