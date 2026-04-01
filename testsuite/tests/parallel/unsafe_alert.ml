(* TEST
 multicore;
 { bytecode; }
 { native; }
*)

let _ = Domain.spawn (fun () -> ())
