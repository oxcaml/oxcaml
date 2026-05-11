(* TEST
 flags += "-alert +unsafe_multidomain";
 multicore;
 { bytecode; }
 { native; }
*)

let _ = Domain.spawn (fun () -> ())
