(* TEST
 flags += "-alert +do_not_spawn_domains";
 multicore;
 { bytecode; }
 { native; }
*)

let _ = Domain.Safe.spawn (fun () -> ())
