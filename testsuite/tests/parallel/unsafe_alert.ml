(* TEST
 flags += "-alert -do_not_spawn_domains";
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

let _ = Domain.spawn (fun () -> ())
