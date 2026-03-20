(* TEST
 flags += "-alert +unsafe_multidomain";
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

let _ = Domain.spawn (fun () -> ())
