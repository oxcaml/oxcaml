(* TEST
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

let _ = Domain.Safe.spawn (fun () -> ())
