(* TEST
 flags += "-alert +unsafe_multidomain";
 multidomain;
 { bytecode; }
 { native; }
*)
(* Blank lines added here to preserve locations. *)

let _ = Domain.spawn (fun () -> ())

