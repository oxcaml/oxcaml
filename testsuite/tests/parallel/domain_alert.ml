(* TEST
 flags += "-alert +do_not_spawn_domains";
 multidomain;
 { bytecode; }
 { native; }
*)
(* Blank lines added here to preserve locations. *)

let _ = Domain.Safe.spawn (fun () -> ())

