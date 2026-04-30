(* TEST
 flags += "-alert +do_not_spawn_domains";
<<<<<<< HEAD
 multicore;
||||||| 5.2.0minus-31
 runtime5;
 multidomain;
=======
 runtime5;
 multidomain;
>>>>>>> 5.2.0minus-37
 { bytecode; }
 { native; }
*)

let _ = Domain.Safe.spawn (fun () -> ())
