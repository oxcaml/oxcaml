(* TEST
 flags += "-alert +unsafe_multidomain";
<<<<<<< HEAD
 multicore;
||||||| 9790921724
 flags += "-alert -do_not_spawn_domains";
 runtime5;
 multidomain;
=======
 runtime5;
 multidomain;
>>>>>>> 5.2.0minus-37
 { bytecode; }
 { native; }
*)

let _ = Domain.spawn (fun () -> ())
