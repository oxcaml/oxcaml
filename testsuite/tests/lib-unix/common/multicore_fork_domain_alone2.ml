(* TEST
<<<<<<< HEAD
 multicore;
||||||| 9790921724
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 runtime5;
 multidomain;
=======
 runtime5;
 multidomain;
>>>>>>> 5.2.0minus-37
 include unix;
 hasunix;
 not-target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* on Multicore, fork is not allowed is another domain is, and was running. *)
(* this test checks that we can't fork if another domain ran before. *)

let () =
  let d = Domain.spawn (fun () -> ()) in
  Domain.join d;
  let res = match Unix.fork () with
    | exception Failure _ -> 0
    | _ -> 1
  in
  exit res
