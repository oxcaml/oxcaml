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
(* this test checks that we can't fork if a domain is currently running. *)

let () =
  let d = Domain.spawn (fun () -> Unix.sleep 1) in
  let res = match Unix.fork () with
    | exception Failure _ -> 0
    | _ -> 1
  in
  Domain.join d;
  exit res
