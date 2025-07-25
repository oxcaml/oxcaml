(* TEST
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 runtime5;
 multidomain;
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)


let _ =
  (* start a dummy domain and shut it down to cause a domain reuse *)
  let d = Domain.spawn (fun _ -> ()) in
  Domain.join d;
  let _d = Domain.spawn (fun _ ->
    Unix.sleep 10;
    print_endline "Should not reach here!") in
  Gc.full_major ();
  print_endline "OK"
