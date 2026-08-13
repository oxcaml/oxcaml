(* TEST
 multicore;
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
    (* FIXME This is regularly timing out on arm64 in CI, but seemingly only
             when part of a full testsuite run. Tripled the timeout in order
             to reduce the flakiness. *)
    Unix.sleep 30;
    print_endline "Should not reach here!") in
  Gc.full_major ();
  print_endline "OK"
