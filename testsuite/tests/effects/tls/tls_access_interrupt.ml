(* TEST
   ocamlrunparam += ",b=1";
   flags += "-alert -unsafe_multidomain";
   exit_status = "2";
   { native; }
*)

(* An interrupt (here a finaliser) that accesses a key while it is being
   accessed raises. Exceptions escaping interrupts are fatal. *)

module TLS = Domain.Safe.TLS

let k = TLS.new_key (fun () -> ref 0)

let () =
  Gc.finalise (fun _ -> ignore (TLS.access k (fun r -> !r) [@nontail])) (ref 0);
  TLS.access k (fun r ->
    Gc.full_major ();
    Gc.full_major ();
    r := 1);
  print_endline "not reached"
