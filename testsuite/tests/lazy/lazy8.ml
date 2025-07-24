(* TEST
   flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
   ocamlopt_flags += " -O3 ";
   runtime5;
   multidomain;
   { bytecode; }
   { native; }
*)

exception E

let main () =
  let l = lazy (raise E) in
  (try Lazy.force_val l with E -> ());
  (try Lazy.force_val l with Lazy.Undefined -> ());
  let d =
    Domain.spawn (fun () -> try Lazy.force_val l with Lazy.Undefined -> ())
  in
  Domain.join d;
  print_endline "OK"

let _ = main ()
