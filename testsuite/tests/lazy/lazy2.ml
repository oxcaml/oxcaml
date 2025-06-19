(* TEST
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 ocamlopt_flags += " -O3 ";
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

open Domain

let () =
  let l = lazy (print_string "Lazy Forced\n") in
  let d = spawn (fun () -> Lazy.force l) in
  join d
