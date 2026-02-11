(* TEST
<<<<<<< oxcaml
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 ocamlrunparam += ",d=1";
 runtime5;
 multidomain;
 { native; }
||||||| upstream-base
=======
 ocamlrunparam += ",d=1";
>>>>>>> upstream-incoming
*)

let _ =
  try
    Domain.spawn (fun _ -> print_endline "Expect failure") |> ignore
  with Failure _ -> print_string "ok\n"
