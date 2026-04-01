(* TEST
 ocamlrunparam += ",d=1";
 runtime5;
 multidomain;
 { native; }
*)

let _ =
  try
    Domain.spawn (fun _ -> print_endline "Expect failure") |> ignore
  with Failure _ -> print_string "ok\n"
