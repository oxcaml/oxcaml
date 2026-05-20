(* TEST
 include ocamlcommon;
 native;
*)

let () =
  Printf.printf
    "running allowed composition checks with partial coverage (spanning values \
     cover each axis element at least once; not every product combination)\n%!";
  Mode.For_testing.check_jobs ~full:false () |> List.iter (fun job -> job ())
