(* TEST
 include ocamlcommon;
 native;
*)

let () =
  Mode.For_testing.check_composition_jobs ~full:true ()
  |> List.iter (fun job ->
    match job () with
    | Ok () -> ()
    | Error error ->
      failwith
        (Format_doc.asprintf "%a"
           Mode.For_testing.print_error
           error));
  print_endline
    "All partial-coverage morphism composition checks succeeded: composed \
     morphisms agree with applying each morphism in sequence."
