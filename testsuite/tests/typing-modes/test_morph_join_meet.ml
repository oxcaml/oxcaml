(* TEST
 include ocamlcommon;
 native;
*)

let check name jobs =
  let rec first_error = function
    | [] -> None
    | job :: jobs -> (
      match job () with Ok () -> first_error jobs | Error error -> Some error)
  in
  match first_error jobs with
  | None ->
    Printf.printf
      "All partial-coverage morphism %s checks succeeded: the %s of two \
       morphisms agrees with the pointwise %s of their outputs.\n"
      name name name
  | Some error ->
    print_endline (Format_doc.asprintf "%a" Mode.For_testing.print_error error)

let () =
  check "join" (Mode.For_testing.check_join_jobs ~full:false ());
  check "meet" (Mode.For_testing.check_meet_jobs ~full:false ())
