(* TEST *)

(*
  Test that multiple handlers coexist happily.
*)

let test_multiple_handlers =
  let trace = ref [] in
  let collect v = trace := v :: !trace in
  let _ =
    match
      match
        collect "one";
        failwith "two"
      with
      | () -> collect "failure one"
      | exception Failure x ->
        collect x;
        failwith "three"
    with
    | () -> collect "failure two"
    | exception Failure x -> (
      collect x;
      match
        collect "four";
        failwith "five"
      with
      | () -> collect "failure three"
      | exception Failure x -> collect x)
  in
  print_endline (String.concat " " !trace);
  assert (!trace = ["five"; "four"; "three"; "two"; "one"])
