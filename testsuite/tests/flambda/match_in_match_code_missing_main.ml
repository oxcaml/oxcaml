(* TEST
 flambda;
 ocamlopt_flags = "-O3 -flambda2-expert-cont-lifting-budget 20 -flambda2-expert-cont-specialization-budget 200";
 all_modules = "match_in_match_code_missing_foo.ml match_in_match_code_missing_main.ml";
 native;
*)

module Foo = Match_in_match_code_missing_foo

let msg_of s = s

let fail o stdout stderr =
  let[@inline] nonempty s =
    match (String.trim[@inlined never]) s with
    | "" -> None
    | s -> Some "foo"
  in
  let stdout = nonempty stdout in
  let stderr = nonempty stderr in
  let maybe tag x msg_of =
    Option.map (fun x ->
        Foo.create tag x msg_of
      ) x
  in
  [ maybe "stdout" stdout msg_of;
    maybe "stderr" stderr msg_of ]
