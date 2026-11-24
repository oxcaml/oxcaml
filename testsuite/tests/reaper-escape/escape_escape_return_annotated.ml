(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_escape_return";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_escape_return] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] mk x y = (x, y)

let[@inline never] maybe x y =
  let p = (x, y) in
  if x > 0 then Some p else None

let () =
  let a, b = mk 1 2 in
  match maybe a b with
  | Some (c, d) -> (Printf.printf [@inlined never]) "%d %d\n" c d
  | None -> print_endline "none"
