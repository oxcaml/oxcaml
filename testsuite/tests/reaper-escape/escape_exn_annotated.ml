(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_exn";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_exn] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
exception E of (int * int)

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] f x y =
  let p = (x, y) in
  if x < 0 then raise (Failure "neg") else sum_pair p

let[@inline never] g x y =
  let p = (x, y) in
  if x < 0 then raise (E p) else sum_pair p

let () =
  (Printf.printf [@inlined never]) "%d\n" (f 1 2);
  (try ignore (g (-1) 2)
   with E (a, b) -> (Printf.printf [@inlined never]) "E %d %d\n" a b)
