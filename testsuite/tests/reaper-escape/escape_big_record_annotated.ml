(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_big_record";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_big_record] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
type t =
  { a : int; b : int; c : int; d : int; e : int; f : int;
    g : int; h : int; i : int; j : int; k : int; l : int }

let[@inline never] sum (local_ r) =
  r.a + r.b + r.c + r.d + r.e + r.f + r.g + r.h + r.i + r.j + r.k + r.l

let[@inline never] f x =
  let local_ r =
    { a = x; b = x + 1; c = x + 2; d = x + 3; e = x + 4; f = x + 5;
      g = x + 6; h = x + 7; i = x + 8; j = x + 9; k = x + 10; l = x + 11 }
  in
  (sum r [@nontail])

let () = (Printf.printf [@inlined never]) "%d\n" (f 1)
