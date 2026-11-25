(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_mutable_record";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_mutable_record] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
type t = { mutable a : int; mutable b : int }

let[@inline never] bump r =
  r.a <- r.a + 1;
  r.b <- r.b + r.a

let[@inline never] f x =
  let r = { a = x; b = 0 } in
  bump r;
  bump r;
  r.a + r.b

let () = (Printf.printf [@inlined never]) "%d\n" (f 1)
