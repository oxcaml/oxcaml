(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_try_body_local";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_try_body_local] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let[@inline never] boom x = if x > 100 then raise Exit else x

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let r = try let local_ p = x, x + 1 in sum_pair p + boom x with Exit -> 0 in
    let local_ q = x + 5, x + 6 in
    sum_pair q + r
end

let () = (Printf.printf [@inlined never]) "%d %d\n" (M.f 1) (M.f 200)
