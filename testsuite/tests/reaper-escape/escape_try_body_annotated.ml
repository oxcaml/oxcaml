(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_try_body";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_try_body] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let local_ r =
      try
        let p = x, x + 1 in
        if x > 100 then raise Exit else p
      with Exit -> 0, 0
    in
    let local_ q = x + 5, x + 6 in
    sum_pair q + sum_pair r
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 1)
