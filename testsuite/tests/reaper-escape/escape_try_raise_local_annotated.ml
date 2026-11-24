(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_try_raise_local";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_try_raise_local] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
exception E of (int * int)

let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let r = try if x > 0 then raise (E (x, x + 1)) else 0, 0 with E p -> p in
    let local_ q = x + 5, x + 6 in
    sum_pair q + sum_pair r
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 1)
