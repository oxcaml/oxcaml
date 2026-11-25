(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_local_return_existing_region";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_local_return_existing_region] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let[@inline never] sum_local (local_ p : int * int) = fst p + snd p

let[@inline always] after p n =
  let local_ q = n, n + 1 in
  let local_ r = n + 2, n + 3 in
  sum_pair q + sum_pair r + sum_pair p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] mk x = x + 1, x + 2

  let[@inline never] f x =
    let local_ l = x, x in
    let n = sum_local l in
    after (mk x) n
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 10)
