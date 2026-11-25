(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_local_return_recursive";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_local_return_recursive] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

module M : sig
  val f : int -> int
end = struct
  let rec build n = exclave_
    if n = 0 then (0, 0) else let p = build (n - 1) in (sum_pair p, n)
  [@@inline never]

  let[@inline never] f n =
    let local_ p = build n in
    (sum_pair p [@nontail])
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 10)
