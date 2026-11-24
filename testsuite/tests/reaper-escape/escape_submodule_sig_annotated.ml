(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_submodule_sig";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_submodule_sig] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
module M : sig
  val sum_pair : local_ int * int -> int
  val f : int -> int -> int
end = struct
  let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

  let[@inline never] f x y =
    let local_ p = (x, y) in
    (sum_pair p [@nontail])
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 3 4)
