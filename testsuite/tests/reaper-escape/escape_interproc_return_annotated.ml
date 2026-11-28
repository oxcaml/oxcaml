(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_interproc_return";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_interproc_return] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

module M : sig
  val f : int -> int -> int
end = struct
  let[@inline never] mk x y = exclave_ (x, y)

  let[@inline never] f x y =
    let local_ p = mk x y in
    (sum_pair p [@nontail])
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 3 4)
