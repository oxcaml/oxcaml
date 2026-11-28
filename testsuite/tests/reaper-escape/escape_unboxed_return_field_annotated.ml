(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_unboxed_return_field";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_unboxed_return_field] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] use (local_ (o : int option)) = match o with Some v -> v | None -> 0

let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

module M : sig
  val test : int -> int
end = struct
  let[@inline never] these x = exclave_ (x + 1, Some x)

  let[@inline never] test x =
    let n, o = these x in
    let local_ p = n + 1, n + 2 in
    n + use o + sum_pair p
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.test 41)
