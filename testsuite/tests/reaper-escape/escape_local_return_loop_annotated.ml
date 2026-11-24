(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_local_return_loop";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_local_return_loop] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] mk i = exclave_ (i, i * 2)

  let[@inline never] f n =
    let acc = ref 0 in
    for i = 1 to n do
      let local_ p = mk i in
      acc := !acc + sum_pair p
    done;
    !acc
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 10)
