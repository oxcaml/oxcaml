(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_local_return_negative";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_local_return_negative] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
module M : sig
  val f : int -> int -> int
  val g : int -> int -> unit
  val cell : (int * int) ref
end = struct
  let[@inline never] mk x y = (x, y)
  let[@inline never] f x y =
    let p = mk x y in
    fst p + snd p
  let cell = ref (0, 0)
  let[@inline never] g x y = cell := mk x y
end
let () =
  M.g 5 6;
  let a, b = !M.cell in
  (Printf.printf [@inlined never]) "%d %d %d\n" (M.f 3 4) a b
