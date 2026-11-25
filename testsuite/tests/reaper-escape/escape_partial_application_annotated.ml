(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_partial_application";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_partial_application] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
module M : sig
  val g : int -> int -> int
  val g2 : int -> int -> int -> int
end = struct
  let[@inline never] papp (local_ (f : int -> int -> int)) = exclave_ f 1
  let[@inline never] papp2 (f : int -> int -> int) = f 1
  let[@inline never] g a b =
    let local_ h y z = (y * a) + (b * z) in
    let local_ p = papp h in
    (p 2 [@nontail])
  let[@inline never] g2 a b =
    let h y z = (y * a) + (b * z) in
    papp2 h
end
let () = (Printf.printf [@inlined never]) "%d %d\n" (M.g 2 3) ((M.g2 2 3) 2)
