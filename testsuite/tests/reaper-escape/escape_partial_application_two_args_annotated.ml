(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_partial_application_two_args";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_partial_application_two_args] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum7 (local_ (t : int * int * int * int * int * int * int)) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

module M : sig
  val g : int -> int -> int
end = struct
  let[@inline never] papp (local_ (f : int -> int -> int -> int)) =
    exclave_ f 1 2

  let[@inline never] g a b =
    let local_ h x y z = (x * a) + (b * y) + z in
    let local_ p = papp h in
    let local_ big = a + 1, a + 2, a + 3, a + 4, a + 5, a + 6, a + 7 in
    let s = sum7 big in
    let local_ big2 = b + 1, b + 2, b + 3, b + 4, b + 5, b + 6, b + 7 in
    let s2 = sum7 big2 in
    (p 3 [@nontail]) + s + s2
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.g 2 3)
