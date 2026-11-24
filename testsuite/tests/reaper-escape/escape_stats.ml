(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=escape-stats-verbose";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

(* One allocation of each kind made local (a pair, a boxed float, a closure of
   arity two with one value slot), a candidate left on the heap (the pair
   returned by the exported [g]) and a heap allocation that is not a candidate
   (at toplevel). The statistics are checked against the compiler output. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] add_one (f : float) = f +. 1.

let[@inline never] apply2 (f : int -> int -> int) = f 1 2

module M : sig
  val f : int -> int

  val g : int -> int * int
end = struct
  let[@inline never] f x =
    let p = x, x + 1 in
    let b = float_of_int x in
    let h a c = a + c + x in
    sum_pair p + int_of_float (add_one b) + apply2 h

  let[@inline never] g x = x, x
end

let r = ref (Sys.opaque_identity 0)

let () =
  r := M.f 10 + fst (M.g 1);
  (print_int [@inlined never]) !r;
  (print_newline [@inlined never]) ()
