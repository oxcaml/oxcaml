(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A three-argument closure partially applied to two arguments through an
   unknown-arity call. Such an application goes through [caml_apply2], which,
   for a call returning at mode global, runs the successive applications in a
   region of its own: the partial application allocated locally by the curry
   functions would be freed before [papp] gets it. The application is thus
   given a local return mode, as the type checker does for the same program
   written with [local_] and [exclave_] (see
   [escape_type_checker_partial_application]). [big] and [big2] reuse the
   memory that the partial application would have been freed from. *)
let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

module M : sig
  val g : int -> int -> int
end = struct
  let[@inline never] papp (f : int -> int -> int -> int) = (f 1 2 [@nontail])

  let[@inline never] g a b =
    let h x y z = (x * a) + (b * y) + z in
    let p = papp h in
    let big = a + 1, a + 2, a + 3, a + 4, a + 5, a + 6, a + 7 in
    let s = sum7 big in
    let big2 = b + 1, b + 2, b + 3, b + 4, b + 5, b + 6, b + 7 in
    let s2 = sum7 big2 in
    (p 3 [@nontail]) + s + s2
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.g 2 3)
