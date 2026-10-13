(* TEST *)

(* GC-safe sections are currently compiled transparently: check that accepted
   sections (including in tail position, which once broke region tracking in
   Simplif and Flambda2) compile and compute normally. *)

external int_compare : int -> int -> int = "caml_int_compare"
  [@@noalloc] [@@critical safe]

external int_compare_b : int -> int -> int = "caml_int_compare"
  [@@noalloc] [@@blocking]

let double (x : int) = x * 2 [@@critical safe]

let rec sum_to (n : int) (acc : int) =
  if n = 0 then acc else sum_to (n - 1) (acc + n)
  [@@critical safe]

(* Section in tail position of the enclosing function. *)
let f (x : int) = [%critical double (double x)]

(* Section as a subexpression. *)
let g (x : int) = 1 + [%critical sum_to x 0]

let h (a : int) (b : int) = [%blocking int_compare_b a b]

let () =
  Printf.printf "%d %d %d %d\n"
    (f 3) (g 4) [%critical int_compare 3 4] (h 5 5)
