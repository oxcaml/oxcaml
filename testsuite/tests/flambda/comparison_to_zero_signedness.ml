(* TEST
 flambda2;
 native;
*)

(* Regression test for a Flambda 2 miscompilation in
   [Comparison_result.convert_result_compared_to_tagged_zero]. The rewrite of
   [(compare x y) <op> 0] reused the signedness of the inner three-way
   comparison and ignored the signedness of the outer comparison [<op>]. For an
   *unsigned* outer comparison this is unsound: [compare x y] is in {-1, 0, 1},
   so [compare x y <u 0] is always [false] (-1 is the largest unsigned value),
   whereas the buggy rewrite produced the signed comparison [x < y].

   Requires a higher optimisation level: the compare result that the rewrite
   matches on is only registered above -Oclassic. *)

[@@@ocaml.flambda_o3]

external compare_int : int -> int -> int = "%int_compare"

external unsigned_lt : int -> int -> bool = "%int_unsigned_lessthan"

let[@inline never] f x y = unsigned_lt (compare_int x y) 0

let () =
  Printf.printf "%b %b %b\n"
    (f (Sys.opaque_identity 1) (Sys.opaque_identity 2))
    (f (Sys.opaque_identity 2) (Sys.opaque_identity 1))
    (f (Sys.opaque_identity 3) (Sys.opaque_identity 3))
