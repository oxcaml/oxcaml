(* TEST
 include stdlib_stable;
 expect;
*)

open Stdlib_stable

type t : bits64
external bad_advance : t -> int -> t = "%unsafe_idx_array_advance"
let f (x : t) = bad_advance x 1
[%%expect{|
type t : bits64
external bad_advance : t -> int -> t = "%unsafe_idx_array_advance"
Line 5, characters 16-31:
5 | let f (x : t) = bad_advance x 1
                    ^^^^^^^^^^^^^^^
Error: Unable to determine the array kind for array index primitive: the
       type of its first argument, "t",
       should be equal to a "(_, _) idx_mut" or "(_, _) idx_imm".
|}]

let f (i : (int ref, unit#) Idx_mut.t) = Idx_mut.unsafe_advance i ~by:1
[%%expect{|
Line 1, characters 41-71:
1 | let f (i : (int ref, unit#) Idx_mut.t) = Idx_mut.unsafe_advance i ~by:1
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

let f (i : (int ref, #(string * float#)) Idx_mut.t) =
  Idx_mut.unsafe_advance i ~by:1
[%%expect{|
Line 2, characters 2-32:
2 |   Idx_mut.unsafe_advance i ~by:1
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An unboxed product array or slice element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this operation is performed for an array or slice whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]
