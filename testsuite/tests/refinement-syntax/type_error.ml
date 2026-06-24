(* TEST
 expect;
*)

(* Type-checking refinement types is not yet implemented: the type-checker
   raises a located "not yet supported" error whenever a refinement type is
   actually checked.  These expect-tests pin that behaviour (and its
   location) for each surface form. *)

let f (x : (int | true)) = x
[%%expect{|
Line 1, characters 11-23:
1 | let f (x : (int | true)) = x
               ^^^^^^^^^^^^
Error: Refinement types are not yet supported
|}]

let g (x : (y : int | y > 0)) = x
[%%expect{|
Line 1, characters 11-28:
1 | let g (x : (y : int | y > 0)) = x
               ^^^^^^^^^^^^^^^^^
Error: Refinement types are not yet supported
|}]

let h (x : {[ true ]}) = x
[%%expect{|
Line 1, characters 11-21:
1 | let h (x : {[ true ]}) = x
               ^^^^^^^^^^
Error: Refinement types are not yet supported
|}]
